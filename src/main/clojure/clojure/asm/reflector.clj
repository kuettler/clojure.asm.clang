(ns clojure.asm.reflector
  (:require [clojure.asm.clang :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.asm.type-mapper :refer [clang->llvm]])
  (:import (clojure.asm ClangLibrary$CXTranslationUnit
                        ClangLibrary$CXType$ByValue)
           (com.sun.jna Pointer)
           (com.sun.jna.ptr PointerByReference)
           (java.nio IntBuffer)
           (org.bridj BridJ)))

(def strong-references (java.util.HashMap.))

(defn parse-flags
  [cursor]
  (let [t (clang-get-cursor-type cursor)]
    (->> [(when (== (clang-is-const-qualified-type t) 1) :const)
          (when (== (clang-is-volatile-qualified-type t) 1) :volatile)
          (when (== (clang-is-restrict-qualified-type t) 1) :restrict)
          (when (== (clang-is-function-type-variadic t) 1) :variadic)]
         (remove nil?)
         (into #{}))))

(defn resolve-header
  [header]
  (first (for [path *include-paths*
               :let [header-path (str path "/" header)
                     file (io/file header-path)]
               :when (.exists file)]
           header-path)))

(defn resolve-import-path
  [import]
  (resolve-header (str (str/replace import #"\." "/") ".h")))

(defn ^ClangLibrary$CXTranslationUnit translation-unit
  [^String path]
  (let [idx ^Pointer (clang-create-index 0 0)
        tu (PointerByReference.)
        status (clang-parse-translation-unit2 idx path *args*
                                              (count *args*) nil 0
                                              CXTranslationUnit_None tu)]
    (when (== status 0)
      (clojure.asm.ClangLibrary$CXTranslationUnit. (.getValue tu)))))

(declare visit-children)

(def cursor-kinds
  (let [names (->> clojure.asm.ClangLibrary$CXCursorKind
                   (clojure.reflect/reflect)
                   (:members)
                   (map :name))]
    (zipmap (map keyword names)
            (map #(eval `(. clojure.asm.ClangLibrary$CXCursorKind ~%)) names))))

(defn reflect
  [import]
  (when-let [header ^String (and import (resolve-import-path (name import)))]
    (let [tu ^ClangLibrary$CXTranslationUnit (translation-unit header)
          ret (visit-children tu)]
      ret)))

(defn source-location-start
  [cursor]
  (clang-get-range-start (clang-get-cursor-extent cursor)))

(defn source-info
  [loc]
  (let [file (PointerByReference.)
        line-buf (IntBuffer/allocate 1)
        column-buf (IntBuffer/allocate 1)]
    (clang-get-file-location loc file line-buf column-buf nil)
    {:file (.getValue file)
     :file-name (clang-get-file-name (.getValue file))
     :line (.get line-buf)
     :column (.get column-buf)}))

(defn cursor-type
  [cursor]
  (get (set/map-invert cursor-kinds) (clang-get-cursor-kind cursor)))

(defrecord Type [name type flags])
(defrecord Constructor [name parameter-types size align flags])
(defrecord Function [name type return-type parameter-types flags])
(defrecord Field [name type declaring-struct flags])
(defrecord Variable [name type flags])

(defn type? [x]
  (instance? Type x))

(defn function? [x]
  (instance? Function x))

(defn field? [x]
  (instance? Field x))

(defn variable? [x]
  (instance? Variable x))

(defn constructor? [x]
  (instance? Constructor x))

(defn cursor-name
  [cursor]
  (symbol (pr-str (clang-get-cursor-spelling cursor))))

(defmulti -cursor-visitor (fn [cursor parent client-data]
                            (clang-get-cursor-kind cursor)))

(defmethod -cursor-visitor CXCursor_StructDecl
  [cursor parent client-data]
  (let [t (clang-get-canonical-type (clang-get-cursor-type cursor))]
    (Constructor. (cursor-name cursor)
                  (clang->llvm t)
                  (clang-type-get-size-of t)
                  (clang-type-get-align-of t)
                  (parse-flags cursor))))

(defmethod -cursor-visitor CXCursor_TypedefDecl
  [cursor parent client-data]
  (Type. (cursor-name cursor)
         (clang->llvm (clang-get-canonical-type (clang-get-cursor-type cursor)))
         (parse-flags cursor)))

(defmethod -cursor-visitor CXCursor_FunctionDecl
  [cursor parent client-data]
  (let [t (clang-get-canonical-type (clang-get-cursor-type cursor))
        return (clang-get-canonical-type (clang-get-result-type t))
        params (mapv (fn [n]
                       (clang-get-canonical-type (clang-get-arg-type t n)))
                     (range (clang-get-num-arg-types t)))]
    (Function. (cursor-name cursor) (clang->llvm t)
               (clang->llvm return) (mapv clang->llvm params)
               (parse-flags cursor))))

(defmethod -cursor-visitor CXCursor_VarDecl
  [cursor parent client-data]
  (let [t (clang-get-canonical-type (clang-get-cursor-type cursor))]
    (Variable. (cursor-name cursor)
               (clang->llvm t)
               (parse-flags cursor))))

(defmethod -cursor-visitor CXCursor_FieldDecl
  [cursor parent client-data]
  (let [t (clang-get-canonical-type (clang-get-cursor-type cursor))]
    (Field. (cursor-name cursor)
            (clang->llvm t)
            (cursor-name parent)
            (parse-flags cursor))))

(defmethod -cursor-visitor :default
  [cursor parent client-data]
  nil)

(defn cursor-visitor
  [tset & {:keys [recursive?]}]
  (let [visitor (reify clojure.asm.ClangLibrary$CXCursorVisitor
                  (apply [this cursor parent client-data]
                    (try
                      (when-let [x (-cursor-visitor cursor parent client-data)]
                        (conj! tset x))
                      (if recursive?
                        CXChildVisit_Recurse
                        CXChildVisit_Continue)
                      (catch Throwable t
                        (println (.getMessage t))))))]
    (.put strong-references (hash visitor) visitor)
    visitor))

(defn visit-children
  [^ClangLibrary$CXTranslationUnit tu]
  (let [tset (transient #{})
        visitor (cursor-visitor tset)]
    (clang-visit-children (clang-get-translation-unit-cursor tu) visitor nil)
    (.remove strong-references (hash visitor))
    (persistent! tset)))

(defn visit-fields
  [^ClangLibrary$CXType$ByValue t]
  (let [tset (transient #{})
        visitor (cursor-visitor tset :recursive? true)]
    (clang-visit-children (clang-get-type-declaration t) visitor nil)
    (.remove strong-references (hash visitor))
    (persistent! tset)))
