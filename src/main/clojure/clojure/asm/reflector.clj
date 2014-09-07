(ns clojure.asm.reflector
  (:require [clojure.asm.clang :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.asm.type-mapper :refer [clang->llvm]])
  (:import (clojure.asm ClangLibrary$CXTranslationUnit
                        ClangLibrary$CXType$ByValue
                        ClangLibrary$CXCursor)
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

(defmacro with-translation-unit
  [import & body]
  `(do (clang-enable-stack-traces)
       (clang-toggle-crash-recovery 1)
       (let [idx# (clang-create-index 0 1)
             tu# (PointerByReference.)
             status# (try
                       (when-let [path# (resolve-import-path ~import)]
                         (clang-parse-translation-unit2
                          idx# path# *args*
                          (count *args*) nil 0
                          (bit-or CXTranslationUnit_DetailedPreprocessingRecord
                                  CXTranslationUnit_PrecompiledPreamble)
                          tu#))
                       (catch Throwable t#
                         (println (.getMessage t#))))
             tu# (when-let [x# (and (== status# 0) (.getValue tu#))]
                   (clojure.asm.ClangLibrary$CXTranslationUnit. x#))]
         (when tu#
           (binding [*translation-unit* tu#]
             (let [ret# (do ~@body)]
               (clang-dispose-index idx#)
               (clang-dispose-translation-unit tu#)
               ret#))))))

(declare visit-children visit-recursively)

(def cursor-kinds
  (let [names (->> clojure.asm.ClangLibrary$CXCursorKind
                   (clojure.reflect/reflect)
                   (:members)
                   (map :name))]
    (zipmap (map keyword names)
            (map #(eval `(. clojure.asm.ClangLibrary$CXCursorKind ~%)) names))))

(defn reflect
  [import]
  (with-translation-unit import
    (visit-children *translation-unit*)))

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
(defrecord Macro [name type flags])

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

(defn macro? [x]
  (instance? Macro x))

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

(defmethod -cursor-visitor CXCursor_MacroDefinition
  [cursor parent client-data]
  (Macro. (cursor-name cursor) nil (parse-flags cursor)))

(defmethod -cursor-visitor CXCursor_MacroExpansion
  [cursor parent client-data]
  (Macro. (cursor-name cursor) nil (parse-flags cursor)))

(defmethod -cursor-visitor CXCursor_MacroInstantiation
  [cursor parent client-data]
  (Macro. (cursor-name cursor) nil (parse-flags cursor)))

(defmethod -cursor-visitor :default
  [cursor parent client-data]
  nil)

(def ^:dynamic *visitor* nil)

(defprotocol AsCursor
  (cursor [_]))

(extend-protocol AsCursor
  ClangLibrary$CXTranslationUnit
  (cursor [x]
    (clang-get-translation-unit-cursor x))
  ClangLibrary$CXType$ByValue
  (cursor [x]
    (clang-get-type-declaration x))
  ClangLibrary$CXCursor
  (cursor [x] x))

(defmacro with-visitor
  [f & {:keys [recursive? target] :as options}]
  `(let [tset# (transient #{})
         f# ~f
         target# ~(or target '*translation-unit*)
         recursive# ~recursive?]
     (binding [*visitor* (reify clojure.asm.ClangLibrary$CXCursorVisitor
                           (apply [this# cursor# parent# client-data#]
                             (try
                               (when-let [x# (f# cursor# parent# client-data#)]
                                 (conj! tset# x#))
                               (if recursive#
                                 CXChildVisit_Recurse
                                 CXChildVisit_Continue)
                               (catch Throwable t#
                                 (println (.getMessage t#))))))]
       (.put strong-references (hash *visitor*) *visitor*)
       (clang-visit-children (cursor target#) *visitor* nil)
       (persistent! tset#))))

(defn visit-children
  [x]
  (with-visitor -cursor-visitor :target x))

(defn visit-recursively
  [x]
  (with-visitor -cursor-visitor :recursive? true :target x))
