(ns clojure.asm.api
  (:require [clojure.reflect :refer [reflect]]
            [clojure.string :as str])
  (:import (java.lang.reflect Field Method)))

(defn primitive?
  [type]
  (case type
    (boolean byte char short int long float double) true
    false))

(defn array-type?
  [parameter-type]
  (boolean (re-find #"<>" (name parameter-type))))

(defn valid-type-hint?
  [type]
  (if (primitive? type)
    (or (= type Long/TYPE)
        (= type Double/TYPE))
    (not (array-type? type))))

(defn with-hint
  [arg parameter-type]
  (with-meta arg
    {:tag (case parameter-type
            void nil
            boolean Boolean
            byte Long/TYPE
            char Long/TYPE
            short Long/TYPE
            int Long/TYPE
            long Long/TYPE
            float Double/TYPE
            double Double/TYPE
            boolean<> booleans
            byte<> bytes
            char<> chars
            short<> shorts
            int<> ints
            long<> longs
            float<> floats
            double<> doubles
            (when-not (array-type? parameter-type)
              parameter-type))}))

(defn with-type-hint
  [parameter-type]
  (let [arg (-> (name parameter-type) (str/split #"\.") peek gensym)]
    (with-hint arg parameter-type)))

(defn lispify
  [sym]
  (let [s (->> (partition-by #(Character/isUpperCase ^Character %)
                             (str/replace (name sym) #"_" "-"))
               (reduce (fn [s chars]
                         (if (and (Character/isUpperCase ^Character
                                                         (first chars))
                                  (seq s))
                           (apply str s "-" (first chars) (rest chars))
                           (apply str s chars)))
                       "")
               (str/lower-case))
        s (str/replace s #"--" "-")]
    (symbol (cond-> s
              (.startsWith s "get") (str/replace #"get-" "")
              (.startsWith s "set") (str "!")
              (and (.startsWith s "is")
                   (not (Character/isDigit ^Character (nth s 2))))
              (-> (str/replace #"^is-?" "") (str "?"))
              (and (.startsWith s "is")
                   (Character/isDigit ^Character (nth s 2)))
              (str "?")))))

(defn dedupe-arities
  [fn-methods]
  (reduce (fn [fn-methods {:keys [parameter-types] :as fn-method}]
            (let [args (map :parameter-types fn-methods)
                  arities (set (map count args))
                  argc (count parameter-types)]
              (if-not (contains? arities argc)
                (conj fn-methods fn-method)
                fn-methods)))
          [] fn-methods))

(defmacro generate-wrapper-api
  [classname & {:keys [lispify-names? object-oriented? static-object]}]
  (let [class (resolve classname)]
    `(do ~@(for [{:keys [name type flags] :as member} (:members (reflect class))
                 :when (and (:static flags)
                            (:public flags)
                            (instance? clojure.reflect.Field member))]
             `(def ~(with-meta name
                      {:const true :tag type}) (. ~class ~name)))
         ~@(for [fn-methods (->> (if object-oriented?
                                   (remove #(:static (:flags %))
                                           (:members (reflect class)))
                                   (filter #(:static (:flags %))
                                           (:members (reflect class))))
                                 (filter #(instance? clojure.reflect.Method %))
                                 (filter #(:public (:flags %)))
                                 (group-by :name)
                                 vals
                                 (map dedupe-arities))
                 :let [name (:name (first fn-methods))
                       inline-arities (->> (map :parameter-types fn-methods)
                                           (map (fn [types]
                                                  (if object-oriented?
                                                    (inc (count types))
                                                    (count types))))
                                           (into #{}))
                       name' (if lispify-names?
                               (lispify (:name (first fn-methods)))
                               (:name (first fn-methods)))
                       return-type (:return-type (first fn-methods))
                       types (map :parameter-types fn-methods)
                       args (map (fn [{:keys [parameter-types]}]
                                   (mapv with-type-hint
                                         (cond
                                           static-object parameter-types
                                           object-oriented?
                                           (cons (symbol (pr-str class))
                                                 parameter-types)
                                           :else parameter-types)))
                                 fn-methods)
                       args (map (fn [parameter-types]
                                     (mapv #(with-meta % {}) parameter-types))
                                   args)]]
             `(defn ~name'
                ""
                {:inline
                 (fn ~@(map (fn [class name args types]
                              (cond
                                static-object
                                `(~args (list '. '~static-object '~name
                                                ~@args))
                                object-oriented?
                                `(~args (list '. ~(first args) '~name
                                              ~@(rest args)))
                                :else `(~args (list '. ~class '~name ~@args))))
                            (repeat class) (repeat name) args types))
                 :inline-arities ~inline-arities}
                ~@(map (fn [class name args types]
                         (cond
                           static-object
                           (list args (list* '. static-object name args))
                           object-oriented?
                           (list args (list* '. (first args) name (rest args)))
                           :else (list args (list* '. class name args))))
                       (repeat class) (repeat name) args types))))))
