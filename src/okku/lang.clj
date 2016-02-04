(ns okku.lang
  "The beginning of a spike whose intent is to make full Java OO available inside Clojure in a
  Clojure-idiomatic manner."
  (:require [okku.caller :refer :all]
            [schema.core :as t :refer [defschema]]
            [potemkin :as p]
            [clojure.string :as s])
  (:import [clojure.lang IFn]))


(defmacro let-map
  "A version of let that returns its local variables in a map.
If a result is computed in the body, let-map returns a vector
containing the map of local variables followed by the result."
  [var-exprs & body]
  (if-not (even? (count var-exprs))
    (throw (IllegalStateException. (str "A let expression must accept an even number of arguments at " *ns* " line " (:line (meta &form))))))
  (let [vars (map (fn [[var form]] [(keyword var) var]) (partition 2 var-exprs))
        has-body (not (empty? body))]
    `(let [~@var-exprs
           result# (do ~@body)
           mapvars# (into {} [~@vars])]
       (if ~has-body
         [mapvars# result#]
         mapvars#))))



(defn extract-letfn-names
  "Given a function in the form expected by letfn, return [(keyword function-name) function-name]."
  [fn-expr]
  (let [fn-name (first fn-expr)]
    [(keyword fn-name) fn-name]))


(defmacro letfn-map
  "A version of letfn that returns its functions in a map.
If a result is computed in the body, let-fnmap returns a vector
containing the map of functions followed by the result."
  [fn-exprs & body]
  (let [fn-map (into {} (map extract-letfn-names fn-exprs))
        has-body (not (empty? body))]
    `(letfn [~@fn-exprs]
       (if ~has-body
         [~fn-map ~@body]
         ~fn-map))))



(defschema InstanceData {t/Keyword t/Any})
(defschema ParameterNames [t/Keyword])
(defschema MethodTable {t/Keyword IFn})


;; An object instance constructed from a function that exposes its parameter values
;; as map keys and a map of methods
(p/def-map-type ObjectInstanceMetadata
  [parameter-values                     ;- InstanceData
   parameter-names                      ;- ParameterNames
   methods]                             ;- MethodTable

  (get [_ k default-value]
       (if (contains? parameter-values k)
         (get parameter-values k)
         default-value))

  (assoc [_ k v]
         (ObjectInstanceMetadata. (assoc parameter-values k v) parameter-names  methods))

  (dissoc [this k]
          (if (contains? parameter-names k)
            (throw (IllegalStateException. (str "Cannot remove a core object parameter value: " k)))
            (ObjectInstanceMetadata. (dissoc parameter-values k) parameter-names methods)))

  (keys [_]
        (keys parameter-values))

  (meta [_]
        (meta parameter-values))

  (with-meta [_ metadata]
    (ObjectInstanceMetadata. (with-meta parameter-values metadata) parameter-names methods)))


(defschema ClassHeader {})

