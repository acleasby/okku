(ns okku.lang
  "The beginning of a spike whose intent is to make full Java OO available inside Clojure in a
  Clojure-idiomatic manner."
  (:require [okku.caller :refer :all]
            [annotate.core :refer :all]
            [annotate.records :refer :all]
            [annotate.types :refer :all]
            [potemkin :as p]
            [clojure.string :as s]))


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


