(ns okku.lang
  "The beginning of a spike whose intent is to make full Java OO available inside Clojure in a
  Clojure-idiomatic manner."
  (:require [okku.caller :refer :all]
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



(defmacro let-fnmap
  "A version of letfn that returns its functions in a map.
If a result is computed in the body, let-fnmap returns a vector
containing the map of local variables followed by the result."
  [fn-exprs & body]
  (let [vars (map (fn [[var form]] [(keyword var) var]) (partition 2 fn-exprs))
        has-body (not (empty? body))]
    `(let [~@fn-exprs
           result# (do ~@body)
           mapvars# (into {} [~@vars])]
       (if ~has-body
         [mapvars# result#]
         mapvars#))))


