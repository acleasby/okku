(ns okku.object
  "The beginning of a spike whose intent is to make full Java OO available inside Clojure in a
  Clojure-idiomatic manner."
  (:require [okku.caller :refer :all]
            [okku.lang :refer :all]
            [clojure.string :as s]))



;; Define type names; no way to type check right now, but it's still useful for documentation
(def InstanceData {Keyword Any})
(def ParameterNames [Keyword])
(def Methods {Keyword Fn})

;; An object instance constructed from a function that exposes its parameter values
;; as map keys and a map of
(p/def-map-type ObjectInstanceMetadata
  [param-values parameter-names methods] ; :- [InstanceData ParameterNames Methods]

  (get [_ k default-value]
       (if (contains? param-values k)
         (let [v (get param-values k)]
           (if (instance? clojure.lang.Delay v)
             @v
             v))
         default-value))

  (assoc [_ k v]
         (ObjectInstance. (assoc param-values k v)  methods))

  (dissoc [this k]
          (if (contains? parameter-names k)
            (throw (IllegalStateException. (str "Cannot remove a core object parameter value: " k)))
            (ObjectInstance. (dissoc param-values k) methods)))

  (keys [_]
        (keys param-values))

  (meta [_]
        (meta param-values))

  (with-meta [_ metadata]
    (ObjectInstance. (with-meta param-values metadata) methods)))



;(defclass Hello [name :- s/Str] :extends Object :implements [Serializable]
;  "Doc string for class here"
;
;  [count (atom 1)]
;
;  (restart
;   "doc"
;   []
;   (reset! count 0))
;
;  (toString
;   []
;   (str "Hello " name ": " @count))
;
;  (sayhello
;   []
;   (swap! count (fn [i] (+ i 1)))
;   (println (toString)))
;
;  (status
;   []
;   @count))


(defmacro defclass
  "An easier way to use gen-class to make classed in Clojure that extend Java types.
  The purpose here is not to implement every Java class feature in Clojure, but to
  implement enough so that Clojure can more easily be used as a multi-paradigm
  functional-OO language.

  Usage:

  (defclass ClassName [public fields] :extends Supertype :implements [A B C]
  \"Doc string\"
  [private fields]

  Annotationclass {:key :value}
  (method1
    \"doc string\"
    [params]
    forms...)

  (method2
    \"doc string\"
    [params]
    forms...))"
  [class-name & body])


;; @See https://www.deepbluelambda.org/programming/clojure/generate-your-class-and-proxy-it-too
;; ... macroexpands to something like:

;; Use def-map-type from https://github.com/coconutpalm/potemkin ?
;; or maybe code from that to make Class types defined in Clojure implement IPersistentMap
(defn- hello-constructor
 "Create a Hello object."
 [name]

 ;; Object instance data
 (let [count (atom 1)]

   ;; Object methods
   (letfn
     [(restart
        []
        (reset! count 0))

      (toString
        []
        (str "Hello " name ": " @count))

      (sayhello
        []
        (swap! count (fn [i] (+ i 1)))
        (println (toString)))

      (status
        []
        @count)]
     [[] {:name name
           :restart restart
           :toString toString
           :sayhello sayhello
           :status status}])))

(defn hello-restart [this]
  ((:restart this))
  this)

(defn hello-toString [this]
  ((:toString)))

(defn hello-sayhello [this]
  ((:sayhello this))
  this)

(defn hello-status [this]
  ((:status this)))

(defn this-class [name] (symbol (str *ns* "." name)))

(gen-class
  :name (this-class "Hello")
  :extends java.lang.Object
  :implements []
  :prefix "hello-"
  :exposes-methods {toString superToString}
  :methods [[restart [] (this-class "Hello")]
            [toString [] String]
            [sayhello [] (this-class "Hello")]
            [status [] Long]]
  :state state
  :init hello-constructor)


;; gen-class with Java annotations
(gen-class :name ^{Deprecated {}
                   SuppressWarnings ["Warning1"] ; discarded
                   java.lang.annotation.Target []}
                 clojure.test_clojure.genclass.examples.ExampleAnnotationClass
           :prefix "annot-"
           :methods [[^{Deprecated {}
                        Override {}}    ;discarded
                      foo
                      [^{java.lang.annotation.Retention java.lang.annotation.RetentionPolicy/SOURCE
                         java.lang.annotation.Target    [java.lang.annotation.ElementType/TYPE
                                                         java.lang.annotation.ElementType/PARAMETER]}
                       String]
                      void]])


;(when-not *compile-files*
;  (compile (symbol (.toString *ns*))))





;; (def Hello (Clazz. "Hello" "" [] hello (Method. "hello" "Create a Hello object." [(Arg. "name" Object)]) []))


;; Compile the code following this comment into:
;;
;; a (defprotocol ISomething) declaring Something's public API functions
;; a (defprotocol ISomething-test declaring Something's public + test API)
;; a (defrecord Something [--metaclass constructor parameters plus supertype parameters])
;;   containing all methods in Something, implementing ISomething
;; a (defn something [constructor parameters]) returning a Something-impl implementing
;;   ISomething plus all ISupertypes via delegating to the Caller protocol
;; a (def Something (Class. supertypes constructor-fn dispatch-fn methods fields)
;;
;; Implement the Caller protocol over Instance.
;
;(defclass Something [field1 field2 ... fieldn] :extends [(Superclass. arg1 arg2) ...]
;  "Docstring"
;
;  (something
;    "Docstring"
;    [constructor parameters]
;    (Something. field1 field2 ... fieldn))
;
;  (private
;    [private variables]
;    [(private-fn [] :blah)])
;
;  (testable
;    [(testable-fn [] :blah)])
;
;  (public
;    [(public-fn [] :blah)]))


;; From: https://github.com/sjl/caves

(defn make-fnmap
  "Make a function map out of the given sequence of fnspecs.

  A function map is a map of functions that you'd pass to extend.  For example,
  this sequence of fnspecs:

  ((foo [a] (println a)
   (bar [a b] (+ a b)))

  Would be turned into this fnmap:

  {:foo (fn [a] (println a))
   :bar (fn [a b] (+ a b))}

  "
  [fns]
  (into {} (for [[label fntail] (map (juxt first rest) fns)]
             [(keyword label)
              `(fn ~@fntail)])))

(defn make-fnheads
  "Make a sequence of fnheads of of the given sequence of fnspecs.

  A fnhead is a sequence of (name args) like you'd pass to defprotocol.  For
  example, this sequence of fnspecs:

  ((foo [a] (println a))
   (bar [a b] (+ a b)))

  Would be turned into this sequence of fnheads:

  ((foo [a])
   (bar [a b]))

  "
  [fns]
  (map #(take 2 %) fns))


(defmacro defaspect
  "Define an aspect with the given functions and default implementations.

  For example:

  (defaspect Fooable
    (foo [this world]
      (println \"Foo!\"))
    (can-foo? [this world]
      (contains? world :foo)))

  This will define a Clojure protocol Fooable with the given functions as usual.
  It will also attach the function implementations as metadata, which is used by
  the add-aspect macro.  Aside from the metadata, Fooable is a normal Clojure
  protocol.

  "
  [label & fns]
  (let [fnmap (make-fnmap fns)
        fnheads (make-fnheads fns)]
    `(do
       (defprotocol ~label
         ~@fnheads)
       (def ~label
         (with-meta ~label {:defaults ~fnmap})))))


(defmacro add-aspect
  "Add an aspect to an existing record type.

  This is similar to extend-type, with two differences:

  * It must be used on a protocol defined with defaspect
  * It will use the aspect's default function implementation for any functions
    not given.

  This allows us to define common aspect functions (like can-move? and move for
  Mobile) once and only once, while still allowing them to be overridden to
  customize behavior.

  For example:

  (add-aspect Fooer Fooable
    (foo [this world]
      (println \"Bar!\")))

  This will extend the type Fooer to implement the Fooable protocol.  It will
  use the default implementation of can-foo? that was defined in the addaspect
  call, but overrides the implementation of foo to do something special.

  "
  [entity aspect & fns]
  (let [fnmap (make-fnmap fns)]
    `(extend ~entity ~aspect (merge (:defaults (meta ~aspect))
                                    ~fnmap))))

