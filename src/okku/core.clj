(ns okku.core
  (import [akka.actor ActorRef ActorSystem Props UntypedActor
           UntypedActorFactory]
          [akka.routing RoundRobinRouter]
          [com.typesafe.config ConfigFactory])
  (require [clojure.walk :as w]))

(defn round-robin-router [n]
  "Creates a round-robin router with n replicas."
  (RoundRobinRouter. n))

(defn actor-system [name & {:keys [config]}]
  "Creates a new actor system. config should be the name of the corresponding
  section in the application.conf file."
  (if config
    (ActorSystem/create name (.getConfig (ConfigFactory/load) config))
    (ActorSystem/create name)))

(defmacro !
  "Sends the msg value as a message to target, or to current sender if target
  is not specified. Can only be used inside an actor."
  ([msg] `(.tell (.getSender ~'this) ~msg (.getSelf ~'this)))
  ([target msg] `(.tell ~target ~msg (.getSelf ~'this))))

(defmacro defactor [aname [& arglist] & forms]
  `(defn ~aname [~@arglist & {c# :context r# :router n# :name}]
     (let [p# (Props. (proxy [UntypedActorFactory] []
                        (~'create []
                          (let []
                            (proxy [UntypedActor] []
                              ~@forms)))))
           p# (if r# (.withRouter p# r#) p#)]
       (if n#
         (.actorOf c# p# n#)
         (.actorOf c# p#)))))
