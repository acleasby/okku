(ns okku.core
  (import [akka.actor ActorRef ActorSystem Props UntypedActor
           UntypedActorFactory]
          [akka.routing RoundRobinRouter]
          [com.typesafe.config ConfigFactory])
  (require [clojure.walk :as w]))

(defn round-robin-router [n]
  "Creates a round-robin router with n replicas."
  (RoundRobinRouter. n))

(defn actor-system [name & {:keys [config file port local hostname]
                            :or {file "application"
                                 config false
                                 port 2552
                                 hostname "127.0.0.1"
                                 local false}}]
  "Creates a new actor system.
  config should be the name of the corresponding section in the application.conf file.
  file should be the name of the config file (.conf appended by the library).
  port should be the port number for this ActorSystem (lower priority than config file).
  local creates a local actor system (port option is then ignored, default to false)."
  (ActorSystem/create
    name
    (ConfigFactory/load
      (-> (ConfigFactory/parseResourcesAnySyntax file)
        (#(if config (.getConfig % config) %))
        (#(if-not local
            (.withFallback %
              (ConfigFactory/parseString
                (format "akka.remote.netty.port = %d
                        akka.remote.netty.hostname = \"%s\"
                        akka.actor.provider = akka.remote.RemoteActorRefProvider"
                        port hostname)))
            %))))))

(defmacro !
  "Sends the msg value as a message to target, or to current sender if target
  is not specified. Can only be used inside an actor."
  ([msg] `(.tell (.getSender ~'this) ~msg (.getSelf ~'this)))
  ([target msg] `(.tell ~target ~msg (.getSelf ~'this))))

(defmacro dispatch-on [dv & forms]
  `(cond ~@(mapcat (fn [[v f]] `[(= ~dv ~v) ~f]) (partition 2 forms))
         :else (.unhandled ~'this ~dv)))

(defmacro spawn [name args & {c :in r :router n :name}]
  (let [c (if c c '(.getContext this))
        p (#(if r `(.withRouter ~% ~r) %) (cons name args))]
    (if n `(.actorOf ~c ~p ~n)
      `(.actorOf ~c ~p))))

(defn look-up [address & {s :in}]
  (if-not s (throw (IllegalArgumentException. "okku.core/look-up needs an :in argument")))
  (.actorFor s address))

(defmacro stop []
  '(.stop (.getContext this) (.getSelf this)))

(defmacro shutdown []
  '(-> this .getContext .system .shutdown))

(defn extract-let [forms]
  (if (and (= (count forms) 1)
           (= (first (first forms)) 'let))
    [(second (first forms)) (drop 2 (first forms))]
    [nil forms]))

(defmacro defactor [aname [& arglist] & forms]
  (let [[binds forms] (extract-let forms)]
    `(defn ~aname [~@arglist]
       (Props. (proxy [UntypedActorFactory] []
                 (~'create []
                   (let [~@binds]
                     (proxy [UntypedActor] []
                       ~@forms))))))))
