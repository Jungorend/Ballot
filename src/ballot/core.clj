(ns ballot.core
  (:gen-class)
  (:require [datahike.api :as d]
            [clojure.edn :as edn]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]))


(defn create-idents
  "Returns a vector of hashmaps each containing a schema entity.
   This is a helper function to reduce the amount of typing necessary.
   Requires each item to have the following keys: `:ident` `:type`. Optionally,
   `:cardinality` can be set, otherwise will be assumed to be one.
   `:unique` can be set as well to define an attribute as unique.
   
   Type values: :i (long) :s (string) :k (keyword) :r (ref)"
  [idents]
  (mapv (fn [{:keys [id type cardinality unique doc]}]
          (let [i {:db/ident id
                   :db/valueType (case type
                                   :i :db.type/long
                                   :s :db.type/string
                                   :k :db.type/keyword
                                   :r :db.type/ref)
                   :db/cardinality (if (nil? cardinality)
                                     :db.cardinality/one
                                     cardinality)}]
            (reduce #(assoc %1 (first %2) (second %2))
                    i
                    (filter #(not (nil? (second %))) [[:db/doc doc] [:db/unique unique]]))))
        idents))

(def schema
  (create-idents
   [;; Season
    {:id :season/mechanics :type :s}
    {:id :season/number :type :i}
    {:id :season/name :type :s :unique :db.unique/identity}
    {:id :season/creator :type :s}
    ;; Character
    {:id :character/name :type :s}
    {:id :character/exceed-name :type :s}
    {:id :character/innate-ability :type :s}
    {:id :character/exceed-ability :type :s}
    {:id :character/description :type :s}
    {:id :character/exceed-description :type :s}
    {:id :character/gauge-cost :type :i}
    {:id :character/seasons :type :r :cardinality :db.cardinality/many}
    ;; Card
    {:id :card/id :type :k :unique :db.unique/identity}
    {:id :card/name :type :s}
    {:id :card/description :type :s}
    {:id :card/type :type :k :doc "options: :special :ultra :normal :astral-heat :character-card"}
    {:id :card/cost :type :i}
    {:id :card/min-range :type :i}
    {:id :card/max-range :type :i}
    {:id :card/power :type :i}
    {:id :card/speed :type :i}
    {:id :card/armor :type :i}
    {:id :card/guard :type :i}
    {:id :card/owners-mark :type :r}
    {:id :card/boost-name :type :s}
    {:id :card/boost-description :type :s}
    {:id :card/boost-cost :type :i}
    {:id :card/boost-type :type :k :doc "options :transform :instant :continuous :gauge-instant :gauge-continuous"}
    {:id :card/abilities :type :r :cardinality :db.cardinality/many}
    ;; Deck
    {:id :deck/name :type :s :unique :db.unique/identity}
    {:id :deck/character :type :r}
    {:id :deck/cards :type :r :cardinality :db.cardinality/many}
    ;; Abilities
    {:id :ability/description :type :s :doc "Full text for the ability."}
    {:id :ability/trigger :type :k :doc ":passive, :hit, :before, :after, :cleanup..."}
    {:id :ability/keywords :type :s :cardinality :db.cardinality/many :doc "Common keywords for lookup. Ex: Ignore Armor"}
    {:id :ability/notes :type :s :doc "For rules information and notes."}
    {:id :ability/location :type :k :doc ":attack :boost"}]))

(def entries [{:season/number 1, :season/name "Red Horizon" :season/creator "Level99"}
              {:season/number 2, :season/name "Seventh Cross" :season/creator "Level99" :season/mechanics "In this season you have transforms. For every transform in your transformation area, your exceed cost is reduced by 2. During a Strike, if you hit, you may move your attack to your transformation area during cleanup. You may also discard one card to transform the same of the other card if both are in your hand."}
              {:season/number 3, :season/name "Street Fighter" :season/creator "Level99" :season/mechanics "You have the ability to use Criticals. When setting your attack, you may discard 1 Gauge. If you do, your attack is Critical."}
              {:season/number 4, :season/name "Shovel Knight" :season/creator "Level99"}
              {:season/number 5, :season/name "Blazblue" :season/creator "Level99" :season/mechanics "You have an overdrive area. When you exceed, the cards spent to Exceed are moved to the Overdrive area. If you ever have 0 cards in your Overdrive area, you revert to your normal side. You also have an astral heat which starts outside your deck. If you reshuffle manually, instead of drawing one card at the end of your turn, draw your astral heat."}])

(def seventh-cross (edn/read-string (slurp "resources/seventh_cross.edn")))

(defn describe-attack-card
  [card abilities]
  (let [sorted-abilities (sort-by #(case (:ability/trigger %)
                                     :passive 1
                                     :now 2
                                     :before 3
                                     :hit 4
                                     :after 5
                                     :cleanup 6)
                                  abilities)
        attacks (filter #(= :attack (:ability/location %)) sorted-abilities)
        boosts (filter #(= :boost (:ability/location %)) sorted-abilities)
        range (if (= (:card/min-range card) (:card/max-range card))
                (:card/min-range card)
                (str (:card/min-range card) " ~ " (:card/max-range card)))]
    (str (:card/name card) "\n"
         (if (= (:card/type card) :ultra)
           (str (:card/cost card) " Gauge.\n")
           (when (> (:card/cost card) 0)
             (str (:card/cost card) " Force.\n")))
         "Power: " (:card/power card) " | Speed: " (:card/speed card) " | Range: " range
         (when (> (:card/armor card) 0) (str " | Armor: " (:card/armor card)))
         (when (> (:card/guard card) 0) (str " | Guard: " (:card/guard card))) "\n"
         (reduce #(str %1 (:ability/description %2) "\n") "" attacks)
         "\n" (:card/boost-name card)
         (cond
           (= (:card/boost-type card) :transform) " (T)"
           (= (:card/boost-type card) :instant) (str " - " (:card/boost-cost card) " Force.")
           (= (:card/boost-type card) :continuous) (str " - " (:card/boost-cost card) " Force. (+)")
           (= (:card/boost-type card) :gauge-instant) (str " - " (:card/boost-cost card) " Gauge.")
           :else (str " - " (:card/boost-cost card) " Gauge (+)"))
         "\n"
         (reduce #(str %1 (:ability/description %2) "\n") "" boosts))))

(defn describe-character-card
 [card]
 (str (:card/name card) "\n"
      (:card/description card)))

(defn display-card
  "Returns a string in a clean format showing the information of an Exceed card.
   Expects an id for the card."
  [card-id conn]
  (let [card (d/pull @conn '["*"] [:card/id card-id])]
    (if (= :character (:card/type card))
      (describe-character-card card)
      (describe-attack-card card (:card/abilities (d/pull @conn '[:ability/description :ability/location :ability/trigger :ability/notes {:card/abilities ...}]
                                                          [:card/id card-id]))))))

(defn remove-unsupported-characters
  [string]
  (apply str (remove #(#{\. \' \:} %) string)))

(defn lookup-card
  [card-name conn]
  (let [card-keyword (-> (clojure.string/join "-" card-name)
                         (clojure.string/lower-case)
                         (remove-unsupported-characters)
                         (keyword))
        card (d/q `[:find ?id
                    :where [?id :card/id ~card-keyword]]
                  @conn)]
    (if (empty? card)
      (let [names (d/q `[:find ?card-id :where [?id :card/id ?card-id]
                         [?id :card/name ~(clojure.string/join " " card-name)]]
                       @conn)]
        (cond (empty? names) "No cards could be found with that name."
              (= (count names) 1) (display-card (first (first names)) conn)
              :else (str "Multiple potential cards. Try one of the following:\n"
                         (reduce #(str %1 "!card " (let [s (clojure.string/split (name (first %2)) #"-")]
                                            (-> (clojure.string/join " " s)
                                                (remove-unsupported-characters))) "\n")
                                 "" names))))
      (display-card card-keyword conn))))

(def cfg {:store {:backend :file :path "db"}})

(d/create-database cfg)
(def conn (d/connect cfg))
(d/transact conn schema)
(d/transact conn entries)
(d/transact conn seventh-cross)


(def state (atom nil))
(def bot-id (atom nil))

(def config (edn/read-string (slurp "config.edn")))

(defmulti handle-event (fn [type _data] type))

(defn toggle-role!
  [user-id new-role]
  (let [role-id (get-in config [:roles new-role])
        user-roles (:roles @(discord-rest/get-guild-member! (:rest @state) (:server-id config) user-id))]
    (if (some #(= role-id %) user-roles)
      (discord-rest/remove-guild-member-role! (:rest @state) (:server-id config) user-id role-id)
      (discord-rest/add-guild-member-role! (:rest @state) (:server-id config) user-id role-id))))

(defn update-role!
  "Sets a user to a role based on the emoji they react with"
  [user-id emoji]
  (let [e (:emoji config)]
    (condp = emoji
      (:heart e)        (toggle-role! user-id :tester)
      (:black-heart e)  (toggle-role! user-id :designer)
      (:yellow-heart e) (toggle-role! user-id :east-coast)
      (:purple-heart e) (toggle-role! user-id :west-coast)
      (:blue-heart e)   (toggle-role! user-id :oceania)
      (:europe e)       (toggle-role! user-id :europe)
      nil)))

(defmethod handle-event :message-reaction-add
  [_ {:keys [message-id channel-id user-id emoji] :as _data}]
  (when (and (= channel-id (:role-channel config))
             (= message-id (:role-message config)))
    (update-role! user-id (:name emoji))
    (discord-rest/delete-user-reaction! (:rest @state) channel-id message-id (:name emoji) user-id)))

(defmethod handle-event :message-create
  [_ {:keys [channel-id author mentions] :as _data}]
  (when (some #{@bot-id} (map :id mentions))
    (discord-rest/create-message! (:rest @state) channel-id :content "x")))

(defn update-lfg-queue []
  (swap! state update :lfg-queue (fn [time] (filter #(not= 1 (.compareTo (java.time.LocalDateTime/now)
                                                                         (second %))) time))))
(defn exceed-lookup
  [_ _]
  "")

(defmethod handle-event :message-create
  [_ {:keys [channel-id content mentions author] :as _data}]
  (let [first-word (first (clojure.string/split content #" "))
        args (rest (clojure.string/split content #" "))]
    (cond
      (some #{@bot-id} (map :id mentions)) (discord-rest/create-message! (:rest @state) channel-id :content (:help config))
      (= "!help" first-word) (discord-rest/create-message! (:rest @state) channel-id :content (:help config))
      (= "!character" first-word) (let [description (exceed-lookup args :characters)]
                                    (when description (discord-rest/create-message! (:rest @state) channel-id :content (str "```" description "```"))))
      (= "!card" first-word) (let [description (lookup-card (clojure.string/join " " args) conn)]
                               (when description (discord-rest/create-message! (:rest @state) channel-id :content (str "```" description "```"))))
      (= "!lfg" first-word) (do (update-lfg-queue)
                                (cond (empty? (:lfg-queue @state)) (let [time (if (empty? args)
                                                                                60
                                                                                (Integer/parseInt (first args)))]
                                                                     (discord-rest/create-message! (:rest @state) channel-id :content (str "You have been added to the queue for " time " minutes."))
                                                                     (swap! state assoc :lfg-queue [[author (.plusMinutes (java.time.LocalDateTime/now) time)]]))
                                      (some #(= (:id author) %) (mapv #(:id (first %)) (:lfg-queue @state))) (do (swap! state update :lfg-queue
                                                                                                                        (fn [queue] (filter #(not= (:id author) (:id (first %))) queue)))
                                                                                                                 (discord-rest/create-message! (:rest @state) channel-id :content "You have been removed from the queue."))
                                      :else (do (discord-rest/create-message! (:rest @state) channel-id :content (str "Someone is available for a match! Please reach out to "
                                                                                                                      (:username (first (first (:lfg-queue @state))))
                                                                                                                      "."))
                                                (swap! state update :lfg-queue #(drop 1 %)))))
      :else nil)))

(defmethod handle-event :ready
  [_ _]
  (discord-ws/status-update! (:gateway @state) :activity (discord-ws/create-activity :name (:playing config))))

(defmethod handle-event :default [_ _])

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guilds :guild-messages :guild-message-reactions))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (try
    (message-pump! (:events @state) handle-event)
    (finally (stop-bot! @state))))