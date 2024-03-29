(ns ballot.core
  (:gen-class)
  (:require [datahike.api :as d]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]
            [ballot.deck :as deck]))

;; NOTE: For Power/Speed/Range -1 is X, and -2 is N/A

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
    {:id :character/id :type :k :unique :db.unique/identity}
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
    {:id :card/exceed-description :type :s}
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
    ;; Card Instances
    {:id :card-instance/card :type :r}
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

(def entries [#:season {:number 1 :name "Red Horizon" :creator "Level99"}
              #:season {:number 2 :name "Seventh Cross" :creator "Level99" :mechanics "In this season you have transforms. For every transform in your transformation area, your exceed cost is reduced by 2. During a Strike, if you hit, you may move your attack to your transformation area during cleanup. You may also discard one card to transform the same of the other card if both are in your hand."}
              #:season {:number 3 :name "Street Fighter" :creator "Level99" :mechanics "When setting your attack, you may discard 1 Gauge. If you do, your attack is Critical."}
              #:season {:number 4 :name "Shovel Knight" :creator "Level99"}
              #:season {:number 5 :name "Blazblue" :creator "Level99" :mechanics "You have an overdrive area. When you exceed, the cards spent to Exceed are moved to the Overdrive area. If you ever have 0 cards in your Overdrive area, you revert to your normal side. You also have an astral heat which starts outside your deck. If you reshuffle manually, instead of drawing one card at the end of your turn, draw your astral heat."}
              #:season {:number 6 :name "Undernight" :creator "Level99"}
              #:season {:number 7 :name "Guilty Gear" :creator "Level99" :mechanics "In this season characters have cancels. After resolving a boost, if the boost was cancelable, you may spend 1 Gauge in order to take another action."}])
;; TODO: Have a mechanics: explanation
;; TODO: Have characters display gauge cost amount

(def card-shorthand
  {"juno-live" :s1-juno-live})

(defn print-stats
  "Stores stats as numbers. -1 refers to X, and -2 refers to N/A. This converts them to a string if so."
  [stat]
  (cond (= -1 stat) "X"
        (= -2 stat) "N/A"
        :else stat))

(defn boost-description [card]
  (str (:card/boost-name card)
       (case (:card/boost-type card)
         :transform " (T)"
         :trap (str " - " (:card/boost-cost card) " Force. (Trap)")
         :instant (str " - " (:card/boost-cost card) " Force.")
         :continuous (str " - " (:card/boost-cost card) " Force. (+)")
         :cancelable-continuous (str " - " (:card/boost-cost card) " Force. Cancelable. (+)")
         :cancelable-instant (str " - " (:card/boost-cost card) " Force. Cancelable.")
         :gauge-instant (str " - " (:card/boost-cost card) " Guage.")
         (str " - " (:card/boost-cost card) " Gauge. (+)"))
       "\n"))

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
                (print-stats (:card/min-range card))
                (str (print-stats (:card/min-range card)) " ~ " (print-stats (:card/max-range card))))]
    (str (:card/name card) "\n"
         (if (or (= (:card/type card) :astral)
                 (= (:card/type card) :ultra))
           (str (print-stats (:card/cost card)) " Gauge.\n")
           (when (> (:card/cost card) 0)
             (str (:card/cost card) " Force.\n")))
         "Range: " range " | Power: " (print-stats (:card/power card)) " | Speed: " (print-stats (:card/speed card))
         (when (not= (:card/armor card) 0) (str " | Armor: " (print-stats (:card/armor card))))
         (when (not= (:card/guard card) 0) (str " | Guard: " (print-stats (:card/guard card)))) "\n"
         (when (= (:card/type card) :astral)
           (str "\nThis is an Astral. It begins sealed. If you manually reshuffle your deck, instead of drawing a card at the end of your turn, put this into your hand.\n"))
         (if (:card/description card) (:card/description card) "") "\n"
         (reduce #(str %1 (:ability/description %2) "\n") "" attacks)
         "\n"
         (boost-description card)
         "\n"
         (reduce #(str %1 (:ability/description %2) "\n") "" boosts))))

; Note: Presently does not report on any ability fields. Characters like King Knight's Decrees aren't searchable as a result.
(defn describe-character-card
  [card]
  (str (:card/name card)
       (when (:card/boost-type card)
         (boost-description card)) "\n"
       (:card/description card) "\n"
       (:card/exceed-description card) "\n"))

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

(defn equal-strings?
  "Returns true if both strings are equal regardless of capitalization"
  [dictionary-name string2]
  (clojure.string/includes? (.toLowerCase (remove-unsupported-characters dictionary-name)) (remove-unsupported-characters (.toLowerCase string2))))

(defn lookup-card
  [card-name conn]
  (let [safe-name (-> (clojure.string/join "-" card-name)
                      (clojure.string/lower-case)
                      (remove-unsupported-characters))
        card-keyword (keyword safe-name)
        card (d/q `[:find ?id
                    :where [?id :card/id ~card-keyword]]
                  @conn)]
    (cond (seq card) (display-card card-keyword conn)
          (get card-shorthand safe-name)
          (display-card (get card-shorthand safe-name) conn)
          :else (let [names (d/q '[:find ?card-id
                                   :in $ ?card-name
                                   :where
                                   [?id :card/id ?card-id]
                                   [?id :card/name ?name]
                                   [(ballot.core/equal-strings? ?name ?card-name)]]
                                 @conn (clojure.string/join " " card-name))]
                  (cond (empty? names) "No cards could be found with that name."
                        (= (count names) 1) (display-card (first (first names)) conn)
                        :else (str "Multiple potential cards. Try one of the following:\n"
                                   (reduce #(str %1 "!card " (let [s (clojure.string/split (name (first %2)) #"-")]
                                                               (-> (clojure.string/join " " s)
                                                                   (remove-unsupported-characters))) "\n")
                                           "" names)))))))

(defn lookup-boost
  [boost-name conn]
  (let [names (d/q '[:find ?card-id
                     :in $ ?boost-name
                     :where
                     [?id :card/id ?card-id]
                     [?id :card/boost-name ?name]
                     [(ballot.core/equal-strings? ?name ?boost-name)]]
                   @conn (clojure.string/join " " boost-name))]
    (cond (empty? names) "No cards could be found with that boost name."
          (= (count names) 1) (display-card (first (first names)) conn)
          :else (str "Multiple potential cards. Try one of the following:\n"
                     (reduce #(str %1 "!card " (let [s (clojure.string/split (name (first %2)) #"-")]
                                                 (-> (clojure.string/join " " s)
                                                     (remove-unsupported-characters))) "\n")
                             "" names)))))

(defn fill-db
  "Populates a fresh database with Exceed custom data"
  [db]
  (let [r (fn [x] (edn/read-string (slurp (str "resources/" x ".edn"))))]
    (dorun (map #(d/transact db %) [schema entries
                                    (r "normals")
                                    (r "redhorizon")
                                    (r "seventh_cross")
                                    (r "streetfighter")
                                    (r "undernight")
                                    (r "shovelknight")
                                    (r "blazblue")
                                    (r "guiltygear")
                                    deck/s1-decks
                                    deck/s2-decks
                                    deck/s3-decks
                                    deck/s4-decks
                                    deck/s5-decks
                                    deck/s6-decks
                                    deck/s7-decks]))))

(defn create-new-db
  [cfg]
  (let [conn (do (d/delete-database cfg)
                 (d/create-database cfg)
                 (d/connect cfg))]
    (fill-db conn)
    conn))

(def cfg {:store {:backend :file :path "db"}})

(def conn (if (.exists (clojure.java.io/file "db"))
            (d/connect cfg)
            (create-new-db cfg)))

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
      (:diamonds e)     (toggle-role! user-id :europe)
      nil)))

(defmethod handle-event :message-reaction-add
  [_ {:keys [message-id channel-id user-id emoji] :as _data}]
  (when (and (= channel-id (:role-channel config))
             (= message-id (:role-message config)))
    (update-role! user-id (:name emoji))
    (discord-rest/delete-user-reaction! (:rest @state) channel-id message-id (:name emoji) user-id)))

(defn clear-old-lfq-entries!
  "Removes any lfg entries that are past their expiration time"
  []
  (swap! state update :lfg-queue (fn [time] (filter #(not= 1 (.compareTo (java.time.LocalDateTime/now)
                                                                         (second %))) time))))

(defn display-character [deck]
  (let [char (:deck/character deck)
        cards (frequencies (map #(:card-instance/card %) (:deck/cards deck)))
        sorted-cards (sort-by #(case (:card/type (first %))
                                 :normal 5
                                 :special 4
                                 :ultra 3
                                 :astral 2
                                 :character 1
                                 0)
                              (into [] cards))]
    (str (:character/name char) (if (:character/gauge-cost char)
                                  (str " (" (:character/gauge-cost char) "G)\n")
                                  "\n")
         (or (:character/description char) "") "\n"
         (:character/innate-ability char) "\n\n"
         "Exceed Mode: " (or (:character/exceed-name char) "") "\n"
         (or (:character/exceed-description char) "") "\n"
         (:character/exceed-ability char) "\n"
         (reduce #(str %1 (:season/mechanics %2) "\n") "" (:character/seasons char)) "\n"
         "Cards:\n" (reduce #(str %1
                                  (:card/name (first %2))
                                  (if (or (nil? (:card/cost (first %2))) (= 0 (:card/cost (first %2))))
                                    " "
                                    (str " (" (:card/cost (first %2))
                                         (if (or (= :astral (:card/type (first %2)))
                                                 (= :ultra (:card/type (first %2)))) "G" "F") ")"))
                                  " x" (second %2) "\n")
                            "" sorted-cards))))

()

;; No real error handling if number passed in is higher/lower than possibilities.
;; This solution also precludes a character whose name starts with a number.
(defn lookup-character
  [character conn]
  (let [number? (if (re-matches #"\d+" (first character))
                  (Integer/parseInt (first character))
                  nil)
        safe-name (-> (s/join " " (if number? (rest character) character))
                      (remove-unsupported-characters))
        characters (d/q '[:find ?deck :in $ ?name :where
                          [?deck :deck/name ?deck-name]
                          [(ballot.core/equal-strings? ?deck-name ?name)]]
                        @conn safe-name)]
    (cond (= 1 (count characters)) (display-character (d/entity @conn (ffirst characters)))
          (empty? characters) "No characters found."
          number? (display-character (d/entity @conn (first (nth (vec characters) (dec number?)))))
          :else (str "Multiple potential characters found. Please try again with one of the following:\n"
                     (loop [number 1
                            results ""
                            remaining-characters characters]
                       (if (empty? remaining-characters)
                         results
                         (recur (inc number)
                                (str results "\"" number " " (:character/name (:deck/character (d/entity @conn (ffirst remaining-characters)))) "\"\n")
                                (rest remaining-characters))))))))

(defn stat-search
  [args]
  (cond (or (nil? (#{"power" "speed" "armor" "guard" "range"} (.toLowerCase (first args))))
            (nil? (#{"=" ">" ">=" "<" "<="} (second args)))) nil
        (= "range" (.toLowerCase (first args))) (let [r (clojure.string/split (nth args 2) #"~")
                                                      min-range (Integer/parseInt (get r 0))
                                                      max-range (if (= 1 (count r))
                                                                  min-range
                                                                  (Integer/parseInt (get r 1)))]
                                                  (apply conj '[[?e :card/min-range ?min-range]
                                                                [?e :card/max-range ?max-range]]
                                                         (case (second args)
                                                           "=" `[[(= ~'?min-range ~min-range)]
                                                                 [(= ~'?max-range ~max-range)]]
                                                           ">" `[[(> ~'?min-range ~min-range)]]
                                                           ">=" `[[(>= ~'?min-range ~min-range)]]
                                                           "<" `[[(< ~'?min-range ~min-range)]]
                                                           "<=" `[[(<= ~'?min-range ~min-range)]])))
        :else (let [stat-type (keyword (.toLowerCase (str "card/" (first args))))
                    comparison (symbol (second args))
                    stat-keyword (->> (first args)
                                      (.toLowerCase)
                                      (str "?")
                                      (symbol))
                    value (Integer/parseInt (nth args 2))]
                `[[~'?e ~stat-type ~(symbol stat-keyword)]
                  [(~comparison ~stat-keyword ~value)]])))

(defn any-strings-equal?
  [description strings]
  (some #(equal-strings? description %) strings))

(defn text-search
  "Assembles a datalog :where clause vector for the card filters
  passed in."
  [args]
  (let [texts (clojure.string/split (clojure.string/join " " (rest args)) #" -or ")
        trigger (case (first args)
                  "-b" :before
                  "-h" :hit
                  "-a" :after
                  "-o" :boost
                  nil)]
    (apply conj `[[~'?e :card/abilities ~'?ability]
                  [~'?ability :ability/description ~'?description]]
           `[(ballot.core/any-strings-equal? ~'?description ~texts)]
           (cond
             (nil? trigger) []
             (= trigger :boost) `[[~'?ability :ability/location ~trigger]]
             :else `[[~'?ability :ability/trigger ~trigger]]))))

(defn search-cards
  [args conn]
  (loop [filters []
         a args]
    (if (empty? a)
      (if (empty? filters)
        "No valid search queries provided."
        (reduce #(str %1 (first %2) "\n") "Found the following cards:\n" (d/q (apply conj '[:find ?card-name
                                                                                            :where [?e :card/name ?card-name]]
                                                                                     filters) @conn)))
      (cond
        (= (first a) "-s") (recur (let [update (stat-search (take 3 (rest a)))]
                                    (if update
                                      (apply conj filters update)
                                      filters)) (drop 4 a))
        (re-matches #"-[abhcpo*]" (first a)) (let [len (-> (clojure.string/join " " a)
                                                           (clojure.string/split #"-[abhcpo\*]\s+")
                                                           (second)
                                                           (clojure.string/split #"\s+")
                                                           (count)
                                                           (inc))
                                                   update (text-search (take len a))]
                                               (recur (if update
                                                        (apply conj filters update)
                                                        filters) (drop len a)))
        :else (recur filters (rest a))))))

(defn post-message!
  "Posts a message to the channel server in block quotes. If no content, does nothing"
  ([content channel-id] (post-message! content channel-id "text"))
  ([content channel-id format]
   (when content
     (discord-rest/create-message! (:rest @state) channel-id :content (str "```" format "\n" content "```")))))

(def nomoretfs "https://cdn.discordapp.com/attachments/816004662754541628/981942732845088808/FirstCustomsRule.png")
(def guiltygear "https://cdn.discordapp.com/attachments/819408415851544638/1139459811474411590/S7.png")
(def playerdb "The Player Database is located here: https://docs.google.com/spreadsheets/d/1dYfLiUI0cacy8-SJ6UZF6WWcRSUVVUCdp_LzYHq95Qo/edit?usp=sharing")
(def tts-mod-link "https://steamcommunity.com/sharedfiles/filedetails/?id=1430620409&searchtext=exceed")
(def json "* Installing Customs
Exceed customs are stored in a file called a 'json'. We call them this as the file format ends in a .json extension.
Please first ensure you have the most recent json of the character you want to try downloaded.

Next, you need to place it in your Saved Objects directory. The base directory depends on your Operating System:
- Windows: %USERPROFILE%\\Documents\\My Games\\Tabletop Simulator
- Mac: ~/Library/Tabletop Simulator
- Linux: ~/.local/share/Tabletop Simulator

Inside this directory will be a 'Saves/Saved Objects' directory. Place your json file in here.

Lastly, when you are in Tabletop Simulator, you can access the custom by going to the Object > Saved Objects section of the UI.
You can only access this menu if you have either created the room or been promoted.
")

;; This method is called whenever a new message appears on the server.
;; If Ballot is pinged, it will send the help message.
;; Otherwise it will check the first word of the message and see if it matches any of the below keywords.
(defmethod handle-event :message-create
  [_ {:keys [channel-id content mentions author] :as _data}]
  (let [[first-word & args] (clojure.string/split content #" ")]
    (if (some #{@bot-id} (map :id mentions)) (discord-rest/create-message! (:rest @state) channel-id :content (:help config))
        (case first-word
          "!help"       (discord-rest/create-message! (:rest @state) channel-id :content (:help config))
          "!nomoretfs"  (discord-rest/create-message! (:rest @state) channel-id :content nomoretfs)
          "!guiltygear" (discord-rest/create-message! (:rest @state) channel-id :content guiltygear)
          "!customs"    (post-message! json channel-id "markdown")
          "!playerdb"   (discord-rest/create-message! (:rest @state) channel-id :content playerdb)
          "!character"  (post-message! (lookup-character args conn) channel-id)
          "!card"       (post-message! (lookup-card args conn) channel-id)
          "!boost"      (post-message! (lookup-boost args conn) channel-id)
          "!search"     (post-message! (search-cards args conn) channel-id)
          "!tts"        (discord-rest/create-message! (:rest @state) channel-id :content tts-mod-link)
          "!lfg"        (do (clear-old-lfq-entries!)
                            (cond (empty? (:lfg-queue @state)) (let [time (if (empty? args)
                                                                            60
                                                                            (Integer/parseInt (first args)))]
                                                                 (discord-rest/create-message! (:rest @state) channel-id :content (str "You have been added to the queue for " time " minutes."))
                                                                 (swap! state assoc :lfg-queue [[author (.plusMinutes (java.time.LocalDateTime/now) time)]]))
                                  (some #(= (:id author) %) (mapv #(:id (first %)) (:lfg-queue @state))) (do (swap! state update :lfg-queue
                                                                                                                    (fn [queue] (filter #(not= (:id author) (:id (first %))) queue)))
                                                                                                             (discord-rest/create-message! (:rest @state) channel-id :content "You have been removed from the queue."))
                                  :else (do (discord-rest/create-message! (:rest @state) channel-id :content (str "Someone is available for a match! Please reach out to "
                                                                                                                  (mention-user (:id (first (first (:lfg-queue @state))))) "."))
                                            (swap! state update :lfg-queue #(drop 1 %)))))
          nil))))

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
