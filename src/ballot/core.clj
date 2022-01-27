(ns exceed-db.core
  (:require [datahike.api :as d]
            [clojure.edn :as edn]))

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

(def cfg {:store {:backend :file :path "db"}})

(d/create-database cfg)
(def conn (d/connect cfg))
(d/transact conn schema)

(def entries [{:season/number 1, :season/name "Red Horizon" :season/creator "Level99"}
              {:season/number 2, :season/name "Seventh Cross" :season/creator "Level99" :season/mechanics "In this season you have transforms. For every transform in your transformation area, your exceed cost is reduced by 2. During a Strike, if you hit, you may move your attack to your transformation area during cleanup. You may also discard one card to transform the same of the other card if both are in your hand."}
              {:season/number 3, :season/name "Street Fighter" :season/creator "Level99" :season/mechanics "You have the ability to use Criticals. When setting your attack, you may discard 1 Gauge. If you do, your attack is Critical."}
              {:season/number 4, :season/name "Shovel Knight" :season/creator "Level99"}
              {:season/number 5, :season/name "Blazblue" :season/creator "Level99" :season/mechanics "You have an overdrive area. When you exceed, the cards spent to Exceed are moved to the Overdrive area. If you ever have 0 cards in your Overdrive area, you revert to your normal side. You also have an astral heat which starts outside your deck. If you reshuffle manually, instead of drawing one card at the end of your turn, draw your astral heat."}])

(def seventh-cross (edn/read-string (slurp "resources/seventh_cross.edn")))

(defn display-card
  "Returns a string in a clean format showing the important information of an Exceed attack card. 
   Expects an id for the card."
  [card-id]
  (let [card (d/pull @conn '["*"] [:card/id card-id])
        abilities (:card/abilities (d/pull @conn '[:ability/description :ability/location :ability/trigger :ability/notes {:card/abilities ...}]
                                           [:card/id card-id]))
        sorted-abilities (sort-by #(case (:ability/trigger %)
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

(d/transact conn entries)
(d/transact conn seventh-cross)

(d/q '[:find ?name
       :where
       [?card :card/name ?name]
       [?card :card/speed ?speed]
       [(> ?speed 4)]]
     @conn)

(d/q '[:find ?name ?desc
       :where
       [?card :card/name ?name]
       [?card :card/abilities ?ability]
       [?ability :ability/trigger :passive]
       [?ability :ability/description ?desc]]
     @conn)

(d/q '[:find ?name ?season
       :where
       [?character :character/name ?name]
       [?character :character/seasons ?sid]
       [?sid :season/name ?season]]
     @conn)
