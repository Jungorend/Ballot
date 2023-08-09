(ns ballot.card-generator)

(defn de-key
  [token]
  (clojure.string/join " " (map clojure.string/capitalize (rest (clojure.string/split (str token) #"-")))))

(defn create-cards
  [deck-name character season & cards]
  (let [create-deck-str
        (str "(create-deck \"" deck-name "\" " character "\n"
             (reduce (fn [r c]
                       (str r "  " (first c) " " (second c) "\n"))
                     "" (partition 2 cards))
             ")")
        character-cards (str "#:character {:name \"" deck-name "\"\n"
                             "             :id " character "\n"
                             "             :innate-ability \"\"\n"
                             "             :exceed-ability \"\"\n"
                             "             :gauge-cost X\n"
                             "             :seasons {:season/name \"" season "\"}}\n\n")
        card-strs (reduce (fn [r c]
                            (str r
                                 "#:card {:name \"" (de-key (first c)) "\"\n"
                                 "        :type :special/:ultra\n"
                                 "        :id " (first c) "\n"
                                 "        :cost 0\n"
                                 "        :min-range 0\n"
                                 "        :max-range 0\n"
                                 "        :power 0\n"
                                 "        :speed 0\n"
                                 "        :armor 0\n"
                                 "        :guard 0\n"
                                 "        :boost-name \"\"\n"
                                 "        :boost-cost 0\n"
                                 "        :boost-type :instant\n"
                                 "        :abilities []}\n\n"))
                          "" (partition 2 cards))]
    (str character-cards
         card-strs
         create-deck-str)))

(spit "results.edn" (create-cards "Syrus" :s2-syrus "Seventh Cross"
                                  :s2-albatross-talon 2
                                  :s2-aria-of-the-wind 2
                                  :s2-siren-call 2
                                  :s2-tidal-whirl 2
                                  :s2-treasure-hunter 2
                                  :s2-dredge-fury 2
                                  :s2-symphony-of-the-deep 2))
