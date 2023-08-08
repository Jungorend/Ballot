(ns ballot.card-generator)

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
                             "             :seasons {:season/name \"" season "\"}}\n\n")]
    (str character-cards create-deck-str)))
