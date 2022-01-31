(ns ballot.deck)

(defn create-deck
  [deck-name character & cards]
  (let [c (reduce (fn [r [value times]]
                    (concat r (take times (repeat value))))
                  []
                  (partition 2 cards))]
    (reduce (fn [x y]
              (update x :deck/cards #(conj %1 {:card/id %2}) y))
            {:deck/name deck-name
             :deck/character {:character/id character}
             :deck/cards []}
            c)))

(def s2-decks
  [(create-deck "Renea" :s2-renea
                :s2-lethal-force 2
                :s2-anticipation 2
                :s2-strafe-fire 2
                :s2-neutralizer 2
                :s2-called-shot 2
                :s2-paranormal-investigation 2
                :s2-flare 2)
   (create-deck "Remiliss" :s2-remiliss
                :s2-caustic-vent 2
                :s2-ground-zero 2
                :s2-irradiate 2
                :s2-napalm-stream 2
                :s2-toxic-tendrils 2
                :s2-consumption 2
                :s2-nuclear-option 2)
   (create-deck "Iaquis" :s2-iaquis
                :s2-dragons-fire 2
                :s2-dragons-flight 2
                :s2-dragons-spine 2
                :s2-dragons-tail 2
                :s2-dragons-tongue 2
                :s2-dragons-heart 2
                :s2-dragons-descent 2)
   (create-deck "Eugenia" :s2-eugenia
                :s2-absinthin-arrow 2
                :s2-color-spray 2
                :s2-plot-hook 2
                :s2-shimmer-of-madness 2
                :s2-werelight 2
                :s2-wonderlight 1
                :s2-cats-cradle 2
                :s2-queen-of-hearts 2)])