(ns ballot.deck)

(defn create-deck
  [deck-name character & cards]
  (let [c (reduce (fn [r [value times]]
                    (concat r (take times (repeat value))))
                  []
                  (partition 2 cards))]
    (reduce (fn [x y]
              (update x :deck/cards #(conj %1 {:card-instance/card {:card/id %2}}) y))
            {:deck/name deck-name
             :deck/character {:character/id character}
             :deck/cards []}
            c)))

(def s2-decks
  [(create-deck "Renea" :s2-renea
                :s2-briefcase 1
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
                :s2-wonderland 1
                :s2-absinthin-arrow 2
                :s2-color-spray 2
                :s2-plot-hook 2
                :s2-shimmer-of-madness 2
                :s2-werelight 2
                :s2-wonderlight 1
                :s2-cats-cradle 2
                :s2-queen-of-hearts 2)])

(def s6-decks
  [(create-deck "Linne" :s6-linne
                :s6-the-diviner 2
                :s6-elusive-flash 2
                :s6-divine-blaze 2
                :s6-flying-swallow 2
                :s6-sky-fangs 2
                :s6-tenacious-mist 2
                :s6-moon-gyre 2)
   (create-deck "Hyde" :s6-hyde
                :s6-dead-set-daze 2
                :s6-gyro-vortex 2
                :s6-shadow-scare 2
                :s6-hydecar 3
                :s6-black-orbiter 2
                :s6-red-clad-craver 3
                :s6-vacant-shift 2)
   (create-deck "Carmine" :s6-carmine
                :s6-twist 2
                :s6-be-devoured 2
                :s6-this-is-the-end 2
                :s6-spin 2
                :s6-give-me-that 2
                :s6-pulverize 2
                :s6-thrust 2)
   (create-deck "Phonon" :s6-phonon
                :s6-tuning-satisfaction 4
                :s6-sliding-affliction 2
                :s6-suppressive-restriction 2
                :s6-guidance-ascension 2
                :s6-impulsive-frustration 2
                :s6-binding-beatitude 2
                :s6-complete-servitude 2)
   (create-deck "Orie" :s6-orie
                :s6-luminous-embrace 2
                :s6-rest-in-peace 2
                :s6-succession 2
                :s6-to-me 2
                :s6-sacred-arrow 2
                :s6-sealing-hoplon 2
                :s6-divine-thrust 2)])