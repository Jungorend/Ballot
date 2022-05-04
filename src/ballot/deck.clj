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

(def s1-decks
  [(create-deck "Reese" :s1-reese
                :s1-ballista 2
                :s1-chivalry 2

                )])

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
                :s6-pale-bringer 3
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
                :s6-divine-thrust 2)
   (create-deck "Gordeau" :s6-gordeau
                :s6-mortal-slide 2
                :s6-grim-reaper 2
                :s6-precise-aim 2
                :s6-mortal-glide 2
                :s6-rusty-nail 2
                :s6-soul-exodus 1
                :s6-turbulence 2)
   (create-deck "Yuzuriha" :s6-yuzuriha
                :s6-zero-no-kata-hi-ougi-inochi-kurenai 2
                :s6-sogetsu-ittou-ryuu-ougi-kashou 2
                :s6-battoujutsu-ichi-no-kata-kiri 2
                :s6-battoujutsu-ni-no-kata-saki 2
                :s6-battoujutsu-san-no-kata-tachi 2
                :s6-over-here 2
                :s6-sougetsu-ittou-ryu-et-cetera-yae-ichirin 2)
   (create-deck "Chaos" :s6-chaos
                :s6-deep-revenance 2
                :s6-dissect-barrage 2
                :s6-thats-your-prey 2
                :s6-conceal 2
                :s6-repel 2
                :s6-spew-out 2
                :s6-cold-reflection 2)
   (create-deck "Waldstein" :s6-waldstein
                :s6-verderben 2
                :s6-werfen-erschlagen 2
                :s6-katastrophe 2
                :s6-ferzen-volf 2
                :s6-sturmangriff 2
                :s6-eisen-nagel 2
                :s6-wirbelwind 2)
   (create-deck "Wagner" :s6-wagner
                :s6-schild-zack 2
                :s6-filthy-dog 2
                :s6-kugel-blitz 2
                :s6-wackenroder 2
                :s6-sturm-brecher 2
                :s6-megiddo-lor-celeste-grace 2
                :s6-hitze-falke 2)
   (create-deck "Nanase" :s6-nanase
                :s6-lumiere 2
                :s6-atmosphere 2
                :s6-anges-invitation 2
                :s6-let-the-fleur 2
                :s6-vrai-couer 2
                :s6-plumage 2
                :s6-le-reve 2)
   (create-deck "Merkava" :s6-merkava
                :s6-resentfully-rage 2
                :s6-defile 2
                :s6-persistently-cling 2
                :s6-agitate 2
                :s6-drill-through 2
                :s6-breathe-out 2
                :s6-capture-and-devour 2
                :s6-rampage 2)
   (create-deck "Enkidu" :s6-enkidu
                :s6-demon-seal-abyssal-force 2
                :s6-spiral-dual-palm-strike 2
                :s6-thunder-stomp 2
                :s6-gale-edge 2
                :s6-chained-kick 2
                :s6-three-precept-strike 6
                :s6-tidal-spin 2)])

(def s3-decks
  [(create-deck "C. Viper" :s3-cviper
                :s3-seismic-hammer 2
                :s3-temple-massage 2
                :s3-thunder-knuckle 2
                :s3-burning-kick 2
                :s3-emergency-combination 2
                :s3-burst-time 2
                :s3-burning-dance 2)
   (create-deck "Vega" :s3-vega
                :s3-rolling-crystal-flash 2
                :s3-pounce 2
                :s3-sky-high-claw 2
                :s3-flying-barcelona-attack 2
                :s3-scarlet-terror 2
                :s3-splendid-claw 2
                :s3-bloody-high-claw 2)
   (create-deck "M. Bison" :s3-mbison
                :s3-somersault-skull-diver 2
                :s3-psycho-crusher 2
                :s3-sliding-kick 2
                :s3-devil-reverse 2
                :s3-head-stomp-bison 2
                :s3-psycho-crusher 2
                :s3-nightmare-booster 2)
   (create-deck "Chun-Li" :s3-chunli
                :s3-kikoken 2
                :s3-spinning-bird-kick 2
                :s3-head-stomp 2
                :s3-flipping-ax-kick 2
                :s3-lightning-legs 2
                :s3-kikousho 2
                :s3-hosenka 2)
   (create-deck "Cammy" :s3-cammy
                :s3-cannonball 2
                :s3-spiral-arrow 2
                :s3-dive-kick 2
                :s3-cannon-spike 2
                :s3-cqc 2
                :s3-gyro-drive-smasher 2)
   (create-deck "Zangief" :s3-zangief
                :s3-flying-power-bomb 2
                :s3-double-lariat 2
                :s3-spinning-piledriver 2
                :s3-banishing-flat 2
                :s3-atomic-suplex 2
                :s3-siberian-blizzard 2
                :s3-ultimate-atomic-buster 2)
   (create-deck "Dan" :s3-dan
                :s3-gadouken 2
                :s3-koryuken 2
                :s3-dankukyaku 2
                :s3-saikyo-haraigoshi 2
                :s3-haoh-gadouken 2
                :s3-legendary-taunt 2
                :s3-shisso-buraiken 2)
   (create-deck "Akuma" :s3-akuma
                :s3-hyakkishu 2
                :s3-tatsumaki-zankukyaku 2
                :s3-gohadouken 2
                :s3-zugaihasatsu 2
                :s3-goshoryuken 2
                :s3-wrath-of-the-raging-demon 2
                :s3-demon-armageddon 2)
   (create-deck "Ryu" :s3-ryu
                :s3-one-inch-punch 2
                :s3-tatsumaki-senpukyaku-ryu 2
                :s3-donkey-kick 2
                :s3-hadoken-ryu 2
                :s3-shoryuken-ryu 2
                :s3-metsu-shoryuken 2
                :s3-metsu-hadoken 2)
   (create-deck "Ken" :s3-ken
                :s3-knee-bash 2
                :s3-tatsumaki-senpukyaku-ken 2
                :s3-axe-kick 2
                :s3-hadoken-ken 2
                :s3-shoryuken-ken 2
                :s3-guren-senpukyaku 2
                :s3-shinryuken 2)
   (create-deck "Guile" :s3-guile
                :s3-flash-kick 2
                :s3-reverse-spin-kick 2
                :s3-sonic-boom 2
                :s3-spinning-back-knuckle 2
                :s3-double-sweep-kick 2
                :s3-flash-explosion 2
                :s3-sonic-hurricane 2)
   (create-deck "Sagat" :s3-sagat
                :s3-low-tiger-shot 2
                :s3-tiger-knee 2
                :s3-tiger-uppercut 2
                :s3-tiger-shot 2
                :s3-low-step-kick 2
                :s3-tiger-destruction 2
                :s3-tiger-cannon 2)])

(def s4-decks
  [(create-deck "Shovel Knight and Shield Knight" :s4-shovel-shield
                :s4-buckler-blow 2
                :s4-charge-slash 2
                :s4-discovery 2
                :s4-shield-gong 2
                :s4-shovel-drop 2
                :s4-shield-boomerang 2
                :s4-tandem-attack 2
                :s4-shield-knight 1)
   (create-deck "Shovel Knight" :s4-propeller-knight
                :s4-cannonball 2
                :s4-headwind 2
                :s4-propeller-pull 2
                :s4-saber-lunge 2
                :s4-swoop 2
                :s4-full-broadside 2
                :s4-launcher 2)
   (create-deck "Mole Knight" :s4-mole-knight
                :s4-belly-slide 2
                :s4-block-push 2
                :s4-burrow-dig 2
                :s4-diving-dig 2
                :s4-headbutt 2
                :s4-cave-in 2
                :s4-erupt 2)])
