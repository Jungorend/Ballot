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
                :s1-checkmate 2
                :s1-sovereign-glory 2
                :s1-gallant-defender 2
                :s1-knight-wave 2
                :s1-gauntlet-flurry 2)
   (create-deck "Nehtali" :s1-nehtali
                :s1-azazels-torment 2
                :s1-darkness-barrier 2
                :s1-hellfire 2
                :s1-soul-thresher 2
                :s1-volt-damnation 2
                :s1-heavens-punishment 2
                :s1-hells-salvation 2)
   (create-deck "Heidi" :s1-heidi
                :s1-artillery-cannon 2
                :s1-dagger-strike-install 2
                :s1-iron-knuckle 2
                :s1-mech-cannon 2
                :s1-steel-driver 2
                :s1-dagger-storm-drei-install 2
                :s1-rail-driver 2)
   (create-deck "Vincent" :s1-vincent
                :s1-crimson-barrage 2
                :s1-gatling-punch 2
                :s1-majority-whip 2
                :s1-national-guard 2
                :s1-phoenix-ascent 2
                :s1-ballot-fixing 2
                :s1-phoenix-revival 2)
   (create-deck "Eva" :s1-eva
                :s1-cyber-destroyer 2
                :s1-enki-thresher 2
                :s1-plasma-barrage 2
                :s1-riot-machine 2
                :s1-upgrade 2
                :s1-harnessing-chaos 2
                :s1-shifting-technology 2)
   (create-deck "Kaden" :s1-kaden
                :s1-calamity-bell 2
                :s1-chain-strike 2
                :s1-double-charge-executioner 2
                :s1-fusion-bane 2
                :s1-havoc-call 2
                :s1-dark-tide-summon 2
                :s1-rising-host 2)
   (create-deck "Miska" :s1-miska
                :s1-bear-rush 2
                :s1-canine-strike 2
                :s1-fire-in-the-hole 2
                :s1-knee-capper 2
                :s1-silver-fang 2
                :s1-savage-wildsider 2
                :s1-scorched-earth 2)
   (create-deck "Lily" :s1-lily
                :s1-bullet-barrage 2
                :s1-double-tap 2
                :s1-excessive-force 2
                :s1-hair-trigger 2
                :s1-mug-shot 2
                :s1-magic-bullet 2
                :s1-wild-bunch 2)
   (create-deck "Satoshi" :s1-satoshi
                :s1-demon-slayer-slash 2
                :s1-paralyzing-dart 2
                :s1-sealing-strike 2
                :s1-shuriken-illusion 2
                :s1-yokai-fury 2
                :s1-jingoku-banishment 2
                :s1-paralyzing-dart 2)
   (create-deck "Morathi" :s1-morathi
                :s1-gyro-chain-gash 2
                :s1-ivory-ghost-charge 2
                :s1-ivory-ghost-impalement 2
                :s1-neck-snapper 2
                :s1-revenger 2
                :s1-god-of-war 2
                :s1-shadow-of-death 2)
   (create-deck "Baelkhor" :s1-baelkhor
                :s1-accursed-gaze 2
                :s1-blade-of-souls 2
                :s1-desperate-might 2
                :s1-soul-ripper 2
                :s1-storm-of-souls 2
                :s1-desperate-gambit 2
                :s1-from-hell 2)
   (create-deck "Mei Lien" :s1-mei-lien
                :s1-dragon-thrash 2
                :s1-fujin-drum 2
                :s1-halberdier 2
                :s1-raijin-knife 2
                :s1-dragon-tempest 2
                :s1-raijin-oath 2
                :s1-cloud-rider 2)
   (create-deck "Zoey" :s1-zoey
                :s1-tsunami-slicer 2
                :s1-neo-cosmic-flare 2
                :s1-sure-you-can 2
                :s1-maori-defender 2
                :s1-gut-shot 2
                :s1-gale-blade 2
                :s1-focus-charge 2)
   (create-deck "Super Skull Man 33" :s1-skullman
                :s1-zing-zing-zing 2
                :s1-kaplow 2
                :s1-zaaaap 2
                :s1-splam 2
                :s1-slam-evil 2
                :s1-bing-bong 2
                :s1-bang-bang-bang 2)
   (create-deck "Devris" :s1-devris
                :s1-firestream 2
                :s1-demonblood 2
                :s1-summoned-assailants 2
                :s1-phantasmal-might 2
                :s1-marked-for-death 2
                :s1-drain-life 2
                :s1-combustion 2)
   (create-deck "Juno" :s1-juno
                :s1-meteor-jam 2
                :s1-juno-live 2
                :s1-technic-beat 2
                :s1-star-struck 2
                :s1-sonic-comet 2
                :s1-rhythm-beat 2
                :s1-comet-tail 2)
   (create-deck "Gabrek" :s1-gabrek
                :s1-death-valley-face-plant 2
                :s1-13th-story-oblivion 2
                :s1-shrug-off 2
                :s1-rolling-ankle-grab 2
                :s1-perilous-descent 2
                :s1-lunar-launcher 2
                :s1-choke-hold 2)
   (create-deck "Ulrik" :s1-ulrik
                :s1-second-strike 2
                :s1-atomic-bolt 2
                :s1-lightning-javelin 2
                :s1-ionization 2
                :s1-inevitability 2
                :s1-blitz-hammer 2
                :s1-100-million-volts 2)
   (create-deck "Alice" :s1-alice
                :s1-surprise-punishment 2
                :s1-cross-blades 2
                :s1-sword-and-cross 2
                :s1-soul-gazer 2
                :s1-guardian-slasher 2
                :s1-dark-corruption 2
                :s1-bloody-baptism 2)])

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
   (create-deck "Celinka" :s2-celinka
                :s2-dispelling-horn 2
                :s2-moon-fall 2
                :s2-moon-flare 2
                :s2-swift-exorcism 2
                :s2-wishing-ward 2
                :s2-moon-ritual-dance 2
                :s2-purifying-roar 2)
   (create-deck "Remiliss" :s2-remiliss
                :s2-caustic-vent 2
                :s2-ground-zero 2
                :s2-irradiate 2
                :s2-napalm-stream 2
                :s2-toxic-tendrils 2
                :s2-consumption 2
                :s2-nuclear-option 2)
   (create-deck "Geoffrey" :s2-geoffrey
                :s2-bastion-stance 2
                :s2-golden-arrow 2
                :s2-inquisition 2
                :s2-sacrement-of-blades 2
                :s2-solemn-exorcism 2
                :s2-crusaders-oath 2
                :s2-inviolable-judgement 2)
   (create-deck "Taisei" :s2-taisei
                :s2-anathema-surge 2
                :s2-ashen-claws 2
                :s2-blackvolt 2
                :s2-bloodthirst 2
                :s2-dust-to-dust 2
                :s2-chaos-scissors 2
                :s2-nightmare-tares 2)
   (create-deck "D'janette" :s2-djanette
                :s2-affliction 2
                :s2-black-death 2
                :s2-blood-thorns 2
                :s2-charnel-blast 2
                :s2-profane-sanctuary 2
                :s2-carmine-offering 2
                :s2-death-knell 2)
   (create-deck "Zsolt" :s2-zsolt
                :s2-blaze-of-fervor 2
                :s2-cross-up 2
                :s2-fatal-eye 2
                :s2-gunblaze 2
                :s2-whip-crack 2
                :s2-fantical-purification 2
                :s2-wild-hunt 2)
   (create-deck "Minato" :s2-minato
                :s2-barnstorming 2
                :s2-bus-stop 2
                :s2-cabstand 2
                :s2-flight-13 2
                :s2-jump-the-shark 2
                :s2-a-streetcar-named-disaster 2
                :s2-hellward-bound 2)
   (create-deck "Tournelouse" :s2-tournelouse
                :s2-death-omen 2
                :s2-evil-eye 2
                :s2-grim-thundercalling 2
                :s2-lightning-spike 2
                :s2-southpaw 2
                :s2-bargeist-fang 2
                :s2-netherstorm 2)
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
                :s2-queen-of-hearts 2)
   (create-deck "Tournelouse" :s2-tournelouse
                :s2-death-omen 2
                :s2-evil-eye 2
                :s2-grim-thundercalling 2
                :s2-lightning-spike 2
                :s2-southpaw 2
                :s2-bargeist-fang 2
                :s2-netherstorm 2)
   (create-deck "Galdred" :s2-galdred
                :s2-blood-frenzy 2
                :s2-eviscerate 2
                :s2-explosive-cocktail 2
                :s2-violent-transgression 2
                :s2-withering-toxin 2
                :s2-hydra-helix 2
                :s2-metamorphosis 2)
   (create-deck "Umina" :s2-umina
                :s2-dreamlands 1
                :s2-dark-thoughts 2
                :s2-hollow-space 2
                :s2-out-of-mind 2
                :s2-shadow-chorus 2
                :s2-terror-whispers 2
                :s2-call-of-the-dreamlands 2
                :s2-unknown-khadath 2)
   (create-deck "Luciya" :s2-luciya
                :s2-bug-zapper 2
                :s2-downburst 2
                :s2-firefly-gunner 2
                :s2-mantis-strike 2
                :s2-talon-sweep 2
                :s2-ride-the-lightning 2
                :s2-skies-aflame 2)
   (create-deck "Syrus" :s2-syrus
                :s2-albatross-talon 2
                :s2-aria-of-the-wind 2
                :s2-siren-call 2
                :s2-tidal-whirl 2
                :s2-treasure-hunter 2
                :s2-dredge-fury 2
                :s2-symphony-of-the-deep 2)
   (create-deck "Seijun" :s2-seijun
                :s2-fox-fire 2
                :s2-inari-guidance 2
                :s2-ink-spike 2
                :s2-ink-splash 2
                :s2-yokai-banishing 2
                :s2-tale-of-nine-sorrows 2
                :s2-tales-of-seven-trials 2)
   (create-deck "Sydney" :s2-sydney
                :s2-serena 1
                :s2-blossom-haze 2
                :s2-choking-thorns 2
                :s2-pea-shooter 2
                :s2-spore-burst 2
                :s2-venom-lash 2
                :s2-aluraunes-kiss 2
                :s2-verdant-slaughter 2)
   (create-deck "Emogine" :s2-emogine
                :s2-blood-for-blood 2
                :s2-guilty-paean 2
                :s2-holy-warding 2
                :s2-martyrs-lash 2
                :s2-purifying-chime 2
                :s2-hand-of-judgement 2
                :s2-touch-of-divinity 2)
   (create-deck "Carl Swangee" :s2-swangee
                :s2-cease-and-desist 2
                :s2-disarming-strike 2
                :s2-improvised-weapon 2
                :s2-power-short 2
                :s2-swangee-elbow 2
                :s2-authorized-force 2
                :s2-autonomic-response 2)
   (create-deck "Shovel Knight" :s2-shovel-knight
                :s2-alchemy-coin 2
                :s2-chaos-sphere 2
                :s2-flare-wand 2
                :s2-mobile-gear 2
                :s2-war-horn 2
                :s2-propeller-dagger 2
                :s2-troupple-chalice 2)
   (create-deck "Pooky" :s2-pooky
                :s2-gambling 2
                :s2-long-in-the-tooth 2
                :s2-pooky-cheats 2
                :s2-pooky-drinks 2
                :s2-snack-attack 2
                :s2-drunken-rampage 2
                :s2-hat-trick 2)])

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
                :s6-tidal-spin 2)
   (create-deck "Hilda" :s6-hilda
                :s6-impalement 2
                :s6-in-the-darkness 2
                :s6-condensity-gloom 2
                :s6-interference 2
                :s6-revenant-pillar 2
                :s6-tri-furket 2
                :s6-skewer 4)
   (create-deck "Seth" :s6-seth
                :s6-distant-frontier 2
                :s6-abyssal-geometry 2
                :s6-captive-segment 2
                :s6-piercing-penetration 2
                :s6-transgressing-convict 2
                :s6-vanishing-confusion 2
                :s6-dead-space-of-intrusion 2)
   (create-deck "Londrekia" :s6-londrekia
                :s6-cocytus-ice-prison 2
                :s6-frozen-cleave 2
                :s6-hail-storm 2
                :s6-circular-step 4
                :s6-snow-blossom 2
                :s6-frozen-vine 2
                :s6-frozen-spire 2)
   (create-deck "Mika" :s6-mika
                :s6-mikas-galaxy 2
                :s6-mikas-revolution 2
                :s6-mikas-cannon 2
                :s6-mikas-hip-attack 2
                :s6-mikas-tornado 2
                :s6-mikas-missile 2
                :s6-mikas-crash 2)
   (create-deck "Byakuya" :s6-byakuya
                :s6-endless-nightmare 2
                :s6-become-a-part-of-me 2
                :s6-or-shredded 4
                :s6-caught-you 2
                :s6-how-shall-i-cook-you 4
                :s6-ill-plant-it-somewhere-here 2
                :s6-minced 4)
   (create-deck "Vatista" :s6-vatista
                :s6-ruber-angelus 2
                :s6-armabellum 2
                :s6-lateus-orbis 2
                :s6-transvoranse 2
                :s6-mikoruseo 2
                :s6-lumen-stella 2
                :s6-zahhishio 2)])

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
                :s3-gohadoken 2
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
   (create-deck "Propeller Knight" :s4-propeller-knight
                :s4-cannonball 2
                :s4-headwind 2
                :s4-propeller-pull 2
                :s4-saber-lunge 2
                :s4-swoop 2
                :s4-full-broadside 2
                :s4-launcher 2)
   (create-deck "Mole Knight" :s4-mole-knight
                :s4-burrow 1
                :s4-belly-slide 2
                :s4-block-push 2
                :s4-burrow-dig 2
                :s4-diving-dig 2
                :s4-headbutt 2
                :s4-cave-in 2
                :s4-erupt 2)
   (create-deck "Tinker Knight" :s4-tinker-knight
                :s4-drill-arm 2
                :s4-flail 2
                :s4-missiles 2
                :s4-mobile-gear 2
                :s4-wrench-toss 2
                :s4-bomb-bounce 2
                :s4-mech-charge 2)
   (create-deck "Plague Knight" :s4-plague-knight
                :s4-chain-reaction 2
                :s4-giant-bomb 2
                :s4-long-pitch 2
                :s4-perfect-pitch 2
                :s4-staff-of-surging 2
                :s4-castle-crasher 2
                :s4-triple-dose 2)
   (create-deck "Polar Knight" :s4-polar-knight
                :s4-ice-spike 5
                :s4-polar-plow 2
                :s4-shovel-charge 2
                :s4-snow-shovel-drop 2
                :s4-shovel-slam 2
                :s4-stomp 2
                :s4-icicle-drop 2
                :s4-snow-slash 2)
   (create-deck "Treasure Knight" :s4-treasure-knight
                :s4-anchor-launch 2
                :s4-aqua-mine 2
                :s4-dive-charge 2
                :s4-scuttle-slam 2
                :s4-treasure-coin 2
                :s4-angler-call 2
                :s4-maelstrom-chest 2)
   (create-deck "Enchantress" :s4-enchantress
                :s4-rapid-beam 2
                :s4-shattering-scream 2
                :s4-magic-shot 2
                :s4-fly-charge 2
                :s4-fire-wave 2
                :s4-homing-orb 2
                :s4-spiral-orb 2)
   (create-deck "Specter Knight" :s4-specter-knight
                :s4-bounding-soul 2
                :s4-dread-talon 2
                :s4-spider-scythe 2
                :s4-spin-scythe 2
                :s4-throwing-sickle 2
                :s4-barrier-lantern 2
                :s4-dread-reaper 2)
   (create-deck "Fight" :s4-fight
                :s4-explosive-shot 2
                :s4-electro-charge 2
                :s4-power-jump 2
                :s4-retreating-bolts 2
                :s4-flamethrower 2
                :s4-lightning-gun 2
                :s4-rail-gun 2)
   (create-deck "King Knight" :s4-king-knight
                :s4-kingly-strut 1
                :s4-lordly-might 1
                :s4-magnificent-cape 1
                :s4-pay-to-win 1
                :s4-healing-hammer 2
                :s4-scepter-slam 2
                :s4-scepter-smite 2
                :s4-shoulder-bash 2
                :s4-spin-jump 2
                :s4-king-of-cards 2
                :s4-victory-trumpets 2)
   (create-deck "Beheaded" :s4-the-beheaded
                :s4-brutality 1
                :s4-tactics 1
                :s4-survival 1
                :s4-assault-shield 2
                :s4-dive-attack 2
                :s4-infantry-bow 2
                :s4-twin-daggers 2
                :s4-wrenching-whip 2
                :s4-phaser 2
                :s4-wave-of-denial 2)])

(def s5-decks
  [(create-deck "Ragna the Bloodedge" :s5-ragna
                :s5-inferno-divider 2
                :s5-hells-fang 2
                :s5-dead-spike 2
                :s5-gauntlet-hades 2
                :s5-blood-scythe 2
                :s5-devoured-by-darkness 2
                :s5-carnage-scissors 2
                :s5-black-onslaught 1)
   (create-deck "Jin Kisaragi" :s5-jin
                :s5-violent-ice 2
                :s5-crystal-strike 2
                :s5-permafrost 2
                :s5-ice-blade 2
                :s5-dual-ice-strike 2
                :s5-moonsong 2
                :s5-ice-fang 2
                :s5-arctic-dungeon 1)
   (create-deck "Hazama" :s5-hazama
                :s5-ouroboros-1 1
                :s5-ouroboros-2 1
                :s5-hungry-darkness-of-1000-souls 1
                :s5-eternal-coils-of-the-dragon-serpent 2
                :s5-serpents-infernal-rapture 2
                :s5-rising-fang 2
                :s5-devouring-fang 2
                :s5-falling-fang 2
                :s5-venom-sword 2
                :s5-hungry-coils 2)
   (create-deck "Rachel Alucard" :s5-rachel
                :s5-electric-chair 2
                :s5-flying-lobelia 2
                :s5-spike-drop 2
                :s5-tiny-lobelia 2
                :s5-whirlwind 2
                :s5-baden-baden-lily 2
                :s5-tempest-dahlia 2
                :s5-clownish-calendula 1)
   (create-deck "Noel Vermillion" :s5-noel
                :s5-assault-through 2
                :s5-muzzle-flitter 2
                :s5-optic-barrel 2
                :s5-spring-raid 2
                :s5-type-one 2
                :s5-bullet-storm 2
                :s5-zero-gun-fenrir 2
                :s5-valkyrie-veil 1)
   (create-deck "Iron Tager" :s5-iron-tager
                :s5-king-of-tager 1
                :s5-magna-tech-wheel 2
                :s5-genesic-emerald-tager-buster 2
                :s5-atomic-collider 2
                :s5-crimson-punisher 2
                :s5-gigantic-tager-driver 2
                :s5-sledgehammer 2
                :s5-spark-bolt 2)
   (create-deck "Taokaka" :s5-taokaka
                :s5-attack-meow-pow 1
                :s5-imma-beat-the-crap-outta-you 2
                :s5-hexa-edge 2
                :s5-cat-spirit-one 2
                :s5-double-paw-strike 2
                :s5-kitty-litter-special 2
                :s5-slashy-slashy 2
                :s5-trick-edge 2)
   (create-deck "Arakune" :s5-arakune
                :s5-n-to-infinity 1
                :s5-f-of-g 2
                :s5-f-inverse 2
                :s5-disjoint-union 2
                :s5-f-piecewise 2
                :s5-if-p-then-q 2
                :s5-permutation-n-r 2
                :s5-y-two-dash 2)
   (create-deck "Nu-13" :s5-nu-13
                :s5-sword-of-destruction 1
                :s5-legacy-edge 2
                :s5-calamity-sword 2
                :s5-sickle-storm 2
                :s5-spike-chaser 2
                :s5-supra-rage 2
                :s5-sword-dance 2
                :s5-sword-summoner 2)
   (create-deck "Litchi Faye-Ling" :s5-litchi
                :s5-renchan 2
                :s5-unarmed-lunge 2
                :s5-tsubame-gaeshi 2
                :s5-reach-robbing-the-kong 2
                :s5-four-winds 2
                :s5-all-green 2
                :s5-thirteen-orphans 2
                :s5-nine-gates-of-heaven 1)
   (create-deck "Platinum the Trinity" :s5-platinum
                :s5-shining-layered-force 1
                :s5-miracle-jeanne 2
                :s5-cure-dot-typhoon 2
                :s5-dramatic-sammy 2
                :s5-dream-sally 2
                :s5-happy-magicka 2
                :s5-mami-circular 2
                :s5-mystique-momo 2)
   (create-deck "Kokonoe Mercury" :s5-kokonoe
                :s5-graviton 1
                :s5-ultimate-impact 1
                :s5-flaming-belabog 2
                :s5-dreadnought-exterminator 2
                :s5-absolute-zero 2
                :s5-banishing-rays 2
                :s5-broken-bunker-assault 2
                :s5-flame-cage 2
                :s5-solid-wheel 2)
   (create-deck "Nine The Phantom" :s5-nine
                :s5-colorless-void 1
                :s5-azurite-inferno 2
                :s5-lapis-lazuli-of-lamentation 2
                :s5-emerald-of-enmity 2
                :s5-morganite-of-malice 2
                :s5-coral-of-catastrophe 2
                :s5-kunzite-of-keep-breaker 2
                :s5-amethyst-of-annihilation 2
                :s5-navy-pressure 2
                :s5-flame-punisher 2)
   (create-deck "Bang Shishigami" :s5-bang
                :s5-VOID-TEMPEST-KICK 2
                :s5-UNSTOPPABLE-PALM-THRUST 2
                :s5-PULVERIZING-BLAST-JUTSU 2
                :s5-BANG-STYLE-SHURIKEN 2
                :s5-EXODIA-OBLITERATE 2
                :s5-FATAL-ERUPTION 2
                :s5-INFINITE-CHAOS-FIST-OF-THE-VOID 2
                :s5-THE-ULTIMATE-BANG 1)
   (create-deck "Hakumen" :s5-hakumen
                :s5-akumetsu 1
                :s5-kokuujin-yukikaze 2
                :s5-kokuujin-shippu 2
                :s5-enma 2
                :s5-guren 2
                :s5-renka 2
                :s5-yanagi 2
                :s5-zantetsu 2)
   (create-deck "Carl Clover" :s5-carl-clover
                :s5-nirvana 1
                :s5-deus-ex-machina 1
                :s5-rhapsody-of-memories 2
                :s5-laetabilis-cantato 2
                :s5-cantabile 2
                :s5-con-anima 2
                :s5-con-brio 2
                :s5-con-fuoco 2
                :s5-volante 2)])

(def s7-decks
  [(create-deck "Sol Badguy" :s7-sol
                :s7-heavy-mob-cemetery 2
                :s7-tyrant-rave 2
                :s7-wild-throw 2
                :s7-gun-flame 2
                :s7-fafnir 2
                :s7-night-raid-vortex 2
                :s7-bandit-bringer 2
                :s7-volcanic-viper 2)
   (create-deck "Ky Kiske" :s7-ky
                :s7-ride-the-lightning 2
                :s7-sacred-edge 2
                :s7-foudre-arc 2
                :s7-dire-eclat 2
                :s7-stun-dipper 2
                :s7-stun-edge 2
                :s7-vapor-thrust 2)
   (create-deck "May" :s7-may
                :s7-arisugawa-sparkle 2
                :s7-overhead-kiss 2
                :s7-mr-dolphin 2
                :s7-totsugeki 2
                :s7-anchor-swing 2
                :s7-the-wonderful-and-dynamic-goshogawara 2
                :s7-great-yamada-attack 2)
   (create-deck "Faust" :s7-faust
                :s7-mix-mix-mix 2
                :s7-thrust 2
                :s7-love 2
                :s7-scarecrow 2
                :s7-snip-snip-snip 2
                :s7-bone-crushing-excitement 2
                :s7-what-could-this-be? 2)

   (create-deck "Zato-1" :s7-zato
                :s7-eddie 1
                :s7-pierce 2
                :s7-invite-hell 2
                :s7-damned-fang 2
                :s7-oppose 2
                :s7-leap 2
                :s7-thats-a-lot 2
                :s7-sun-void 2
                :s7-amorphous 2)

   (create-deck "Baiken" :s7-baiken
                :s7-karatekewari 2
                :s7-youzansen 2
                :s7-hiiragi 2
                :s7-tatami-gaeshi 2
                :s7-kabari 2
                :s7-kenjyu 2
                :s7-tsurane-sanzu-watashi 2)

   (create-deck "Ramlethal" :s7-ramlethal
                :s7-bajoneto 2
                :s7-sabrobato 2
                :s7-dauro 2
                :s7-agresa-ordono 2
                :s7-erarlumo 2
                :s7-calvados 2
                :s7-mortobato 2)

   (create-deck "Potemkin" :s7-potemkin
                :s7-mega-fist 2
                :s7-heat-knuckle 2
                :s7-hammer-fall 2
                :s7-garuda-impact 2
                :s7-slide-head 2
                :s7-potemkin-buster 2
                :s7-giganter-kal 2
                :s7-heavenly-potemkin-buster 2)

   (create-deck "Axl Low" :s7-axl
                :s7-rainwater 2
                :s7-winter-mantis 2
                :s7-snail 2
                :s7-axl-bomber 2
                :s7-sickle-flash 4
                :s7-sickle-storm 2
                :s7-one-vision 2)

   (create-deck "Giovanna" :s7-giovanna
                :s7-triple-kick 2
                :s7-sol-poente 2
                :s7-sol-nascente 2
                :s7-trovao 2
                :s7-sepultura 2
                :s7-tempestade 2
                :s7-ventania 2)

   (create-deck "Anji Mito" :s7-anji
                :s7-nagiha 2
                :s7-rin 2
                :s7-shitsu 2
                :s7-kou 2
                :s7-fuujin 2
                :s7-kachoufuugetsu-kai 2
                :s7-issei-ougi-sai 2)

   (create-deck "Happy Chaos" :s7-happy-chaos
                :s7-fire 3
                :s7-at-the-ready 2
                :s7-cheap-shot 3
                :s7-steady-aim 3
                :s7-gun-down 3)

   (create-deck "I-No" :s7-ino
                :s7-chemical-love 2
                :s7-guitar-standing 2
                :s7-sultry-performance 2
                :s7-stroke-the-big-tree 2
                :s7-antidepressant-scale 2
                :s7-ultimate-fortissimo 2
                :s7-megalomania 2)

   (create-deck "Jack-O'" :s7-jacko
                :s7-countdown 2
                :s7-iron-pumpkin 2
                :s7-chain-of-chiron 2
                :s7-throw-servant 2
                :s7-servant-shoot 2
                :s7-cheer-servant-on 2
                :s7-forever-elysion-driver 2)

   (create-deck "Millia Rage" :s7-millia
                :s7-swing-braid 2
                :s7-low-kick 2
                :s7-iron-savior 2
                :s7-bad-moon 2
                :s7-tandem-top 2
                :s7-septem-voices 2
                :s7-winger 2)

   (create-deck "Testament" :s7-testament
                :s7-arbiter-sign-rising 2
                :s7-arbiter-sign-falling 2
                :s7-unholy-diver 2
                :s7-scythe-swing 2
                :s7-grave-reaper 2
                :s7-calamity-one 2
                :s7-nostrovia 2)

   (create-deck "Nagoriyuki" :s7-nagoriyuki
                :s7-shizuriyuki 2
                :s7-bloodsucking-universe 2
                :s7-kamuriyuki 2
                :s7-zarameyuki 2
                :s7-kirioroshi 2
                :s7-zansetsu 2
                :s7-wasureyuki 2)

   (create-deck "Chipp Zanuff" :s7-chipp
                :s7-genrouzan 2
                :s7-resshou 2
                :s7-gamma-blade 2
                :s7-beta-blade 2
                :s7-alpha-blade 2
                :s7-banki-messai 2
                :s7-zansei-rouga 2)

   (create-deck "Goldlewis Dickinson" :s7-goldlewis
                :s7-behemoth-typhoon-rise 2
                :s7-behemoth-typhoon-hurl 2
                :s7-behemoth-typoon-spin 2
                :s7-behemoth-typhoon-swing 2
                :s7-behemoth-typhoon-crush 2
                :s7-behemoth-typhoon-slam 2
                :s7-behemoth-typhoon-smash 2
                :s7-behemoth-typhoon-drop 2
                :s7-down-with-the-system 2
                :s7-burn-it-down 2)

   (create-deck "Leo Whitefang" :s7-leo
                :s7-kahn-schild 2
                :s7-eisensturm 2
                :s7-zweites-kaltes-gestober 2
                :s7-gravierte-wurde 2
                :s7-blitzschlag 2
                :s7-leidenschaft-des-dirigenten 2
                :s7-stahlwirbel 2)])
