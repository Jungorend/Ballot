(ns ballot.card-generator
  (:require [clojure.pprint :refer [pprint]]))

(defn de-key
  [token]
  (clojure.string/join " " (map clojure.string/capitalize (rest (clojure.string/split (str token) #"-")))))

(defn get-trigger! []
  (println "Trigger:")
  (let [trigger (read-line)]
    (if (#{"now"
           "passive"
           "before"
           "hit"
           "after"
           "cleanup"} trigger)
      (keyword trigger)
      (do (println "That isn't a valid trigger. Please try again.")
          (recur)))))

(defn get-location! []
  (println "Location:")
  (let [location (read-line)]
    (if (or (= location "attack") (= location "a"))
      :attack
      :boost)))

(defn get-abilities! [name]
  (println (str "Please print the abilities for " name))
  (loop [abilities []]
    (println "Description: ")
    (let [desc (read-line)]
      (if (seq desc)
        (recur (conj abilities
                     #:ability {:description desc
                                :location (get-location!)
                                :trigger (get-trigger!)}))
        abilities))))

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
                            (let [name (de-key (first c))
                                  abilities (get-abilities! name)
                                  ppabilities (with-out-str (pprint abilities))]
                              (str r
                                   "#:card {:name \"" (de-key (first c)) "\"\n"
                                   "        :type :special\n"
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
                                   "        :abilities " ppabilities "}\n\n")))
                          "" (partition 2 cards))]
    (str character-cards
         card-strs
         create-deck-str)))

(spit "results.edn" (create-cards "Baiken" :s7-baiken "Guilty Gear"
                                  :s7-karatekewari 2
                                  :s7-youzansen 2
                                  :s7-hiiragi 2
                                  :s7-tatami-gaeshi 2
                                  :s7-kabari 2
                                  :s7-kenjyu 2
                                  :s7-tsurane-sanzu-watashi 2))
