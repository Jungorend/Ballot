(ns ballot.card-generator
  (:require [selmer.parser :as selmer]
            [clojure.string :refer [replace]]
            [hickory.core :as hickory]))

;; Because of the amount of {} in latex, rebinding what the parser uses for readability
(def format-opts
  {:tag-open \<
   :tag-close \>
   :filter-open \<
   :filter-close \>})

(def conf
  {:paragraph-spacing 5
   :font "FiraSans"
   :outline {:color {:range "000000"
                     :power "000000"
                     :speed "000000"
                     :armor "000000"
                     :guard "000000"
                     :flavor "000000"
                     :passive "000000"
                     :effect "000000"
                     :reminder "000000"
                     :trigger "000000"}
             :width {:flavor 1
                     :passive 0.7
                     :reminder 0.6
                     :trigger 0.7
                     :effect 0.6
                     :stats 1.2}
             :density {:stats 16
                       :effects 32
                       :flavor 16
                       :passive 16
                       :reminder 16
                       :trigger 16}}
   :text {:color {:range    "00abea"
                  :power    "f54137"
                  :speed    "fff5a5"
                  :armor    "ae96c3"
                  :guard    "39ab55"
                  :flavor   "c06000"
                  :passive  "ffffff"
                  :effect   "ffffff"
                  :reminder "ffffff"
                  :trigger  "ffffff"}
          :format {:range [:bold]
                   :power [:bold]
                   :speed [:bold]
                   :armor [:bold]
                   :guard [:bold]
                   :effect []
                   :flavor [:bold :italic]
                   :passive [:bold]
                   :reminder [:italic]
                   :trigger [:bold]}}})

(selmer/add-filter! :get-contour-type
                    (fn [stats]
                      (case stats
                        :reminder "effect"
                        :trigger "effect"
                        :range "stats"
                        :power "stats"
                        :speed "stats"
                        :guard "stats"
                        :armor "stats"
                        (name stats))))

(selmer/add-filter!
 :wrap-formatoptions
 (fn [opts]
   (reduce #(str (case %2
                   :bold "\\textbf{"
                   :italic "\\textit{")
                 %1 "}")
           "##1"
           opts)))

;; TODO: Replace keyword->latex and "statement-ender" with the actual latex start and enders

(defn- keyword->latex
  "This accepts the tags that will be used for the HTML side, and converts them to the form that LaTeX expects."
  [keyword]
  (str "\\"
       (case keyword
         :flavor "flavor"
         :bold "passive"
         :i "reminder"
         :guard "guard"
         :armor "armor"
         :speed "speed"
         :power "power"
         :range "range")
       "{"))

(defn- hiccup->latex
  "Takes in a Hiccup format document and calls keyword->latex on all keywords"
  ([hiccup] (hiccup->latex hiccup ""))
  ([hiccup result]
   (cond (string? (first hiccup)) (recur (rest hiccup) (str result (first hiccup)))
         (vector? (first hiccup)) (recur (rest hiccup) (str result (hiccup->latex (first hiccup) "")))
         (keyword? (first hiccup)) (str result (keyword->latex (first hiccup))
                                        (hiccup->latex (nnext hiccup) "")
                                        "}")
         :else result)))

(defn- cleanup-url-encoding
  "Hickory encodes the files and so quotes and similar will be replaced. Manually moving back a few."
  [text]
  (-> text
      (replace #"\&amp;quot;" "\"")
      (replace #"\&#39;" "'")
      ;; If we need to replace more we'll add them here
      ))

(defn- format-card
  "Takes in a string representing one card chunk to confirm. Converts it to hiccup format and then parses each tag with the respective equivalent latex format."
  [card]
  (->> card
       hickory/parse-fragment
       (map hickory/as-hiccup)
       hiccup->latex))

(defn render-attack-card
  "Pass in `card-text` and this produces the latex document string for it."
  [card-text]
  (cleanup-url-encoding
   (selmer/render (slurp "resources/templates/attack.template")
                  (assoc conf :text-content (format-card card-text))
                  format-opts)))

;; tests
(spit "sample.tex" (render-attack-card (slurp "sample_text.txt")))
