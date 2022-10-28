(ns ballot.card-generator
  (:require [selmer.parser :as selmer]
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
                     :flavor "0000000"
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
                  :flavor   "c060000"
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

(defn keyword->latex
  "This accepts the tags that will be used for the HTML side, and converts them to the form that LaTeX expects."
  [keyword]
  (case keyword
    :power "\\lalalaLaTeXLalalalala"
    :range "\\LalalaLaTexLalalala"))

(defn hiccup->latex
  "Takes in a Hiccup format document and calls keyword->latex on all keywords"
  ([hiccup] (hiccup->latex hiccup ""))
  ([hiccup result]
   (cond (string? (first hiccup)) (recur (rest hiccup) (str result (first hiccup)))
         (vector? (first hiccup)) (recur (rest hiccup) (str result (hiccup->latex (first hiccup) "")))
         (keyword? (first hiccup)) (str result (keyword->latex (first hiccup))
                                        (hiccup->latex (nnext hiccup) "")
                                        "statement-ender")
         :else result)))

(defn format-card
  "Takes in a string representing one card chunk to confirm. Converts it to hiccup format and then parses each tag with the respective equivalent latex format."
  [card]
  (->> card
       hickory/parse-fragment
       (map hickory/as-hiccup)
       hiccup->latex))

;; tests
(spit "sample" (selmer/render (slurp "resources/templates/attack.template") conf format-opts))
(format-card "This card expects <power>+2 Power</power> and <range>+0~1 Range</range>")
