(ns talks-specter.core
  (:require [com.rpl.specter :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as str]))

;; A new way of thinking about how to manipulate your data structures.





































;; A Look at vanilla clojure

;; Representing a game of Blackjack

(defonce blackjack-data
  [{:name "Alex" :hand [5 3 :ace] :done? true}
   {:name "Christian" :hand [2 :king 4] :done? false}
   {:name "Björn" :hand [:queen :king :ace] :done? false}])





;; Writing an engine for the game = manipulating data-structures

(defn random-bj-card!
  []
  (rand-nth [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace]))

(defn hit-card!
  "Give a player a random card."
  [player-name data]
  data)

(comment
  (hit-card! "Alex" blackjack-data)
  )




;; Lets solve this with specter

;; transform

(defonce tiny-map {:a 3 :b 4 :c 5})
(transform MAP-VALS inc tiny-map)

(defonce tiny-vec [1 2 3 4 5 6 7 8 9 10])
(transform [ALL even?] #(* % %) tiny-vec)

(defonce more-realistic [{:a 1 :b 3} {:a 2 :b 4} {:a :Jürgen :b :Klinsmann}])
(transform [ALL :a number?] inc more-realistic)




;; Back to Blackjack

(defn hit-card-specter!
  [player-name data]
  data)

(comment
  (hit-card-specter! "Christian" blackjack-data)
  )













;; Create own paths and compose
(defn player [name]
  (path ALL #(= name (:name %))))






;; Game over
(defn game-over [data]
  (transform [ALL :done?] (fn [_] true) data))

(comment
  (game-over blackjack-data)
  )































;; "Problems" that specter solves. Separate
;; * Navigation
;; * Modification
;; * Reconstruction

;; Performance 30% faster than `get-in`. Comparable speed to hand-written, inlined code.

























;; A few more functions
;; Cheater-Mode

(transform [ALL :hand ALL (pred= :king)] (fn [_] :ace) blackjack-data)

;; Just take a look at things
(select [ALL :hand] blackjack-data)























;; Substructure navigators
(transform (srange 1 5) reverse [0 1 2 3 4 5 6 7 8 9])

(transform [(srange 2 7) (filterer even?)] reverse [0 1 2 3 4 5 6 7 8 9])

(setval [:a (srange 2 4)] [] {:a [0 1 2 3 4 5 6] :b [7 8 9]})

;; Works on strings as well
(setval BEGINNING "C" "lojureDus")




































;; Lets take a look th the step by step inner workings
(comment
 (transform [ALL :a even?]
            dec
            [{:a 2 :b 3} {:a 1} {:a 4}])

  ;;=>input
  [{:a 2 :b 3} {:a 1} {:a 4}]
  ;;=>ALL
  {:a 2 :b 3}
  {:a 1}
  {:a 4}
  ;;=>:a
  2
  1
  4
  ;;=>even?
  2
  4
  ;;=> dec
  1
  3
  ;;=>even?
  1
  1
  3
  ;;=>:a
  {:a 1 :b 3}
  {:a 1}
  {:a 3}
  ;;=>ALL
  [{:a 1 :b 3} {:a 1} {:a 3}]
  ;;=>output


 (transform [(filterer odd?) LAST]
            inc
            [1 2 3 4 5 6 7 8 9 18 12 14])

  ;;=>input
  [1 2 3 4 5 6 7 8 9 18 12 14]
  ;;=>(filterer odd?)
  [1 3 5 7 9]
  ;;=>LAST
  9
  ;;=>inc
  10
  ;;=>LAST
  [1 3 5 7 10]
  ;;=>(filterer odd?)
  [1 2 3 4 5 6 7 8 10 18 12 14]
  ;;=>output
  )













































;; Lets start the game
(defonce dota-data (json/read-str (slurp "dota_sample.json")
                                  :key-fn keyword))
(set! *print-length* 20)

;; Let's start easy and use the things we learned.
#_(keys (first dota-data))


;; What gamemode did the players use?
(comment
  (select [ALL :game_mode] dota-data)
  )


;; Game Update for game-mode ids
;; Mode 22 has been deprecated and is treated as mode 42 - we need to accomodate historical maps
(comment
  (defonce new-gamemode-data
    dota-data)
  (select [ALL :game_mode] new-gamemode-data)
  )
















;; Lets take a look at the friendlieness of the dota community
(select [ALL :chat ALL :key] dota-data)


;; Enhance this so my mom does understand as well
(comment
  (def polite-data
    (setval [ALL :chat ALL :key WORDS ALL (pred= "gl")] "Good Luck" dota-data))
  (select [ALL :chat ALL :key] polite-data)
  )

;; Lets take a look at the teamfights
;; How much damage was done over the whole record?
#_(keys (first dota-data))
(comment
  (select [ALL] dota-data)
  )

;; Lets do this in Vanilla Clojure...




















;; A little more deep dive
;; Following code courtesy of Nathan Marz
;; Navigators
(comment

  ;; (select (keypath :a) {:a 1 :b 2}) => 1
  ;; (select [ALL (keypath :a)] [{:a 1} {:b 2}]) => [1 nil]
  ;; (select (keypath :a :b) {:a {:b 2}})
  ;; Mainly usefull for creating dynamic paths

  (defnav keypath [key]
    (select* [this structure next-fn]
             (next-fn (get structure key)))
    (transform* [this structure next-fn]
                (assoc structure key (next-fn (get structure key)))
                ))

  ;; Select a subset inside a set
  ;; Inside a transform this just unions the result back into the superset

  (defnav subset [aset]
    (select* [this structure next-fn]
             (next-fn (set/intersection structure aset)))
    (transform* [this structure next-fn]
                (let [subset (set/intersection structure aset)
                      newset (next-fn subset)]
                  (-> structure
                      (set/difference subset)
                      (set/union newset))
                  )))
  )


;; Recursive Navigation
(comment
  (def tree [1 [2 [[3]] 4] [[5] 6] [7] 8 [[9]]])

  (def TREE-VALUES
    (recursive-path [] p  ; p is the path definition itself
                    (if-path vector?
                      [ALL p]
                      STAY)))

  (transform [TREE-VALUES even?] inc tree)

  ;; Get odd leaves
  (select [TREE-VALUES odd?] tree)

  ;; Reverse order of even leaves (order based on depth-first search)
  (transform (subselect TREE-VALUES even?) reverse tree)
  ;; (subselect selects as a sequence inside bigger structure instead of individual values)
  )
