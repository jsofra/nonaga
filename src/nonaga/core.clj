(ns nonaga.core
  (:require [clojure.set :as s]))

(def initial-game
  {:rings
    #{   [1 4] [2 4] [3 4]  
       [0 3] [1 3] [2 3] [3 3]
     [0 2] [1 2] [2 2] [3 2] [4 2] 
       [0 1] [1 1] [2 1] [3 1] 
         [1 0] [2 0] [3 0]}
  
   :whites
    #{   [1 4]
    
                             [4 2] 
     
         [1 0]}
  
   :blacks
    #{               [3 4]  
       
     [0 2]
       
                     [3 0]}})

(defn nw [[x y]] [(if (odd?  y) x (- x 1)) (+ y 1)])
(defn ne [[x y]] [(if (even? y) x (+ x 1)) (+ y 1)])
(defn w  [[x y]] [(- x 1) y])
(defn e  [[x y]] [(+ x 1) y])
(defn sw [[x y]] [(if (odd?  y) x (- x 1)) (- y 1)])
(defn se [[x y]] [(if (even? y) x (+ x 1)) (- y 1)])

(defn ordered-neighbours [cell]
  ((juxt nw ne e se sw w) cell))

(defn neighbours [cell]
  (into #{} (ordered-neighbours cell)))

(defn invalid-space? [{:keys [rings whites blacks]} coord]
  (not (and (rings coord)
            (not (or (whites coord)
                     (blacks coord))))))

(defn move [board coord direction]
  (first
    (for [step (iterate direction coord)
          :when (invalid-space? board (direction step))]
      step)))

(defn neighbouring-pairs [cell]
  (->> (ordered-neighbours cell)
       cycle
       (partition 2 1)
       (map set)
       (take 6)))

(defn can-move-ring? [{:keys [rings whites blacks]} cell]
  (let [n (neighbours cell)]
    (some #(empty? (s/intersection % n rings))
          (neighbouring-pairs cell))))
