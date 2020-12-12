(ns solve.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.core.matrix :as m])

(def input (-> (slurp "resources/input")
               (str/split #"\n")))

(defn char->dir
  "Return the x and y change for the given direction char"
  [c]
  (cond
    (= c \N) [ 0  1]
    (= c \E) [ 1  0]
    (= c \S) [ 0 -1]
    (= c \W) [-1  0]
    :else [0 0]))

(defn makeRotMatrix
  "Make a rotation matrix for counterclockwise rotations of rads radians.
  Rounded to the nearest integer for our purposes"
  [rads]
  [[(-> rads Math/cos Math/round) (-> rads Math/sin Math/round -)]
   [(-> rads Math/sin Math/round) (-> rads Math/cos Math/round)]])

(defn degs->rads
  [degs]
  (Math/toRadians degs))

(defn rot
  [coord c degs]
  (let [rads (cond
               (= c \L) (degs->rads degs)
               :else    (degs->rads (- 360 degs)))]
    (m/mmul (makeRotMatrix rads) coord)))

(defn translate
  [coord wp value]
  (map + coord (map (fn [x] (* x value)) wp)))

; position is a list of direction char, x coord, y coord
(defn move
  "Return the new position after using inst on pos"
  [pos inst]
  (let [val (Integer. (subs inst 1))
        c   (first inst)]
    (cond
      (or (= c \L)
          (= c \R)) (update-in pos [:wp] rot c val)
      (= c \F) (update-in pos [:ship] translate (:wp pos) val)
      :else (update-in pos [:wp] translate (char->dir c) val))))

(def start {:ship [0 0] :wp [10 1]})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [target (first (reduce (fn [xs mv] (cons (move (first xs) mv) xs)) [start] input))
        x (nth (:ship target) 0)
        y (nth (:ship target) 1)]
    (println "Part 2")
    (println (int (+ (Math/abs x) (Math/abs y))))))
