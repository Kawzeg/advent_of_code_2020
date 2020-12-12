(ns solve.core
  (:gen-class))

(require '[clojure.string :as str])

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

(defn rot90
  [c]
  (cond
    (= c \N) \E
    (= c \E) \S
    (= c \S) \W
    (= c \W) \N
    :else c))

(defn apply-rot
  [pos f]
  (cons (f (first pos))
        (rest pos)))

(defn rot
  [pos c degs]
  (cond
    (= degs 180) (apply-rot pos (comp rot90 rot90))
    (or (and (= c \R) (= degs 90))
        (and (= c \L) (= degs 270))) (apply-rot pos rot90)
    (or (and (= c \R) (= degs 270))
        (and (= c \L) (= degs 90))) (apply-rot pos (comp rot90 rot90 rot90))
    :else pos)
  )

(defn translate
  [pos dir value]
  (cons (first pos) (map + (map (fn [x] (* x value)) (char->dir dir)) (rest pos))))

; position is a list of direction char, x coord, y coord
(defn move
  "Return the new position after using inst on pos"
  [pos inst]
  (let [val (Integer. (subs inst 1))
        c   (first inst)]
    (cond
      (or (= c \L)
          (= c \R)) (rot pos c val)
      (= c \F) (translate pos (first pos) val)
      :else (translate pos c val))))

(def start [\E 0 0])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [target (reduce move start input)
        x (nth target 1)
        y (nth target 2)]
    (println "Part 1")
    (println (+ (Math/abs x) (Math/abs y)))))
