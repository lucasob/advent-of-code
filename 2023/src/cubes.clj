(ns cubes
  (:require
    [clojure.string :as string]
    [medley.core :as medley]
    [shared]))

(defn ->game [^String row]
  (let [split-pattern #"Game [\d]{1,3}:"]
    [(re-find split-pattern row) (string/trim (last (string/split row split-pattern)))]))

(defn ->game-id [[game-info]]
  (-> game-info
      (string/split #"\s")
      (second)
      (->> (drop-last 1))
      (->> (apply str))
      (Integer/parseInt)))

(defn ->set [info]
  (->>
    (string/split info #",")
    (map #(string/split % #"\s"))
    (map #(remove empty? %))
    (map (comp vec reverse))
    (into {})
    (medley/map-kv (fn [k v] [(keyword k) (Integer/parseInt v)]))))

(defn ->sets [[_ sets]]
  (-> sets
      (string/split #";")
      (->> (mapv ->set))))

(defn row->game [row]
  (let [[game-id sets] ((juxt ->game-id ->sets) (->game row))]
    {game-id sets}))

(defn rows->game [rows]
  (map row->game rows))

(defn file->summary [file-path]
  (->
    file-path
    (shared/file->rows)
    (rows->game)
    (->> (into {}))))

(defn set-possible? [{:keys [red green blue]} turn]
  (and
    (>= red (or (:red turn) 0))
    (>= green (or (:green turn) 0))
    (>= blue (or (:blue turn) 0))))

(defn game-possible? [constraints sets]
  (->> sets
       (map (partial set-possible? constraints))
       (every? true?)))

(defn possible-games
  "Returns the IDs of the games that are possible given the constraints"
  [constraints summary]
  (->>
    summary
    (medley/filter-vals (partial game-possible? constraints))
    (map first)))

(defn minimum-possible [summary]
  (into {} (medley/map-kv
             (fn [game-id sets]
               (let [reds (->> sets
                               (map :red)
                               (filter identity))
                     greens (->> sets
                                 (map :green)
                                 (filter identity))
                     blues (->> sets
                                (map :blue)
                                (filter identity))]
                 [game-id {:red   (or (apply max reds) 0)
                           :green (or (apply max greens) 0)
                           :blue  (or (apply max blues) 0)}]))
             summary)))

(defn minimum-possible->power [minimum-possible-summary]
  (->>
    minimum-possible-summary
    (medley/map-vals
      (fn [{:keys [red green blue]}]
        (* red green blue)))
    (vals)
    (reduce +)))

(defn possible-game-sum [file-path constraints]
  (->>
    (file->summary file-path)
    (possible-games constraints)
    (reduce +)))
