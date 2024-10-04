(ns rec-parser.core
  (:gen-class)
  (:require
   [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defonce data
  (filter #(not (str/starts-with? % "%"))
          (str/split-lines (slurp "./house.rec"))))

(defn get-keys [input]
  (->> input
       (filter #(not (str/blank? %)))
       (map #(subs % 0 (str/index-of % ":")))
       distinct))

(defn get-by-key [input key]
  (->> input
       (filter #(str/includes? (str/lower-case %) (str/lower-case key)))))

(defn get-numbers [input key]
  (->> key
       (get-by-key input)
       (map #(re-find #"\d+"  %))))

(defn sum [input key]
  (->> key
       (get-numbers input)
       (map #(Integer/parseInt %))
       (reduce +)))

(sum data "amount")
