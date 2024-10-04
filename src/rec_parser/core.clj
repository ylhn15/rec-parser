(ns rec-parser.core
  (:gen-class)
  (:require
   [babashka.cli :as cli]
   [clojure.string :as str]))

(defonce data
  (filter #(not (str/starts-with? % "%"))
          (str/split-lines (slurp "./house.rec"))))

(defn pretty-print [output]
  (dorun (map #(println %) output)))

(defn get-keys [input]
  (pretty-print
   (into [] (distinct (map #(subs % 0 (str/index-of % ":")) (filter #(not (str/blank? %)) input))))))

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

(defn show-help
  [spec]
  (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))

(def cli-spec
  {:spec
   {:get-keys {:desc "Print all keys in rec-file"
               :alias :gk}
    :sum {:coerce :string
          :desc "Sum up the values with key"
          :alias :s}}
   :error-fn
   (fn [{:keys [spec type cause msg option] :as data}]
     (if (= :org.babashka/cli type)
       (case cause
         :require
         (println
          (format "Missing required argument: %s\n" option))
         :validate
         (println
          (format "%s does not exist!\n" msg)))))})

(defn -main
  [args]
  (let [opts (cli/parse-opts args cli-spec)]
    (if (or (:help opts) (:h opts))
      (println (show-help cli-spec))
      (cond (contains? opts :get-keys) (get-keys data)
            (contains? opts :sum) (println (sum data (get opts :sum)))
            :else
            (println "Here are your cli args!:" opts)))))

(-main *command-line-args*)
