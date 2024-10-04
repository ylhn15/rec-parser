(ns rec-parser.core
  (:gen-class)
  (:require
   [babashka.cli :as cli]
   [clojure.string :as str]))

(defonce data
  (filter #(not (str/starts-with? % "%"))
          (str/split-lines (slurp "./house.rec"))))

(type "Test")

(defn pretty-print [output]
  (if (= (type output) java.lang.String)
    (println output)
    (dorun (map #(println %) output))))

(defn split-at-empty-line [input]
  (->> input
       (partition-by #(= % ""))
       (filter #(seq %))))

(defn remove-empty-entries [nested]
  (->> nested
       (mapv #(vec (remove empty? %)))
       (filterv seq)))

(defn get-by-id [input  id]
  (-> input
      rest
      split-at-empty-line
      remove-empty-entries
      (nth id "Not found")))

(get-by-id data 2)

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

(defn show-help
  [spec]
  (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))

(def cli-spec
  {:spec
   {:get-keys {:desc "Print all keys in rec-file"
               :alias :gk}
    :get {:coerce :string
          :desc "Get all values with key"
          :alias :g}
    :get-by-id {:coerce :int
                :desc "Get values by id"
                :alias :id}
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
      (cond (contains? opts :get-keys) (pretty-print  (get-keys data))
            (contains? opts :sum) (pretty-print (sum data (get opts :sum)))
            (contains? opts :get) (pretty-print (get-by-key data (get opts :get)))
            (contains? opts :get-by-id) (pretty-print (get-by-id data (get opts :get-by-id)))
            :else
            (println "Here are your cli args!:" opts)))))

(-main *command-line-args*)
