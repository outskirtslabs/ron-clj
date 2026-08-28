(ns ol.ron.jmh
  "Forked JMH throughput and allocation benchmarks for RON operations.

  Usage: `clojure -M:jmh [quick|full] [scalar|vector] [payload-or-benchmark ...]`."
  (:refer-clojure :exclude [read-string])
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [jmh.core :as jmh]
   [ol.ron :as ron]
   [ol.ron.payloads :as payloads])
  (:import
   [java.io ByteArrayOutputStream]
   [ol.ron Ron Ron$Mode])
  (:gen-class))

(defn exact-ron-parse [state]
  (Ron/parseRon ^String (:ron state)))

(defn exact-json-parse [state]
  (Ron/parseJson ^String (:json state)))

(defn read-string [state]
  (ron/read-string (:ron state)))

(defn read-bytes [state]
  (ron/read-bytes (:ron-bytes state)))

(defn write-string-compact [state]
  (Ron/writeData (:value state) Ron$Mode/COMPACT))

(defn write-string-pretty [state]
  (Ron/writeData (:value state) Ron$Mode/PRETTY))

(defn write-string-canonical [state]
  (Ron/writeData (:value state) Ron$Mode/CANONICAL))

(defn write-bytes-compact [state]
  (Ron/writeDataBytes (:value state) Ron$Mode/COMPACT))

(defn write-bytes-pretty [state]
  (Ron/writeDataBytes (:value state) Ron$Mode/PRETTY))

(defn write-bytes-canonical [state]
  (Ron/writeDataBytes (:value state) Ron$Mode/CANONICAL))

(defn write-output-stream [state]
  (let [^ByteArrayOutputStream output (:output state)]
    (.reset output)
    (Ron/writeData (:value state) output Ron$Mode/COMPACT)))

(defn ron->json-string [state]
  (Ron/ronToJson ^String (:ron state) Ron$Mode/COMPACT))

(defn json->ron-string [state]
  (Ron/jsonToRon ^String (:json state) Ron$Mode/COMPACT))

(defn ron->json-bytes [state]
  (Ron/ronToJson ^bytes (:ron-bytes state) Ron$Mode/COMPACT 1000))

(defn json->ron-bytes [state]
  (Ron/jsonToRon ^bytes (:json-bytes state) Ron$Mode/COMPACT (java.util.ArrayList.) 1000))

(def benchmark-spec
  {:benchmarks
   [{:name :exact-ron-parse :fn `exact-ron-parse :args [:state/payload]}
    {:name :exact-json-parse :fn `exact-json-parse :args [:state/payload]}
    {:name :read-string :fn `read-string :args [:state/payload]}
    {:name :read-bytes :fn `read-bytes :args [:state/payload]}
    {:name :write-string-compact :fn `write-string-compact :args [:state/payload]}
    {:name :write-string-pretty :fn `write-string-pretty :args [:state/payload]}
    {:name :write-string-canonical :fn `write-string-canonical :args [:state/payload]}
    {:name :write-bytes-compact :fn `write-bytes-compact :args [:state/payload]}
    {:name :write-bytes-pretty :fn `write-bytes-pretty :args [:state/payload]}
    {:name :write-bytes-canonical :fn `write-bytes-canonical :args [:state/payload]}
    {:name :write-output-stream :fn `write-output-stream :args [:state/payload]}
    {:name :ron->json-string :fn `ron->json-string :args [:state/payload]}
    {:name :json->ron-string :fn `json->ron-string :args [:state/payload]}
    {:name :ron->json-bytes :fn `ron->json-bytes :args [:state/payload]}
    {:name :json->ron-bytes :fn `json->ron-bytes :args [:state/payload]}]
   :states     {:payload {:fn `payloads/state :args [:param/payload]}}
   :params     {:payload ["tiny-scalar"
                          "tiny-object"
                          "repeated-records"
                          "unique-records"
                          "unique-keys"
                          "numbers"
                          "short-bare"
                          "short-escaped"
                          "long-ascii"
                          "raw-unicode"
                          "escape-heavy"
                          "wide-object"
                          "deep"]}})

(def base-options
  {:mode             :average
   :output-time-unit :us
   :profilers        ["gc"]
   :status           true
   :fail-on-error    true
   :compile-path     "target/jmh-classes"})

(def quick-payloads ["tiny-object" "repeated-records" "long-ascii"])

(defn run-options [preset vector-mode]
  (let [jvm-args (cond-> ["-Xms1g" "-Xmx1g"]
                   (= vector-mode "scalar") (conj "-Dol.ron.vector=false")
                   (= vector-mode "vector") (conj "--add-modules" "jdk.incubator.vector"))]
    (merge base-options
           (if (= preset "full")
             {:warmup      {:iterations 5 :time [2 :s]}
              :measurement {:iterations 5 :time [2 :s]}
              :fork        {:count 2 :jvm {:append-args jvm-args}}}
             {:warmup      {:iterations 2 :time [1 :s]}
              :measurement {:iterations 3 :time [1 :s]}
              :fork        {:count 1 :jvm {:append-args jvm-args}}}))))

(def benchmark-names
  (into #{} (map (comp name :name)) (:benchmarks benchmark-spec)))

(def payload-names
  (set (get-in benchmark-spec [:params :payload])))

(defn score [value]
  (double (if (number? value) value (first value))))
(defn report [results]
  (println)
  (printf "%-24s %-18s %12s %14s%n" "benchmark" "payload" "µs/op" "alloc B/op")
  (doseq [{:keys [name params statistics secondary] :as result} results]
    (if (and name statistics)
      (printf "%-24s %-18s %12.2f %14.0f%n"
              (clojure.core/name name)
              (str (:payload params))
              (score (:mean statistics))
              (score (or (get-in secondary ["gc.alloc.rate.norm" :score])
                         (get-in secondary [:gc.alloc.rate.norm :score])
                         -1.0)))
      (pp/pprint result)))
  (flush))

(defn -main [& args]
  (let [[preset vector-mode & selections] args
        preset                            (or preset "quick")
        vector-mode                       (or vector-mode "scalar")
        selected-benchmarks               (filterv benchmark-names selections)
        selected-payloads                 (filterv payload-names selections)
        payloads                          (cond
                                            (seq selected-payloads) selected-payloads
                                            (= preset "full") (vec payload-names)
                                            :else quick-payloads)
        spec                              (assoc-in benchmark-spec [:params :payload] payloads)
        options                           (cond-> (run-options preset vector-mode)
                                            (seq selected-benchmarks) (assoc :select (mapv keyword selected-benchmarks)))]
    (when-not (contains? #{"quick" "full"} preset)
      (throw (IllegalArgumentException. (str "unknown preset " preset))))
    (when-not (contains? #{"scalar" "vector"} vector-mode)
      (throw (IllegalArgumentException. (str "unknown runtime mode " vector-mode))))
    (println "JMH" preset vector-mode (str/join "," payloads))
    (report (jmh/run spec options))
    (shutdown-agents)))
