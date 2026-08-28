;; Quick throughput sanity check, not a rigorous benchmark.
;; Run: clojure -M:dev:kaocha -e '(load-file "dev/bench.clj")'
(ns bench
  (:require
   [charred.api :as charred]
   [ol.ron :as ron])
  (:import
   [ol.ron Ron]))

(defn rand-record [i]
  {"id" i
   "name" (str "user-" i)
   "email" (str "user" i "@example.com")
   "active" (zero? (mod i 3))
   "score" (* 0.5 (mod i 1000))
   "note" (rand-nth ["plain text" "it's quoted" "she said \"hi\"" "multi word note here"])
   "tags" (vec (repeatedly (inc (mod i 4)) #(rand-nth ["alpha" "beta" "gamma" "delta"])))
   "address" {"street" (str (mod i 999) " Main St")
              "city" (rand-nth ["Vienna" "Berlin" "Lisbon" "Osaka"])
              "zip" (str (+ 10000 (mod i 89999)))}})

(def data {"users" (mapv rand-record (range 5000))})

(def ron-doc (ron/write-string data {:pretty true}))
(def json-doc (ron/ron->json ron-doc {:pretty true}))

(defn mb [^String s] (/ (count (.getBytes s "UTF-8")) 1e6))

(defn bench [label ^String input f]
  (dotimes [_ 30] (f input))                       ; warmup
  (let [runs 50
        t0 (System/nanoTime)
        _ (dotimes [_ runs] (f input))
        elapsed-s (/ (- (System/nanoTime) t0) 1e9)
        per-run-ms (* 1000 (/ elapsed-s runs))]
    (printf "%-34s %8.2f ms/run  %8.1f MB/s%n"
            label per-run-ms (/ (* runs (mb input)) elapsed-s))
    (flush)))

(printf "RON doc: %.2f MB, JSON doc: %.2f MB%n" (mb ron-doc) (mb json-doc))
(bench "Ron/parseRon (RON -> model)" ron-doc #(Ron/parseRon %))
(bench "ron->json compact" ron-doc #(ron/ron->json % {:mode :compact}))
(bench "ron/read-string (RON -> Clojure)" ron-doc #(ron/read-string %))
(bench "Ron/parseJson (JSON -> model)" json-doc #(Ron/parseJson %))
(bench "json->ron compact" json-doc #(ron/json->ron % {:mode :compact}))
(bench "charred read-json (reference)" json-doc #(charred/read-json %))
