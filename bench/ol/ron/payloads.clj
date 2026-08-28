(ns ol.ron.payloads
  "Deterministic payloads for the RON JMH harness."
  (:require
   [ol.ron :as ron])
  (:import
   [java.io ByteArrayOutputStream]
   [java.nio.charset StandardCharsets]))

(defn record [i]
  (array-map "id" i
             "name" (str "user-" i)
             "email" (str "user" i "@example.com")
             "active" (zero? (mod i 3))
             "score" (* 0.5 (mod i 1000))
             "tags" ["alpha" "beta" "gamma"]
             "created_at" "2026-08-28T12:00:00Z"))

(defn unique-record [i]
  (array-map (str "id-" i) i
             (str "name-" i) (str "user-" i)
             (str "email-" i) (str "user" i "@example.com")
             (str "active-" i) (zero? (mod i 3))
             (str "score-" i) (* 0.5 (mod i 1000))
             (str "tags-" i) ["alpha" "beta" "gamma"]
             (str "created_at-" i) "2026-08-28T12:00:00Z"))

(defn deep-array [depth]
  (loop [n     depth
         value (Long/valueOf 1)]
    (if (zero? n)
      value
      (recur (dec n) [value]))))

(defn data []
  {:tiny-scalar      42
   :tiny-object      (array-map "id" 1 "name" "Ada" "active" true)
   :repeated-records (mapv record (range 5000))
   :unique-records   (mapv unique-record (range 5000))
   :unique-keys      (into (sorted-map)
                           (map (fn [i] [(str "field-" i) i]))
                           (range 5000))
   :numbers          (vec (concat (map #(* 12345 %) (range 2500))
                                  (map #(* 0.12345 %) (range 2500))))
   :short-bare       (vec (repeat 5000 "abcdefgh"))
   :short-escaped    (vec (repeat 5000 "line\nquote ' slash \\"))
   :long-ascii       (vec (repeat 500 (apply str (repeat 300 "abcdefgh "))))
   :raw-unicode      (vec (repeat 500 (apply str (repeat 200 "éΩ😀 "))))
   :escape-heavy     (vec (repeat 500 (apply str (repeat 200 "\b\f\n\r\t\\"))))
   :wide-object      (into (sorted-map)
                           (map (fn [i] [(str "key-" i) (str "value-" i)]))
                           (range 2000))
   :deep             (deep-array 128)})

(defn state [payload]
  (let [value     (or (get (data) (keyword payload))
                      (throw (IllegalArgumentException. (str "unknown payload " payload))))
        ron-text  (ron/write-string value {:mode :compact})
        json-text (ron/ron->json ron-text {:mode :compact})]
    {:value      value
     :ron        ron-text
     :ron-bytes  (.getBytes ^String ron-text StandardCharsets/UTF_8)
     :json       json-text
     :json-bytes (.getBytes ^String json-text StandardCharsets/UTF_8)
     :output     (ByteArrayOutputStream. (count ron-text))}))
