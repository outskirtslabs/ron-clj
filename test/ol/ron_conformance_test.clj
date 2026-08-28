;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.ron-conformance-test
  "Manifest-driven RON v0.3.0 conformance tests."
  (:require
   [charred.api :as charred]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [ol.ron :as ron])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]
   [ol.ron Ron Ron$Mode]))

(def corpus-root (io/file "test" "ron" "testdata"))
(def conformance-root (io/file corpus-root "conformance"))
(def rfc-root (io/file corpus-root "rfc8785"))
(def vocab-root (io/file corpus-root "vocabularies"))

(def conformance-manifest
  (charred/read-json (io/file conformance-root "manifest.json") :key-fn keyword))
(def rfc-manifest
  (charred/read-json (io/file rfc-root "manifest.json") :key-fn keyword))
(def vocab-manifest
  (charred/read-json (io/file vocab-root "manifest.json") :key-fn keyword))

(defn fixture ^String [root path]
  (slurp (io/file root path)))

(defn json-value [text]
  (charred/read-json text :bigdec true))

(defn sha256 [^bytes bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn utf8 ^bytes [^String text]
  (.getBytes text StandardCharsets/UTF_8))

(defn bytes->hex [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and 0xff %)) bytes)))

(deftest ordinary-valid-cases
  (doseq [{:keys [name ronInputs jsonInput
                  expectedPrettyJSON expectedCompactJSON expectedCanonicalJSON
                  expectedPrettyRON expectedCompactRON expectedCanonicalRON
                  expectedCanonicalJSONSHA256 expectedCanonicalRONSHA256]}
          (:valid conformance-manifest)]
    (testing name
      (let [json-input     (fixture conformance-root jsonInput)
            expected-value (json-value json-input)
            expected-json  {:pretty    (fixture conformance-root expectedPrettyJSON)
                            :compact   (fixture conformance-root expectedCompactJSON)
                            :canonical (fixture conformance-root expectedCanonicalJSON)}
            expected-ron   {:pretty    (fixture conformance-root expectedPrettyRON)
                            :compact   (fixture conformance-root expectedCompactRON)
                            :canonical (fixture conformance-root expectedCanonicalRON)}]
        (doseq [ron-path ronInputs
                mode     [:pretty :compact :canonical]]
          (let [actual         (try
                                 (ron/ron->json (fixture conformance-root ron-path) {:mode mode})
                                 (catch Throwable error error))
                message        (str ron-path " " (clojure.core/name mode))
                semantic-value (if (= mode :canonical)
                                 (json-value (:canonical expected-json))
                                 expected-value)]
            (is (string? actual) message)
            (when (string? actual)
              (is (= (mode expected-json) actual) message)
              (is (= semantic-value (json-value actual)) (str message " semantic")))))
        (doseq [mode [:pretty :compact :canonical]]
          (let [actual         (ron/json->ron json-input {:mode mode})
                semantic-value (if (= mode :canonical)
                                 (json-value (:canonical expected-json))
                                 expected-value)]
            (is (= (mode expected-ron) actual) (clojure.core/name mode))
            (is (= semantic-value
                   (json-value (ron/ron->json actual {:mode :compact})))
                (str "RON round-trip " (clojure.core/name mode)))))
        (is (= expectedCanonicalJSONSHA256
               (sha256 (utf8 (:canonical expected-json)))))
        (is (= expectedCanonicalRONSHA256
               (sha256 (utf8 (:canonical expected-ron)))))))))

(deftest byte-api-valid-cases
  (doseq [{:keys [name ronInputs jsonInput
                  expectedPrettyJSON expectedCompactJSON expectedCanonicalJSON
                  expectedPrettyRON expectedCompactRON expectedCanonicalRON]}
          (:valid conformance-manifest)]
    (testing name
      (let [expected-json {:pretty    (fixture conformance-root expectedPrettyJSON)
                           :compact   (fixture conformance-root expectedCompactJSON)
                           :canonical (fixture conformance-root expectedCanonicalJSON)}
            expected-ron  {:pretty    (fixture conformance-root expectedPrettyRON)
                           :compact   (fixture conformance-root expectedCompactRON)
                           :canonical (fixture conformance-root expectedCanonicalRON)}]
        (doseq [ron-path ronInputs
                mode     [:pretty :compact :canonical]]
          (is (= (mode expected-json)
                 (String. ^bytes
                  (ron/ron-bytes->json-bytes
                   (utf8 (fixture conformance-root ron-path))
                   {:mode mode})
                          StandardCharsets/UTF_8))
              (str ron-path " " (clojure.core/name mode))))
        (doseq [mode [:pretty :compact :canonical]]
          (is (= (mode expected-ron)
                 (String. ^bytes
                  (ron/json-bytes->ron-bytes
                   (utf8 (fixture conformance-root jsonInput))
                   {:mode mode})
                          StandardCharsets/UTF_8))
              (clojure.core/name mode)))))))

(deftest ordinary-invalid-cases
  (doseq [path (:invalidRON conformance-manifest)]
    (testing path
      (is (thrown? Exception
                   (ron/ron->json (fixture conformance-root path) {:mode :compact})))))
  (doseq [path (:invalidJSON conformance-manifest)]
    (testing path
      (is (thrown? Exception
                   (ron/json->ron (fixture conformance-root path) {:mode :compact}))))))

(deftest json-to-ron-rendering-cases
  (doseq [{:keys [name jsonInput options typedValueHooks expectedRON]}
          (:jsonToRONRendering conformance-manifest)]
    (testing name
      (let [hooks (mapv (fn [{:keys [path replaceWith]}]
                          {:path path :replace-with replaceWith})
                        typedValueHooks)
            opts  (cond-> {:mode (keyword (:mode options))}
                    (seq hooks) (assoc :typed-value-hooks hooks))]
        (is (= (fixture conformance-root expectedRON)
               (ron/json->ron (fixture conformance-root jsonInput) opts)))))))

(deftest canonical-ron-source-cases
  (doseq [{:keys [name inputRON expectedCanonicalRON expectedCanonicalRONSHA256]}
          (get-in conformance-manifest [:canonicalRON :validRON])]
    (testing name
      (let [actual (ron/json->ron
                    (ron/ron->json (fixture conformance-root inputRON) {:mode :canonical})
                    {:mode :canonical})]
        (is (= (fixture conformance-root expectedCanonicalRON) actual))
        (is (= expectedCanonicalRONSHA256 (sha256 (utf8 actual)))))))
  (doseq [path (get-in conformance-manifest [:canonicalRON :invalidRON])]
    (testing path
      (is (thrown? Exception
                   (ron/ron->json (fixture conformance-root path) {:mode :canonical}))))))

(deftest rfc8785-valid-cases
  (doseq [{:keys [name inputJSON expectedCanonicalJSON expectedCanonicalUTF8Hex
                  expectedCanonicalJSONSHA256 expectedCanonicalRON
                  expectedCanonicalRONSHA256]}
          (:valid rfc-manifest)]
    (testing name
      (let [input          (fixture rfc-root inputJSON)
            canonical-json (ron/ron->json
                            (ron/json->ron input {:mode :canonical})
                            {:mode :canonical})
            canonical-ron  (ron/json->ron input {:mode :canonical})]
        (is (= (fixture rfc-root expectedCanonicalJSON) canonical-json))
        (is (= (str/trim (fixture rfc-root expectedCanonicalUTF8Hex))
               (bytes->hex (utf8 canonical-json))))
        (is (= expectedCanonicalJSONSHA256 (sha256 (utf8 canonical-json))))
        (is (= (fixture rfc-root expectedCanonicalRON) canonical-ron))
        (is (= expectedCanonicalRONSHA256 (sha256 (utf8 canonical-ron)))))))
  (doseq [{:keys [name inputJSON]} (:invalidIJSON rfc-manifest)]
    (testing name
      (is (thrown? Exception
                   (ron/json->ron (fixture rfc-root inputJSON) {:mode :canonical}))))))

(deftest rfc8785-number-serialization
  (let [vectors (charred/read-json
                 (io/file rfc-root (:numberSerialization rfc-manifest))
                 :key-fn keyword)]
    (doseq [{:keys [ieee754Hex expectedJSON expectedCanonicalRON
                    expectedCanonicalRONSHA256]}
            (:finite vectors)]
      (testing ieee754Hex
        (let [value    (Double/longBitsToDouble (Long/parseUnsignedLong ieee754Hex 16))
              json     (Ron/writeJson (Double/valueOf value) Ron$Mode/CANONICAL)
              ron-text (ron/write-string value {:mode :canonical})]
          (is (= expectedJSON json))
          (is (= expectedCanonicalRON ron-text))
          (is (= expectedCanonicalRONSHA256 (sha256 (utf8 ron-text)))))))
    (doseq [{:keys [ieee754Hex]} (:rejectedNativeValues vectors)]
      (let [value (Double/longBitsToDouble (Long/parseUnsignedLong ieee754Hex 16))]
        (is (thrown? Exception (ron/write-string value {:mode :canonical})))))))

(deftest vocabulary-base-values
  (doseq [{:keys [name inputJSON expectedRON]} (:valid vocab-manifest)]
    (testing name
      (let [input    (fixture vocab-root inputJSON)
            expected (fixture vocab-root expectedRON)
            actual   (ron/json->ron input)]
        (is (string? (ron/ron->json expected {:mode :compact})))
        (is (= (json-value input)
               (json-value (ron/ron->json actual {:mode :compact}))))))))
