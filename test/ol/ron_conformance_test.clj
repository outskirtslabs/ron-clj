;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.ron-conformance-test
  "Manifest-driven runner for the RON conformance corpus vendored in
   test/ron/conformance/ (from starfederation/ron testdata)."
  (:require
   [charred.api :as charred]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]])
  (:import
   [ol.ron Ron]))

(def corpus-dir (io/file "test" "ron" "conformance"))

(def manifest
  (charred/read-json (io/file corpus-dir "manifest.json") :key-fn keyword))

(defn fixture ^String [rel-path]
  (slurp (io/file corpus-dir rel-path)))

(defn json-value
  "Parse JSON text to Clojure data for semantic (not textual) comparison.
   Both sides of every comparison go through this same parser, so its number
   representation choices cancel out."
  [s]
  (charred/read-json s :bigdec true))

(deftest valid-cases
  (doseq [{:keys [name ronInputs jsonInput
                  expectedPrettyJSON expectedCompactJSON
                  expectedPrettyRON expectedCompactRON]} (:valid manifest)]
    (testing name
      (let [json-input     (fixture jsonInput)
            expected-value (json-value json-input)
            compact-json   (fixture expectedCompactJSON)
            pretty-json    (fixture expectedPrettyJSON)
            compact-ron    (fixture expectedCompactRON)
            pretty-ron     (fixture expectedPrettyRON)]
        (testing "RON -> JSON"
          (doseq [ron-path ronInputs]
            (let [ron (fixture ron-path)]
              (is (= compact-json (Ron/ronToJson ron false)) (str ron-path " compact"))
              (is (= pretty-json (Ron/ronToJson ron true)) (str ron-path " pretty")))))
        (testing "JSON -> RON"
          (is (= pretty-ron (Ron/jsonToRon json-input true)))
          (is (= compact-ron (Ron/jsonToRon json-input false))))
        (testing "semantic round-trips"
          (is (= expected-value (json-value compact-json))
              "expected compact JSON fixture matches input.json value")
          (is (= expected-value (json-value (Ron/ronToJson pretty-ron false)))
              "generated pretty RON parses back to the input value")
          (is (= expected-value (json-value (Ron/ronToJson compact-ron false)))
              "generated compact RON parses back to the input value"))))))

(deftest invalid-ron
  (doseq [path (:invalidRON manifest)]
    (testing path
      (is (thrown? Exception (Ron/ronToJson (fixture path) false))))))

(deftest invalid-json
  (doseq [path (:invalidJSON manifest)]
    (testing path
      (is (thrown? Exception (Ron/jsonToRon (fixture path) false))))))
