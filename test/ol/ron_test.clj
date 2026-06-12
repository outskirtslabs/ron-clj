;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.ron-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [ol.ron :as ron]))

(deftest ron->json-test
  (testing "compact"
    (is (= "{\"age\":37,\"name\":\"Ada\"}" (ron/ron->json "name Ada\nage 37"))))
  (testing "pretty"
    (is (= "{\n  \"age\": 37,\n  \"name\": \"Ada\"\n}"
           (ron/ron->json "name Ada\nage 37" {:pretty true}))))
  (testing "number text is preserved"
    (is (= "[1E2,-0,18446744073709551615,-12.5e+2]"
           (ron/ron->json "[1E2 -0 18446744073709551615 -12.5e+2]"))))
  (testing "invalid input throws"
    (is (thrown? ol.ron.Ron$ParseException (ron/ron->json "")))
    (is (thrown? ol.ron.Ron$ParseException (ron/ron->json "[1 2")))
    (is (thrown? ol.ron.Ron$ParseException (ron/ron->json "{name Ada} nope")))))

(deftest json->ron-test
  (testing "compact elides root braces"
    (is (= "age 37 name Ada" (ron/json->ron "{\"age\": 37, \"name\": \"Ada\"}"))))
  (testing "pretty keeps root braces and trailing newline"
    (is (= "{\n  age 37\n  name Ada\n}\n"
           (ron/json->ron "{\"name\": \"Ada\", \"age\": 37}" {:pretty true}))))
  (testing "strings needing quotes"
    (is (= "note''it's fine''" (ron/json->ron "{\"note\": \"it's fine\"}")))
    (is (= "'true'" (ron/json->ron "\"true\""))))
  (testing "invalid input throws"
    (is (thrown? ol.ron.Ron$ParseException (ron/json->ron "{")))
    (is (thrown? ol.ron.Ron$ParseException (ron/json->ron "null null")))))

(deftest read-string-test
  (testing "elided root object"
    (is (= {"name" "Ada" "age" 37 "roles" ["admin" "writer"]}
           (ron/read-string "name Ada\nage 37\nroles [admin writer]"))))
  (testing "key-fn"
    (is (= {:name "Ada" :manager {:# 200}}
           (ron/read-string "name Ada\nmanager {# 200}" {:key-fn keyword}))))
  (testing "scalar roots"
    (is (true? (ron/read-string "true")))
    (is (nil? (ron/read-string "null")))
    (is (= "hello" (ron/read-string "hello")))
    (is (= 123 (ron/read-string "123"))))
  (testing "number realization"
    (is (= [100.0 0 18446744073709551615N -1250.0]
           (ron/read-string "[1E2 -0 18446744073709551615 -12.5e+2]")))
    (is (instance? Long (ron/read-string "42")))
    (is (instance? Double (ron/read-string "1.5"))))
  (testing "duplicate keys: last wins"
    (is (= {"a" 2} (ron/read-string "{a 1 a 2}")))))

(deftest write-string-test
  (testing "string keys"
    (is (= "age 37 name Ada" (ron/write-string {"name" "Ada" "age" 37}))))
  (testing "keyword keys and values render as names"
    (is (= "role admin" (ron/write-string {:role :admin}))))
  (testing "pretty"
    (is (= "{\n  nums [1 2 3]\n  who Ada\n}\n"
           (ron/write-string {:who "Ada" :nums [1 2 3]} {:pretty true}))))
  (testing "quoting"
    (is (= "'key name''Ada Lovelace'"
           (ron/write-string {"key name" "Ada Lovelace"})))
    (is (= "s'123'" (ron/write-string {:s "123"}))))
  (testing "round-trip"
    (let [data {"users" [{"id" 100 "name" "Ada" "active" true}
                         {"id" 200 "name" "Grace" "active" false}]
                "meta"  {"count" 2 "tags" ["a" "b"] "score" 1.5 "none" nil}}]
      (is (= data (ron/read-string (ron/write-string data))))
      (is (= data (ron/read-string (ron/write-string data {:pretty true}))))))
  (testing "unrepresentable values throw"
    (is (thrown? clojure.lang.ExceptionInfo (ron/write-string {:x ##NaN})))
    (is (thrown? clojure.lang.ExceptionInfo (ron/write-string {:x 1/3})))
    (is (thrown? clojure.lang.ExceptionInfo (ron/write-string #{1 2})))))
