;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.ron-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [ol.ron :as ron])
  (:import
   [java.io ByteArrayOutputStream IOException OutputStream]
   [java.nio.charset StandardCharsets]
   [ol.ron Ron$ParseException]))

(defn utf8 ^String [^bytes bytes]
  (String. bytes StandardCharsets/UTF_8))

(deftest output-modes
  (testing "pretty is the default"
    (is (= "{\n  \"name\": \"Ada\",\n  \"age\": 37\n}"
           (ron/ron->json "name Ada\nage 37")))
    (is (= "name Ada\nage 37\n"
           (ron/json->ron "{\"name\":\"Ada\",\"age\":37}")))
    (is (= "name Ada\nage 37\n"
           (ron/write-string (array-map "name" "Ada" "age" 37)))))
  (testing "compact preserves member order"
    (is (= "{\"name\":\"Ada\",\"age\":37}"
           (ron/ron->json "name Ada\nage 37" {:mode :compact})))
    (is (= "name Ada age 37"
           (ron/json->ron "{\"name\":\"Ada\",\"age\":37}" {:mode :compact}))))
  (testing ":pretty remains compatible when :mode is absent"
    (is (= "name Ada age 37"
           (ron/write-string (array-map "name" "Ada" "age" 37) {:pretty false})))
    (is (= "name Ada\nage 37\n"
           (ron/write-string (array-map "name" "Ada" "age" 37) {:pretty true}))))
  (testing "bad mode options fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ron/write-string {} {:mode :compact :pretty false})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ron/write-string {} {:mode :wide})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ron/write-string {} {:pretty :yes})))))

(deftest string-escapes-and-numbers
  (testing "RON escapes are universal and classification precedes decoding"
    (is (= ["a b" "true" "a\nb" "\\"]
           (ron/read-string "[a\\u0020b tr\\u0075e a\\nb \\\\]")))
    (is (= ["a" "b" "c" "d"]
           (ron/read-string "[a b c d]")))
    (is (= ["a" "b" "c" "d"]
           (ron/read-bytes (.getBytes "[a b c d]" StandardCharsets/UTF_8))))
    (is (= ["a\"b" "a \"quoted\" phrase" "contains '' inside"]
           (ron/read-string "['a\"b' \"\"\"a \"quoted\" phrase\"\"\" '''contains '' inside''']"))))
  (testing "number realization"
    (let [values (ron/read-string "[1E2 -0 18446744073709551615 -12.5e+2]")]
      (is (= [100.0 0 18446744073709551615N -1250.0] values))
      (is (instance? Long (second values)))
      (is (instance? Double (first values)))))
  (testing "non-canonical conversions preserve source number text"
    (is (= "[1E2,-0,18446744073709551615,-12.5e+2]"
           (ron/ron->json "[1E2 -0 18446744073709551615 -12.5e+2]"
                          {:mode :compact}))))
  (testing "malformed escapes fail"
    (doseq [input ["\\q" "\\u12" "\\u12xz" "\\uD800" "\\uDC00"]]
      (is (thrown? Ron$ParseException (ron/read-string input)) input))))

(deftest direct-data-apis
  (let [data (array-map "name" "Ada"
                        "roles" ["admin" "writer"]
                        "active" true
                        "score" 1.5
                        "none" nil)]
    (testing "String and byte paths agree"
      (doseq [mode [:pretty :compact :canonical]]
        (let [text  (ron/write-string data {:mode mode})
              bytes (ron/write-bytes data {:mode mode})]
          (is (= text (utf8 bytes)) (name mode))
          (is (= data (ron/read-string text)) (name mode))
          (is (= data (ron/read-bytes bytes)) (name mode)))))
    (testing "keyword and custom key functions"
      (is (= {:name "Ada" :roles ["admin" "writer"] :active true :score 1.5 :none nil}
             (ron/read-string (ron/write-string data) {:key-fn keyword})))
      (let [calls (atom [])]
        (is (= {"NAME" 2}
               (ron/read-string "name 1 name 2"
                                {:key-fn #(do (swap! calls conj %) (.toUpperCase ^String %))})))
        (is (= ["name" "name"] @calls))))
    (testing "stream output flushes but does not close"
      (let [flushed? (atom false)
            closed?  (atom false)
            output   (proxy [ByteArrayOutputStream] []
                       (flush [] (reset! flushed? true))
                       (close [] (reset! closed? true)))]
        (is (identical? output (ron/write data output {:mode :compact})))
        (is @flushed?)
        (is (false? @closed?))
        (is (= (ron/write-string data {:mode :compact})
               (utf8 (.toByteArray output))))))))

(deftest long-byte-input-scalar-fallbacks
  (let [text       (apply str (repeat 4096 "a"))
        utf8-bytes #(.getBytes ^String % StandardCharsets/UTF_8)
        bare-ron   (utf8-bytes (str "[" text "]"))
        quoted-ron (utf8-bytes (str "['" text "']"))
        json       (utf8-bytes (str "{\"value\":\"" text "\"}"))]
    (is (= [text] (ron/read-bytes bare-ron)))
    (is (= [text] (ron/read-bytes quoted-ron)))
    (is (= text
           (get (ron/read-bytes
                 (ron/json-bytes->ron-bytes json {:mode :compact}))
                "value")))))

(deftest buffered-stream-output
  (testing "all modes and oversized scalars match String output"
    (let [data (array-map "text" (apply str (repeat 70000 "x"))
                          "nested" [{"value" 1}])]
      (doseq [mode [:pretty :compact :canonical]]
        (let [output (ByteArrayOutputStream.)]
          (ron/write data output {:mode mode})
          (is (= (ron/write-string data {:mode mode})
                 (utf8 (.toByteArray output)))
              (name mode))))))
  (testing "I/O errors retain their checked type"
    (let [output (proxy [OutputStream] []
                   (write
                     ([_] (throw (IOException. "boom")))
                     ([_ _ _] (throw (IOException. "boom")))))]
      (is (thrown-with-msg? IOException #"boom"
                            (ron/write {:value 1} output {:mode :compact}))))))

(deftest byte-conversions
  (let [ron-input  (.getBytes "name Ada\nage 37" StandardCharsets/UTF_8)
        json-input (.getBytes "{\"name\":\"Ada\",\"age\":37}" StandardCharsets/UTF_8)]
    (is (= "{\"name\":\"Ada\",\"age\":37}"
           (utf8 (ron/ron-bytes->json-bytes ron-input {:mode :compact}))))
    (is (= "name Ada age 37"
           (utf8 (ron/json-bytes->ron-bytes json-input {:mode :compact}))))
    (is (thrown? Ron$ParseException
                 (ron/read-bytes (byte-array [(unchecked-byte 0xc0) (unchecked-byte 0xaf)]))))))

(deftest typed-value-hooks
  (let [json "{\"tx\":\"BE\",\"events\":[{\"tx\":\"CA\"}]}"]
    (is (= "tx {# BE}\nevents [{tx {# CA}}]\n"
           (ron/json->ron
            json
            {:typed-value-hooks [{:path ["tx"] :replace-with {"#" "BE"}}
                                 {:path ["events" 0 "tx"] :replace-with {"#" "CA"}}]})))))

(deftest depth-and-invalid-values
  (testing "maximum depth"
    (is (= [[1]] (ron/read-string "[[1]]" {:max-depth 2})))
    (is (= [[1]]
           (ron/read-bytes (.getBytes "[[1]]" StandardCharsets/UTF_8)
                           {:max-depth 2})))
    (is (thrown? Ron$ParseException
                 (ron/read-bytes (.getBytes "[[1]]" StandardCharsets/UTF_8)
                                 {:max-depth 1})))
    (is (thrown? Ron$ParseException
                 (ron/read-string "[[1]]" {:max-depth 1})))
    (doseq [mode [:pretty :compact :canonical]]
      (let [opts {:mode mode :max-depth 2}]
        (is (= (ron/write-string [[1]] opts)
               (utf8 (ron/write-bytes [[1]] opts))))
        (is (thrown? IllegalArgumentException
                     (ron/write-string [[1]] {:mode mode :max-depth 1})))
        (is (thrown? IllegalArgumentException
                     (ron/write-bytes [[1]] {:mode mode :max-depth 1})))))
    (is (thrown? IllegalArgumentException
                 (ron/write [[1]]
                            (ByteArrayOutputStream.)
                            {:mode :compact :max-depth 1})))
    (let [too-deep-text (str (apply str (repeat 1001 "["))
                             "0"
                             (apply str (repeat 1001 "]")))
          too-deep-data (nth (iterate vector 0) 1001)]
      (is (thrown? Ron$ParseException (ron/read-string too-deep-text)))
      (is (thrown? IllegalArgumentException (ron/write-string too-deep-data))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ron/write-string [] {:max-depth 0})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ron/read-string "[]" {:max-depth 0}))))
  (testing "unsupported values"
    (is (thrown? IllegalArgumentException (ron/write-string {:x ##NaN})))
    (is (thrown? IllegalArgumentException (ron/write-string {:x 1/3})))
    (is (thrown? IllegalArgumentException (ron/write-string #{1 2})))))

(deftest numeric-boundaries
  (is (= [Long/MIN_VALUE
          Long/MAX_VALUE
          -9223372036854775809N
          9223372036854775808N]
         (ron/read-string
          "[-9223372036854775808 9223372036854775807 -9223372036854775809 9223372036854775808]")))
  (is (= [Long/MIN_VALUE
          Long/MAX_VALUE
          -9223372036854775809N
          9223372036854775808N]
         (ron/read-bytes
          (.getBytes
           "[-9223372036854775808 9223372036854775807 -9223372036854775809 9223372036854775808]"
           StandardCharsets/UTF_8)))))

(deftest strict-utf8
  (doseq [octets [[0x80]
                  [0xc0 0xaf]
                  [0xe0 0x80 0x80]
                  [0xed 0xa0 0x80]
                  [0xf4 0x90 0x80 0x80]
                  [0xc2]]]
    (let [input (byte-array (map unchecked-byte octets))]
      (is (thrown? Ron$ParseException (ron/read-bytes input)) (pr-str octets))
      (is (thrown? Ron$ParseException (ron/ron-bytes->json-bytes input)) (pr-str octets))
      (is (thrown? Ron$ParseException (ron/json-bytes->ron-bytes input)) (pr-str octets))))
  (let [error (try
                (ron/read-bytes (byte-array [(unchecked-byte 0xc2)]))
                (catch Ron$ParseException exception
                  exception))]
    (is (instance? Ron$ParseException error))
    (is (zero? (.offset ^Ron$ParseException error)))))

(deftest concurrent-pools
  (let [data         (array-map "name" "Ada" "values" (vec (range 100)))
        expected     (ron/write-string data {:mode :compact})
        calls        (doall
                      (repeatedly 32
                                  #(future
                                     (let [bytes (ron/write-bytes data {:mode :compact})]
                                       [(utf8 bytes) (ron/read-bytes bytes)]))))
        stream-calls (doall
                      (repeatedly 32
                                  #(future
                                     (let [output (ByteArrayOutputStream.)]
                                       (ron/write data output {:mode :compact})
                                       (utf8 (.toByteArray output))))))]
    (doseq [[text value] (map deref calls)]
      (is (= expected text))
      (is (= data value)))
    (doseq [text (map deref stream-calls)]
      (is (= expected text)))))

(deftest conversion-depth
  (is (= "values [[1]]\n"
         (ron/json->ron "{\"values\":[[1]]}" {:max-depth 3})))
  (is (= "values [[1]]\n"
         (utf8
          (ron/json-bytes->ron-bytes
           (.getBytes "{\"values\":[[1]]}" StandardCharsets/UTF_8)
           {:max-depth 3}))))
  (is (thrown? Ron$ParseException
               (ron/json->ron "{\"values\":[[1]]}" {:max-depth 2})))
  (is (thrown? Ron$ParseException
               (ron/json-bytes->ron-bytes
                (.getBytes "{\"values\":[[1]]}" StandardCharsets/UTF_8)
                {:max-depth 2}))))
