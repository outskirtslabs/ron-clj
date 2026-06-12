;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.ron
  "Fast RON (Readable Object Notation) for Clojure.

  RON keeps the JSON data model while removing avoidable syntax: quotes,
  colons, commas, and root braces are optional where unambiguous. See
  https://github.com/starfederation/ron for the format reference. Note this
  is not Rusty Object Notation.

  ```ron
  name Ada
  age 37
  roles [admin writer]
  manager {# 200}
  ```

  ## Conversion

  [[ron->json]] and [[json->ron]] convert between RON and JSON text. Both
  preserve number source text exactly (`1E2`, `-0`, large integers), and both
  have `:pretty` and compact output modes that match the reference
  conformance corpus byte for byte.

  ## Reading and writing Clojure data

  [[read-string]] parses RON into Clojure maps, vectors, strings, numbers,
  booleans, and nil. [[write-string]] renders Clojure data as RON.

  ## Performance

  Parsing and rendering run in a single Java class ([[ol.ron.Ron]]) that
  scans `char[]` buffers with the techniques of tonsky/fast-edn: lookup-table
  character classification, fast-path token scanning, and Strings built
  directly from buffer slices."
  (:refer-clojure :exclude [read-string])
  (:import
   [java.math BigInteger]
   [java.util ArrayList HashMap List Map Map$Entry]
   [ol.ron Ron Ron$Num]))

(set! *warn-on-reflection* true)

;;;; RON <-> JSON text conversion

(defn ron->json
  "Converts RON text to JSON text.

  Number source text is preserved exactly. Object keys are emitted in
  Unicode code point order. Throws `ol.ron.Ron$ParseException` on invalid
  input.

  Options:

  | key        | description
  |------------|-------------
  | `:pretty`  | Pretty-print with 2-space indent (default `false`, compact)

  ```clojure
  (ron->json \"name Ada\\nage 37\")
  ;; => \"{\\\"age\\\":37,\\\"name\\\":\\\"Ada\\\"}\"
  ```

  See also [[json->ron]]."
  (^String [^String ron]
   (Ron/ronToJson ron false))
  (^String [^String ron {:keys [pretty]}]
   (Ron/ronToJson ron (boolean pretty))))

(defn json->ron
  "Converts JSON text to RON text.

  Number source text is preserved exactly. Object keys are emitted in
  Unicode code point order. Throws `ol.ron.Ron$ParseException` on invalid
  JSON, multiple roots, or trailing data.

  Options:

  | key        | description
  |------------|-------------
  | `:pretty`  | Pretty-print with 2-space indent and trailing newline (default `false`, compact)

  Compact output elides root object braces; pretty output keeps them:

  ```clojure
  (json->ron \"{\\\"age\\\": 37, \\\"name\\\": \\\"Ada\\\"}\")
  ;; => \"age 37 name Ada\"
  ```

  See also [[ron->json]]."
  (^String [^String json]
   (Ron/jsonToRon json false))
  (^String [^String json {:keys [pretty]}]
   (Ron/jsonToRon json (boolean pretty))))

;;;; RON -> Clojure data

(defn- num->clj
  "Realizes a number kept as source text: integers become longs (or bigints
   on overflow), anything with a fraction or exponent becomes a double."
  [^String text]
  (if (or (.contains text ".") (.contains text "e") (.contains text "E"))
    (Double/parseDouble text)
    (try
      (Long/parseLong text)
      (catch NumberFormatException _
        (bigint (BigInteger. text))))))

(defn- model->clj [v key-fn]
  (cond
    (instance? Ron$Num v)
    (num->clj (.-text ^Ron$Num v))

    (instance? Map v)
    (persistent!
     (reduce (fn [acc ^Map$Entry e]
               (assoc! acc (key-fn (.getKey e)) (model->clj (.getValue e) key-fn)))
             (transient {})
             (.entrySet ^Map v)))

    (instance? List v)
    (mapv #(model->clj % key-fn) v)

    :else v))

(defn read-string
  "Parses RON text into Clojure data.

  Objects become maps, arrays become vectors. Integers are read as longs
  (bigints on overflow); numbers with a fraction or exponent as doubles. Use
  [[ron->json]] when exact number text matters. Throws
  `ol.ron.Ron$ParseException` on invalid input, including empty input.

  Options:

  | key        | description
  |------------|-------------
  | `:key-fn`  | Applied to each object key string (default `identity`)

  ```clojure
  (read-string \"name Ada\\nroles [admin writer]\" {:key-fn keyword})
  ;; => {:name \"Ada\", :roles [\"admin\" \"writer\"]}
  ```

  See also [[write-string]]."
  ([^String ron]
   (model->clj (Ron/parseRon ron) identity))
  ([^String ron {:keys [key-fn] :or {key-fn identity}}]
   (model->clj (Ron/parseRon ron) key-fn)))

;;;; Clojure data -> RON

(defn- key->str ^String [k]
  (cond
    (string? k) k
    (keyword? k) (if-let [ns (namespace k)] (str ns "/" (name k)) (name k))
    (symbol? k) (str k)
    :else (throw (ex-info "RON object keys must be strings, keywords, or symbols"
                          {:key k :type (type k)}))))

(defn- number->text ^String [n]
  (cond
    (or (instance? Double n) (instance? Float n))
    (let [d (double n)]
      (when (or (Double/isNaN d) (Double/isInfinite d))
        (throw (ex-info "NaN and infinity have no RON representation" {:value n})))
      (str d))

    (instance? clojure.lang.Ratio n)
    (throw (ex-info "Ratios have no RON representation; convert explicitly" {:value n}))

    :else (str n)))

(defn- clj->model [v]
  (cond
    (nil? v) nil
    (boolean? v) v
    (string? v) v
    (number? v) (Ron$Num. (number->text v))
    (keyword? v) (key->str v)
    (symbol? v) (str v)
    (map? v) (let [m (HashMap. (* 2 (count v)))]
               (reduce-kv (fn [^HashMap m k val]
                            (.put m (key->str k) (clj->model val))
                            m)
                          m v))
    (sequential? v) (let [a (ArrayList. (count v))]
                      (doseq [x v]
                        (.add a (clj->model x)))
                      a)
    :else (throw (ex-info "Value has no RON representation"
                          {:value v :type (type v)}))))

(defn write-string
  "Renders Clojure data as RON text.

  Map keys may be strings, keywords, or symbols (keywords and symbols render
  as their names). Values may be nil, booleans, strings, numbers, keywords,
  symbols, maps, and sequential collections. NaN, infinity, and ratios throw
  since JSON has no representation for them.

  Options:

  | key        | description
  |------------|-------------
  | `:pretty`  | Pretty-print with 2-space indent and trailing newline (default `false`, compact)

  ```clojure
  (write-string {:name \"Ada\" :roles [\"admin\" \"writer\"]})
  ;; => \"name Ada roles[admin writer]\"
  ```

  See also [[read-string]]."
  (^String [data]
   (Ron/writeRon (clj->model data) false))
  (^String [data {:keys [pretty]}]
   (Ron/writeRon (clj->model data) (boolean pretty))))
