# clj-ron

Fast [RON (Readable Object Notation)](https://github.com/starfederation/ron) parsing and rendering for Clojure.

RON keeps the JSON value model while omitting punctuation where the result stays unambiguous. This library implements starfederation/ron v0.3.0, including universal JSON escapes, source-order formatting, typed rendering hooks, canonical RON, and RFC 8785 canonical JSON.

## Requirements

- JDK 25 or newer
- Clojure 1.12 or newer

The scalar implementation works without optional JDK modules. Resolve the Vector API to accelerate long ASCII spans:

```sh
clojure -J--add-modules=jdk.incubator.vector -M
```

Disable Vector code in a JVM that resolves the module with:

```sh
-Dol.ron.vector=false
```

The Vector API does not materially improve typical short RON tokens. Scalar and Vector modes produce identical values and bytes.

## Clojure data

```clojure
(require '[ol.ron :as ron])

(ron/read-string "name Ada\nroles [admin writer]")
;; => {"name" "Ada", "roles" ["admin" "writer"]}

(ron/read-string "name Ada" {:key-fn keyword})
;; => {:name "Ada"}

(ron/write-string {:name "Ada" :roles ["admin" "writer"]})
;; => "name Ada\nroles [admin writer]\n"
```

Pretty output is the default. Select compact or canonical output with `:mode`:

```clojure
(ron/write-string {:name "Ada"} {:mode :compact})
;; => "name Ada"

(ron/write-string {:z 1 :a 2} {:mode :canonical})
;; => "a 2 z 1"
```

`:pretty true` and `:pretty false` remain compatibility options when `:mode` is absent. Passing both options is an error.

Use `:max-depth` to limit collection nesting during parsing and rendering. Its default is 1000.

## UTF-8 bytes and streams

The byte APIs parse and render UTF-8 directly:

```clojure
(def input (.getBytes "name Ada" java.nio.charset.StandardCharsets/UTF_8))

(ron/read-bytes input)
;; => {"name" "Ada"}

(ron/write-bytes {:name "Ada"} {:mode :compact})
;; => byte[]

(with-open [output (java.io.FileOutputStream. "data.ron")]
  (ron/write {:name "Ada"} output {:mode :compact}))
```

`write` uses a bounded 64 KiB buffer, flushes but does not close its stream, and returns the stream. Byte parsing rejects malformed UTF-8 with an `ol.ron.Ron$ParseException` whose offset is a byte offset.

## RON and JSON text conversion

```clojure
(ron/ron->json "name Ada\nage 37" {:mode :compact})
;; => "{\"name\":\"Ada\",\"age\":37}"

(ron/json->ron "{\"name\":\"Ada\",\"age\":37}")
;; => "name Ada\nage 37\n"
```

Pretty and compact conversions preserve parsed member order and exact number source text. Canonical mode validates I-JSON, sorts object names by UTF-16 code units, converts numbers to IEEE 754 double precision, and applies ECMAScript number serialization.

Byte conversion equivalents are available as `ron-bytes->json-bytes` and `json-bytes->ron-bytes`.

### Typed rendering hooks

`json->ron` can replace JSON values by path before rendering:

```clojure
(ron/json->ron
  "{\"tx\":\"BE\"}"
  {:typed-value-hooks
   [{:path ["tx"]
     :replace-with {"#" "BE"}}]})
;; => "tx {# BE}\n"
```

A replacement is rendered as ordinary JSON-shaped RON. Hooks do not add native vocabulary types, and traversal does not continue inside a replacement value.

## Output modes

| Mode | Behavior |
|------|----------|
| `:pretty` | Multiline output, available member order, trailing newline for RON |
| `:compact` | Single-line output, available member order |
| `:canonical` | RFC 8785/I-JSON validation and UTF-16 object-name ordering |

Clojure hash maps have no source-order contract. Non-canonical data rendering uses their deterministic iteration order as a fallback. Array maps and sorted maps retain their meaningful iteration order. Canonical mode always uses RFC 8785 ordering.

Typed vocabulary objects such as `{#utc ...}` remain ordinary maps. Vocabulary validation and native Clojure mappings are not implemented.

## Development

```sh
clojure -T:build clean
clojure -T:build compile-java
bb test
bb lint
bb fmt:check
```

Run forked JMH benchmarks with allocation profiling:

```sh
bb jmh quick scalar repeated-records read-string write-string-compact
bb jmh quick vector long-ascii read-bytes write-bytes-compact
bb jmh full scalar
```

`quick` uses one fork and short iterations. `full` uses two forks, five warmup iterations, five measurement iterations, and the complete deterministic payload matrix.

### Measured results

On OpenJDK 25.0.4, full two-fork JMH measurements for the representative repeated-record workload produced:

| Operation | Earlier JDK 25 result | Direct-path result | Allocation change |
|---|---:|---:|---:|
| `read-string` | 4.747 ms, 16.49 MB/op | 3.429 ms, 5.90 MB/op | 64% lower |
| compact `write-string` | 4.335 ms, 8.41 MB/op | 2.858 ms, 0.65 MB/op | 92% lower |
| pretty `write-string` | 6.336 ms, 19.05 MB/op | 3.252 ms, 5.79 MB/op | 70% lower |

On the long-clean-ASCII payload, full JMH measured Vector byte reading at 5.7 times scalar speed and Vector byte writing at 3.0 times scalar speed. Tiny-object Vector results stayed within 5% of scalar speed. These results describe this machine and payload matrix; short-token RON should not be expected to receive the same Vector gain.

The number-array `write-bytes` probe returned 49,593 bytes and allocated 49,657 bytes per operation, which is 64 bytes beyond the returned array. Bounded stream writing allocated 40 bytes per operation in the same quick JMH run.

`-XX:+UseCompactObjectHeaders` can reduce allocation on supported JDKs, but clj-ron does not require it.
