;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.ron
  "Fast RON (Readable Object Notation) for Clojure.

  RON keeps the JSON data model while removing punctuation where the meaning is
  unambiguous. Pretty output is the default. Compact output preserves available
  member order, and canonical output implements RFC 8785 and I-JSON.

  [[read-string]] and [[read-bytes]] parse directly into Clojure data.
  [[write-string]], [[write-bytes]], and [[write]] render Clojure data directly.
  [[ron->json]] and [[json->ron]] preserve number source text outside canonical
  mode."
  (:refer-clojure :exclude [read-string])
  (:import
   [clojure.lang IFn]
   [java.io OutputStream]
   [java.util ArrayList]
   [ol.ron Ron Ron$Hook Ron$Mode]))

(set! *warn-on-reflection* true)

(defn- output-mode ^Ron$Mode [opts]
  (when (and (contains? opts :mode) (contains? opts :pretty))
    (throw (ex-info "Use either :mode or :pretty, not both"
                    {:mode (:mode opts) :pretty (:pretty opts)})))
  (let [mode (if (contains? opts :mode)
               (:mode opts)
               (if (contains? opts :pretty)
                 (case (:pretty opts)
                   true :pretty
                   false :compact
                   (throw (ex-info ":pretty must be true or false"
                                   {:pretty (:pretty opts)})))
                 :pretty))]
    (case mode
      :pretty Ron$Mode/PRETTY
      :compact Ron$Mode/COMPACT
      :canonical Ron$Mode/CANONICAL
      (throw (ex-info "Unknown RON output mode"
                      {:mode mode :supported #{:pretty :compact :canonical}})))))

(defn- max-depth ^long [opts]
  (let [depth (get opts :max-depth 1000)]
    (when-not (and (integer? depth) (pos? depth) (<= depth Integer/MAX_VALUE))
      (throw (ex-info ":max-depth must be a positive 32-bit integer"
                      {:max-depth depth})))
    (long depth)))

(defn- key-options [opts]
  (let [key-fn (get opts :key-fn identity)]
    (when-not (ifn? key-fn)
      (throw (ex-info ":key-fn must be callable" {:key-fn key-fn})))
    (cond
      (identical? key-fn identity) [nil 0]
      (identical? key-fn keyword) [nil 1]
      :else [key-fn 2])))

(defn- typed-hooks ^ArrayList [opts]
  (let [hooks (get opts :typed-value-hooks [])]
    (when-not (sequential? hooks)
      (throw (ex-info ":typed-value-hooks must be sequential"
                      {:typed-value-hooks hooks})))
    (ArrayList. ^java.util.Collection
     (mapv (fn [hook]
             (when-not (and (map? hook)
                            (sequential? (:path hook))
                            (contains? hook :replace-with)
                            (every? #(or (string? %)
                                         (and (integer? %) (not (neg? %))))
                                    (:path hook)))
               (throw (ex-info "Invalid typed-value hook" {:hook hook})))
             (Ron$Hook. (vec (:path hook)) (:replace-with hook)))
           hooks))))

(defn ron->json
  "Converts RON text to JSON text.

  Pretty and compact modes preserve parsed member order and number spelling.
  Canonical mode applies RFC 8785 and I-JSON.

  Options:

  | key          | description                                           |
  |--------------|-------------------------------------------------------|
  | `:mode`      | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`    | Compatibility option used only when `:mode` is absent |
  | `:max-depth` | Maximum object/array nesting depth (default `1000`)   |

  See also [[json->ron]]."
  (^String [^String ron]
   (Ron/ronToJson ron Ron$Mode/PRETTY))
  (^String [^String ron opts]
   (let [depth (max-depth opts)]
     (Ron/writeJson (Ron/parseRon ron depth) (output-mode opts) depth))))

(defn json->ron
  "Converts JSON text to RON text.

  Pretty output is the default and elides non-empty root-object braces. Typed
  value hooks replace values by path before rendering and do not recurse into a
  replacement value.

  Options:

  | key                  | description                                           |
  |----------------------|-------------------------------------------------------|
  | `:mode`              | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`            | Compatibility option used only when `:mode` is absent |
  | `:max-depth`         | Maximum object/array nesting depth (default `1000`)   |
  | `:typed-value-hooks` | Path replacements with `:path` and `:replace-with`    |

  See also [[ron->json]]."
  (^String [^String json]
   (Ron/jsonToRon json Ron$Mode/PRETTY))
  (^String [^String json opts]
   (Ron/jsonToRon json (output-mode opts) (typed-hooks opts) (max-depth opts))))

(defn read-string
  "Parses RON text directly into Clojure data.

  Objects become maps, arrays become vectors, integers become longs or bigints,
  and decimal/exponent numbers become doubles.

  Options:

  | key          | description                                               |
  |--------------|-----------------------------------------------------------|
  | `:key-fn`    | Function applied to every object key (default `identity`) |
  | `:max-depth` | Maximum object/array nesting depth (default `1000`)       |

  The built-in `identity` and `keyword` key functions use cached key paths.
  See also [[read-bytes]] and [[write-string]]."
  ([^String ron]
   (Ron/readRon ron nil 0 1000))
  ([^String ron opts]
   (let [[key-fn key-mode] (key-options opts)]
     (Ron/readRon ron ^IFn key-fn (int key-mode) (max-depth opts)))))

(defn read-bytes
  "Parses a UTF-8 byte array directly into Clojure data.

  Malformed UTF-8 throws [[ol.ron.Ron$ParseException]] with a byte offset.

  Options:

  | key          | description                                               |
  |--------------|-----------------------------------------------------------|
  | `:key-fn`    | Function applied to every object key (default `identity`) |
  | `:max-depth` | Maximum object/array nesting depth (default `1000`)       |

  See also [[read-string]] and [[write-bytes]]."
  ([^bytes input]
   (Ron/readRon input nil 0 1000))
  ([^bytes input opts]
   (let [[key-fn key-mode] (key-options opts)]
     (Ron/readRon input ^IFn key-fn (int key-mode) (max-depth opts)))))

(defn write-string
  "Renders Clojure data directly as RON text.

  Map keys may be strings, keywords, or symbols. Values may be nil, booleans,
  strings, finite numbers, keywords, symbols, maps, and sequential collections.

  Options:

  | key          | description                                           |
  |--------------|-------------------------------------------------------|
  | `:mode`      | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`    | Compatibility option used only when `:mode` is absent |
  | `:max-depth` | Maximum collection nesting depth (default `1000`)     |

  See also [[write-bytes]] and [[read-string]]."
  (^String [data]
   (Ron/writeData data Ron$Mode/PRETTY))
  (^String [data opts]
   (Ron/writeData data (output-mode opts) (max-depth opts))))

(defn write-bytes
  "Renders Clojure data directly as UTF-8 RON bytes.

  Options:

  | key          | description                                           |
  |--------------|-------------------------------------------------------|
  | `:mode`      | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`    | Compatibility option used only when `:mode` is absent |
  | `:max-depth` | Maximum collection nesting depth (default `1000`)     |

  See also [[write]] and [[write-string]]."
  ([data]
   (Ron/writeDataBytes data Ron$Mode/PRETTY))
  ([data opts]
   (Ron/writeDataBytes data (output-mode opts) (max-depth opts))))

(defn write
  "Writes UTF-8 RON to `output-stream`, flushes it, and returns it.

  The function does not close the caller's stream.

  Options:

  | key          | description                                           |
  |--------------|-------------------------------------------------------|
  | `:mode`      | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`    | Compatibility option used only when `:mode` is absent |
  | `:max-depth` | Maximum collection nesting depth (default `1000`)     |

  See also [[write-bytes]]."
  (^OutputStream [data ^OutputStream output-stream]
   (Ron/writeData data output-stream Ron$Mode/PRETTY))
  (^OutputStream [data ^OutputStream output-stream opts]
   (Ron/writeData data output-stream (output-mode opts) (max-depth opts))))

(defn ron-bytes->json-bytes
  "Converts UTF-8 RON bytes to UTF-8 JSON bytes.

  Options:

  | key          | description                                           |
  |--------------|-------------------------------------------------------|
  | `:mode`      | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`    | Compatibility option used only when `:mode` is absent |
  | `:max-depth` | Maximum object/array nesting depth (default `1000`)   |

  See also [[ron->json]]."
  ([^bytes input]
   (Ron/ronToJson input Ron$Mode/PRETTY 1000))
  ([^bytes input opts]
   (Ron/ronToJson input (output-mode opts) (max-depth opts))))

(defn json-bytes->ron-bytes
  "Converts UTF-8 JSON bytes to UTF-8 RON bytes.

  Options:

  | key                  | description                                           |
  |----------------------|-------------------------------------------------------|
  | `:mode`              | `:pretty` (default), `:compact`, or `:canonical`      |
  | `:pretty`            | Compatibility option used only when `:mode` is absent |
  | `:max-depth`         | Maximum object/array nesting depth (default `1000`)   |
  | `:typed-value-hooks` | Path replacements with `:path` and `:replace-with`    |

  See also [[json->ron]]."
  ([^bytes input]
   (Ron/jsonToRon input Ron$Mode/PRETTY (ArrayList.) 1000))
  ([^bytes input opts]
   (Ron/jsonToRon input
                  (output-mode opts)
                  (typed-hooks opts)
                  (max-depth opts))))
