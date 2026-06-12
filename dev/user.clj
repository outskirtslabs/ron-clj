(ns user
  (:require
   [clj-reload.core :as clj-reload]
   [ol.dev.portal :as portal]))

((requiring-resolve 'hashp.install/install!))

(set! *warn-on-reflection* true)

(defonce portal! (portal/open-portals))

;; Configure the paths containing clojure sources we want clj-reload to reload
(clj-reload/init {:dirs      ["src" "dev" "test"]
                  :no-reload #{'user 'dev 'ol.dev.portal}})

(comment
  (portal/logs 5)
  (portal/last-log)
  (portal/clear-logs!)

  (clj-reload/reload)
  (clj-reload/reload {:only :all}) ;; rcf
  (clojure.repl.deps/sync-deps)
  ;;;
  )
