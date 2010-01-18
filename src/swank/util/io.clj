;; Simple interface to provide println for overriding.  -wilig
(gen-interface
 :name "swank.util.io.PrintLine"
 :methods [[println [String] void]])

(ns swank.util.io
  (:use [swank util]
        [swank.util.concurrent thread])
  (:import [java.io StringWriter Reader]))

(defn read-chars
  ([rdr n] (read-chars rdr n false))
  ([#^Reader rdr n throw-exception]
     (let [sb (StringBuilder.)]
       (dotimes [i n]
         (let [c (.read rdr)]
           (if (not= c -1)
             (.append sb (char c))
             (when throw-exception
               (throw throw-exception)))))
       (str sb))))

(defn call-on-flush-stream
  "Creates a stream that will call a given function when flushed."
  ([flushf]
     (let [closed? (atom false)
           #^StringWriter stream
           (proxy [StringWriter swank.util.io.PrintLine] []
             (close [] (reset! closed? true))
	     (println [arg] (flushf arg))
             (flush []
                    (let [#^StringWriter me this
                          len (.. me getBuffer length)]
                      (when (> len 0)
                        (flushf (.. me getBuffer (substring 0 len)))
                        (.. me getBuffer (delete 0 len))))))]
       (dothread
        (thread-set-name "Call-on-write Stream")
        (continuously
         (Thread/sleep 200)
         (when-not @closed?
           (.flush stream))))
       stream))
  {:tag StringWriter})

;; A simpler replacement for call-on-flush-stream.  If any weirdness
;; occurs in writing results switch back to the original.  -wilig
;;
;; The addition of println here is the critical fix to get some
;; of the contrib.sql functions to work properly with slime.  Some
;; functions in contrib.sql are found of printing thier error
;; messages before raising an exception.  Unclear whether a warning
;; should be emitted in such cases.
(defn call-on-write-stream
  "Creates a stream that will call a given function when written too.
   Only handles write with one argument."
  ([flushf]
     (let [#^StringWriter stream
           (proxy [StringWriter swank.util.io.PrintLine] []
	     (println [arg] (flushf arg))
	     (write [arg] (flushf arg)))]
       stream))
  {:tag StringWriter})