(ns regexp-common.core
  (:use [clojure.contrib.except :only (throwf)]
        [regexp-common.number])
  (:require [clojure.string :as str]))

(defn- maybe-keep
  [flags pattern-str]
  (if (flags :keep)
    (str/replace pattern-str "(?k:" "(")
    (str/replace pattern-str "(?k:" "(?:")))

(defn- maybe-case-insensitive
  [flags pattern-str]
  (if (flags :i)
    (str "(?i)" pattern-str)
    pattern-str))

(defn- parse-flags
  "Parse a list of flags. Each flag name must be a keyword, and the
  value must not be a keyword. If the value is omitted, true is
  assumed. Returns a map.

  e.g. (parse-flags [:i :j 1 :k \"two\"])
  => {:i true, :j 1, :k \"two\"}"
  [flags]
  (loop [flags (seq flags) accum {}]
    (if flags
      (let [k (first flags)]
        (if (and (next flags) (not (keyword? (second flags))))
          (recur (nnext flags) (assoc accum k (second flags)))
          (recur (next flags) (assoc accum k true))))
      accum)))

(defn re
  ([type & fs]
     (let [flags        (parse-flags fs)
           pattern-str  (->> (case type
                                  :int     (re-int flags)
                                  :real    (re-real flags)
                                  :decimal (re-decimal flags)
                                  :hex     (re-real (assoc flags :base 16))
                                  :dec     (re-real (assoc flags :base 10))
                                  :oct     (re-real (assoc flags :base 8))
                                  :bin     (re-real (assoc flags :base 2)))
                            (maybe-keep flags)
                            (maybe-case-insensitive flags))]
       (re-pattern pattern-str))))
