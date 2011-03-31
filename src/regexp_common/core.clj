(ns regexp-common.core
  (:use [clojure.contrib.except :only (throwf)])
  (:require [clojure.string :as str]))

(def digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn re-int
  [flags]
  (let [base        (or (:base flags) 10)
        gmin        (or (:gmin flags) (:group flags) 3)
        gmax        (or (:gmax flags) (:group flags) 3)
        sep         (:sep flags)
        quant       (if (:places flags) (str "{" (:places flags) "}") "+")
        chars       (subs digits 0 base)]
    (if sep
      (str "(?k:(?k:[+-]?)(?k:[" chars "]{1," gmax "}"
           "(?:" sep "[" chars "]{" gmin "," gmax "})*))")
      (str "(?k:(?k:[+-]?)(?k:[" chars "]" quant "))"))))

(defn- maybe-keep
  [keep? pattern-str]
  (if keep?
    (str/replace pattern-str "(?k:" "(")
    (str/replace pattern-str "(?k:" "(?:")))

(defn- maybe-case-insensitive
  [case-insensitive? pattern-str]
  (if case-insensitive?
    (str "(?i)" pattern-str)
    pattern-str))

(defn re
  ([type]
     (re type {}))
  ([type flags]
     (let [pattern-str (->> (case type
                                  :int (re-int flags))
                            (maybe-keep (flags :keep))
                            (maybe-case-insensitive (flags :case-insensitive)))]
       (re-pattern pattern-str))))
