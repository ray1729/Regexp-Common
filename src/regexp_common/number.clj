(ns regexp-common.number
  (:use [clojure.contrib.except :only (throwf)]))

(defn- digits-for-base
  [flags]
  (let [base (condp #(get %2 %1) flags
               :base (:base flags)
               :bin  2
               :oct  8
               :dec  10
               :hex  16
                     10)]
    (when (or (< base 1) (> base 36))
      (throwf "Base must be between 1 and 36"))
    (subs "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 base)))

(defn- group-single-char
  [s]
  (if (= 1 (count (seq (str s)))) (str "[" s "]") s))

(defn- separator
  [flags]
  (when-let [s (:sep flags)]
    (group-single-char s)))

(defn re-int
  [flags]
  (let [chars       (digits-for-base flags)
        gmin        (or (:gmin flags) (:group flags) 3)
        gmax        (or (:gmax flags) (:group flags) 3)
        sep         (separator flags)
        quant       (if (:places flags) (str "{" (:places flags) "}") "+")]
    (if sep
      (str "(?k:(?k:[+-]?)(?k:[" chars "]{1," gmax "}"
           "(?:" sep "[" chars "]{" gmin "," gmax "})*))")
      (str "(?k:(?k:[+-]?)(?k:[" chars "]" quant "))"))))

(defn re-real
  [flags]
  (let [chars  (digits-for-base flags)
        group  (or (:group flags) 3)
        sep    (separator flags)
        radix  (if-let [r (:radix flags)] (group-single-char r) "[.]")
        places (or (:places flags) "0,")
        expon  (if-let [e (:expon flags)] (group-single-char e) "[e]")]
    (if sep
      (str "(?k:(?i)(?k:[+-]?)(?k:(?=[" chars "]|" radix ")"
           "(?k:[" chars "]{1," group "}(?:(?:" sep ")[" chars "]{" group "})*)"
           "(?:(?k:" radix ")(?k:[" chars "]{" places "}))?)"
           "(?:(?k:" expon ")(?k:(?k:[+-]?)(?k:[" chars "]+))|))")
      (str "(?k:(?i)(?k:[+-]?)(?k:(?=[" chars "]|" radix ")"
           "(?k:[" chars "]*)(?:(?k:" radix ")(?k:[" chars "]{" places "}))?)"
           "(?:(?k:" expon ")(?k:(?k:[+-]?)(?k:[" chars "]+))|))"))))

(defn re-decimal
  [flags]
  (let [chars  (digits-for-base flags)
        group  (or (:group flags) 3)
        sep    (separator flags)
        radix  (if-let [r (:radix flags)] (group-single-char r) "[.]")
        places (or (:places flags) "0,")]
    (if sep
      (str "(?k:(?i)(?k:[+-]?)(?k:(?=[" chars "]|" radix ")"
           "(?k:[" chars "]{1," group "}(?:(?:" sep ")[" chars "]{" group "})*)"
           "(?:(?k:" radix ")(?k:[" chars "]{" places "}))?))")
      (str "(?k:(?i)(?k:[+-]?)(?k:(?=[" chars "]|" radix ")"
           "(?k:[" chars "]*)(?:(?k:" radix ")(?k:[" chars "]{" places "}))?))"))))
