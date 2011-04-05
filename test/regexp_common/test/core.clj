(ns regexp-common.test.core
  (:use [regexp-common.core] :reload)
  (:use [clojure.test]))

(deftest re-int-test
  (testing "Basic re-int functionality"
    (let [r (re :int)]
      (is (re-matches r "123") "it matches 123")
      (is (re-matches r "+123") "it matches +123")
      (is (re-matches r "-987") "it matches -987")
      (is (re-matches r "0") "it matches 0")
      (is (not (re-matches r "ABC")) "it doest't match ABC")))
  (testing "Base 16 re-int functionality"
    (let [r (re :int :base 16)]
      (is (re-matches r "123") "it matches 123")
      (is (re-matches r "+123") "it matches +123")
      (is (re-matches r "-987") "it matches -987")
      (is (re-matches r "0") "it matches 0")
      (is (re-matches r "ABC") "it matches ABC")
      (is (re-matches r "+ABC") "it matches +ABC")
      (is (re-matches r "-ABC") "it matches -ABC")
      (is (not (re-matches r "ABCZ")) "it doesn't match ABCZ"))))
