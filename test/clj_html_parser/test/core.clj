(ns clj-html-parser.test.core
  (:use [clj-html-parser.core] :reload)
  (:use [clojure.test]))

(def cwd (System/getProperty "user.dir"))
(def h (slurp (str cwd "/test-data/one-link.html")))

(deftest in-links-test
  (is (= '("http://yakkstr.com/users/ddonnell" "http://yakkstr.com/users/nmurray") (in-links (slurp (str cwd "/test-data/outlinks.html")) "http://yakkstr.com"))))

(deftest get-elements-by-name-test
  (let [out (get-elements-by-name h "a")]
    (is (= 1 (count out)))))

(deftest href-to-url-test
  (is (= "http://yakkstr.com/users/ddonnell" (href-to-url "/users/ddonnell" "http://yakkstr.com")))
  (is (= "http://nmurray.com/something" (href-to-url "http://nmurray.com/something" "http://yakkstr.com"))))
