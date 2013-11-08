(ns com.lemonodor.getopt-test
  (:require [clojure.test :refer :all]
            [com.lemonodor.getopt :as getopt]))


(deftest short-opts-tests
  (testing "shortopts with no args, no other args"
    (let [[optlist args] (getopt/getopt ["-a" "-b"] "abc")]
      (is (= optlist [["-a" ""] ["-b" ""]])
      (is (= args [])))))
  (testing "shortopts with no args, other args"
    (let [[optlist args] (getopt/getopt ["-a" "-b" "foo"] "abc")]
      (is (= optlist [["-a" ""] ["-b" ""]])
      (is (= args ["foo"])))))
  (testing "shortopts with args, no other args"
    (let [args ["-a" "foo" "-b"]
          [optlist args] (getopt/getopt args "a:b")]
      (is (= optlist [["-a" "foo"] ["-b" ""]]))
      (is (= args []))))
  (testing "shortopts merged with args, no other args"
    (let [args ["-afoo" "-b"]
          [optlist args] (getopt/getopt args "a:b")]
      (is (= optlist [["-a" "foo"] ["-b" ""]]))
      (is (= args []))))
  (testing "shortopts interspered with args"
    (let [args ["-a" "foo" "-b"]
          [optlist args] (getopt/getopt args "ab")]
      (is (= optlist [["-a" ""] ["-b" ""]]))
      (is (= args ["foo"]))))
  (testing "unrecognized option"
    (is (thrown-with-msg?
         Exception #"option -y not recognized"
         (getopt/getopt ["-y"] "x"))))
  (testing "option requires argument"
    (is (thrown-with-msg?
         Exception #"option -y requires argument"
         (getopt/getopt ["-y"] "y:")))))


(deftest long-opts-tests
  (testing "easy longopts"
    (let [args ["--condition=foo", "--testing", "--output-file", "abc.def",
                "a1", "a2"]]
      (let [[optlist args] (getopt/getopt
                            args
                            ""
                            ["condition=" "output-file=" "testing"])]
        (is (= optlist [["--condition" "foo"]
                        ["--testing" ""]
                        ["--output-file" "abc.def"]]))
        (is (= args ["a1" "a2"])))))
  (testing "interspersed longopts"
    (let [args ["a1" "--condition=foo", "a2" "--testing", "--output-file",
                "abc.def"]]
      (let [[optlist args] (getopt/getopt
                            args
                            ""
                            ["condition=" "output-file=" "testing"])]
        (is (= optlist [["--condition" "foo"]
                        ["--testing" ""]
                        ["--output-file" "abc.def"]]))
        (is (= args ["a1" "a2"])))))
  (testing "unique prefix"
    (let [[optlist args] (getopt/getopt
                          ["--state-wis"]
                          ""
                          ["state-wisconsin" "state-illinois"])]
      (is (= optlist [["--state-wisconsin" ""]]))
      (is (= args []))))
  (testing "unrecognized option"
    (is (thrown-with-msg?
         Exception #"option --unknown not recognized"
         (getopt/getopt ["--unknown"] "" []))))
  (testing "not a unique prefix"
    (is (thrown-with-msg?
         Exception #"option --common is not a unique prefix"
         (getopt/getopt ["--common"] "" ["common1" "common2"]))))
  (testing "requires an argument"
    (is (thrown-with-msg?
         Exception #"option --name requires an argument"
         (getopt/getopt ["--name"] "" ["name="])))))


(deftest mixed-opts-tests
  (testing "combo test"
    (let [args ["--condition=foo" "--testing" "--output-file" "abc.def"
                "a1" "a2" "-xoptvalue" "-y" "optvalue" "-z" "lastarg"]]
      (let [[optlist args] (getopt/getopt
                            args
                            "x:y:z"
                            ["condition=" "output-file=" "testing"])]
        (is (= optlist [["--condition" "foo"]
                        ["--testing" ""]
                        ["--output-file" "abc.def"]
                        ["-x" "optvalue"]
                        ["-y" "optvalue"]
                        ["-z" ""]]))
        (is (= args ["a1" "a2" "lastarg"]))))))
