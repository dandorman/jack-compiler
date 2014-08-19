(ns jack-compiler.lexical-analyzer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.lexical-analyzer :refer :all]))

(deftest analyze-test

  (testing "non-terminal -> terminal"
    (let [grammar {:foo ["foo"]}
          tokens  [[:foo "foo"]]
          [_ ast] (analyze grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:keyword "foo"]] ast))))

  (testing "non-terminal -> terminal terminal"
    (let [grammar {:foo ["foo" "bar"]}
          tokens  [[:foo "foo"] [:bar "bar"]]
          [_ ast] (analyze grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:keyword "foo"] [:keyword "bar"]] ast))))

  (testing "non-terminal -> non-terminal -> terminal"
    (let [grammar {:foo [:bar]
                   :bar ["bar"]}
          tokens  [[:bar "bar"]]
          [_ ast] (analyze grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:bar [:keyword "bar"]]] ast))))
  )

(deftest process-construct-test

  (testing "terminals"
    (let [grammar    {:foo ["foo"]}
          [_ ast] (process-construct grammar [[:foo "foo"]] "foo" [:foo])]
      (is (= [:foo [:keyword "foo"]] ast))))

  (testing "non-terminals"
    (let [grammar    {:foo [:bar]
                      :bar ["bar"]}
          [_ ast] (process-construct grammar
                                     [[:bar "bar"]]
                                     :bar
                                     [:foo])]
      (is (= ast [:foo [:bar [:keyword "bar"]]]))))

  (testing "set | of | terminals"
    (let [grammar {:foo [#{"bar" "baz" "qux"}]}
          process (partial process-construct grammar)
          ast-of  (fn [[_ ast]] ast)]
      (is (= [:foo [:keyword "bar"]] (ast-of (process [[:bar "bar"]] #{"bar" "baz" "qux"} [:foo]))))
      (is (= [:foo [:keyword "baz"]] (ast-of (process [[:baz "baz"]] #{"bar" "baz" "qux"} [:foo]))))
      (is (= [:foo [:keyword "qux"]] (ast-of (process [[:qux "qux"]] #{"bar" "baz" "qux"} [:foo]))))))

  (testing "non-terminals in a set"
    (let [grammar {:foo [#{:bar}]
                   :bar ["bar"]}
          process (partial process-construct grammar)
          ast-of  (fn [[_ ast]] ast)]
      (is (= [:foo [:bar [:keyword "bar"]]] (ast-of (process [[:bar "bar"]] #{:bar} [:foo]))))))
  )
