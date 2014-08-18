(ns jack-compiler.lexical-analyzer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.lexical-analyzer :refer :all]))

(deftest analyze-test
  (testing "grammar with a single terminal"
    (let [grammar {:foo ["foo"]}
          tokens  [[:foo "foo"]]
          ast     (analyze grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:keyword "foo"]] ast)))))
  (testing "grammar with two terminals"
    (let [grammar {:foo ["foo"]
                   :bar ["bar"]}
          tokens  [[:bar "bar"] [:foo "foo"]]
          actual  (analyze grammar tokens (:foo grammar) [:root])]
      (is (= actual [:root [:foo [:keyword "foo"]] [:bar [:keyword "bar"]]]))))

(deftest process-construct-test

  (testing "terminals"
    (let [grammar    {:foo ["foo"]}
          [_ actual] (process-construct grammar [[:foo "foo"]] "foo" [:root])]
      (is (= actual [:root [:keyword "foo"]]))))

  ; (testing "non-terminals"
  ;   (let [grammar    {:foo [:bar]
  ;                     :bar ["bar"]}
  ;         [_ actual] (process-construct grammar [[:foo "foo"]] :foo [:root])]
  ;     (is (= actual [:root [:bar [:keyword "bar"]]]))))
  )
