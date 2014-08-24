(ns jack-compiler.lexical-analyzer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.lexical-analyzer :refer :all]))

(deftest process-constructs-test

  (testing "non-terminal -> terminal"
    (let [grammar {:foo ["foo"]}
          tokens  [[:foo "foo"]]
          [_ ast] (process-constructs grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:foo "foo"]] ast))))

  (testing "non-terminal -> terminal terminal"
    (let [grammar {:foo ["foo" "bar"]}
          tokens  [[:foo "foo"] [:bar "bar"]]
          [_ ast] (process-constructs grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:foo "foo"] [:bar "bar"]] ast))))

  (testing "non-terminal -> non-terminal -> terminal"
    (let [grammar {:foo [:bar]
                   :bar ["bar"]}
          tokens  [[:bar "bar"]]
          [_ ast] (process-constructs grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:bar [:bar "bar"]]] ast))))

  (testing "any"
    (let [grammar {:foo ["bar" (any "baz") "qux"]}
          tokens  [[:bar "bar"] [:baz "baz"] [:baz "baz"] [:qux "qux"]]
          [_ ast] (process-constructs grammar tokens (:foo grammar) [:foo])]
      (is (= [:foo [:bar "bar"] [:baz "baz"] [:baz "baz"] [:qux "qux"]] ast))))

  (testing "kinda class"
    (let [grammar {:class         ["class" :class-name "{" (any :class-var-dec) "}"]
                   :class-name    ["Foo"]
                   :class-var-dec ["field" "int" #{"foo" "bar"} ";"]}
          tokens  [[:keyword "class"] [:identifier "Foo"] [:symbol "{"]
                   [:keyword "field"] [:keyword "int"] [:identifier "foo"] [:symbol ";"]
                   [:keyword "field"] [:keyword "int"] [:identifier "bar"] [:symbol ";"]
                   [:symbol "}"]]
          [_ ast] (process-constructs grammar tokens (:class grammar) [:class])]
      (is (= [:class [:keyword "class"] [:class-name [:identifier "Foo"]] [:symbol "{"]
              [:class-var-dec [:keyword "field"] [:keyword "int"] [:identifier "foo"] [:symbol ";"]]
              [:class-var-dec [:keyword "field"] [:keyword "int"] [:identifier "bar"] [:symbol ";"]]
              [:symbol "}"]] ast))))

  (testing "simple Jack class"
    (let [tokens  [[:keyword "class"] [:identifier "Foo"] [:symbol "{"]
                   [:keyword "field"] [:keyword "int"] [:identifier "foo"] [:symbol ";"]
                   [:keyword "static"] [:keyword "char"] [:identifier "bar"] [:symbol ";"]
                   [:keyword "method"] [:keyword "int"] [:identifier "baz"] [:symbol "("] [:symbol ")"] [:symbol "{"]
                   [:keyword "return"] [:integer 1] [:symbol ";"]
                   [:symbol "}"]
                   [:symbol "}"]]
          [_ ast] (process-constructs jack-grammar tokens (:class jack-grammar) [:class])]
      (is (= [:class [:keyword "class"] [:class-name [:identifier "Foo"]] [:symbol "{"]
              [:class-var-dec [:keyword "field"] [:type [:keyword "int"]] [:var-name [:identifier "foo"]] [:symbol ";"]]
              [:class-var-dec [:keyword "static"] [:type [:keyword "char"]] [:var-name [:identifier "bar"]] [:symbol ";"]]
              [:subroutine-dec [:keyword "method"] [:type [:keyword "int"]] [:subroutine-name [:identifier "baz"]] [:symbol "("] [:parameter-list] [:symbol ")"]
               [:subroutine-body [:symbol "{"]
                [:statements
                 [:statement
                  [:return-statement [:keyword "return"] [:expression [:term [:integer 1]]] [:symbol ";"]]]]
                [:symbol "}"]]]
              [:symbol "}"]] ast))))
  )

(deftest process-construct-test

  (testing "terminals"
    (let [grammar    {:foo ["foo"]}
          [_ ast] (process-construct grammar [[:foo "foo"]] "foo" [:foo])]
      (is (= [:foo [:foo "foo"]] ast))))

  (testing "non-terminals"
    (let [grammar    {:foo [:bar]
                      :bar ["bar"]}
          [_ ast] (process-construct grammar
                                     [[:bar "bar"]]
                                     :bar
                                     [:foo])]
      (is (= ast [:foo [:bar [:bar "bar"]]]))))

  (testing "set | of | terminals"
    (let [grammar {:foo [#{"bar" "baz" "qux"}]}
          process (partial process-construct grammar)
          ast-of  (fn [[_ ast]] ast)]
      (is (= [:foo [:bar "bar"]] (ast-of (process [[:bar "bar"]] #{"bar" "baz" "qux"} [:foo]))))
      (is (= [:foo [:baz "baz"]] (ast-of (process [[:baz "baz"]] #{"bar" "baz" "qux"} [:foo]))))
      (is (= [:foo [:qux "qux"]] (ast-of (process [[:qux "qux"]] #{"bar" "baz" "qux"} [:foo]))))))

  (testing "non-terminals in a set"
    (let [grammar {:foo [#{:bar}]
                   :bar ["bar"]}
          process (partial process-construct grammar)
          ast-of  (fn [[_ ast]] ast)]
      (is (= [:foo [:bar [:baz "bar"]]] (ast-of (process [[:baz "bar"]] #{:bar} [:foo]))))))

  (testing "a vector of contructs in a set"
    (let [grammar {:foo [#{["bar"]}]}
          process (partial process-construct grammar)
          ast-of  (fn [[_ ast]] ast)]
      (is (= [:foo [:bar "bar"]] (ast-of (process [[:bar "bar"]] #{["bar"]} [:foo]))))))

  (testing "a longer vector of contructs in a set"
    (let [grammar {:foo [#{["bar" "bar"]}]}
          process (partial process-construct grammar)
          tokens  [[:bar "bar"] [:bar "bar"]]
          ast-of  (fn [[_ ast]] ast)]
      (is (= [:foo [:bar "bar"] [:bar "bar"]] (ast-of (process tokens #{["bar" "bar"]} [:foo]))))))

  (testing "a non-matching vector of constructs in a set"
    (let [grammar   {:foo [#{["bar"] "baz"}]}
          process   (partial process-construct grammar)
          tokens    [[:baz "baz"]]
          construct (first (:foo grammar))
          ast-of    (fn [[_ ast]] ast)]
      (is (= [:foo [:baz "baz"]] (ast-of (process tokens construct [:foo]))))))

  (testing "a function"
    (let [grammar   {:foo [(constantly "foo")]}
          process   (partial process-construct grammar)
          tokens    [[:doesnt "matter"]]
          construct (first (:foo grammar))]
      (is (= "foo" (process tokens construct [:foo])))))
  )

(deftest any-test
  (testing "a single repetition"
    (let [grammar {:foo ["bar"]}
          tokens  [[:bar "bar"]]
          [new-tokens ast] ((any "bar") grammar tokens [:foo])]
      (is (= [:foo [:bar "bar"]] ast))
      (is (empty? new-tokens))))

  (testing "two repetitions"
    (let [grammar {:foo ["bar"]}
          tokens  [[:bar "bar"] [:bar "bar"]]
          [new-tokens ast] ((any "bar") grammar tokens [:foo])]
      (is (= [:foo [:bar "bar"] [:bar "bar"]] ast))
      (is (empty? new-tokens))))

  (testing "zero repetitions"
    (let [grammar {:foo ["bar"]}
          tokens  [[:baz "baz"]]
          [new-tokens ast] ((any "bar") grammar tokens [:foo])]
      (is (= [:foo] ast))
      (is (= tokens new-tokens))))
  )

(deftest maybe-test
  (testing "a single repetition"
    (let [grammar {:foo ["bar"]}
          tokens  [[:bar "bar"]]
          [new-tokens ast] ((maybe "bar") grammar tokens [:foo])]
      (is (= [:foo [:bar "bar"]] ast))
      (is (empty? new-tokens))))

  (testing "two repetitions"
    (let [grammar {:foo ["bar"]}
          tokens  [[:bar "bar"] [:bar "bar"]]
          [new-tokens ast] ((maybe "bar") grammar tokens [:foo])]
      (is (= [:foo [:bar "bar"]] ast))
      (is (= [[:bar "bar"]] new-tokens))))

  (testing "zero repetitions"
    (let [grammar {:foo ["bar"]}
          tokens  [[:baz "baz"]]
          [new-tokens ast] ((maybe "bar") grammar tokens [:foo])]
      (is (= [:foo] ast))
      (is (= tokens new-tokens))))
  )
