(ns jack-compiler.xml-formatter-test
  (:require [clojure.test :refer :all]
            [jack-compiler.xml-formatter :refer :all]))

(deftest fmt-test

  (testing "single element with no children"
    (is (= "<class></class>\n" (fmt [:class]))))

  (testing "single element with a single string child"
    (is (= "<class> foo </class>\n" (fmt [:class "foo"]))))

  (testing "single element with a single next child"
    (is (= "<class>\n  <keyword> class </keyword>\n</class>\n" (fmt [:class [:keyword "class"]]))))

  (testing "single element with two children"
    (is (= "<class>\n  <keyword> class </keyword>\n  <identifier> Foo </identifier>\n</class>\n" (fmt [:class [:keyword "class"] [:identifier "Foo"]]))))

  (testing "single element with nested children"
    (is (= "<class>\n  <keyword> class </keyword>\n  <class-name>\n    <identifier> Foo </identifier>\n  </class-name>\n</class>\n" (fmt [:class [:keyword "class"] [:class-name [:identifier "Foo"]]]))))
  )
