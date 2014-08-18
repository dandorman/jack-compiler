(ns jack-compiler.lexical-analyzer)

(def jack-grammar {:class            ["class" :class-name "{" [:any :class-var-dec] [:any :subroutine-dec] "}"]
                   :class-var-dec    [#{"static" "field"} :type :var-name [:any "," :var-name] ";"]
                   :type             [#{"int" "char" "boolean" :class-name}]
                   :subroutine-dec   [#{"constructor" "function" "method"} #{"void" :type}]
                   :parameter-list   [[:maybe :type :var-name [:any "," :type :var-name]]]
                   :subroutine-body  ["{" [:any :var-dec] :statements "}"]
                   :var-dec          ["var" :type :var-name [:any "," :var-name] ";"]
                   :class-name       [:identifier]
                   :subroutine-name  [:identifier]
                   :var-name         [:identifier]

                   :statements       [[:any :statement]]
                   :statement        [#{:let-statement :if-statement :while-statement :do-statement :return-statement}]
                   :let-statement    ["let" :var-name [:maybe "[" :expression "]"] "=" :expression ";"]
                   :if-statement     ["if" "(" :expression ")" "{" :statements "}" [:maybe "else" "{" :statements "}"]]
                   :while-statement  ["while" "(" :expression ")" "{" :statements "}"]
                   :do-statement     ["do" :subroutine-call ";"]
                   :return-statement ["return" [:maybe :expression] ";"]

                   :expression       [:term [:any :op :term]]
                   :term             [#{:integer :string :keyword-constant [:var-name [:maybe "[" :expression "]"]] :subroutine-call ["(" :expression ")"] [:unary-op :term]}]
                   :subroutine-call  [#{[:subroutine-name "(" :expression-list ")"] [#{:class-name :var-name} "." :subroutine-name "(" :expression-list ")"]}]
                   :expression-list  [[:maybe :expression [:any "," :expression]]]
                   :op               [#{"+" "-" "*" "/" "&" "|" "<" ">" "="}]
                   :unary-op         [#{"-" "~"}]
                   :keyword-constant [#{"true" "false" "null" "this"}]})

(defn process-construct
  [grammar tokens construct ast]
  (let [token (first tokens)]
    (cond
      (instance? String construct)
      [(rest tokens) (conj ast [:keyword (last token)])]

      (instance? clojure.lang.Keyword construct)
      (throw (Exception. "Implement me!"))

      :else
      (throw (Exception. "Unexpected token: " token)))))

(defn analyze
  ([tokens]
   (analyze jack-grammar tokens (:class jack-grammar) [:class]))
  ([grammar tokens constructs ast]
   (if-let [construct (first constructs)]
     (let [[new-tokens new-ast] (process-construct grammar tokens construct ast)]
       (analyze grammar
                new-tokens
                (rest constructs)
                new-ast))
     ast)))
