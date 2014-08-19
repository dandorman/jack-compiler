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

(declare analyze)

(defn process-construct
  [grammar tokens construct ast]
  (let [token (first tokens)]
    (cond
      (instance? String construct)
      (if (= (last token) construct)
        [(rest tokens) (conj ast [:keyword construct])]
        (throw (Exception. (str "Invalid terminal " construct ", expected " (last token)))))

      (instance? clojure.lang.Keyword construct)
      (if (= (first token) construct)
        (let [[new-tokens sub-ast] (analyze grammar tokens (construct grammar) [construct])]
          [new-tokens (conj ast sub-ast)])
        (throw (Exception. (str "Invalid non-terminal " construct ", expected " (last token)))))

      (instance? clojure.lang.PersistentHashSet construct)
      (if-let [result (first
                        (remove nil?
                                (map #(try
                                        (process-construct grammar tokens % ast)
                                        (catch Exception e
                                          nil))
                                     construct)))]
        result
        (throw (Exception. (str "Unrecognized token: " token))))

      :else
      (throw (Exception. (str "Unexpected construct: " construct))))))

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
     [tokens ast])))
