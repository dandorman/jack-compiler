(ns jack-compiler.lexical-analyzer)

(declare process-constructs process-construct)

(defn any
  "Builds a function that can match zero or more repetitions of its arguments."
  [& constructs]
  (fn [grammar tokens ast]
    (loop [tokens tokens
           ast    ast]
      (let [[new-tokens new-ast] (try
                                   (process-constructs grammar tokens constructs ast)
                                   (catch Exception e
                                     [:error]))]
        (if (= :error new-tokens)
          [tokens ast]
          (recur new-tokens new-ast))))))

(defn maybe
  "Builds a function that can match zero or one repetitions of its arguments."
  [& constructs]
  (fn [grammar tokens ast]
    (let [[new-tokens new-ast] (try
                                 (process-constructs grammar tokens constructs ast)
                                 (catch Exception e
                                   [:error]))]
      (if (= :error new-tokens)
        [tokens ast]
        [new-tokens new-ast]))))

(defn literal
  "Creates a function for handling an expected literal"
  [expected-token-type]
  (fn [grammar tokens ast]
    (let [token                    (first tokens)
          [token-type token-value] token]
      (if (= token-type expected-token-type)
        [(rest tokens) (conj ast token-value)]
        (throw (Exception. (str "Unexpected literal (" token "); expected " expected-token-type)))))))

(def identifier (partial literal :identifier))
(def integer    (partial literal :integer))
(def string     (partial literal :string))

(defn term
  "Handles the `term` non-terminal. A cruddy kludge :'("
  [grammar tokens ast]
  (let [[token-type token-value] (first tokens)]
    (cond
      (= :identifier token-type)
      (let [[next-type next-value] (second tokens)]
        (if (= :symbol next-type)
          (cond
            (= "[" next-value)
            (process-constructs grammar tokens [:var-name "[" :expression "]"] ast)

            (or (= "(" next-value)
                (= "." next-value))
            (let [[new-tokens sub-ast] (process-constructs grammar tokens (:subroutine-call grammar) [:subroutine-call])]
              [new-tokens (conj ast sub-ast)])

            :else
            (process-constructs grammar tokens [:var-name] ast))
          (process-constructs grammar tokens [:var-name] ast)))

      :else
      (process-construct grammar tokens #{:integer :string :keyword-constant ["(" :expression ")"] [:unary-op :term]} ast))))

(def jack-grammar {:class            ["class" :class-name "{" (any :class-var-dec) (any :subroutine-dec) "}"]
                   :class-var-dec    [#{"static" "field"} :type :var-name (any "," :var-name) ";"]
                   :type             [#{"int" "char" "boolean" :class-name}]
                   :subroutine-dec   [#{"constructor" "function" "method"} #{"void" :type} :subroutine-name "(" :parameter-list ")" :subroutine-body]
                   :parameter-list   [(maybe :type :var-name (any "," :type :var-name))]
                   :subroutine-body  ["{" (any :var-dec) :statements "}"]
                   :var-dec          ["var" :type :var-name (any "," :var-name) ";"]
                   :class-name       [:identifier]
                   :subroutine-name  [:identifier]
                   :var-name         [:identifier]

                   :statements       [(any :statement)]
                   :statement        [#{:let-statement :if-statement :while-statement :do-statement :return-statement}]
                   :let-statement    ["let" :var-name (maybe "[" :expression "]") "=" :expression ";"]
                   :if-statement     ["if" "(" :expression ")" "{" :statements "}" (maybe "else" "{" :statements "}")]
                   :while-statement  ["while" "(" :expression ")" "{" :statements "}"]
                   :do-statement     ["do" :subroutine-call ";"]
                   :return-statement ["return" (maybe :expression) ";"]

                   :expression       [:term (any :op :term)]
                   :term             [term]
                   :subroutine-call  [#{[:subroutine-name "(" :expression-list ")"] [#{:class-name :var-name} "." :subroutine-name "(" :expression-list ")"]}]
                   :expression-list  [(maybe :expression (any "," :expression))]
                   :op               [#{"+" "-" "*" "/" "&" "|" "<" ">" "="}]
                   :unary-op         [#{"-" "~"}]
                   :keyword-constant [#{"true" "false" "null" "this"}]
                   :identifier       [(identifier)]
                   :integer          [(integer)]
                   :string           [(string)]})

(defn process-construct
  [grammar tokens construct ast]
  (let [token (first tokens)]
    (cond
      (string? construct)
      (if (= (last token) construct)
        [(rest tokens) (conj ast [(first token) construct])]
        (throw (Exception. (str "Invalid terminal " construct ", expected " (last token)))))

      (keyword? construct)
      (let [[new-tokens sub-ast] (process-constructs grammar tokens (construct grammar) [construct])]
        [new-tokens (conj ast sub-ast)])

      (set? construct)
      (if-let [result (first
                        (remove nil?
                                (map #(try
                                        (if (vector? %)
                                          (process-constructs grammar tokens % ast)
                                          (process-construct grammar tokens % ast))
                                        (catch Exception e
                                          nil))
                                     construct)))]
        result
        (throw (Exception. (str "Unrecognized token: " token))))

      (fn? construct)
      (construct grammar tokens ast)

      :else
      (throw (Exception. (str "Unexpected construct: " construct))))))

(defn process-constructs
  [grammar tokens constructs ast]
  (loop [tokens     tokens
         constructs constructs
         ast        ast]
    (if-let [construct (first constructs)]
      (let [[new-tokens new-ast] (process-construct grammar tokens construct ast)]
        (recur new-tokens
               (rest constructs)
               new-ast))
      [tokens ast])))

(defn analyze
  [tokens]
  (last (process-constructs jack-grammar tokens (:class jack-grammar) [:class])))
