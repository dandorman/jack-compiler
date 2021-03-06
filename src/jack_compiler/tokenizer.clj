(ns jack-compiler.tokenizer)

(defn char-seq
  "See <http://stackoverflow.com/questions/11669404/processing-a-file-character-by-character-in-clojure>"
  [^java.io.BufferedReader rdr]
  (let [chr (.read rdr)]
    (if (>= chr 0)
      (let [string-chr (-> chr char str)]
        (cons string-chr (lazy-seq (char-seq rdr)))))))

(def jack-keywords #{"class"
                     "constructor"
                     "function"
                     "method"
                     "field"
                     "static"
                     "var"
                     "int"
                     "char"
                     "boolean"
                     "void"
                     "true"
                     "false"
                     "null"
                     "this"
                     "let"
                     "do"
                     "if"
                     "else"
                     "while"
                     "return"})

(def jack-symbols #{"{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"})

(defn classify-token
  [value]
  (cond
    (contains? jack-keywords value)
    :keyword

    (contains? jack-symbols value)
    :symbol

    (and (re-matches #"^\d{1,5}$" value)
         (< (Integer/parseInt value) 32767))
    :integer

    (re-matches #"^[A-Za-z_][0-9A-Za-z_]*$" value)
    :identifier

    :else
    (throw (Exception. (str "Unrecognized token: " value)))))

(defmulti process-chr (fn [state & args] state))

(defmethod process-chr :start
  [state chr current-token]
  (cond
    (re-matches #"\s" chr)
    [:whitespace current-token nil]

    (= "/" chr)
    [:maybe-comment-start current-token nil]

    (= "\"" chr)
    [:string current-token nil]

    (contains? jack-symbols chr)
    [:start [] [[:symbol chr]]]

    :else
    [:token (conj current-token chr) nil]))

(defmethod process-chr :token
  [state chr current-token]
  (cond
    (re-matches #"\s" chr)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:whitespace [] [[token-type value]]])

    (= "/" chr)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:maybe-comment-start [] [[token-type value]]])

    (= "\"" chr)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:string [] [[token-type value]]])

    (contains? jack-symbols chr)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:start [] [[token-type value] [:symbol chr]]])

    :else
    [:token (conj current-token chr) nil]))

(defmethod process-chr :whitespace
  [state chr current-token]
  (cond
    (re-matches #"\s" chr)
    [:whitespace current-token nil]

    (= "/" chr)
    [:maybe-comment-start current-token nil]

    (= "\"" chr)
    [:string current-token nil]

    (contains? jack-symbols chr)
    [:start [] [[:symbol chr]]]

    :else
    [:token (conj current-token chr) nil]))

(defmethod process-chr :maybe-comment-start
  [state chr current-token]
  (cond
    (= "/" chr)
    [:newline-comment current-token nil]

    (= "*" chr)
    [:comment current-token nil]

    :else
    [:start current-token [[:symbol "/"]]]))

(defmethod process-chr :newline-comment
  [state chr current-token]
  (if (= "\n" chr)
    [:start current-token nil]
    [:newline-comment current-token nil]))

(defmethod process-chr :comment
  [state chr current-token]
  (if (= "*" chr)
    [:maybe-comment-end current-token nil]
    [:comment current-token nil]))

(defmethod process-chr :maybe-comment-end
  [state chr current-token]
  (if (= "/" chr)
    [:start current-token nil]
    [:comment current-token nil]))

(defmethod process-chr :string
  [state chr current-token]
  (if (= "\"" chr)
    [:start [] [[:string (apply str current-token)]]]
    [:string (conj current-token chr) nil]))

(defmulti token-seq class)

(defmethod token-seq :default
  [chrs]
  (loop [chr (first chrs)
         others (rest chrs)
         state :start
         current-token []]
    (if chr
      (let [[new-state new-current-token tokens] (process-chr state chr current-token)]
        (if tokens
          (cons (first tokens) (lazy-seq (concat (rest tokens) (lazy-seq (token-seq others)))))
          (recur (first others) (rest others) new-state new-current-token))))))

(defmethod token-seq java.io.BufferedReader
  [reader]
  (let [chrs (char-seq reader)]
    (token-seq chrs)))
