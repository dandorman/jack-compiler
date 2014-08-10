(ns jack-compiler.core)

(defn char-seq
  "See <http://stackoverflow.com/questions/11669404/processing-a-file-character-by-character-in-clojure>"
  [^java.io.BufferedReader rdr]
  (let [chr (.read rdr)]
    (if (>= chr 0)
      (let [string-chr (-> chr char str)]
        (cons string-chr (lazy-seq (char-seq rdr)))))))

(def jack-keywords ["class"
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
                    "return"])

(def jack-symbols ["{"
                   "}"
                   "("
                   ")"
                   "["
                   "]"
                   "."
                   ","
                   ";"
                   "+"
                   "-"
                   "*"
                   "/"
                   "&"
                   "|"
                   "<"
                   ">"
                   "="
                   "-"])

(defn classify-token
  [value]
  (cond
    (some #(= value %) jack-keywords)
    (keyword (some (set [value]) jack-keywords))

    (some #(= value %) jack-symbols)
    (keyword (some (set [value]) jack-symbols))

    (and (re-matches #"^\d{1,5}$" value)
         (< (Integer/parseInt value) 32767))
    :integer

    (re-matches #"^\"[^\"\n]*\"$" value)
    :string

    (re-matches #"^[A-Za-z_][0-9A-Za-z_]*$" value)
    :identifier

    :else
    :unknown))

(defmulti process-chr (fn [state & args] state))

(defmethod process-chr :start
  [state chr current-token tokens]
  (cond
    (re-matches #"\s" chr)
    [:whitespace current-token tokens]

    (= "/" chr)
    [:maybe-comment-start current-token tokens]

    (some #(= chr %) jack-symbols)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:whitespace [] (conj tokens [token-type value] [(keyword chr) chr])])

    :else
    [:token (conj current-token chr) tokens]))

(defmethod process-chr :token
  [state chr current-token tokens]
  (cond
    (re-matches #"\s" chr)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:whitespace [] (conj tokens [token-type value])])

    (= "/" chr)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:maybe-comment-start [] (conj tokens [token-type value])])

    (some #(= chr %) jack-symbols)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:whitespace [] (conj (conj tokens [token-type value]) [(keyword chr) chr])])

    :else
    [:token (conj current-token chr) tokens]))

(defmethod process-chr :whitespace
  [state chr current-token tokens]
  (cond
    (re-matches #"\s" chr)
    [:whitespace current-token tokens]

    (= "/" chr)
    [:maybe-comment-start current-token tokens]

    :else
    [:token (conj current-token chr) tokens]))

(defmethod process-chr :maybe-comment-start
  [state chr current-token tokens]
  (cond
    (= "/" chr)
    [:newline-comment current-token tokens]

    (= "*" chr)
    [:comment current-token tokens]

    :else
    [:start current-token (conj tokens [(keyword "/") "/"])]))

(defmethod process-chr :newline-comment
  [state chr current-token tokens]
  (if (= "\n" chr)
    [:start current-token tokens]
    [:newline-comment current-token tokens]))

(defmethod process-chr :comment
  [state chr current-token tokens]
  (if (= "*" chr)
    [:maybe-comment-end current-token tokens]
    [:comment current-token tokens]))

(defmethod process-chr :maybe-comment-end
  [state chr current-token tokens]
  (if (= "/" chr)
    [:start current-token tokens]
    [:comment current-token tokens]))

(defn get-tokens
  [chrs]
  (loop [chr (first chrs)
         others (rest chrs)
         state :start
         current-token []
         tokens []]
    (if-not chr
      tokens
      (let [[new-state new-current-token new-tokens] (process-chr state chr current-token tokens)]
        ; (println new-state new-current-token new-tokens)
        (recur (first others) (rest others) new-state new-current-token new-tokens)))))

(defn -main []
  (let [chrs (char-seq (java.io.BufferedReader. *in*))
        tokens (get-tokens chrs)]
    (println tokens)))
