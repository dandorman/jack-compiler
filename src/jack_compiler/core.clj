(ns jack-compiler.core
  (:require [clojure.java.io :as io]))

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

(def jack-symbols ["{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "-"])

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

    (re-matches #"^[A-Za-z_][0-9A-Za-z_]*$" value)
    :identifier

    :else
    :unknown))

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

    (some #(= chr %) jack-symbols)
    [:start [] [[(keyword chr) chr]]]

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

    (some #(= chr %) jack-symbols)
    (let [value (apply str current-token)
          token-type (classify-token value)]
      [:whitespace [] [[token-type value] [(keyword chr) chr]]])

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
    [:start current-token [[(keyword "/") "/"]]]))

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

(defn token-seq
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

(defn -main [& args]
  (doseq [filename *command-line-args*]
    (with-open [rdr (io/reader filename)]
      (let [chrs (char-seq rdr)
            tokens (token-seq chrs)]
        (println tokens)))))
