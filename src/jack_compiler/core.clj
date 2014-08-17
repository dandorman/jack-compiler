(ns jack-compiler.core
  (:require [clojure.java.io :as io]
            [jack-compiler.tokenizer :as tokenizer]))

(defn char-seq
  "See <http://stackoverflow.com/questions/11669404/processing-a-file-character-by-character-in-clojure>"
  [^java.io.BufferedReader rdr]
  (let [chr (.read rdr)]
    (if (>= chr 0)
      (let [string-chr (-> chr char str)]
        (cons string-chr (lazy-seq (char-seq rdr)))))))

(defn -main [& args]
  (doseq [filename *command-line-args*]
    (with-open [rdr (io/reader filename)]
      (let [chrs (char-seq rdr)
            tokens (tokenizer/token-seq chrs)]
        (println tokens)))))
