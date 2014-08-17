(ns jack-compiler.core
  (:require [clojure.java.io :as io]
            [jack-compiler.tokenizer :as tokenizer]))

(defn -main [& args]
  (doseq [filename *command-line-args*]
    (with-open [rdr (io/reader filename)]
      (let [tokens (tokenizer/token-seq rdr)]
        (println tokens)))))
