(ns jack-compiler.core
  (:require [clojure.java.io :as io]
            [jack-compiler.tokenizer :as tokenizer]
            [jack-compiler.lexical-analyzer :refer [analyze]]
            [jack-compiler.xml-formatter :refer [fmt]]))

(defn -main [& args]
  (doseq [filename *command-line-args*]
    (with-open [rdr (io/reader filename)]
      (let [tokens (tokenizer/token-seq rdr)
            ast    (analyze tokens)]
        (print (fmt ast))))))
