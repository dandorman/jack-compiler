(ns jack-compiler.core
  (:require [clojure.java.io :as io]
            [jack-compiler.tokenizer :refer [token-seq]]
            [jack-compiler.lexical-analyzer :refer [analyze]]
            [jack-compiler.compiler :as compiler]))

(defn basename [filename]
  (re-find #"^(?:[^.]+)" filename))

(defn jack-file? [filename]
  (re-matches #".*\.jack$" filename))

(defn jack-files [file]
  (if (.isDirectory file)
    (let [dir       (.getName file)
          separator java.io.File/separator]
      (->> file
           file-seq
           (map #(.getName %))
           (filter jack-file?)
           (map #(str dir separator %))))
    (list (.getName file))))

(defn -main [& args]
  (if-let [file (io/file (first *command-line-args*))]
    (doseq [filename (jack-files file)]
      (with-open [rdr (io/reader filename)]
        (let [tokens (token-seq rdr)
              ast    (analyze tokens)
              output (str (basename filename) ".xml")]
          (doseq [line (compiler/compile-jack ast)]
            (println line))
          (comment (with-open [wrtr (io/writer output)]
            (.write wrtr (compiler/compile-jack ast)))))))))
