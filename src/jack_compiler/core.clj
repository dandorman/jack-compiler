(ns jack-compiler.core
  (:require [clojure.java.io :as io]
            [jack-compiler.tokenizer :as tokenizer]
            [jack-compiler.lexical-analyzer :refer [analyze]]
            [jack-compiler.xml-formatter :refer [fmt]]))

(comment (defn -main [& args]
  (doseq [filename *command-line-args*]
    (with-open [rdr (io/reader filename)]
      (let [tokens (tokenizer/token-seq rdr)
            ast    (analyze tokens)]
        (print (fmt ast)))))))

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
        (let [tokens (tokenizer/token-seq rdr)
              ast    (analyze tokens)
              output (str (basename filename) ".xml")]
          (with-open [wrtr (io/writer output)]
            (.write wrtr (fmt ast))))))))
