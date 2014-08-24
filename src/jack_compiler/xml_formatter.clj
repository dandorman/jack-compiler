(ns jack-compiler.xml-formatter)

(def tab "  ")

(defn fmt
  ([ast]
   (fmt ast 0))
  ([ast indent-level]
   (let [xml (let [element  (first ast)
                   children (rest ast)
                   indent   (clojure.string/join (repeat indent-level tab))]
               (if (and (= 1 (count children)) (string? (first children)))
                 (str indent "<" (name element) "> " (first children) " </" (name element) ">")
                 (if (empty? children)
                   (str indent "<" (name element) "></" (name element) ">")
                   (let [children-xml (map #(fmt % (inc indent-level)) children)]
                     (str indent "<" (name element) ">\n" (clojure.string/join children-xml) indent "</" (name element) ">")))))]
     (str xml "\n"))))
