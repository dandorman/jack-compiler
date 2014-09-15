(ns jack-compiler.compiler)

(defn attr [attrs name]
  (first (filter #(= name (first %)) attrs)))

(defn extract-class-name [ast]
  (last (last (attr (rest ast) :class-name))))

(defn attr-type [attrs]
  (last (last (attr attrs :type))))

(defn attr-var-name [attrs]
  (last (last (attr attrs :var-name))))

(defn attr-scope [attrs]
  (last (attr attrs :keyword)))

(defn extract-class-var-dec [dict ast]
  (let [attrs (rest ast)
        type  (attr-type attrs)
        name  (attr-var-name attrs)
        scope (attr-scope attrs)
        index (count (filter #(= scope (:kind %)) (vals dict)))]
    (assoc dict name {:kind scope :type type :index index})))

(defn extract-class-var-decs [ast]
  (reduce extract-class-var-dec {} (filter #(= :class-var-dec (first %)) ast)))

(defn extract-method-name [ast]
  (last (last (attr (rest ast) :subroutine-name))))

(defn extract-parameter-list [ast]
  (let [tokens (rest (attr (rest ast) :parameter-list))]
    (->> tokens
         (partition-by #(not= [:symbol ","] %))
         (take-nth 2))))

(defn extract-parameter [dict ast]
  (let [attrs ast
        type  (attr-type attrs)
        name  (attr-var-name attrs)
        scope "argument"
        index (count (filter #(= scope (:kind %)) (vals dict)))]
    (assoc dict name {:kind scope :type type :index index})))

(defn extract-parameters [ast]
  (reduce extract-parameter {} (extract-parameter-list ast)))

(defn extract-method-body [ast]
  (->> (attr (rest ast) :subroutine-body)
       rest
       rest
       butlast))

(defn extract-local [dict ast]
  (let [attrs (rest ast)
        type  (attr-type attrs)
        name  (attr-var-name attrs)
        scope "var"
        index (count (filter #(= scope (:kind %)) (vals dict)))]
    (assoc dict name {:kind scope :type type :index index})))

(defn extract-locals [ast]
  (let [var-decs (take-while #(= :var-dec (first %)) ast)]
    (reduce extract-local {} var-decs)))

(defn extract-statements [ast]
  (map #(->> % rest first) (rest (attr ast :statements))))

(def scopes {"var"   "local"
             "field" "this"})

(defn var-ref [var-info]
  (let [scope (get scopes (:kind var-info) (:kind var-info))]
    (str scope " " (:index var-info))))

(defn extract-terms
  [ast]
  (->> ast
       (filter #(= :term (first %)))
       (map (comp first rest))))

(defn extract-ops
  [ast]
  (->> ast
       (filter #(= :op (first %)))
       (map (comp last first rest))))

(defmulti compile-term (fn [[term _] _] term))

(defmethod compile-term :var-name
  [[_ [_ var-name]] lookup-table]
  (let [var-info (get lookup-table var-name)]
    [(str "push " (var-ref var-info))]))

(defmethod compile-term :integer
  [[_ value] _]
  [(str "push constant " value)])

(defmethod compile-term :default
  [& args]
  (println "default term"))

(defmulti compile-op identity)

(defmethod compile-op "+"
  [_]
  ["add"])

(defn compile-expression
  [ast lookup-table]
  (let [[first-term & other-terms] (extract-terms ast)
        ops                      (extract-ops ast)]
    (flatten (concat [(compile-term first-term lookup-table)]
                     (for [term other-terms
                           op   ops]
                       [(compile-term term lookup-table)
                        (compile-op op)])))))

(defmulti compile-statement (fn [ast _] (first ast)))

(defmethod compile-statement :let-statement
  [[_ & ast] lookup-table]
  (comment println ast)
  (let [var-name   (attr-var-name ast)
        var-info   (get lookup-table var-name)
        expression (compile-expression (rest (attr ast :expression)) lookup-table)]
    (flatten (concat expression
                     [(str "pop " (var-ref var-info))]))))

(defmethod compile-statement :return-statement
  [ast lookup-table]
  ["return statement"])

(defmethod compile-statement :default
  [& args]
  (println "default!" args))

(defn compile-method-body [class-name method-name lookup-table ast]
  (let [body           (extract-method-body ast)
        locals         (extract-locals body)
        lookup-table   (merge lookup-table locals)
        statement-asts (extract-statements body)
        statements     (map #(compile-statement % lookup-table) statement-asts)]
    (flatten (concat [(str "function " class-name "." method-name " " (count locals))]
                     statements))))

(defn compile-method [class-name class-vars method]
  (let [method-name (extract-method-name method)
        parameters  (extract-parameters method)
        method-body (compile-method-body class-name method-name (merge class-vars parameters) method)]
    method-body))

(defmulti compile-jack first)

(defmethod compile-jack :class
  [ast]
  (let [class-name     (extract-class-name (rest ast))
        class-vars     (extract-class-var-decs (rest ast))
        class-methods  (filter #(= :subroutine-dec (first %)) (rest ast))
        compile-method (partial compile-method class-name class-vars)]
    (reduce #(concat %1 (compile-method %2)) [] class-methods)))

(defmethod compile-jack :keyword
  [ast]
  (comment println "keyword!" (rest ast)))

(defmethod compile-jack :symbol
  [ast]
  (comment println "symbol!" (rest ast)))

(defmethod compile-jack nil [_])

(defmethod compile-jack :default
  [ast]
  (compile-jack (rest ast)))
