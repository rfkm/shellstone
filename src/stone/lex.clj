(ns stone.lex)

(def re #"\s*((//.*)|([0-9]+)|(\"(\\\"|\\\\|\\n|[^\"])*\")|[A-Z_a-z][A-Z_a-z0-9]*|==|<=|>=|&&|\|\||\p{Punct})?")

(def not-nil? (complement nil?))

(defn token-type [match]
  (let [[whole trimmed comment? number? string?] match]
    (when (and trimmed (nil? comment?))
      (cond
       (not-nil? number?) :number
       (not-nil? string?) :string
       :else :id))))

(defn tokenize [line]
  (filter not-nil? (for [m (re-seq re line)]
                     (let [type (token-type m)] 
                       (when type
                         [(m 1) type])))))

(defn lex [reader]
  (for [line (line-seq (clojure.java.io/reader reader))
        t (tokenize line)]
    t))



