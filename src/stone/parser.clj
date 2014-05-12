(ns stone.parser
  (:require [blancas.kern.lexer :as lex])
  (:use [blancas.kern.core]
        [blancas.kern.expr]))

(def stone-style
  (assoc lex/c-style
    :trim-newline        false
    ;; :reserved-names
    ))
(def- rec (lex/make-parsers stone-style))
(def trim       (:trim       rec))
(def lexeme     (:lexeme     rec))
(def sym        (:sym        rec))
(def new-line   (:new-line   rec))
(def one-of1     (:one-of     rec))
(def none-of    (:none-of    rec))
(def token      (:token      rec))
(def word       (:word       rec))
(def identifier (:identifier rec))
(def field      (:field      rec))
(def char-lit   (:char-lit   rec))
(def string-lit (:string-lit rec))
(def dec-lit    (:dec-lit    rec))
(def oct-lit    (:oct-lit    rec))
(def hex-lit    (:hex-lit    rec))
(def float-lit  (:float-lit  rec))
(def bool-lit   (:bool-lit   rec))
(def nil-lit    (:nil-lit    rec))
(def parens     (:parens     rec))
(def braces     (:braces     rec))
(def angles     (:angles     rec))
(def brackets   (:brackets   rec))
(def semi       (:semi       rec))
(def comma      (:comma      rec))
(def colon      (:colon      rec))
(def dot        (:dot        rec))
(def semi-sep   (:semi-sep   rec))
(def semi-sep1  (:semi-sep1  rec))
(def comma-sep  (:comma-sep  rec))
(def comma-sep1 (:comma-sep1 rec))


(def ws 
  "whitespace"
  (<|> (sym* \space) (sym* \tab)))

(def number
  "NUMBER"
  (bind [n (<|> dec-lit float-lit)]
        (return n)))

(def string 
  "STRING"
  string-lit)

(def id 
  "IDENTIFIER"
  (bind [i identifier]
        (return (keyword i))))

(declare expr)
(def primary 
  "primary : '(' expr ')' | NUMBER | IDENTIFIER | STRING"
  (<|> (parens (fwd expr)) number id string))

;; factor
(def factor
  "factor : '-' primary | primary"
  (bind [neg (optional (sym* \-))
         p primary]
        (if neg
          (return {:token :negative :operand p})
          (return p))))

;; operators, expr
(def op-bind
  (bind [op (one-of1 "=")]
        (return (keyword (str op)))))

(def op-rel
  (bind [op (token "==" "!=" ">=" "<=" ">" "<")]
        (return (keyword (str op)))))

(def op-add
  (bind [op (one-of1 "+-")] 
        (return (keyword (str op)))))

(def op-mul
  (bind [op (one-of1 "*/%")] 
        (return (keyword (str op)))))

(def term (chainl1* :binary-expr factor op-mul))

(def sum (chainl1* :binary-expr term op-add))

(def rel (chainl1* :binary-expr sum op-rel))

(def ins (chainr1* :binary-expr rel op-bind))

(def expr ins)

;; block
(declare statement)

(def block
  "block : '{' [ statement ] {(';' | EOL) [ statement ]} '}'"
  (bind [sts (braces (sep-by (<|> semi new-line*)
                             (skip-ws (optional (fwd statement)))))]
        (return {:token :block :children (remove nil? sts)})))

;; simple
(def simple 
  "simple : expr"
  expr)

;;statement
(def if-statement
  "if-statement : 'if' expr block [ 'else' block ]"
  (bind [_ (token* "if")
         _ (many1 ws)
         cond expr
         body block
         else-blk (optional (>> (token* "else")
                                (skip-ws block)))]
        (if else-blk
          (return {:token :if-statement 
                   :condition cond
                   :body body
                   :else-body else-blk
                   })
          (return {:token :if-statement 
                   :condition cond
                   :body body
                   }))))

(def while-statement
  "while-statement : 'while' expr block"
  (bind [_ (token* "while")
         _ (many1 ws)
         cond expr
         body block]
        (return {:token :while-statement 
                 :condition cond
                 :body body})))

(def statement
  "statement : if-statement | while-statement | simple"
  (bind [l (<|> if-statement
                while-statement
                simple)]
        (return l)))

;; program
(def program
  (bind [c (end-by (many (<|> semi new-line*)) (skip-ws statement))]
        (return {:token :root :children c})))


(defn ast [pg]
  (:value (parse-data program pg)))
