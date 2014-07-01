(ns shellstone.parser
  (:require [blancas.kern.lexer :as lex])
  (:use [blancas.kern.core]
        [blancas.kern.expr :exclude [postfix]]))

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


(declare expr statement postfix args lambda dot)

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

(def primary-1
  "primary-1 : ( '(' expr ')' | NUMBER | IDENTIFIER | STRING ) { postfix }"
  (bind [n (<|> (parens (fwd expr)) number id string)
         ps (many postfix)]
        (if (> (count ps) 0)
          (return {:token :primary-expr :children (vec (cons n ps))})
          (return n))))

(def primary
  "primary : lambda | primary-1"
  (<|> (fwd lambda) primary-1))

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
(def block
  "block : '{' [ statement ] {(';' | EOL) [ statement ]} '}'"
  (bind [sts (braces (sep-by (<|> semi new-line*)
                             (skip-ws (optional (fwd statement)))))]
        (return {:token :block :children (remove nil? sts)})))

;; simple
(def simple 
  "simple : expr"
  (bind [e expr
         a (optional args)]
        (if (> (count (:children a)) 0)
          (return {:token :primary-expr :children [e a]})
          (return e))))


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

;; function related
(def param 
  "param : IDENTIFIER"
  id)

(def params 
  "params : param { ',' param }"
  (bind [ps (comma-sep param)]
        (return {:token :param-list :children ps})))
(def param-list 
  "param_list : '(' [ params ] ')'"
  (parens params))

(def defun 
  "defun: 'def' IDENTIFIER param_list block"
  (bind [_ (token* "def")
         _ (many1 (sym* \space))
         n id
         a param-list
         b block]
        (return {:token :def-statement :name n :params a :body b})))

(def args 
  "args : expr { ',' expr }"
  (bind [as (comma-sep expr)]
        (return {:token :arguments :children as})))

(def postfix 
  "postfix : '(' [ args ] ')'"
  (<|> (fwd dot) 
       (parens args)))

(def lambda
  "lambda : 'fun' param_list block"
  (bind [_ (token* "fun")
         ps (skip-ws param-list)
         b block]
        (return {:token :lambda :params ps :body b})))

;; class related
(def method
  (bind [d defun]
        (return (assoc d :token :defmethod))))

(def member 
  "member : method | simple"
  (<|> method simple))


(def class-body
  "class-body : '{' [ member ] {(';' | EOL) [ member ]} '}'"
  (bind [sts (braces (sep-by (<|> semi new-line*)
                             (skip-ws (optional member))))]
        (return {:token :class-body :children (remove nil? sts)})))

(def defclass
  "defclass: 'class' IDENTIFIER [ 'extends' IDENTIFIER ] class-body"
  (bind [_ (token* "class")
         _ (many1 (sym* \space))
         n id
         a (optional (>> (token* "extends")
                         (skip-ws id)))
         b class-body]
        (return {:token :defclass :name n :super-class a :body b})))

(def dot
  "dot : '.' IDENTIFIER"
  (bind [_ (token* ".")
         n id]
        (return {:token :dot :name n})))

(def dec-include
  "dec-include : 'include' IDENTIFIER"
  (bind [_ (token* "include")
         _ (many1 ws)
         tgt id]
        (return {:token :include :target tgt})))

(def dec-link
  "dec-link : 'link' IDENTIFIER"
  (bind [_ (token* "link")
         _ (many1 ws)
         tgt id]
        (return {:token :link :target tgt})))

(def revise-member
  "revise-member : defun | dec-include | dec-link | simple"
  (<|> method dec-include dec-link simple))

(def revise-body
  "revise-body : '{' [ revise-member ] {(';' | EOL) [ revise-member ]} '}'"
  (bind [sts (braces (sep-by (<|> semi new-line*)
                             (skip-ws (optional revise-member))))]
        (return {:token :revise-body :children (remove nil? sts)})))

(def revise
  "defclass: 'revise' IDENTIFIER : IDENTIFIER revise-body"
  (bind [_ (token* "revise")
         _ (many1 ws)
         ms id
         _ (skip-ws (sym* \:))
         n (skip-ws id)
         b revise-body]
        (return {:token :revise :methodshell ms :name n :body b})))

;; program
(def program  
  "[ revise | defclass | def | statement | dec-include ] (';' | EOL)"
  (bind [c (end-by (many (<|> semi new-line*)) (skip-ws (<|> revise defclass dec-include defun statement)))]
        (return {:token :root :children c})))


(defn ast [pg]
  (:value (parse-data program pg)))
