(ns stone.core)

(use 'stone.lex)

(def txt "while i < 10 {
  sum = sum + i // comment
          
  i = i + 1
}
sum")


(lex (java.io.StringReader. txt))
