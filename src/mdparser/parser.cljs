(ns mdparser.parser
  (:require [instaparse.core :as insta]
            [clojure.string]))

(def comment-regex #"//[\s\S]*")

(defn strip-comments-from-line
  [s]
  (clojure.string/trim
   (clojure.string/replace s comment-regex "")))

(defn strip-all-comments
  [s]
  (reduce
   str
   (filter #(not (clojure.string/blank? %))
           (map strip-comments-from-line (seq (clojure.string/split-lines s))))))

(defn pre-parse
  [s]
  (map #(str % ";")
       (filter #(not (clojure.string/blank? %))
               (seq (clojure.string/split (strip-all-comments s) ";")))))

(def parser
  (insta/parser
   "
   PROGRAM     = STATEMENT+
   STATEMENT   = (ASSIGN <';'>) / ((loop | while | if | memcpy) <';'>?) / (bitexpr <';'>)
   <INNER-STATEMENT> = ((ASSIGN | loop | while | if | exec3 | exec2 | bitexpr) <';'>) / ((ASSIGN | loop | while | if | exec3 | exec2 | bitexpr) !<';'>)
   ASSIGN      = lhs assign-op rhs
   exec3       = <'exec3'> lparen INNER-STATEMENT+ <comma> INNER-STATEMENT+ <comma> INNER-STATEMENT+ rparen
   exec2       = <'exec2'> lparen INNER-STATEMENT+ <comma> INNER-STATEMENT+ rparen
   loop        = <'loop'> lparen bitexpr comma INNER-STATEMENT+ rparen
   while       = <'while'> lparen (exec3 | exec2) rparen
   memcpy      = <'memcpy'> lparen INNER-STATEMENT <comma> INNER-STATEMENT <comma> INNER-STATEMENT rparen
   <assign-op> = '=' | '+=' | '-=' | '*=' | '/=' | '%='
   <lhs>       = BUFFER | (SYMBOL !lparen)
   <rhs>       = bitexpr
   <bitexpr>   = loop / cond / BUFFER / if / expr / bitwise
   <expr>      = term | add-sub
   <term>      = factor | mult-div
   <factor>    = (if / BUFFER / funcall / NUMBER / (SYMBOL !lparen)) | NEGATIVE* lparen bitexpr rparen
   bitwise     = bitexpr bitop expr
   add-sub     = expr addop term
   mult-div    = term multop factor
   <bitop>     = '&' | '|'
   <addop>     = '+' | '-'
   <multop>    = '/' | '*' | '%'
   if          = NEGATIVE* <('if' | 'If' | 'IF')> lparen bitexpr comma INNER-STATEMENT+ comma INNER-STATEMENT+ rparen
   funcall     = SYMBOL lparen bitexpr (<comma> bitexpr)* rparen
   cond        = lparen* bitexpr condop bitexpr rparen*
   condop      = '>' | '<' | '>=' | '<=' | '==' | '!=' | '&&' | '||'
   <lparen>    = <'('>
   <rparen>    = <')'>
   comma       = <','>
   NUMBER      = NEGATIVE* <'+'>? (DECIMAL / INTEGER)
   DECIMAL     = (DIGITS '.' DIGITS) / (!DIGITS '.' DIGITS) / (DIGITS '.' !DIGITS)
   INTEGER     = !DECIMAL DIGITS
   <DIGITS>    = #'\\d+'
   SYMBOL      = NEGATIVE* NOT? !RESTRICTED #'[A-Za-z][A-Za-z0-9_]*'
   BUFFER      = NEGATIVE* ('gmegabuf' | 'megabuf') lparen bitexpr rparen
   RESTRICTED  = ('loop' | 'while' | 'if' | 'If' | 'IF' | 'exec3' | 'exec2' | 'megabuf' | 'gmegabuf') (#'\\s+' | lparen)
   NEGATIVE    = <'-'>
   NOT         = <'!'>
   "
   :auto-whitespace :standard))

(defn parse-program
  [input]
  (let [parsed (insta/parse parser input :optimize :memory)]
    (if (insta/failure? parsed)
      (throw (ex-info (pr-str (insta/get-failure parsed)) (clj->js {:input input})))
      (insta/transform {:SYMBOL (fn [& xs] (into [:SYMBOL] (conj (vec (drop-last xs)) (clojure.string/lower-case (last xs)))))}
                       parsed))))

(defn parse
  [input]
  (let [pre-parse-input (reduce str (pre-parse input))]
    (when (not (clojure.string/blank? pre-parse-input))
      (parse-program pre-parse-input))))
