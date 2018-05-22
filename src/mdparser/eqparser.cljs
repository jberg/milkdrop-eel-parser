(ns mdparser.eqparser
  (:require [clojure.set]
            [instaparse.core :as insta]))

(def funmap
  {
   ;unary
   :int "Math.floor"
   :floor "Math.floor"
   :abs "Math.abs"
   :sin "Math.sin"
   :cos "Math.cos"
   :tan "Math.tan"
   :asin "Math.asin"
   :acos "Math.acos"
   :atan "Math.atan"
   :exp "Math.exp"
   :sqr "sqr"
   :sqrt "sqrt"
   :log "Math.log"
   :log10 "log10"
   :sign "sign"
   :rand "rand"
   :bnot "bnot"
   ;binary
   :atan2 "Math.atan2"
   :pow "pow"
   :min "Math.min"
   :max "Math.max"
   :sigmoid "sigmoid"
   :bor "bor"
   :band "band"
   :equal "equal"
   :above "above"
   :below "below"})

(def varmap
  {:frating :rating
   :fgammaadj :gammaadj
   :fdecay :decay
   :fvideoechozoom :echo_zoom
   :fvideoechoalpha :echo_alpha
   :nvideoechoorientation :echo_orient
   :nwavemode :wave_mode
   :badditivewaves :additivewave
   :bwavedots :wave_dots
   :bwavethick :wave_thick
   :bmodwavealphabyvolume :modwavealphabyvolume
   :bmaximizewavecolor :wave_brighten
   :btexwrap :wrap
   :bdarkencenter :darken_center
   :bredbluestereo :red_blue
   :bbrighten :brighten
   :bdarken :darken
   :bsolarize :solarize
   :binvert :invert
   :fwavealpha :wave_a
   :fwavescale :wave_scale
   :fwavesmoothing :wave_smoothing
   :fwaveparam :wave_mystery
   :fmodwavealphastart :modwavealphastart
   :fmodwavealphaend :modwavealphaend
   :fwarpanimspeed :warpanimspeed
   :fwarpscale :warpscale
   :fzoomexponent :zoomexp
   :nmotionvectorsx :mv_x
   :nmotionvectorsy :mv_y})

(def pool-vars
  {:per-frame [:rating :gammaadj :decay :fshader :echo_zoom :echo_alpha :echo_orient :additivewave
               :wave_mode :wave_dots :wave_thick :wave_brighten :wave_scale :wave_smoothing :wave_mystery
               :wave_a :wave_r :wave_g :wave_b :wave_x :wave_y
               :modwavealphabyvolume :modwavealphastart :modwavealphaend
               :wrap :darken_center :red_blue :brighten :darken :solarize :invert
               :warpanimspeed :warpscale :monitor
               :zoomexp :zoom :rot :cx :cy :dx :dy :warp :sx :sy
               :ob_size :ob_r :ob_g :ob_b :ob_a
               :ib_size :ib_r :ib_g :ib_b :ib_a
               :mv_x :mv_y :mv_dx :mv_dy :mv_l :mv_r :mv_g :mv_b :mv_a
               :b1n :b2n :b3n :b1x :b2x :b3x :b1ed]
   :per-pixel [:x :y :rad :ang]
   :per-shape-frame [:r :g :b :a :r2 :g2 :b2 :a2
                     :x :y :rad :ang
                     :border_r :border_g :border_b :border_a
                     :additive :thickoutline
                     :textured :tex_zoom :tex_ang
                     :sides :instance :num_inst]
   :per-wave-frame [:r :g :b :a
                    :samples :scaling :smoothing :sep
                    :additive :usedots :spectrum :thick]
   :per-wave-point [:x :y :r :g :b :a
                    :sample :value1 :value2]})

(def globalvarset
  #{:time :fps :frame
    :meshx :meshy :pixelsx :pixelsy :aspectx :aspecty
    :bass :mid :treb :bass_att :mid_att :treb_att})

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
   STATEMENT   = (ASSIGN <';'>) / ((loop | while | if) <';'>?) / (bitexpr <';'>)
   <INNER-STATEMENT> = ((ASSIGN | loop | while | if | exec3 | exec2 | bitexpr) <';'>) / ((ASSIGN | loop | while | if | exec3 | exec2 | bitexpr) !<';'>)
   ASSIGN      = lhs assign-op rhs
   exec3       = <'exec3'> lparen INNER-STATEMENT+ <comma> INNER-STATEMENT+ <comma> INNER-STATEMENT+ rparen
   exec2       = <'exec2'> lparen INNER-STATEMENT+ <comma> INNER-STATEMENT+ rparen
   loop        = <'loop'> lparen bitexpr comma INNER-STATEMENT+ rparen
   while       = <'while'> lparen (exec3 | exec2) rparen
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
      (insta/transform {:SYMBOL (fn [& xs] (into [:SYMBOL] (conj (vec (drop-last xs)) (.toLowerCase (last xs)))))}
                       parsed))))

(defn parse
  [input]
  (let [pre-parse-input (reduce str (pre-parse input))]
    (when (not (clojure.string/blank? pre-parse-input))
      (parse-program pre-parse-input))))

(defn correct-basevar
  [x]
  (if-let [s (varmap (keyword x))] (name s) x))

(defn get-symbols
  [t]
  (map last
       (filter #(and (sequential? %) (= (first %) :SYMBOL))
          (tree-seq sequential? identity t))))

(defn analyze
  [p eq-type]
   (let [eq-type-vars (if (= eq-type :per-pixel)
                        (into (pool-vars :per-frame) (pool-vars :per-pixel))
                        (pool-vars eq-type))
         basevars (into globalvarset (into (keys funmap) eq-type-vars))
         user-vars (map correct-basevar (get-symbols p))
         user-vars (filter #(nil? (basevars (keyword %))) user-vars)]
     {:user-vars user-vars}))

(defn remove-leading-negs
  [x]
  (let [[ns r] (split-with #(= % [:NEGATIVE]) x)]
    [(pos? (mod (count ns) 2)) r]))

(defn remove-trailing-negs
  [x]
  (let [ns (filter #(= % [:NEGATIVE]) x)
        r (filter #(not (= % [:NEGATIVE])) x)]
    [(pos? (mod (count ns) 2)) r]))

(defn trim-leading-zero
  [n]
  (if (and (> (count n) 1)
           (.startsWith n "0")
           (not (.startsWith n "0.")))
    (trim-leading-zero (.substring n 1))
    n))

(defn emit
  ([l] (emit l ";"))
  ([l line-ending]
  (let [[f & r] l
        return-last-thunk (fn [statements]
                            (if (> (count statements) 1)
                              (str
                                "(function(){"
                                (clojure.string/join " " (map #(emit %) (drop-last statements)))
                                " return " (emit (last statements))
                                "})()")
                              (emit (first statements) "")))]
    (case f
      :PROGRAM (clojure.string/join " " (map emit r))
      :STATEMENT (emit (first r))
      :ASSIGN (let [[rhs-neg r] (if (> (count r) 3)
                                       (remove-trailing-negs r)
                                       [nil r])
                    [lhs op rhs] r]
                (str (emit lhs "") "" op "" (when rhs-neg "-") (emit rhs "") line-ending))
      (:exec2 :exec3) (return-last-thunk r)
      :while (let [idx-var (gensym "mdparser_idx")
                   count-var (gensym "mdparser_count")]
               (str
                 "(function(){"
                 "var " idx-var ";"
                 "var " count-var  "=0;"
                 "do{"
                 count-var "+=1;"
                 idx-var "=" (emit (first r))
                 "}while(" idx-var "!==0&&" count-var "<1048576);"
                 "}())" line-ending))
      :loop (let [[c comma & s] r
                  idx-var (gensym "mdparser_idx")]
              (str
                "for(var " idx-var "=0;" idx-var "<" (emit c "") ";" idx-var "++){"
                (clojure.string/join " " (map emit s))
                "}"))
      (:bitwise
       :add-sub
       :mult-div) (let [[lhs-neg r] (remove-leading-negs r)
                        [rhs-neg r] (remove-trailing-negs r)
                        [lhs op rhs] r]
                     (if (or (= op "/")
                             (= op "%")
                             (= op "|")
                             (= op "&"))
                       (str ({"/" "div"
                              "%" "mod"
                              "|" "bitor"
                              "&" "bitand"} op) "(" (when lhs-neg "-") (emit lhs "") "," (when rhs-neg "-") (emit rhs "") ")")
                       (str "(" (when lhs-neg "-") (emit lhs "") op (when rhs-neg "-") (emit rhs "") ")")))
      :NUMBER (let [[is-neg r] (remove-leading-negs r)]
                (str (when is-neg "-") (emit (last r) "")))
      :DECIMAL (if (== (count r) 3)
                 (let [[lhs _ rhs] r]
                   (str (trim-leading-zero lhs) "." rhs))
                 (let [[lhs rhs] r]
                   (if (= lhs ".")
                     (str "0." rhs)
                     (str (trim-leading-zero lhs) ".0"))))
      :INTEGER (trim-leading-zero (first r))
      :SYMBOL (let [[is-neg r] (remove-leading-negs r)
                    sname (last r)
                    sname (correct-basevar sname)]
                (if (> (count r) 1)
                  (str (when is-neg "-") "bnot(a['" sname "'])")
                  (str (when is-neg "-") "a['" sname "']")))
      :BUFFER (let [[is-neg r] (remove-leading-negs r)]
                (str (when is-neg "-") "a['" (first r) "'][" (emit (second r) "") "]"))
      :condop (last r)
      :cond (let [[lhs c rhs] r]
              (str
                "(("
                (emit lhs "")
                (emit c "")
                (emit rhs "")
                ")?1:0)"))
      :if (let [[is-neg r] (remove-leading-negs r)
                [c t f] (filterv #(not (= % '([:comma]))) (partition-by #(= % [:comma]) r))]
            (str
              (when is-neg "-")
              "((" (emit (first c) "") ")?"
              "(" (return-last-thunk t) ")"
              ":"
              "(" (return-last-thunk f) "))" line-ending))
      :funcall (let [[is-neg-top r] (remove-leading-negs r)
                     [fname & args] r
                     [is-neg fname] (remove-leading-negs (rest fname))
                     f (funmap (keyword (.toLowerCase (first fname))))
                     as (clojure.string/join ", " (map #(emit % "") args))]
                 (if (nil? f)
                   (throw (ex-info (str "No function matching: " (first fname)) {}))
                   (str (when (and (or is-neg-top is-neg) (not (and is-neg-top is-neg))) "-")
                        f "(" as ")")))))))
