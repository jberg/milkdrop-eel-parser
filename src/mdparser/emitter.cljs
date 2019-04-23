(ns mdparser.emitter
  (:require [clojure.set]
            [clojure.string]
            [goog.object]))

; freembuf
; memset

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

(def funmapv1
  (into funmap {:rand "randint"}))

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

(defn correct-basevar
  [x]
  (if-let [s (varmap (keyword x))] (name s) x))

(defn get-symbols
  [t]
  (map last
       (filter #(and (sequential? %) (= (first %) :SYMBOL))
          (tree-seq sequential? identity t))))

(def reg-var-regex #"^reg\d{2}$")

(defn analyze
  [p eq-type]
   (let [eq-type-vars (if (= eq-type :per-pixel)
                        (into (pool-vars :per-frame) (pool-vars :per-pixel))
                        (pool-vars eq-type))
         basevars (into globalvarset (into (keys funmap) eq-type-vars))
         user-vars (map correct-basevar (get-symbols p))
         user-vars (filter #(nil? (basevars (keyword %))) user-vars)
         user-vars (if (or (= eq-type :per-frame) (= eq-type :per-pixel))
                     user-vars
                     (filter #(not (re-find reg-var-regex %)) user-vars))]
     {:user-vars (into #{} user-vars)}))

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
           (clojure.string/starts-with? n "0")
           (not (clojure.string/starts-with? n "0.")))
    (trim-leading-zero (subs n 1))
    n))

(def MDEPSILON 0.00001)

(defn emit
  ([version l] (emit version l ";"))
  ([version l line-ending]
  (let [[f & r] l
        return-last-thunk (fn [statements]
                            (if (> (count statements) 1)
                              (str
                                "(function(){"
                                (clojure.string/join " " (map #(emit version %) (drop-last statements)))
                                ; HANDLE THIS BEING A WHILE or IF
                                " return " (emit version (last statements))
                                "})()")
                              (emit version (first statements) "")))]
    (case f
      :PROGRAM (clojure.string/join " " (map #(emit version %) r))
      :STATEMENT (emit version (first r))
      :ASSIGN (let [[rhs-neg r] (if (> (count r) 3)
                                       (remove-trailing-negs r)
                                       [nil r])
                    [lhs op rhs] r]
                (str (emit version lhs "") "" op "" (when rhs-neg "-") (emit version rhs "") line-ending))
      (:exec2 :exec3) (return-last-thunk r)
      :while (let [idx-var (gensym "mdparser_idx")
                   count-var (gensym "mdparser_count")]
               (str
                 "(function(){"
                 "var " idx-var ";"
                 "var " count-var  "=0;"
                 "do{"
                 count-var "+=1;"
                 idx-var "=" (emit version (first r))
                 "}while(Math.abs(" idx-var ")>" MDEPSILON "0&&" count-var "<1048576);"
                 "}())" line-ending))
      :loop (let [[c comma & s] r
                  idx-var (gensym "mdparser_idx")]
              (str
                "for(var " idx-var "=0;" idx-var "<" (emit version c "") ";" idx-var "++){"
                (clojure.string/join " " (map #(emit version %) s))
                "}"))
      :memcpy (let [[& args] r
                    as (clojure.string/join ", " (map #(emit version % "") args))]
               (str "memcpy(a['megabuf'], " as ")" line-ending))
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
                              "&" "bitand"} op) "(" (when lhs-neg "-") (emit version lhs "") "," (when rhs-neg "-") (emit version rhs "") ")")
                       (str "(" (when lhs-neg "-") (emit version lhs "") op (when rhs-neg "-") (emit version rhs "") ")")))
      :NUMBER (let [[is-neg r] (remove-leading-negs r)]
                (str (when is-neg "-") (emit version (last r) "")))
      :DECIMAL (if (== (count r) 3)
                 (let [[lhs _ rhs] r]
                   (trim-leading-zero (str lhs "." rhs)))
                 (let [[lhs rhs] r]
                   (if (= lhs ".")
                     (str "0." rhs)
                     (trim-leading-zero (str lhs ".0")))))
      :INTEGER (trim-leading-zero (str (first r)))
      :SYMBOL (let [[is-neg r] (remove-leading-negs r)
                    sname (last r)
                    sname (correct-basevar sname)]
                (if (> (count r) 1)
                  (str (when is-neg "-") "bnot(a['" sname "'])")
                  (str (when is-neg "-") "a['" sname "']")))
      :BUFFER (let [[is-neg r] (remove-leading-negs r)]
                (str (when is-neg "-") "a['" (first r) "'][Math.floor(" (emit version (second r) "") ")]"))
      :condop (last r)
      :cond (let [[lhs c rhs] r]
              (if (or (= (last c) "==") (= (last c) "!="))
                (str
                  "(("
                  "Math.abs("
                  "(" (emit version lhs "") ")"
                  "-"
                  "(" (emit version rhs "") "))<" MDEPSILON
                  (if (= (last c) "==")
                    ")?1:0)"
                    ")?0:1)"))
                (str
                  "(("
                  (emit version lhs "")
                  (emit version c "")
                  (emit version rhs "")
                  ")?1:0)")))
      :if (let [[is-neg r] (remove-leading-negs r)
                [c t f] (filterv #(not (= % '([:comma]))) (partition-by #(= % [:comma]) r))]
            (str
              (when is-neg "-")
              "((Math.abs(" (emit version (first c) "") ")>" MDEPSILON ")?"
              "(" (return-last-thunk t) ")"
              ":"
              "(" (return-last-thunk f) "))" line-ending))
      :funcall (let [[is-neg-top r] (remove-leading-negs r)
                     [fname & args] r
                     [is-neg fname] (remove-leading-negs (rest fname))
                     f (if (== version 1)
                         (funmapv1 (keyword (clojure.string/lower-case (first fname))))
                         (funmap (keyword (clojure.string/lower-case (first fname)))))
                     as (clojure.string/join ", " (map #(emit version % "") args))]
                 (if (nil? f)
                   (throw (ex-info (str "No function matching: " (first fname)) {}))
                   (str (when (and (or is-neg-top is-neg) (not (and is-neg-top is-neg))) "-")
                        f "(" as ")")))))))

; from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin#Polyfill
(defn copyWithin
  [arr dst src end]
  (let [len (.-length arr)
        relTarget (.floor js/Math dst)
        to (if (< relTarget 0)
             (.max js/Math (+ len relTarget) 0)
             (.min js/Math relTarget len))
        relStart (.floor js/Math src)
        from (if (< relStart 0)
               (.max js/Math (+ len relStart) 0)
               (.min js/Math relStart len))
        relEnd end
        final (if (< relEnd 0)
               (.max js/Math (+ len relEnd) 0)
               (.min js/Math relEnd len))
        count (.min js/Math (- final from) (- len to))
        direction 1
        [direction from to] (if (and (< from to)
                                     (< to (+ from count)))
                              [-1 (+ from (- count 1)) (+ to (- count 1))]
                              [direction from to])]
    (loop [from from
           to to
           count count]
      (when (> count 0)
        (do
          (aset arr to (aget arr from))
          (recur (+ from direction) (+ to direction) (- count 1)))))))

(defn interp
  ([version env l] (interp version env l ";"))
  ([version env l line-ending]
  (let [[f & r] l
        return-last-thunk (fn [env statements]
                            (if (> (count statements) 1)
                              (reduce
                                (fn [inner-env statement]
                                  (interp version (get inner-env :env) statement line-ending))
                                {:env env}
                                statements)
                              (interp version env (first statements) "")))]
    (case f
      :PROGRAM (reduce
                 (fn [c v]
                   (interp version (get c :env) v line-ending))
                 {:env env}
                 r)
      :STATEMENT (interp version env (first r) line-ending)
      :ASSIGN (let [[rhs-neg r] (if (> (count r) 3)
                                       (remove-trailing-negs r)
                                       [nil r])
                    [lhs op rhs] r
                    op-fun (fn [oldval currval]
                             (case op
                               "=" currval
                               "+=" (+ oldval currval)
                               "-=" (- oldval currval)
                               "*=" (* oldval currval)
                               "/=" (/ oldval currval)
                               "%=" (mod oldval currval)
                               :else (throw (ex-info (str "No operator matching: " op) {}))))
                    rhsinterped (interp version env rhs line-ending)
                    rhsval (rhsinterped :value)
                    rhsval (if rhs-neg (* rhsval -1) rhsval)]
                (case (first lhs)
                  :SYMBOL (let [sname (correct-basevar (first (rest lhs)))
                                oldval (goog.object/get env sname 0)
                                newval (op-fun oldval rhsval)]
                            (goog.object/set env sname newval)
                            {:env env :value newval})
                  :BUFFER (let [bufname (first (rest lhs))
                                bufidxinterped (interp version env (second (rest lhs)) "")
                                bufidxval (.floor js/Math (bufidxinterped :value))
                                buf (goog.object/get env bufname)
                                oldval (aget buf bufidxval 0)
                                newval (op-fun oldval rhsval)]
                            (aset buf bufidxval newval)
                            {:env env :value newval})
                  :else (throw (ex-info (str "No assign var matching: " lhs) {}))))
      (:exec2 :exec3) (return-last-thunk env r)
      :while {:env (loop [env env
                          idx 100 ; any non-zero value to run first time
                          count 0]
                     (if (and (> (.abs js/Math idx) MDEPSILON)
                              (< count 1048576))
                       (let [interped (interp version env (first r) line-ending)]
                         (recur (interped :env)
                                (interped :value)
                                (+ count 1)))
                       env))}
      :loop (let [[c comma & s] r
                  interpedc (interp version env c "")]
              {:env (loop [env (interpedc :env)
                           idx 0
                           n (interpedc :value)]
                       (if (< idx n)
                         (let [interpeds (reduce
                                           #(interp version (%1 :env) %2 "")
                                           {:env env}
                                           s)]
                           (recur (interpeds :env) (+ idx 1) n))
                         env))})
      :memcpy (let [[& args] r
                    argvals (map #((interp version env % "") :value) args)
                    [dst src len] argvals
                    [dst src len] (if (< src 0)
                                    [(- dst src) 0 (+ len src)]
                                    [dst src len])
                    [dst src len] (if (< dst 0)
                                    [0 (- src dst) (+ len dst)]
                                    [dst src len])]
                (when (> len 0)
                  (let [buf (goog.object/get env "megabuf")]
                    (copyWithin buf dst src len)))
                {:env env :value dst})
      (:bitwise
       :add-sub
       :mult-div) (let [[lhs-neg r] (remove-leading-negs r)
                        [rhs-neg r] (remove-trailing-negs r)
                        [lhs op rhs] r
                        lhsinterped (interp version env lhs "")
                        rhsinterped (interp version env rhs "")
                        lhsval (lhsinterped :value)
                        rhsval (rhsinterped :value)
                        lhsval (if lhs-neg (* lhsval -1) lhsval)
                        rhsval (if rhs-neg (* rhsval -1) rhsval)]
                    {:env env
                     :value
                       (case op
                         "+" (+ lhsval rhsval)
                         "-" (- lhsval rhsval)
                         "*" (* lhsval rhsval)
                         "/" (if (== rhsval 0)
                               0
                               (/ lhsval rhsval))
                         "%" (if (== rhsval 0)
                               0
                               (mod (.floor js/Math lhsval) (.floor js/Math rhsval)))
                         "&" (bit-and (.floor js/Math lhsval) (.floor js/Math rhsval))
                         "|" (bit-or (.floor js/Math lhsval) (.floor js/Math rhsval))
                         :else (throw (ex-info (str "No operator matching: " op) {})))})
      :NUMBER (let [[is-neg r] (remove-leading-negs r)
                    num ((interp version env (last r) "") :value)
                    num (if is-neg (* num -1) num)]
                {:env env :value num})
      :DECIMAL {:env env
                :value
                  (js/parseFloat
                    (if (== (count r) 3)
                      (let [[lhs _ rhs] r]
                        (trim-leading-zero (str lhs "." rhs)))
                      (let [[lhs rhs] r]
                        (if (= lhs ".")
                          (str "0." rhs)
                          (trim-leading-zero (str lhs ".0"))))))}
      :INTEGER {:env env :value (js/parseInt (trim-leading-zero (str (first r))))}
      :SYMBOL (let [[is-neg r] (remove-leading-negs r)
                    sname (correct-basevar (last r))
                    symval (goog.object/get env sname 0)
                    symval (if is-neg (* symval -1) symval)]
                {:env env :value symval})
      :BUFFER (let [[is-neg r] (remove-leading-negs r)
                    bufname (first r)
                    bufidxinterped (interp version env (second r) "")
                    bufidxval (.floor js/Math (bufidxinterped :value))
                    bufval (aget (goog.object/get (bufidxinterped :env) bufname) bufidxval)
                    bufval (if is-neg (* -1 bufval) bufval)]
                {:env env :value bufval})
      :cond (let [[lhs c rhs] r
                  lhsinterped (interp version env lhs "")
                  rhsinterped (interp version env rhs "")
                  lhsval (lhsinterped :value)
                  rhsval (rhsinterped :value)
                  op (last c)]
              {:env env
               :value (case op
                        ">" (int (> lhsval rhsval))
                        "<" (int (< lhsval rhsval))
                        ">=" (int (>= lhsval rhsval))
                        "<=" (int (<= lhsval rhsval))
                        "&&" (int (and lhsval rhsval))
                        "||" (int (or lhsval rhsval))
                        "==" (if (< (.abs js/Math (- lhsval rhsval)) MDEPSILON) 1 0)
                        "!=" (if (< (.abs js/Math (- lhsval rhsval)) MDEPSILON) 0 1)
                        :else (throw (ex-info (str "No operator matching: " op) {})))})
      :if (let [[is-neg r] (remove-leading-negs r)
                [c t f] (filterv #(not (= % '([:comma]))) (partition-by #(= % [:comma]) r))
                condinterped (interp 2 env (first c) "")
                condval (condinterped :value)]
            (if (> (.abs js/Math condval) MDEPSILON)
              (let [tinterped (return-last-thunk env t)
                    tval (tinterped :value)
                    tval (if is-neg (* tval -1) tval)]
                {:env (tinterped :env) :value tval})
              (let [finterped (return-last-thunk env f)
                    fval (finterped :value)
                    fval (if is-neg (* fval -1) fval)]
                {:env (finterped :env) :value fval})))
      :funcall (let [[is-neg-top r] (remove-leading-negs r)
                     [fname & args] r
                     [is-neg fname] (remove-leading-negs (rest fname))
                     fname (keyword (clojure.string/lower-case (first fname)))]
                 (case fname
                   ; unary functions
                   (:int :floor :abs
                    :exp :sqr :sqrt
                    :sin :cos :tan
                    :asin :acos :atan
                    :log :log10 :sign
                    :rand :bnot) (let [arg (first args)
                                       interpedarg (interp 2 env arg "")
                                       argval (interpedarg :value)]
                                   {:env (interpedarg :env)
                                    :value (case fname
                                             :int (.floor js/Math argval)
                                             :floor (.floor js/Math argval)
                                             :abs (.abs js/Math argval)
                                             :exp (.exp js/Math argval)
                                             :sqr (* argval argval)
                                             :sqrt (.sqrt js/Math (.abs js/Math argval))
                                             :sin (.sin js/Math argval)
                                             :cos (.cos js/Math argval)
                                             :tan (.tan js/Math argval)
                                             :asin (.asin js/Math argval)
                                             :acos (.acos js/Math argval)
                                             :atan (.atan js/Math argval)
                                             :log (.log js/Math argval)
                                             :log10 (* (.log js/Math argval) (.-LOG10E js/Math))
                                             :sign (if (> argval 0)
                                                     1
                                                     (if (< argval 0)
                                                       -1
                                                       0))
                                             :rand (let [xf (.floor js/Math argval)
                                                         resultval (if (< xf 1)
                                                                     (.random js/Math)
                                                                     (* (.random js/Math) xf))]
                                                     (if (== version 1)
                                                       (.floor js/Math resultval)
                                                       resultval))
                                             :bnot (if (< (.abs js/Math argval) MDEPSILON) 1 0)
                                             :else (throw (ex-info (str "No matching token for unary funcall: " fname) {})))})
                   ; binary functions
                   (:equal :above :below
                    :pow :min :max
                    :atan2 :sigmoid
                    :bor :band) (let [firstarg (first args)
                                      interpedfirstarg (interp 2 env firstarg "")
                                      firstargval (interpedfirstarg :value)
                                      secondarg (second args)
                                      interpedsecondarg (interp 2 (interpedfirstarg :env) secondarg "")
                                      secondargval (interpedsecondarg :value)
                                      resultval (case fname
                                                  :equal (if (< (.abs js/Math (- firstargval secondargval))
                                                                MDEPSILON) 1 0)
                                                  :above (if (> firstargval secondargval) 1 0)
                                                  :below (if (< firstargval secondargval) 1 0)
                                                  :pow (let [result (.pow js/Math firstargval secondargval)]
                                                        (if (not (and (js/isFinite result)
                                                                      (not (js/isNaN result))))
                                                          0
                                                          result))
                                                  :min (.min js/Math firstargval secondargval)
                                                  :max (.max js/Math firstargval secondargval)
                                                  :atan2 (.atan2 js/Math firstargval secondargval)
                                                  :sigmoid (let [t (+ 1 (.exp js/Math (- firstargval) secondargval))]
                                                             (if (> (.abs js/Math t) MDEPSILON)
                                                               (/ 1.0 t)
                                                               0))
                                                  :bor (if (or (> (.abs js/Math firstargval) MDEPSILON)
                                                               (> (.abs js/Math secondargval) MDEPSILON))
                                                         1
                                                         0)
                                                  :band (if (and (> (.abs js/Math firstargval) MDEPSILON)
                                                                 (> (.abs js/Math secondargval) MDEPSILON))
                                                         1
                                                         0)
                                                  :else (throw (ex-info (str "No matching token for binary funcall: " fname) {})))
                                        resultval (if (and (or is-neg-top is-neg) (not (and is-neg-top is-neg))) (* resultval -1) resultval)]
                                  {:env (interpedsecondarg :env)
                                   :value resultval})
                   :else (throw (ex-info (str "No matching token for funcall: " fname) {}))))
      :else (throw (ex-info (str "No matching token for interp: " f) {}))))))
