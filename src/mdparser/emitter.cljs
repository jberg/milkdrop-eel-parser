(ns mdparser.emitter
  (:require [clojure.set]
            [clojure.string]))

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
