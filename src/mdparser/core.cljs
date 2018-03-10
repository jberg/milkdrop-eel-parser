(ns mdparser.core
  (:require [mdparser.eqparser :as eqp]))

(defn ^:export make-shape-map
  [s]
  (let [init_eqs_str (aget s "init_eqs_str")
        frame_eqs_str (aget s "frame_eqs_str")
        per_shape_frame_key (or (keyword (aget s "per_shape_frame_key")) :per-shape-frame)
        per-frame-init-parse (when (seq init_eqs_str) (eqp/parse init_eqs_str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))
        per-frame-parse (when (seq frame_eqs_str) (eqp/parse frame_eqs_str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse per_shape_frame_key))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))
        user-vars (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        exclude-vars #{"r" "g" "b" "a" "r2" "g2" "b2" "a2"}
        user-vars (filter #(not (exclude-vars %)) user-vars)
        per-frame-init-eqs (str (clojure.string/join "\n" (map #(str "m." % " = 0;") user-vars))
                                "\n"
                                per-frame-init-eqs)
        rkeys (if (seq per-frame-a) (per-frame-a :rkeys) [])
      per-frame-init-eqs (str per-frame-init-eqs
                     "\n\t\t\tm.rkeys = [" (clojure.string/join "," (map #(str "'" % "'") rkeys)) "];")]
    (clj->js
      {:perFrameInitEQs per-frame-init-eqs
       :perFrameEQs per-frame-eqs})))

(defn make-shapes-map
  [s]
  (if (seq s)
      (mapv (fn [x]
             (make-shape-map x))
           s)
    []))

(defn ^:export make-wave-map
  [s]
  (let [init_eqs_str (aget s "init_eqs_str")
        frame_eqs_str (aget s "frame_eqs_str")
        point_eqs_str (aget s "point_eqs_str")
        per_wave_frame_key (or (keyword (aget s "per_wave_frame_key")) :per-wave-frame)
        per_wave_point_key (or (keyword (aget s "per_wave_point_key")) :per-wave-point)
        per-frame-init-parse (when (seq init_eqs_str) (eqp/parse init_eqs_str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))
        per-frame-parse (when (seq frame_eqs_str) (eqp/parse frame_eqs_str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse per_wave_frame_key))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))
        per-point-parse (when (seq point_eqs_str) (eqp/parse point_eqs_str))
        per-point-a (when per-point-parse (eqp/analyze per-point-parse per_wave_point_key))
        per-point-eqs (when (seq per-point-parse) (eqp/emit per-point-parse))
        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-point (if-let [vs (and (seq per-point-a) (per-point-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-point) user-vars-frame)
        exclude-vars #{"r" "g" "b" "a"}
        user-vars (filter #(not (exclude-vars %)) user-vars)
        per-frame-init-eqs (str (clojure.string/join "\n" (map #(str "m." % " = 0;") user-vars))
                                "\n"
                                per-frame-init-eqs)
      rkeys (if (seq per-point-a) (per-point-a :rkeys) [])
      per-frame-init-eqs (str per-frame-init-eqs
                     "\n\t\t\tm.rkeys = [" (clojure.string/join "," (map #(str "'" % "'") rkeys)) "];")]
    (clj->js
      {:perFrameInitEQs per-frame-init-eqs
     :perFrameEQs per-frame-eqs
     :perPointEQs per-point-eqs})))

(defn make-waves-map
  [s]
  (if (seq s)
    (mapv (fn [x]
           (make-wave-map x))
         s)
      []))

(defn ^:export convert-basic-preset
  [init-eqs-str frame-eqs-str pixel-eqs-str]
  (let [per-frame-init-parse (when init-eqs-str (eqp/parse init-eqs-str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))

        per-frame-parse (when frame-eqs-str (eqp/parse frame-eqs-str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))

        per-pixel-parse (when pixel-eqs-str (eqp/parse pixel-eqs-str))
        per-pixel-a (when per-pixel-parse (eqp/analyze per-pixel-parse :per-pixel))
        per-pixel-eqs (when (seq per-pixel-parse) (eqp/emit per-pixel-parse))

        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-pixel (if-let [vs (and (seq per-pixel-a) (per-pixel-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-pixel) user-vars-frame)
        per-frame-init-eqs (str (clojure.string/join "\n" (map #(str "m." % " = 0;") user-vars))
                                "\n"
                                per-frame-init-eqs)
        rkeys (if (seq per-pixel-a) (per-pixel-a :rkeys) [])
        per-frame-eqs (str per-frame-eqs
                           "\n\t\tm.rkeys = [" (clojure.string/join "," (map #(str "'" % "'") rkeys)) "];")]
    (clj->js
     {:perFrameInitEQs per-frame-init-eqs
      :perFrameEQs per-frame-eqs
      :perPixelEQs per-pixel-eqs})))

(defn ^:export convert-preset-wave-and-shape
  [init-eqs-str frame-eqs-str pixel-eqs-str shapes waves]
  (let [per-frame-init-parse (when init-eqs-str (eqp/parse init-eqs-str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))

        per-frame-parse (when frame-eqs-str (eqp/parse frame-eqs-str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))

        per-pixel-parse (when pixel-eqs-str (eqp/parse pixel-eqs-str))
        per-pixel-a (when per-pixel-parse (eqp/analyze per-pixel-parse :per-pixel))
        per-pixel-eqs (when (seq per-pixel-parse) (eqp/emit per-pixel-parse))

        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-pixel (if-let [vs (and (seq per-pixel-a) (per-pixel-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-pixel) user-vars-frame)
        per-frame-init-eqs (str (clojure.string/join "\n" (map #(str "m." % " = 0;") user-vars))
                                "\n"
                                per-frame-init-eqs)
        rkeys (if (seq per-pixel-a) (per-pixel-a :rkeys) [])
        per-frame-eqs (str per-frame-eqs
                           "\n\t\tm.rkeys = [" (clojure.string/join "," (map #(str "'" % "'") rkeys)) "];")
        shapesMap (make-shapes-map shapes)
        wavesMap (make-waves-map waves)]
    (clj->js
     {:perFrameInitEQs per-frame-init-eqs
      :perFrameEQs per-frame-eqs
      :perPixelEQs per-pixel-eqs
      :shapes shapesMap
      :waves wavesMap})))
