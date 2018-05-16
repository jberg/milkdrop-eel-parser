(ns mdparser.core
  (:require [goog.object]
            [mdparser.eqparser :as eqp]))

(defn ^:export make-shape-map
  [s]
  (let [init_eqs_str (goog.object/get s "init_eqs_str")
        frame_eqs_str (goog.object/get s "frame_eqs_str")

        per-frame-init-parse (when (seq init_eqs_str) (eqp/parse init_eqs_str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))

        per-frame-parse (when (seq frame_eqs_str) (eqp/parse frame_eqs_str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse :per-shape-frame))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))

        user-vars (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        per-frame-init-eqs (str (clojure.string/join " " (map #(str "a['" % "'] = 0;") user-vars))
                                " "
                                per-frame-init-eqs)]
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
  (let [init_eqs_str (goog.object/get s "init_eqs_str")
        frame_eqs_str (goog.object/get s "frame_eqs_str")
        point_eqs_str (goog.object/get s "point_eqs_str")

        per-frame-init-parse (when (seq init_eqs_str) (eqp/parse init_eqs_str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))

        per-frame-parse (when (seq frame_eqs_str) (eqp/parse frame_eqs_str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse :per-wave-frame))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))

        per-point-parse (when (seq point_eqs_str) (eqp/parse point_eqs_str))
        per-point-a (when per-point-parse (eqp/analyze per-point-parse :per-wave-point))
        per-point-eqs (when (seq per-point-parse) (eqp/emit per-point-parse))

        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-point (if-let [vs (and (seq per-point-a) (per-point-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-point) user-vars-frame)
        per-frame-init-eqs (str (clojure.string/join " " (map #(str "a['" % "'] = 0;") user-vars))
                                " "
                                per-frame-init-eqs)]
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
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse :per-frame))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))

        per-pixel-parse (when pixel-eqs-str (eqp/parse pixel-eqs-str))
        per-pixel-a (when per-pixel-parse (eqp/analyze per-pixel-parse :per-pixel))
        per-pixel-eqs (when (seq per-pixel-parse) (eqp/emit per-pixel-parse))

        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-pixel (if-let [vs (and (seq per-pixel-a) (per-pixel-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-pixel) user-vars-frame)
        per-frame-init-eqs (str (clojure.string/join " " (map #(str "a['" % "'] = 0;") user-vars))
                                " "
                                per-frame-init-eqs)]
    (clj->js
     {:perFrameInitEQs per-frame-init-eqs
      :perFrameEQs per-frame-eqs
      :perPixelEQs per-pixel-eqs})))

(defn ^:export convert-preset-wave-and-shape
  [init-eqs-str frame-eqs-str pixel-eqs-str shapes waves]
  (let [per-frame-init-parse (when init-eqs-str (eqp/parse init-eqs-str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (eqp/emit per-frame-init-parse))

        per-frame-parse (when frame-eqs-str (eqp/parse frame-eqs-str))
        per-frame-a (when per-frame-parse (eqp/analyze per-frame-parse :per-frame))
        per-frame-eqs (when (seq per-frame-parse) (eqp/emit per-frame-parse))

        per-pixel-parse (when pixel-eqs-str (eqp/parse pixel-eqs-str))
        per-pixel-a (when per-pixel-parse (eqp/analyze per-pixel-parse :per-pixel))
        per-pixel-eqs (when (seq per-pixel-parse) (eqp/emit per-pixel-parse))

        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-pixel (if-let [vs (and (seq per-pixel-a) (per-pixel-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-pixel) user-vars-frame)
        per-frame-init-eqs (str (clojure.string/join " " (map #(str "a['" % "'] = 0;") user-vars))
                                " "
                                per-frame-init-eqs)
        shapesMap (make-shapes-map shapes)
        wavesMap (make-waves-map waves)]
    (clj->js
     {:perFrameInitEQs per-frame-init-eqs
      :perFrameEQs per-frame-eqs
      :perPixelEQs per-pixel-eqs
      :shapes shapesMap
      :waves wavesMap})))
