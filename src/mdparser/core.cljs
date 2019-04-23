(ns mdparser.core
  (:require [goog.object]
            [mdparser.parser :as parser]
            [mdparser.emitter :as emitter]))

(defn ^:export make-shape-map
  [version s]
  (let [init_eqs_str (goog.object/get s "init_eqs_str")
        frame_eqs_str (goog.object/get s "frame_eqs_str")

        per-frame-init-parse (when (seq init_eqs_str) (parser/parse init_eqs_str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (emitter/emit version per-frame-init-parse))

        per-frame-parse (when (seq frame_eqs_str) (parser/parse frame_eqs_str))
        per-frame-a (when per-frame-parse (emitter/analyze per-frame-parse :per-shape-frame))
        per-frame-eqs (when (seq per-frame-parse) (emitter/emit version per-frame-parse))

        user-vars (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        per-frame-init-eqs (str (clojure.string/join " " (map #(str "a['" % "'] = 0;") user-vars))
                                " "
                                per-frame-init-eqs)]
    (clj->js
      {:perFrameInitEQs per-frame-init-eqs
       :perFrameEQs per-frame-eqs})))

(defn make-shapes-map
  [version s]
  (if (seq s)
      (mapv (fn [x]
             (make-shape-map version x))
           s)
    []))

(defn ^:export make-wave-map
  [version s]
  (let [init_eqs_str (goog.object/get s "init_eqs_str")
        frame_eqs_str (goog.object/get s "frame_eqs_str")
        point_eqs_str (goog.object/get s "point_eqs_str")

        per-frame-init-parse (when (seq init_eqs_str) (parser/parse init_eqs_str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (emitter/emit version per-frame-init-parse))

        per-frame-parse (when (seq frame_eqs_str) (parser/parse frame_eqs_str))
        per-frame-a (when per-frame-parse (emitter/analyze per-frame-parse :per-wave-frame))
        per-frame-eqs (when (seq per-frame-parse) (emitter/emit version per-frame-parse))

        per-point-parse (when (seq point_eqs_str) (parser/parse point_eqs_str))
        per-point-a (when per-point-parse (emitter/analyze per-point-parse :per-wave-point))
        per-point-eqs (when (seq per-point-parse) (emitter/emit version per-point-parse))

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
  [version s]
  (if (seq s)
    (mapv (fn [x]
           (make-wave-map version x))
         s)
      []))

(defn ^:export convert-basic-preset
  [version init-eqs-str frame-eqs-str pixel-eqs-str]
  (let [per-frame-init-parse (when init-eqs-str (parser/parse init-eqs-str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (emitter/emit version per-frame-init-parse))

        per-frame-parse (when frame-eqs-str (parser/parse frame-eqs-str))
        per-frame-a (when per-frame-parse (emitter/analyze per-frame-parse :per-frame))
        per-frame-eqs (when (seq per-frame-parse) (emitter/emit version per-frame-parse))

        per-pixel-parse (when pixel-eqs-str (parser/parse pixel-eqs-str))
        per-pixel-a (when per-pixel-parse (emitter/analyze per-pixel-parse :per-pixel))
        per-pixel-eqs (when (seq per-pixel-parse) (emitter/emit version per-pixel-parse))

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
  [version init-eqs-str frame-eqs-str pixel-eqs-str shapes waves]
  (let [per-frame-init-parse (when init-eqs-str (parser/parse init-eqs-str))
        per-frame-init-eqs (when (seq per-frame-init-parse) (emitter/emit version per-frame-init-parse))

        per-frame-parse (when frame-eqs-str (parser/parse frame-eqs-str))
        per-frame-a (when per-frame-parse (emitter/analyze per-frame-parse :per-frame))
        per-frame-eqs (when (seq per-frame-parse) (emitter/emit version per-frame-parse))

        per-pixel-parse (when pixel-eqs-str (parser/parse pixel-eqs-str))
        per-pixel-a (when per-pixel-parse (emitter/analyze per-pixel-parse :per-pixel))
        per-pixel-eqs (when (seq per-pixel-parse) (emitter/emit version per-pixel-parse))

        user-vars-frame (if-let [vs (and (seq per-frame-a) (per-frame-a :user-vars))] vs [])
        user-vars-pixel (if-let [vs (and (seq per-pixel-a) (per-pixel-a :user-vars))] vs [])
        user-vars (into (into #{} user-vars-pixel) user-vars-frame)
        per-frame-init-eqs (str (clojure.string/join " " (map #(str "a['" % "'] = 0;") user-vars))
                                " "
                                per-frame-init-eqs)
        shapesMap (make-shapes-map version shapes)
        wavesMap (make-waves-map version waves)]
    (clj->js
     {:perFrameInitEQs per-frame-init-eqs
      :perFrameEQs per-frame-eqs
      :perPixelEQs per-pixel-eqs
      :shapes shapesMap
      :waves wavesMap})))

(defn ^:export interpret
  [version env parse]
  ((emitter/interp version env parse) :env))
