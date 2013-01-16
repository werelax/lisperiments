(ns wizcar.car
  (:import
   (org.lwjgl.input Keyboard Controllers))
  (:require [wizcar.utils.obj-loader :as loader]))

(defn turn [car dir]
  (let [dir (if (= dir :left) 3 -3)]
    (swap! car assoc :direction dir)))

(defn move [car]
  (when (> (:speed @car) 0)
    (let [speed (:speed @car)
          yrot (Math/toRadians (+ (:direction @car) (:yrot @car)))
          ax (* speed (Math/sin yrot))
          az (* speed (Math/cos yrot))
          x (+ (:x @car) ax)
          z (+ (:z @car) az)]
      (swap! car assoc
             :x x
             :z z
             :yrot (+ (:direction @car) (:yrot @car))))))

(defn thrust [car dir]
  (let [acc (* dir (:acc @car))
        speed (:speed @car)]
    (swap! car assoc
           :speed (min (:max-speed @car) (+ acc speed)))))

(defn decelerate [car]
  (let [speed (:speed @car)
        friction 0.3]
    (swap! car assoc :speed (max (- speed friction) 0))))

(defn poll-gamepad [car]
  (Controllers/poll)
  (while (Controllers/next)
    (let [joy (Controllers/getEventSource)
          x (.getAxisValue joy 1)
          y (.getAxisValue joy 0)]
      (thrust car (- x))
      (cond
       (< y -0.1) (turn car :left)
       (> y 0.1) (turn car :right)))))

(defn poll-keyboard [car]
  ;; Free the wheel
  (swap! car assoc :direction 0)
  (poll-gamepad car)
  ;; Controls
  (if (Keyboard/isKeyDown Keyboard/KEY_W)
    (thrust car 1)
    (decelerate car))
  (when (Keyboard/isKeyDown Keyboard/KEY_S)
    (thrust car -1)
    (decelerate car))
  (when (Keyboard/isKeyDown Keyboard/KEY_A)
    (turn car :left))
  (when (Keyboard/isKeyDown Keyboard/KEY_D)
    (turn car :right)))

(defn make-car [model-name]
  (atom {:model (loader/import-model model-name)
         :yrot 0
         :direction 0
         :acc 0.3
         :speed 0
         :max-speed 5
         :x 0
         :y 0
         :z 0}))