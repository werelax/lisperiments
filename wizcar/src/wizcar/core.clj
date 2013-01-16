
(ns wizcar.core
  (:require [wizcar.utils.obj-loader :as loader]
            [wizcar.render :as render]
            [wizcar.camera :as camera]
            [wizcar.car :as car])
  (:import
   (org.lwjgl BufferUtils LWJGLException)
   (org.lwjgl.opengl Display DisplayMode GL11)
   (org.lwjgl.input Keyboard Controllers)
   (org.lwjgl.util.glu GLU)
   (org.newdawn.slick Color)
   (org.newdawn.slick.opengl Texture TextureLoader)
   (org.newdawn.slick.util ResourceLoader))
  (:gen-class))

;;; Scene Logic

(defn update-logic [car]
  (car/move car)
  )

;;; Keyboard Handling

(def close-requested (atom false))
(def full-screen (atom false))

(defn switch-mode []
  (swap! full-screen not)
  (println @full-screen)
  (Display/setFullscreen @full-screen))

(defn poll-input [car]
  ;; Car
  (car/poll-keyboard car)
  ;; Model
  (when (or (Keyboard/isKeyDown Keyboard/KEY_ESCAPE)
            (Display/isCloseRequested))
    (reset! close-requested true))
  (when (Keyboard/isKeyDown Keyboard/KEY_F1)
    (Display/setFullscreen true))
  (when (Keyboard/isKeyDown Keyboard/KEY_F2)
    (Display/setFullscreen false)))

;; Rendering

(defn render [car doll]
  ;; Reset the matrix and clear the scene
  (render/reset)
  ;; Apply the camera transformations
  (camera/look-at @car)
  (camera/set-position {:x 0 :y 20 :z 80})

  ;; Some ground
  (GL11/glBegin GL11/GL_QUADS)
  (GL11/glColor3f 1 1 1)
  (GL11/glVertex3f -1000 0 -1000)
  (GL11/glVertex3f -1000 0  1000)
  (GL11/glVertex3f  1000 0  1000)
  (GL11/glVertex3f  1000 0 -1000)
  (GL11/glEnd)
  (GL11/glColor3f 0.7 0.2 0.2)


  ;; Render the model(s)
  (render/render-object @car)

  (GL11/glPushMatrix)
  (GL11/glTranslatef 40 0 10)
  (GL11/glScalef 20 20 15)
  (render/render-model doll)
  (GL11/glPopMatrix)
  )

;; Initialization

(defn init []
  (Controllers/create)
  ;; Display
  (Display/setDisplayMode (new DisplayMode 800 600))
  (Display/setVSyncEnabled true)
  (Display/setTitle "Testing OpenGL from Clojure")
  (Display/create))

;;; Cleanup

(defn cleanup []
  (Display/destroy))

;;; Entry Point

(defn -main [model-file]
  (init)
  (render/init :width 800 :height 600)
  (let [car (car/make-car model-file)
        doll (loader/import-model "models/diz.obj")]
    (prn "Loaded!")
    (while (not @close-requested)
      (poll-input car)
      (update-logic car)
      (render car doll)
      (Display/update))
    (cleanup)))
