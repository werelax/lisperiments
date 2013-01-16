(ns wizcar.camera
  (:import
   (org.lwjgl BufferUtils)
   (org.lwjgl.opengl GL11)
   (org.lwjgl.util.glu GLU)
   (org.lwjgl.input Keyboard Mouse)
   (org.lwjgl.util.vector Vector3f Matrix4f)))

(declare look-at)

;; Configuration

(def mouse-speed 1)
(def walk-speed 0.5)

;; State

(defn generate-state []
  (atom {:x 0 :y 0 :z -35 :xrot 0 :yrot 0 :zrot 0 :speed 1}))

(def state (generate-state))

;; Movement

(defn- advance [state & {:keys [x y z] :or {x 0 y 0 z 0}}]
  (let [yrot (Math/toRadians (:yrot @state))
        xrot (Math/toRadians (:xrot @state))
        ax (* z -1 walk-speed (Math/sin yrot))
        az (* z walk-speed (Math/cos yrot))
        ay (* z walk-speed (Math/sin xrot))
        strife (Math/toRadians (+ 90 (:yrot @state)))
        sx (* x -1 walk-speed (Math/sin strife))
        sz (* x walk-speed (Math/cos strife))
        ;; x (+ (:x @state) ax sx)
        ;; z (+ (:z @state) az sz)
        x (+ (:x @state) (* -1 x walk-speed))
        z (+ (:z @state) (* -1 z walk-speed))
        y (+ (:y @state) ay (* -1 y walk-speed))]
    (swap! state assoc
           :x x
           :y y
           :z z)))
  
(defn move [state dir]
  (cond
   (= dir :fordward) (advance state :z -1)
   (= dir :backward) (advance state :z 1)
   (= dir :right) (advance state :x 1)
   (= dir :left) (advance state :x -1)
   (= dir :up) (advance state :y -1)
   (= dir :down) (advance state :y 1)))

(defn translate [state position]
  (swap! state assoc
         :x (:x position)
         :y (:y position)
         :z (:z position))
  (GL11/glTranslatef (- (:x @state)) (- (:y @state)) (- (:z @state))))

;; Input

(defn grab-mouse []
  (while (Mouse/next)
    (when (Mouse/isButtonDown 0)
      (Mouse/setGrabbed true))
    (when (Mouse/isButtonDown 1)
      (Mouse/setGrabbed false))))

(defn poll-mouse-orientation [state]
  (when (Mouse/isGrabbed)
    (let [mouse-dy (* (Mouse/getDX) mouse-speed 0.16)
          mouse-dx (* -1 (Mouse/getDY) mouse-speed 0.16)
          xrot (:xrot @state)
          yrot (:yrot @state)
          zrot (:zrot @state)]
      (swap! state assoc
             :xrot (Math/abs (mod (+ xrot mouse-dx) 360))
             :yrot (Math/abs (mod (+ yrot mouse-dy) 360))))))

(defn poll-keyboard [state]
  (when (Keyboard/isKeyDown Keyboard/KEY_W)
    (move state :fordward))
  (when (Keyboard/isKeyDown Keyboard/KEY_S)
    (move state :backward))
  (when (Keyboard/isKeyDown Keyboard/KEY_A)
    (move state :left))
  (when (Keyboard/isKeyDown Keyboard/KEY_D)
    (move state :right))
  (when (Keyboard/isKeyDown Keyboard/KEY_SPACE)
    (move state :up))
  (when (Keyboard/isKeyDown Keyboard/KEY_LSHIFT)
    (move state :down)))
   
;; Transformations

(defn- apply-transformations [state]
  (GL11/glLoadIdentity)
  (GL11/glTranslatef (- (:x @state)) (- (:y @state)) (- (:z @state))) )

;; Interface

(defn apply-camera []
  (apply-transformations state))

(defn set-position [position]
  (let [full-position (merge @state position)]
    (translate state full-position)))

(defn look-at [target]
  (let [position @state
        eye (Vector3f. (:x position) (:y position) (:z position))
        target (Vector3f. (:x target) (:y target) (:z target))
        zaxis (Vector3f.)
        xaxis (Vector3f.)
        yaxis (Vector3f.)
        buf (BufferUtils/createFloatBuffer 16)]
    (Vector3f/sub eye target zaxis)
    (.normalise zaxis zaxis)
    (Vector3f/cross (Vector3f. 0 1 0) zaxis xaxis)
    (.normalise xaxis xaxis)
    (Vector3f/cross zaxis xaxis yaxis)
    (.normalise yaxis yaxis)
    (.put buf (float-array [(.x xaxis) (.x yaxis) (.x zaxis) 0
                            (.y xaxis) (.y yaxis) (.y zaxis) 0
                            (.z xaxis) (.z yaxis) (.z zaxis) 0
                            0          0          0          1]))
    (.flip buf)
    (GL11/glMultMatrix buf)))

(defn poll-input []
  ;; (grab-mouse)
  ;; (poll-mouse-orientation state)
  (poll-keyboard state))
