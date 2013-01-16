(ns wizcar.render
  (:import
   (org.lwjgl BufferUtils LWJGLException)
   (org.lwjgl.opengl Display DisplayMode GL11)
   (org.lwjgl.input Keyboard)
   (org.lwjgl.util.glu GLU)
   (org.newdawn.slick Color)
   (org.newdawn.slick.opengl Texture TextureLoader)
   (org.newdawn.slick.util ResourceLoader)))

;;; Rendering facades

(defn- gl-normal [normal]
  (when normal
    (apply #(GL11/glNormal3f %1 %2 %3) normal)))

(defn- gl-vertex [vertex]
  (if (< (count vertex) 3)
    (prn vertex)
    (when vertex
      (apply #(GL11/glVertex3f %1 %2 %3) vertex))))

(defn- render-shape [shape points]
  (GL11/glBegin shape)
  (doseq [point points]
    (gl-normal (:normal point))
    (gl-vertex (:vertex point)))
  (GL11/glEnd))

(defmulti #^{:private true} render
  (fn [thing]
    (let [nvertices (count thing)]
      (cond
       (= nvertices 3) :tri
       (= nvertices 4) :quad
       :default :poly))))

(defmethod render :tri [thing]
  (render-shape GL11/GL_TRIANGLES thing))

(defmethod render :quad [thing]
  (render-shape GL11/GL_QUADS thing))

(defmethod render :poly [thing]
  (render-shape GL11/GL_POLYGON thing))

;;; Public Interface

(defn render-model [model]
  (if (:list @model)
    (do
      (GL11/glCallList (:list @model))
      model)
    (let [list (GL11/glGenLists 1)]
      (GL11/glNewList list GL11/GL_COMPILE)
      (doseq [thing (:data @model)]
        (render thing))
      (GL11/glEndList)
      (GL11/glCallList list)
      (swap! model assoc :list list))))

(defn render-object [object]
  (let [object (merge {:xrot 0 :yrot 0 :zrot 0 :x 0 :y 0 :z 0} object)]
    (GL11/glPushMatrix)
    (GL11/glTranslatef (:x object) (:y object) (:z object))
    (GL11/glTranslatef 0 0 10)
    (GL11/glRotatef (:xrot object) 1 0 0)
    (GL11/glRotatef (:yrot object) 0 1 0)
    (GL11/glRotatef (:zrot object) 0 0 1)
    (GL11/glTranslatef 0 0 -10)
    (render-model (:model object))
    (GL11/glPopMatrix)))

(defn init [& {:keys [wireframe light width height] :or {wireframe false light true}}]
  ;; OpenGL
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glShadeModel GL11/GL_SMOOTH)
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClearDepth 1)
  ;; Depth Test
  (GL11/glEnable GL11/GL_DEPTH_TEST)
  (GL11/glDepthFunc GL11/GL_LEQUAL)
  ;; Blend
  ;; (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE)
  ;; (GL11/glEnable GL11/GL_BLEND)
  ;; Viewport
  (GL11/glViewport 0 0 width height)
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glLoadIdentity)
  (GLU/gluPerspective 45.0 (/ width height) 0.1 2000.0)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)
  ;(GL11/glHint GL11/GL_PERSPECTIVE_CORRECTION_HINT GL11/GL_NICEST)
  (GL11/glHint GL11/GL_PERSPECTIVE_CORRECTION_HINT GL11/GL_FASTEST)
  ;; Lights
  (GL11/glLight GL11/GL_LIGHT1 GL11/GL_AMBIENT
                (.flip (.put (BufferUtils/createFloatBuffer 4) (float-array [0.2 0.2 0.2 1]))))
  (GL11/glLight GL11/GL_LIGHT1 GL11/GL_DIFFUSE
                (.flip (.put (BufferUtils/createFloatBuffer 4) (float-array [1 1 1 1]))))
  (GL11/glLight GL11/GL_LIGHT1 GL11/GL_POSITION
                (.flip (.put (BufferUtils/createFloatBuffer 4) (float-array [0 0 2 1]))))
  (when light
    (GL11/glEnable GL11/GL_LIGHTING)
    (GL11/glEnable GL11/GL_LIGHT1))
  ;; Wireframe ON/OFF
  (if wireframe
    (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK GL11/GL_LINE)
    (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK GL11/GL_FILL))
  ;; Culling
  (GL11/glCullFace GL11/GL_BACK)
  (GL11/glEnable GL11/GL_CULL_FACE))

(defn reset []
  (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
  (GL11/glLoadIdentity))