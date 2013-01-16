(asdf:clear-system :cocoahelper)
(asdf:operate 'asdf:load-op :cocoahelper)

(defpackage :test.sdl
  (:use :cl))

(in-package :test.sdl)

(defun opengl-test-1 ()
  (sdl:with-init ()
    (sdl:window 250 250
                :title-caption "OpenGL Example"
                :icon-caption "OpenGL Example"
                :opengl t
                :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl:sdl-gl-get-proc-address)
    (gl:clear-color 0 0 0 0)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    (sdl:with-events ()
      (:quit-events () t)
      (:idle ()
        (gl:clear :color-buffer-bit)
        (gl:color 1 1 1)
        (gl:with-primitive :polygon
                           (gl:vertex 0.25 0.25 0)
                           (gl:vertex 0.75 0.25 0)
                           (gl:vertex 0.75 0.75 0)
                           (gl:vertex 0.25 0.75 0))
        (gl:flush)
        (sdl:update-display)))))

(opengl-test-1)
