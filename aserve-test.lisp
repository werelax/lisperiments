(ql:quickload :aserve)

(defpackage :aserve.test
  (:use :cl :net.aserve))

(in-package :aserve.test)

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (format
       (request-reply-stream request)
       "<html><body> ~d</body></html>"
       (random 10000)))))

(publish :path "/random" :function 'random-number)

(start :port 2001)
