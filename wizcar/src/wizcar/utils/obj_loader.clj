;;; Still a little bit rough, and SLOW
;;; But quite solid so far...

(ns wizcar.utils.obj-loader)

;;; Parsing

;; update the state

(defn- add-item [state k value]
  (swap! state assoc k (conj (k @state)
                               value)))

;; dispatch the line processor

(defmulti #^{:private true} process-line
  (fn [line data]
    (cond
     (.startsWith line "v ") :vertex
     (.startsWith line "vn ") :normal
     (.startsWith line "vt ") :texture
     (.startsWith line "f ") :face
     :default :other)))

(defmethod process-line :vertex [line data]
  (try
    (let [vertex (doall (map read-string
                             (rest (clojure.string/split line #"\s+"))))]
      (add-item data :vertices vertex))
    (catch Exception e
      (prn e)
      (prn line))))
    

(defmethod process-line :normal [line data]
  (try
    (let [normal (doall (map read-string
                             (rest (clojure.string/split line #"\s+"))))]
      (add-item data :normals normal))
    (catch Exception e
      (prn e)
      (prn line))))

(defmethod process-line :texture [line data]
  ;; not yet
  )

(defmethod process-line :face [line data]
  (let [face-points (rest (clojure.string/split line #"\s+"))
        face (map (fn [chunk]
                    (try
                      (let [parts (map #(if (= (.length %) 0)
                                          0
                                          (read-string %))
                                       (clojure.string/split chunk #"/"))
                            vertex ((:vertices @data) (dec (first parts)))
                            normal (if (= (count parts) 3)
                                     ((:normals @data) (dec (nth parts 2)))
                                      nil)]
                        {:vertex vertex :normal normal})
                      (catch Exception e
                        (prn e)
                        (prn chunk)
                        (prn "v:" (count (:vertices @data)) " n: " (count (:normals @data)))
                        (prn "vi: " (first (clojure.string/split chunk #"/")))
                        (prn "ni: " (nth (clojure.string/split chunk #"/") 2))
                        (prn line))))
                  face-points)]
    (add-item data :faces face)))

(defmethod process-line :other [line data] ) ; Had to put something

;;; Entry Point

(defn import-model [filename]
  (let [obj (clojure.string/split (slurp filename) #"\n")
        data (atom {:vertices [] :normals [] :faces []})]
    (doall (map #(process-line (clojure.string/trim %) data)
                obj))
    (atom {:data (:faces @data)})))