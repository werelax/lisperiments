(defproject wizcar "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xms1g" "-Xmx1g" "-Djava.library.path=native/macosx"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.lwjgl.lwjgl/lwjgl "2.8.4"]
                 [org.lwjgl.lwjgl/lwjgl_util "2.8.4"]
                 [slick-util "1.0.0"]]
  :main wizcar.core)
