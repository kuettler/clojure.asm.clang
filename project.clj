(defproject clojure.asm.clang "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"sonatype" "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [net.java.dev.jna/jna "4.1.0"]
                 [com.nativelibs4java/jnaerator "0.12-SNAPSHOT"]
                 [com.nativelibs4java/bridj "0.7-SNAPSHOT"]]
  :jvm-opts ^:replace ["-Dclojure.compiler.disable-locals-clearing=true"]
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"])
