;; Idea is to provide lots of handy utils needed to start probing or
;; writing a solve script. Quick and dirty baby.
(ns basifangs.utils
  #?(:lpy (:import socket)
     :clj (:import java.nio.file.Paths java.nio.file.Files))
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as xf]
            #?(:clj [net.cgrand.xforms.io :as xfio])
            #?(:clj [clojure.java.io :as io])))

(def ^:dynamic *line-width* 80)

(defn tee [x]
  (println x)
  x)

(def flag-chars
  "Ideally these would be ordered by the how commonly used they are in flags"
  ;; Some chars are placed at the end of the list as they tend to interfere.
  ;; CHAR | EXAMPLE
  ;; %    | LIKE "<prefix>%"
  ;; .    | regex
  ;; *    | regex
  ;; +    | regex
  ;; #    | interpeted as a comment in some injection contexts
  
  "-_{}0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"$&'(),/:;<=>?@[\\]^`|~%.*+#")

(def flag-chars-no-regex-specials "-_{}0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'(),/:;<=>?@[\\]^`|~")

#?(:lpy (defn recvall [sock]
          (let [s (loop [s ""
                         suffix (-> sock
                                    (.recv 4096)
                                    (.decode "utf-8"))]
                    (if (= suffix "")
                      s
                      (recur (str s suffix)
                             (-> sock
                                 (.recv 4096)
                                 (.decode "utf-8")))))]
            (.close sock)
            (def recvallout s)
            s)))

#?(:lpy (defn quick-tcp
          "string in, string out tcp"
          [host port msg]
          (let [sock (socket/socket #_ip socket/AF_INET #_tcp socket/SOCK_STREAM)]
            (.settimeout sock 5.0)
            (.connect sock #py(host port))
            (.sendall sock (.encode msg "utf-8"))
            (recvall sock))))

#?(:cljs nil
   :default 
   (defn char-by-char 
     "In many challenges you achieve a fn checking if the flag starts with
  a given prefix. This fn captures the logic to go from such a fn, to
  the flag in an efficient manner."
     ([starts-with?]
      (char-by-char starts-with? ""))
     
     ([starts-with? prefix]
      (char-by-char starts-with? prefix {:chars flag-chars
                                         :max-length 100}))
     
     ([starts-with?
       prefix
       {:as opts
        :keys [chars max-length]}]
      (if (>= (count prefix) max-length)
        ;; Sometimes we get false positives. E.g a regex prefix is
        ;; evolving like so:
        ;;
        ;; 1. fjdkal.
        ;; 2. fjdkal..
        ;; 3. fjdkal...
        ;; n. fjdkal...........................
        ;;
        ;; We don't want to accidentally DOS any servers so we
        ;; automatically quit after a while.
        (println "Prefix hit :max-length. Stopping search.")
        (let [new-prefix 
              (binding #?(:lpy [*pmap-cpu-count* (count chars)]
                          :default [])
                (->> chars
                     (pmap (comp (juxt identity starts-with?) (partial str prefix)))
                     (map (fn [[new-prefix match?]]
                            (println [new-prefix match?])
                            (when match? new-prefix)))
                     (filter identity)
                     first))]
          (if-not new-prefix
            (println "No new prefix found. Stopping search.")
            (do (println "New prefix:" new-prefix)
                (char-by-char starts-with? new-prefix opts))))))))

(defn str-partition-all [n s]
  (mapv #(apply str %) (partition-all n s)))

(defn rcomp [& fns]
  (apply comp (reverse fns)))


(do (defn byte->hex [b]
      (str
       (nth "0123456789ABCDEF" (quot b 16))
       (nth "0123456789ABCDEF" (mod b 16))))

    (def xf:bytes->hex (comp (map byte->hex)
                             (interpose " ")
                             (partition-all (* 2 (quot *line-width* 3)))
                             (interpose "\n")
                             cat))

    (defn bytes->hex [bs] (xf/str xf:bytes->hex bs)))

(do (defn hex->byte [[left right]]
      (+ (* (str/index-of "0123456789ABCDEF" left) 16)
         (str/index-of "0123456789ABCDEF" right)))

    (def xf:hex->bytes (comp
                        (remove #{\space \newline})
                        (partition-all 2)
                        (map hex->byte)))

    (defn hex->bytes [s] (into [] xf:hex->bytes s)))








