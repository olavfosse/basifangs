(ns electric-starter-app.main
  #?(:clj (:import java.io.File
                   java.nio.file.Path))
  (:require #?(:clj [clojure.java.io :as jio])
            #?(:clj [clojure.pprint :as pp])
            [basifangs.utils :as bfu]
            [basifangs.bintoys :as bintoys]
            [contrib.str :refer [includes-str?]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [lambdaisland.deep-diff2 :as ddiff]
            #?(:clj [duratom.core :refer [duratom]])
            #?(:clj [no.olavfosse.xio :as xio])))

#_(defn pprint-str [x]
    (with-out-str (pp/pprint x)))
#?(:clj (defn pprint-str [x]
          (with-out-str
            (pp/pprint x))))

#?(:cljs (defonce !state (atom {:file nil})))

#?(:cljs (add-watch !state :diff (fn [key atom old-state new-state]
                                   (-> (ddiff/diff old-state new-state)
                                       ddiff/minimize
                                       ddiff/pretty-print ;; Printing directly with
                                                          ;; ddiff/pretty-print does not look
                                                          ;; right
                                       with-out-str
                                       print))))

#_(e/defn Hex-editor [s ResetS]
  (dom/textarea
    (dom/text s)))

(e/defn Dir-tree* [h s SelectFile]
  (e/server
    (let [name_ (.getName h)]
      (cond
        (.isDirectory h)
        (dom/li (dom/text name_)
          (dom/ul
            (e/for-by hash [x (.listFiles h)]
              (Dir-tree* x s SelectFile))))

        (and (.isFile h) (includes-str? name_ s))
        (dom/li (e/client (dom/button
                            (when-some [e (dom/On "click" identity nil)]
                              (when-some [t (first (e/Token e))]
                                (e/server (SelectFile (str (File/.getPath h))))
                                #_(e/server (bfo/bytes->hex (xio/bslurp )))
                                (t)))
                            (dom/text name_))))))))

(e/defn DirTree [SelectFile]
  (e/client
    (let [s (dom/input (dom/On "input" #(-> % .-target .-value) ""))
          h (e/server (-> "workspace"
                          (Path/of (into-array String []))
                          Path/.toAbsolutePath
                          str
                          jio/file))]
      (dom/ul
        (Dir-tree* h s SelectFile)))))

(defn tee [x]
  (println x)
  x)

#?(:clj (defn try-elf64:parse [bs]
          (try (bintoys/elf64:parse bs)
               (catch Exception e (str e)))))

#?(:clj (defn capture [x]
          (def fjkdasfjsakl x)
          x))

#?(:clj (defn bytes-fileatom [path]
          (let [!a (atom (xio/bslurp path))]
            (add-watch !a :write (fn [_key _ref _old new]
                                   (prn :writing (take 10 new))
                                   (xio/bspit path new)))
            !a)))

#?(:clj (defn try-hex->bytes [bs]
          (when (even? (count bs))
              (try (bfu/hex->bytes bs)
                   (catch Throwable e nil)))))

(e/defn Hex-textarea [hex Set-hex]
  (dom/textarea
    (dom/props {:cols bfu/*line-width*
                :rows 30})
    (dom/text hex)
    (e/client
      (let [e (dom/On "input" identity nil)
            [t err] (e/Token e)]
        (when t
          (println :event-received (subs (-> e .-target .-value) 0 10) (type (-> e .-target .-value)) (type ""))
          (e/client (Set-hex (-> e .-target .-value)))
          (t))))))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body
              e/http-request (e/server ring-request)]
                                        ; mandatory wrapper div https://github.com/hyperfiddle/electric/issues/74
      (dom/div (dom/props {:style {:display "contents"}})

               (e/server
                 (let [!path (e/server (atom nil))
                       path (e/server (e/watch !path))]
                   (e/client (DirTree (e/fn SelectFile [path]
                                        (e/client (println path))
                                        (e/server (reset! !path path)))))
                   (when path
                     (dom/div (dom/b (dom/text path)))
                     (let [!bs (e/server (bytes-fileatom path))
                           bs (e/server (e/watch !bs))]
                       
                       #_(dom/pre (dom/text (e/server (pprint-str (try-elf64:parse bs)))))
                       (dom/text (e/server (str (type bs))))


                       (Hex-textarea (bfu/bytes->hex bs)
                                     (e/client (e/fn Set-hex [h]
                                                 (e/server 
                                                   (when-let [bs (try-hex->bytes h)]
                                                     (reset! !bs bs)))
                                                 nil))
                                     )
                       
                       #_(dom/textarea
                         (dom/props {:cols bfu/*line-width*})
                         (dom/text (bfu/bytes->hex bs))
                         
                         (e/client
                             (let [e (dom/On "input" identity nil)
                                   ;; Note: token can take nil
                                   [t err] (e/Token e)]
                               (when t
                                 (do (when-not err
                                       (do 
                                         (e/client (prn (-> e .-target .-value)))
                                         (e/client (prn (e/server (try-hex->bytes (-> e .-target .-value)))))
                                         #_(e/client (print (e/server (try-hex->bytes (e/client (str (-> e .-target .-value)))))))
                                         (if-let [bs (e/server (try-hex->bytes (e/client (-> e .-target .-value))))]
                                           (e/client nil #_(prn bs))
                                           (e/client (prn :err)) #_(do (dom/props {:style {:background-color "red"}}))))))
                                 (t)))))))))))))
