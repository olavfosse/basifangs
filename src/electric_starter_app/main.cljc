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
                                (println t)
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

(defn try-elf64:parse [bs]
  (try (bintoys/elf64:parse bs)
       (catch Exception e (str e))))

#?(:clj (defn capture [x]
          (def fjkdasfjsakl x)
          x))
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
                     (let [bs (e/server (xio/bslurp path))]
                       (dom/pre (dom/text (e/server (pprint-str (try-elf64:parse bs)))))
                       (dom/textarea
                         (dom/props {:cols bfu/*line-width*})
                         (dom/text (bfu/bytes->hex bs)))))))))))
