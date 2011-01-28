(ns clj-html-parser.core
  (:require
   [clojure.contrib.io :as io]
   [clojure.contrib.str-utils2 :as su]
   [clojure.contrib.logging :as log])
  (:use [url-normalizer.core])
  (:import [org.htmlcleaner HtmlCleaner PrettyXmlSerializer TagNode])
  (:import [java.net URI URL]))

(defn as-url
  "takes urlish and attempts to make it a valid URL"
  [urlish]
  (let [nospaces (su/replace (str urlish) #"\s" "%20")
        url (io/as-url nospaces)]
    url))

(defn re-sub-all-but-last [regex replacement string]
  (let [count (count (doall (re-seq regex string)))]
    (loop [str string i (- count 1)]
      (if (<= i 0)
        str
        (recur (su/replace-first str regex replacement) (dec i))))))

(defn url-encode-href [str]
  (su/replace str #" " "%20"))
(defn fix-invalid-fragment [str]
  (re-sub-all-but-last #"#" "%23" str))
(defn preprocess-href [href]
  (-> href
   (fix-invalid-fragment)
   (url-encode-href)))

(defn convert-url-to-uri [url]
  (to-uri (as-url url)))

; A single cleaner can safely be reused multiple times.  See:
; http://htmlcleaner.sourceforge.net/javause.php
(defn cleaner []
  (let [c (HtmlCleaner.)]
    (do
      (doto (.getProperties c)
        (.setOmitComments true)
        (.setPruneTags "script,style"))
      c)))

(defn clean-html
  "Transform malformed HTML into well formed HTML"
  [#^String html]
  (.clean #^HtmlCleaner (cleaner) html))

(defmulti get-elements-by-name (fn [x y] [(class x) (class y)]))
(defmethod get-elements-by-name [String String] [html tag]
           (get-elements-by-name (clean-html html) tag))
(defmethod get-elements-by-name [TagNode String] [node tag]
           (.getElementsByName node tag true))

(defn href-to-url [href url]
  (try
    (let [uri #^URI (.resolve (convert-url-to-uri url)
                              (preprocess-href href))]
      (if (.getHost uri)
        (canonicalize-url uri)
        nil))
    (catch java.lang.IllegalArgumentException e
      (do
        (log/info (str "Exception parsing uri " url))
        nil))
    (catch java.net.URISyntaxException e
      (do
        (log/info (str "Exception parsing uri " url))
        nil))))

(defn get-domain [s]
  (.getHost #^URL (as-url s)))

(defn in-link? [link-url page-url]
  (and (re-find #"^http" link-url)
       (= (get-domain link-url) (get-domain page-url))))

(def reject-regexp #".*\.(a|ai|aif|aifc|aiff|asc|avi|bcpio|bin|bmp|bz2|c|cdf|cgi|cgm|class|cpio|cpp?|cpt|csh|css|cxx|dcr|dif|dir|djv|djvu|dll|dmg|dms|doc|dtd|dv|dvi|dxr|eps|etx|exe|ez|gif|gram|grxml|gtar|h|hdf|hqx|ice|ico|ics|ief|ifb|iges|igs|iso|jar|jnlp|jp2|jpe|jpeg|jpg|js|kar|latex|lha|lzh|m3u|mac|man|mathml|me|mesh|mid|midi|mif|mov|movie|mp2|mp3|mp4|mpe|mpeg|mpg|mpga|ms|msh|mxu|nc|o|oda|ogg|pbm|pct|pdb|pdf|pgm|pgn|pic|pict|pl|png|pnm|pnt|pntg|ppm|ppt|ps|py|qt|qti|qtif|ra|ram|ras|rdf|rgb|rm|roff|rpm|rtf|rtx|s|sgm|sgml|sh|shar|silo|sit|skd|skm|skp|skt|smi|smil|snd|so|spl|src|srpm|sv4cpio|sv4crc|svg|swf|t|tar|tcl|tex|texi|texinfo|tgz|tif|tiff|tr|tsv|ustar|vcd|vrml|vxml|wav|wbmp|wbxml|wml|wmlc|wmls|wmlsc|wrl|xbm|xht|xhtml|xls|xml|xpm|xsl|xslt|xwd|xyz|z|zip|rss|atom|json)$")

(defn in-links [html url]
  (let [links (->> (get-elements-by-name html "a")
                   (remove (fn [e] (nil? (.getAttributeByName e "href"))))
                   (map
                    (fn [anchor] (href-to-url (.getAttributeByName anchor "href") url)))
                   (remove nil?))
        
        filtered  (for [anchor links 
                        :when (not (re-matches reject-regexp anchor))] anchor)
        i-links (filter #(in-link? % url) filtered)
        ]
    i-links))
