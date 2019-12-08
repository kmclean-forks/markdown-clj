(ns markdown.plaintext.transformers
  (:require [markdown.transformers :as t]
            [markdown.common :as c]
            [clojure.string :as str]
            [markdown.links :as links]))

(defn- heading [text]
  (if (c/heading-level text)
    (c/heading-text text)
    text))

(defn- divider [text]
  (if (or (c/hr? text) (-> text str/trim (str/starts-with? "=")))
    "\n"
    text))

(defn- br [text]
  (if-let [parsed (c/br-text text)]
    (str parsed "\n")
    text))

(def strong (c/make-separator "**"))

(def bold-italic (c/make-separator "***"))

(def bold (c/make-separator "__"))

(def em (c/make-separator "*"))

(def italics (c/make-separator "_"))

(def strikethrough (c/make-separator "~~"))

(def inline-code (c/make-separator "`"))

(defn code-block [text]
  (if (-> text str/trim (str/starts-with? "```"))
    ""
    text))

(defn superscript [text]
  (str/replace text #"\^" " "))

(defn md-link [text]
  (let [open-bracket (str/index-of text "[")
        open-link (str/index-of text "](")
        close-link (str/index-of text ")")]
    (if (and open-bracket open-link close-link)
      (recur
        (str
          (subs text 0 open-bracket)
          (subs text (+ open-bracket 1) open-link)
          (subs text (+ close-link 1))))
      text)))

(defn- html-link [text]
  (let [begin-open-tag (str/index-of text "<a")
        end-open-tag (str/index-of text ">")
        close-tag (str/index-of text "</a>")]
    (if (and begin-open-tag end-open-tag close-tag)
      (recur
        (str
          (subs text 0 begin-open-tag)
          (subs text (+ end-open-tag 1) close-tag)
          (subs text (+ close-tag 4))))
      text)))

(def plaintext-transformers
  [heading
   md-link
   html-link
   divider
   br
   em
   strong
   bold-italic
   bold
   italics
   strikethrough
   code-block
   inline-code
   superscript
   ])



