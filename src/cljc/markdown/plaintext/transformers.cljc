(ns markdown.plaintext.transformers
  (:require [markdown.transformers :as t]
            [markdown.common :as c]
            [clojure.string :as str]))

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

(defn link [text]
  (let [[_ link-text _href] (re-matches #"\[(.+)\]\((.+)\)" text)]
    (if link-text
      link-text
      text)))

(def plaintext-transformers
  [
   heading
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



