(ns markdown.plaintext.md-plaintext-test
  (:require [markdown.core :as markdown]
            [markdown.tables :as tables]
            #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:cljs [goog.string])))

(def entry-function
  #?(:clj markdown/md-to-plaintext
     ;:cljs markdown/md->plaintext
     ))

(deftest heading1
         (is (= "Ticket #123" (entry-function "# Ticket #123")))
         (is (= "Foo" (entry-function " # Foo")))
         (is (= "foo" (entry-function "#foo")))
         (is (= "foo\n\n" (entry-function "foo\n===")))
         (is (= "foo" (entry-function "#foo#")))
         (is (= "foo" (entry-function "#foo#\n")))
         (is (= "some header with_an_underscore"
                (entry-function "# some header `with_an_underscore`")))
         (is (= "one\n\nheading1\n" (entry-function "* one\n\nheading1\n========\n"))))

(deftest heading2
         (is (= "foo" (entry-function "##foo")))
         (is (= "foo\n\n" (entry-function "foo\n---")))
         (is (= "foo" (entry-function "##foo##")))
         (is (= "foo" (entry-function "##foo##\n"))))

(deftest heading-with-complex-anchor
         (is (= "foo bar BAz\nsome text" (entry-function "###foo bar BAz\nsome text")))
         (is (= "foo bar BAz\nsome text" (entry-function "###foo bar BAz##\nsome text"))))

(deftest br
         (is (= "foo\n" (entry-function "foo  "))))

(deftest hr
         (is (= "\n" (entry-function "***")))
         (is (= "\n" (entry-function " * * * ")))
         (is (= "\n" (entry-function " *****")))
         (is (= "\n" (entry-function "- - - "))))

(deftest em
         (is (= "foo" (entry-function "*foo*"))))

(deftest italics
         (is (= "foo" (entry-function "_foo_")))
         (is (= "inlineitalicsgetstripped"
                (entry-function "inline_italics_getstripped"))))

(deftest strong
         (is (= "foo" (entry-function "**foo**"))))

(deftest bold-italics
         (is (= "foo" (entry-function "***foo***"))))

(deftest bold
         (is (= "foo" (entry-function "__foo__"))))

(deftest strong-inside-em
         (is (= "foobarbaz" (entry-function "*foo**bar**baz*"))))


(deftest bold-inside-a-list
         (is (= "1. chickens. \n\n See more: Cluck Cluck \n\n"
                (entry-function "1. chickens. \n\n **See more: [Cluck Cluck](http://cluck.cluck.com)** \n\n"))))

(deftest em-inside-strong
         (is (= "foobarbaz" (entry-function "**foo*bar*baz**"))))

(deftest paragraph
         (is (= "\nLorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore"
                (entry-function "\nLorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore"))))

(deftest paragraph-multiline
         (is (= "\nLorem ipsum dolor\nsit amet, consectetur adipisicing elit,\nsed do eiusmod tempor incididunt ut labore"
                (entry-function "\nLorem ipsum dolor\nsit amet, consectetur adipisicing elit,\nsed do eiusmod tempor incididunt ut labore"))))

(deftest paragraph-before-codeblock
         (is (= "foo\n\nbar\n\nbaz"
                (entry-function "foo\n```\nbar\n```\nbaz")))
         (is (= "\nfoo\n\nbar\n" (entry-function "```\nfoo  \nbar\n```")))
         (is (= "\n" (entry-function "```\n```")))
         (is (= "\n" (entry-function "```go\n```")))
         (is (= "\n<html>\n</html>\n" (entry-function "```\n<html>\n</html>\n```  "))))

(deftest paragraph-after-codeblock
         (is (= "\nfoo\n\nbar\nbaz"
                (entry-function "```\nfoo\n```\nbar\nbaz"))))

(deftest mulitple-paragraphs
         (is (= "\nfoo bar baz\n\n\nfoo bar baz"
                (entry-function "\nfoo bar baz\n\n\nfoo bar baz"))))

(deftest ul
         (is (= "* foo\n* bar\n* baz"
                (entry-function "* foo\n* bar\n* baz")))
         (is (= "- foo\n- bar\n- baz"
                (entry-function "- foo\n- bar\n- baz")))
         (is (= "+ foo\n+ bar\n+ baz"
                (entry-function "+ foo\n+ bar\n+ baz"))))

(deftest ul-followed-by-multiline-paragraph
         (is (= "* foo\n* bar\n* baz\n\nparagraph\nnext line"
                (entry-function "* foo\n* bar\n* baz\n\nparagraph\nnext line"))))

(deftest ul-with-codeblock
         (is (= "\n* foo\n* bar\n\n  (defn foo []\n  bar)\n\n*  baz\n*  more text"
                (entry-function
                  "\n* foo\n* bar\n  ```\n  (defn foo []\n  bar)\n  ```\n*  baz\n*  more text\n")))
         (is (= "\n* foo\n* bar\n\n  (defn foo []\n  bar)\n\n  text\n*  baz\n*  more text"
                (entry-function
                  "\n* foo\n* bar\n  ```\n  (defn foo []\n  bar)\n  ```\n  text\n*  baz\n*  more text\n"))))

(deftest ul-followed-by-paragraph
         (is (= "* foo\n* bar\n* baz\n\nparagraph"
                (entry-function "* foo\n* bar\n* baz\n\nparagraph"))))

(deftest ul-nested
         (is (= "* first item\n * first sub-item\n  * second sub-item\n * third sub-item\n* second item\n * first sub-item\n * second sub-item\n* third item"
                (entry-function "* first item\n * first sub-item\n  * second sub-item\n * third sub-item\n* second item\n * first sub-item\n * second sub-item\n* third item")))
         (is (= "* first item\n - first sub-item\n  - second sub-item\n - third sub-item\n* second item\n + first sub-item\n + second sub-item\n* third item"
                (entry-function "* first item\n - first sub-item\n  - second sub-item\n - third sub-item\n* second item\n + first sub-item\n + second sub-item\n* third item")))
         (is (= " * abc\n\n+ def" (entry-function " * abc\n\n+ def"))))

(deftest ol
         (is (= "1. Foo\n2. Bar\n3. Baz"
                (entry-function "1. Foo\n2. Bar\n3. Baz"))))

(deftest ul-in-ol
         (is (= "1. Bar\n 2. Subbar\n  * foo\n  * bar\n  * baz\n3. Baz"
                (entry-function "1. Bar\n 2. Subbar\n  * foo\n  * bar\n  * baz\n3. Baz"))))

(deftest ol-in-ul
         (is (= "* Foo\n 1. Bar\n  1. Subbar\n* Baz"
                (entry-function "* Foo\n 1. Bar\n  1. Subbar\n* Baz")))
         (is (= "* Foo\n 1. Bar"
                (entry-function "* Foo\n 1. Bar"))))

(deftest multilist
         (is (=
               "* foo
        * bar

           * baz
             1. foo
             2. bar

           * fuzz

              * blah
              * blue
        * brass"
               (entry-function
                 "* foo
        * bar

           * baz
             1. foo
             2. bar

           * fuzz

              * blah
              * blue
        * brass"))))

(deftest code
         (is (= "foo bar baz x = y + z; foo"
                (entry-function "foo bar baz `x = y + z;` foo")))
         (is (= "bar foo --- -- bar foo"
                (entry-function "bar `foo --- -- bar` foo")))
         (is (= "<?xml version='1.0' encoding='UTF-8'?><channel></channel>"
                (entry-function "`<?xml version='1.0' encoding='UTF-8'?><channel></channel>`")))
         (is (= "foo bar baz (fn [x & xs] (str \"x:\" x)) foo"
                (entry-function "foo bar baz `(fn [x & xs] (str \"x:\" x))` foo")))
         (is (= "\n    foo\n"
                (entry-function "    ```\n    foo\n    ```"))))

(deftest multiline-code
         (is (= "    x = 5\n    y = 6\n    z = x + y"
                (entry-function "    x = 5\n    y = 6\n    z = x + y")))
         (is (= "    x = 5\n    y = 6\n    z = x + y\n    (fn [x & xs] (str \"x\"))"
                (entry-function "    x = 5\n    y = 6\n    z = x + y\n    (fn [x & xs] (str \"x\"))"))))

(deftest codeblock
         (is (= "\n(defn- write [writer text]\n  (doseq [c text]\n    (.write writer (int c))))\n"
                (entry-function "```\n(defn- write [writer text]\n  (doseq [c text]\n    (.write writer (int c))))\n```")))
         (is (= "\n(fn [x & xs]\n  (str \"x\"))\n"
                (entry-function "```\n(fn [x & xs]\n  (str \"x\"))\n```")))
         (is (= "\n(fn [x & xs]\n  (str \"x\"))\n"
                (entry-function "```\n(fn [x & xs]\n  (str \"x\"))\n```")))
         (is (= "\n(fn [x & xs]\n  (str \"x\"))\n"
                (entry-function "```clojure\n(fn [x & xs]\n  (str \"x\"))\n```")))
         (is (= "\n\n\n\n\n\n\n\n\n\n"
                (entry-function
                  "
       ```nohighlight
       ------------
       ============
           ------------
           ============
       ```"))))

(deftest strikethrough
         (is (= "foo" (entry-function "~~foo~~"))))

(deftest superscriptL
         (is (= "foo bar baz" (entry-function "foo^bar baz"))))

(deftest md-link
         (is (= "github"
                (entry-function "[github](http://github.com)")))
         (is (= "github"
                (entry-function "[github](http://github.com/~)")))
         (is (= "github"
                (entry-function "[github](http://github.com/^)")))
         (is (= "github"
                (entry-function "[github](http://github.com/*)")))
         (is (= "* github"
                (entry-function "* [github](http://github.com/*asdf)")))
         (is (= "* hi\n\na link"
                (entry-function "* hi\n\n[a link](https://see-here)")))
         (is (= ">!"
                (entry-function "[>!](https://clojure.github.io/core.async/#clojure.core.async/>!)")))
         (is (= "<!"
                (entry-function "[<!](https://clojure.github.io/core.async/#clojure.core.async/<!)")))
         (is (= "one link two links"
                (entry-function "[one link](http://example.com) [two links](http://example.com)")))
         (is (= "some text with a link in the middle of it"
                (entry-function "some text with a [link in the middle](http://example.com) of it")))
         (is (= "just [some] brackets"
                (entry-function "just [some] brackets")))
         (is (= "no url given"
                (entry-function "no [url]() given")))
         (is (= "un closed [link](text"
                (entry-function "un closed [link](text"))))

(deftest html-link
         (is (= "link in the middle"
                (entry-function "link <a href='http://example.com'>in the</a> middle")))
         (is (= "link at the end"
                (entry-function "link at the <a href='http://example.com'>end</a>")))
         (is (= "link at the beginning"
                (entry-function "<a href='http://example.com'>link</a> at the beginning"))))

(deftest styled-link
         (is (= "<p><a href='http://github.com'><em>github</em></a></p>"
                (entry-function "[*github*](http://github.com)")))
         (is (= "<p><a href='http://github.com'><i>github</i></a></p>"
                (entry-function "[_github_](http://github.com)")))
         (is (= "<p><a href='http://github.com'><b>github</b></a></p>"
                (entry-function "[__github__](http://github.com)")))
         (is (= "<p><a href='http://github.com'><strong>github</strong></a></p>"
                (entry-function "[**github**](http://github.com)")))
         (is (= "<p><a href='http://github.com'><del>github</del></a></p>"
                (entry-function "[~~github~~](http://github.com)"))))

(deftest img
         (is (= "<p><img src=\"/path/to/img.jpg\" alt=\"Alt text\" /></p>"
                (entry-function "![Alt text](/path/to/img.jpg)")))
         (is (= "<p><img src=\"/path/to/_img_.jpg\" alt=\"Alt text\" title=\"Optional Title\" /></p>"
                (entry-function "![Alt text](/path/to/_img_.jpg \"Optional Title\")"))))

(deftest img-link
         (is (= "<p><a href='http://travis-ci.org/yogthos/markdown-clj'><img src=\"https://secure.travis-ci.org/yogthos/markdown-clj.png\" alt=\"Continuous Integration status\" /></a></p>"
                (entry-function "[![Continuous Integration status](https://secure.travis-ci.org/yogthos/markdown-clj.png)](http://travis-ci.org/yogthos/markdown-clj)")))
         (is (= "<p><img src=\"https://secure.travis-ci.org/yogthos/markdown-clj.png\" alt=\"\" /></p>"
                (entry-function "![](https://secure.travis-ci.org/yogthos/markdown-clj.png)"))))

(deftest bad-link
         (is (= "<p>[github](http://github.comfooo</p>"
                (entry-function "[github](http://github.comfooo")))
         (is (= "<p>[github] no way (http://github.com)</p>"
                (entry-function "[github] no way (http://github.com)"))))

(deftest bad-link-title
         (is (= "<p>[github(http://github.comfooo)</p>"
                (entry-function "[github(http://github.comfooo)"))))

(deftest blockquote
         (is (= "<blockquote><p>Foo bar baz </p></blockquote>"
                (entry-function ">Foo bar baz"))))

(deftest blockquote-footer
         (is (= "<blockquote><p> Foo bar baz </p><footer> Leo Tolstoy</footer></blockquote>"
                (entry-function "> Foo bar baz\n>- Leo Tolstoy"))))

(deftest blockquote-empty-footer
         (is (= "<blockquote><p> Foo bar baz </p><footer></footer></blockquote>"
                (entry-function "> Foo bar baz\n>-"))))

(deftest blockquote-multiline-without-leading-angle-bracket
         (is (= "<blockquote><p> Foo bar baz </p></blockquote>"
                (entry-function "> Foo bar\nbaz"))))

(deftest blockquote-multiple-paragraphs
         (is (= "<blockquote><p> Foo bar </p><p> baz </p></blockquote>"
                (entry-function "> Foo bar\n>\n> baz"))))

(deftest blockquote-bullets
         (is (= "<blockquote><p> list: <ul><li>foo</li><li>bar</li></ul></p></blockquote><p>end.</p>"
                (entry-function "> list:\n>* foo\n>* bar\n\nend.")))
         (is (= "<blockquote><p><ul><li>foo</li><li>bar</li><li>baz</li></ul></p></blockquote>"
                (entry-function ">* foo\n>* bar\n>* baz"))))

(deftest blockquote-headings
         (is (= "<blockquote><p><h2>Foo</h2>bar baz </p></blockquote>"
                (entry-function "> ## Foo\n>bar baz")))
         (is (= "<blockquote><p> Foo <h2>bar</h2> baz </p></blockquote>"
                (entry-function "> Foo\n>## bar\n> baz"))))

(deftest escaped-characters
         (is
           (= "<p>&#42;&#8216;&#95;&#123;&#125;&#91;&#93;<em>foo</em><code>test</code><i>bar</i>{x}[y]</p>"
              (entry-function "\\*\\`\\_\\{\\}\\[\\]*foo*`test`_bar_{x}[y]"))))

(deftest paragraph-after-list
         (is (= "<ol><li>a</li><li>b</li></ol><p>test <strong>bold</strong> and <em>italic</em></p>"
                (entry-function "1. a\n2. b\n\ntest **bold** and *italic*"))))

(deftest paragraph-close-before-list
         (is (= "<p>in paragraph</p><ul><li>list</li></ul>"
                (entry-function "in paragraph\n- list"))))

;(deftest autourl
;  (is (= "<p><a href=\"http://example.com/\">http://example.com/</a></p>"
;         (entry-function "<http://example.com/>")))
;
;  (is (= "<p>Some content with a <a href=\"http://www.google.com/abc__123__efg\">http://www.google.com/abc__123__efg</a> link it in</p>"
;         (entry-function "Some content with a <http://www.google.com/abc__123__efg> link it in")))
;
;  (is (= "<p><a href=\"http://foo\">http://foo</a> <a href=\"https://bar/baz\">https://bar/baz</a> <a href=\"http://foo/bar\">foo bar</a></p>"
;         (entry-function "<http://foo> <https://bar/baz> <a href=\"http://foo/bar\">foo bar</a>")))
;
;  (is (= "<p><a href=\"mailto:abc@google.com\">abc@google.com</a></p>"
;         (#?(:clj  org.apache.commons.lang.StringEscapeUtils/unescapeHtml
;             :cljs goog.string/unescapeEntities)
;           (entry-function "<abc@google.com>"))))
;
;  (is (= "<p><a href=\"mailto:abc_def_ghi@google.com\">abc_def_ghi@google.com</a></p>"
;         (#?(:clj  org.apache.commons.lang.StringEscapeUtils/unescapeHtml
;             :cljs goog.string/unescapeEntities)
;           (entry-function "<abc_def_ghi@google.com>")))))

(deftest not-a-list
         (is (= "<p>The fish was 192.8 lbs and was amazing to see.</p>"
                (entry-function "The fish was\n192.8 lbs and was amazing to see."))))

(deftest dont-encode-chars-in-hrefs
         (is (= "<p><a href='http://www.google.com/example_link_foo~_^*'>example_link with tilde ~ and carat ^ and splat *</a></p>"
                (entry-function "[example_link with tilde ~ and carat ^ and splat *](http://www.google.com/example_link_foo~_^*)"))))

(deftest complex-link-with-terminal-encoding-inside-header
         (is (= "<h2>With a link <a href='http://a.com/under_score_in_the_link/'>the contents of the_link</a></h2>"
                (entry-function "##With a link [the contents of the_link](http://a.com/under_score_in_the_link/)"))))

(deftest two-links-tests-link-processing
         (is (= "<h2>When you have a pair of links <a href='http://123.com/1'>link1</a> and you want both <a href='That%27s%20crazy'>Wow</a></h2>"
                (entry-function "## When you have a pair of links [link1](http://123.com/1) and you want both [Wow](That%27s%20crazy)"))))

(deftest link-then-image-processing
         (is (= "<p>You can have a <a href='github.com'>link</a> followed by an image <img src=\"img.png\" alt=\"\" /></p>"
                (entry-function "You can have a [link](github.com) followed by an image ![](img.png)"))))

(deftest image-then-link-processing
         (is (= "<p>You can have an image <img src=\"img.png\" alt=\"\" /> followed by a <a href='github.com'>link</a></p>"
                (entry-function "You can have an image ![](img.png) followed by a [link](github.com)"))))

(deftest link-with-optional-title
         (is (= "<p><a href='https://github.com/cryogen-project/cryogen' title=\"Cryogen Github\">Cryogens site</a></p>"
                (entry-function "[Cryogens site](https://github.com/cryogen-project/cryogen \"Cryogen Github\")"))))

(deftest parse-table-row
         (is (= (tables/parse-table-row "| table cell contents |") [{:text "table cell contents"}]))
         (is (= (tables/parse-table-row "| contents 1 | contents 2 | contents 3 | contents 4 |")
                [{:text "contents 1"} {:text "contents 2"} {:text "contents 3"} {:text "contents 4"}])))

(deftest table-row->str
         (is (= (tables/table-row->str
                  [{:text "contents 1"} {:text "contents 2"} {:text "contents 3"} {:text "contents 4"}]
                  true)
                "<th>contents 1</th><th>contents 2</th><th>contents 3</th><th>contents 4</th>"))
         (is (= (tables/table-row->str
                  [{:text "contents 1"} {:text "contents 2"} {:text "contents 3"} {:text "contents 4"}]
                  false)
                "<td>contents 1</td><td>contents 2</td><td>contents 3</td><td>contents 4</td>"))
         (is (= (tables/table-row->str
                  [{:text "contents 1" :alignment :left}
                   {:text "contents 2" :alignment :center}
                   {:text "contents 3" :alignment :right}
                   {:text "contents 4"}]
                  false)
                "<td style='text-align:left'>contents 1</td><td style='text-align:center'>contents 2</td><td style='text-align:right'>contents 3</td><td>contents 4</td>")))

(deftest table->str
         (is (= (tables/table->str
                  {:alignment-seq
                   [{:alignment :left} {:alignment :center} {:alignment :right} {:alignment nil}]
                   :data [[{:text "Header 1"}
                           {:text "Header 2"}
                           {:text "Header 3"}
                           {:text "Header 4"}]
                          [{:text "contents 1"}
                           {:text "contents 2"}
                           {:text "contents 3"}
                           {:text "contents 4"}]]})
                "<table><thead><tr><th style='text-align:left'>Header 1</th><th style='text-align:center'>Header 2</th><th style='text-align:right'>Header 3</th><th>Header 4</th></tr></thead><tbody><tr><td style='text-align:left'>contents 1</td><td style='text-align:center'>contents 2</td><td style='text-align:right'>contents 3</td><td>contents 4</td></tr></tbody></table>")))

(deftest divider-seq->alignment
         (is (= (tables/divider-seq->alignment
                  [{:text "-----"} {:text ":-----"} {:text "-----:"} {:text ":-----:"}])
                [nil {:alignment :left} {:alignment :right} {:alignment :center}])))

(deftest n-dash
         (is (= "<p>boo &ndash; bar</p>" (entry-function "boo -- bar"))))

(deftest m-dash
         (is (= "<p>boo &mdash; bar</p>" (entry-function "boo --- bar"))))

(deftest inhibit-simple
         (is (= "<p>_abc_</p>" (entry-function "$_abc_$" :inhibit-separator "$"))))

(deftest inhibit-simple-seq
         (is (= "<p>_abc_</p>" (entry-function "$_abc_$" :inhibit-separator [\$]))))

(deftest inhibit-inline-code
         (is (= "<p>`abc`</p>" (entry-function "$`abc`$" :inhibit-separator [\$]))))

(deftest inhibit-inside-code
         (is (= "<p><code>a*b* & dc</code></p>" (entry-function "`a$*b* & d$c`" :inhibit-separator "$"))))

(deftest inhibit-across-backticks
         (is (= "<p><code>one` `two</code></p>" (entry-function "`one$` `$two`" :inhibit-separator "$"))))

(deftest inhibit-escape
         (is (= "<p>$</p>" (entry-function "$$" :inhibit-separator [\$]))))

(deftest inhibit-escape-twice
         (is (= "<p>$$</p>" (entry-function "$$$$" :inhibit-separator "$"))))

(deftest img-reprocess
         (is (= "<p><img src=\"img.jpg\" alt=\"Text\" /> and <a href='#'>Edit</a></p>"
                (entry-function "![Text](img.jpg) and [Edit](#)"))))

(deftest dont-inhibit-text-within-escapes
         (is (= "<p>$<em>abc</em>$</p>" (entry-function "$$*abc*$$" :inhibit-separator "$"))))

(deftest inhibit-escape-inside-code
         (is (= "<p><code>$</code></p>" (entry-function "`$$`" :inhibit-separator "$"))))

(deftest whitespace-paragraphs
         (is (= "<p>foo  </p><p>bar</p>" (entry-function "foo\n \nbar"))))
