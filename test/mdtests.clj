(ns mdtests  
  (:require markdown)
  (:use clojure.test))

(deftest heading1 
  (is (= "<h1>foo</h1>" (markdown/md-to-html-string "#foo"))))

(deftest heading2 
  (is (= "<h2>foo</h2>" (markdown/md-to-html-string "##foo"))))

(deftest hr
  (is (= "<hr/>" (markdown/md-to-html-string "***"))))

(deftest italics
  (is (= "<i>foo</i>" (markdown/md-to-html-string "*foo*"))))

(deftest bold
  (is (= "<b>foo</b>" (markdown/md-to-html-string "**foo**"))))

(deftest bold-inside-italics
  (is (= "<i>foo<b>bar</b>baz</i>" (markdown/md-to-html-string "*foo**bar**baz*"))))

(deftest italics-inside-bold
  (is (= "<b>foo<i>bar</i>baz</b>" (markdown/md-to-html-string "**foo*bar*baz**"))))

(deftest paragraph
  (is (= "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore</p>"
         (markdown/md-to-html-string "\nLorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore"))))

(deftest paragraph-multiline
  (is (= "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore</p>"
         (markdown/md-to-html-string "\nLorem ipsum dolor sit amet, consectetur adipisicing elit, \nsed do eiusmod tempor incididunt ut labore"))))

(deftest mulitple-paragraphs
  (is (= "<p>foo bar baz</p><p>foo bar baz</p>" 
      (markdown/md-to-html-string "\nfoo bar baz\n\n\nfoo bar baz"))))

(deftest ul
  (is (= "<ul><li>foo</li><li>bar</li><li>baz</li></ul>" 
         (markdown/md-to-html-string "* foo\n* bar\n* baz"))))

(deftest ul-nested
  (is (= "<ul><li>foo<ul><li>bar</li><li>baz</li><li>baz</li></ul></li><li>BAZ</li></ul>" 
         (markdown/md-to-html-string "* foo\n*  bar\n*  baz\n*  baz\n* BAZ"))))

(deftest code
  (is (= "foo bar baz <pre><code>x = y + z;</code></pre> foo"
         (markdown/md-to-html-string "foo bar baz `x = y + z;` foo"))))

(deftest multiline-code
  (is (= "<pre><code>    x = 5\n    y = 6\n    z = x + y</code></pre>"
         (markdown/md-to-html-string "    x = 5\n    y = 6\n    z = x + y")))) 

(deftest stirkethrough
  (is (= "<del>foo</del>"
         (markdown/md-to-html-string "~~foo~~"))))

(deftest stirkethrough
  (is (= "foo<sup>bar</sup> baz"
         (markdown/md-to-html-string "foo^bar baz"))))

(deftest link
  (is (= "<a href='http://github.com'>github</a>"
         (markdown/md-to-html-string "[github](http://github.com)"))))

(deftest bad-link
  (is (= "[github](http://github.comfooo" 
         (markdown/md-to-html-string "[github](http://github.comfooo"))))

(deftest bad-link-title
  (is (= "[github(http://github.comfooo)" 
         (markdown/md-to-html-string "[github(http://github.comfooo)"))))

(run-tests)


