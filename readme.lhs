<link rel="stylesheet" href="lhs.css">

readme-lhs
===

[![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs)

Liberate your workflow from the analytic industrial complex and try literate programming.  `readme.lhs` literately says read and run my literate haskell code. Demographic social interactivity nailed.

This is (or has been rendered from) [readme.lhs](https://github.com/tonyday567/readme-lhs/blob/master/readme.lhs).  The project is an environment to test a new template that I want to pull request into stack.

This repo comes with extra goodies;

- some css
- examples of connecting `stack new projectname readme-lhs` with a complete workflow.
- `lhs` executable that converts between hs and lhs

usage
---

~~~
stack new projectname readme-lhs
cd projectname
stack build
stack install
readme
~~~

old-school
---

~~~
git clone https://github.com/tonyday567/readme-lhs
cd readme-lhs
stack build
~~~

This gives you
- a readme executable based on readme.lhs inclined towards markdown-friendly stdout output
- a readme.lhs that will be well rendered


Workflow
---

By wiring in a single readme.lhs as the executable, there is now extra flexibility in the type of workflow adopted.  To build & run:

~~~
stack install && readme
~~~

Watch for file changes, build, run, render output, and render readme.lhs.

~~~
filewatcher '**/*.{lhs,hs,cabal}' 'stack install && echo "built" && readme && pandoc -f markdown+lhs -t markdown -i readme.lhs -o readme.md && pandoc -f markdown+lhs -t html -i readme.lhs -o ~/git/tonyday567.github.io/readme-lhs.html &&echo "run"'
~~~

Markdown
---

Markdown-awareness in stdout output brings an ability to:

1. print formulae that will be rendered downstream using (mathjax)[https://kerzol.github.io/markdown-mathjax/editor.html]
2. Include formulae in the literate bit of an lhs
3. Create embedded content such as charts

raw template
---

As well as being in stack, the template sits [here](https://github.com/tonyday567/readme-lhs/blob/master/readme-lhs.hsfiles).  You can always just drop a template in local and stack will use it.

code
---

[protolude](http://www.stephendiehl.com/posts/protolude.html)

> {-# LANGUAGE NoImplicitPrelude #-}
> import Protolude
>
> main :: IO ()
> main = do
>   print "readme-lhs template"
