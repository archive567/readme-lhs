<meta charset="utf-8"> <link rel="stylesheet" href="other/lhs.css">
<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
readme-lhs [![repo](https://a248.e.akamai.net/assets.github.com/images/icons/emoji/octocat.png)](https://github.com/tonyday567/readme-lhs) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs) [html](https://tonyday567.github.io/readme-lhs.html)
===============================================================================================================================================================================================================================================================================================================

I've long suffered from procrastination in new projects. The coders
equivalent for sharpening pencils is the creation of bespoke,
boiler-plate scaffolding as one prevaricates about the guts of the
project. I see evidence littered through my old repos - self-obsfucating
technical debt encrusted on the interface between the project and
RealWorld.

So, starting at the pointy end of a new project, I aligned my workflow
with stack, and started looking at a reproducible, exact startup
routine. I followed the path to stack templates and wrote my own, which
was kindy accepted by the crew.

design
------

Stripping away optional extras, I went with a minimalist design:

-   markdown for all docs. Anything outside what markdown can do is me
    being too fancy.
-   pandoc for doc conversion
-   css for styling
-   lhs as a valid haskell artform
-   readme.lhs as a general purpose executable, tester, example holder
    and all-round centralising communication document.
-   most commonly-used, and mostly-benign language flags,
    NoImplicitPrelude & UnicodeSyntax

    todo: make use of UnicodeSyntax

A partial list of what I let go of in the initial project build:

-   haddock. haddock and markdown don't mix well.
-   test directory. I looked through my test and example directories and
    noted the mess in many. I love to test - creating a realistic
    arbitrary instance is the best way to get to know your data
    structures - but tests and multiple examples didn't belong in the
    start-up phase.
-   hakyll. Markdown created for a separate blogging process is better
    inside the actual project as readme's.
-   app directory. The boundary between what the app is and what the
    library is is very fluid for me, and a source of technical debt as
    concepts bounce between directories.

I then keep this repo around for the odd occasions when I'm looking for
test and app boiler-plate.

Workflow
--------

This design must be monadic cause I seem to get a hyper-efficient
workflow for free! The template reproducably installs and runs out of
the box, with zero TODO items littered through the structure.

    stack install && readme

    ...

    Copied executables to /Users/tonyday/.local/bin:
    - readme
    "ok"

I gather libraries I guess I'm needing, adding to .cabal and imports
added to readme.lhs. Firing up a repl there, I start hacking some code
together, saving the good bits to readme.lhs, adding notes and links,
and getting some initial learnings coded up in main, so I can eyeball
results. I use the following command to build, run, and render an
index.html to publish on github.

    stack install && readme && pandoc -f markdown+lhs -i readme.lhs -t html -o index.html --filter pandoc-include --mathjax

todo:

-   \[ \] can stack --file-watch help?
-   \[ \] should pandoc be wrapped in the stack version ie
    `stack exec pandoc --`
-   \[ \] reproducible build instructions with stack-only
    environment (travis)

usage
-----

To just use the stack template:

    stack new project-name readme-lhs
    cd project-name
    stack install
    readme

and it should chirp back a cheery "ok".

The [template file](other/readme-lhs.hsfiles) can always be edited,
renamed etc and dropped into a directory, and stack will find it.

lhs
---

I use both .lhs and .hs styles, and often need to flip between the two.
In particular, hlint didn't always play nice with .lhs, and for a while
I needed to flip from .lhs to .hs, run hlint, and then flip back to
.lhs.

To install and use lhs:

    git clone https://github.com/tonyday567/readme-lhs
    cd readme-lhs
    stack install
    lhs readme.lhs

code
----

I tend to stick
[protolude](http://www.stephendiehl.com/posts/protolude.html) in most
modules, which really cuts down on boiler-plate.

``` {.sourceCode .literate .haskell}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Protolude

main :: IO ()
main = do
  print "readme-lhs library"
  let fac n = foldr (\x g n' -> g (x*n')) identity [1..n] 1
  writeFile "other/example.md" $
      "$\\prod_{x=1}^{20} x = " <> show (fac 20) <> "$\n"
```

Output from the code above appears in this readme.lhs when rendered with
pandoc-include (except if you're reading this in github, sorry):

$\prod_{x=1}^{20} x = 2432902008176640000$
