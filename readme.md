``` {.include}
other/header.md
```

readme-lhs [![repo](https://a248.e.akamai.net/assets.github.com/images/icons/emoji/octocat.png)](https://github.com/tonyday567/readme-lhs) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs)
==========================================================================================================================================================================================================================================================

I've long suffered from procrastination in new projects. The coders
equivalent for sharpening pencils is the creation of bespoke,
boiler-plate scaffolding as one prevaricates about the guts of the
project. I see evidence littered through my old repos - self-obsfucating
technical debt encrusted on the interface between the project and
RealWorld.

So, starting at the pointy end of a new project, I aligned my workflow
with stack, and started looking at a reproducible, exact startup
routine. I got to stack templates and wrote my own, which was kindy
accepted by the crew.

Stripping away optional extras, I went with a minimalist design:

-   markdown for all docs. Anything outside what markdown can do is me
    being too fancy.
-   pandoc for doc conversion
-   css for styling
-   lhs as a valid haskell artform
-   readme.lhs as a general purpose executable, tester, example holder
    and all-round centralising communication document.

A partial list of what I let go:

-   haddock. I will never ever learn haddock -- \* -- | {-! is cruel and
    unusual \*-}
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

Workflow
========

This design must be monadic cause I seem to get a hyper-efficient
workflow for free! As an example, I use the following command to build,
run, and render readme.lhs as html.

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o readme.html --filter pandoc-include

todo:

-   \[ \] add `stack watch`
-   \[ \] wrap pandoc to be stack version ie `stack exec pandoc --`
-   \[ \] reproducible build instructions with stack-only environment

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

To install and use lhs:

    git clone https://github.com/tonyday567/readme-lhs
    cd readme-lhs
    stack install
    lhs readme.lhs

code
----

[protolude](http://www.stephendiehl.com/posts/protolude.html)

``` {.sourceCode .literate .haskell}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Protolude
import Control.Category (id)
import Data.Text.IO (writeFile)
main :: IO ()
main = do
  print "readme-lhs library"
  let fac n = foldr (\x g n' -> g (x*n')) id [1..n] 1
  writeFile "other/example.md" $ show (fac 10)
```

Output from the code above appears in this readme.lhs when rendered with
pandoc-include.

$\prod_{x=1}^{10} x$ is equal to:

``` {.include}
other/example.md
```
