```include
other/header.md
```

[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs)
===

** starting a literate haskell narrative.**

I've long suffered from procrastination in new projects. The coders equivalent for sharpening pencils is the creation of bespoke, boiler-plate scaffolding as one prevaricates about the guts of the project. I see evidence littered through my old repos - self-obsfucating technical debt encrusted on the interface between the project and RealWorld.

So, starting at the pointy end of a new project, I aligned my workflow with stack, and started looking at a reproducible, exact startup routine.  I followed the path to stack templates and wrote my own (`stack new project-name readme-lhs`), which was kindy accepted.

design
---

Stripping away optional extras, I went with a minimalist design:

- markdown for all docs.  Anything outside what markdown can do is me being too fancy.
- pandoc for document conversion. `-f markdown+lhs` is a georgeous rendering of a .lhs file. I turn [github pages](https://help.github.com/articles/user-organization-and-project-pages/) on using the User Pages site method for each project I'd like to blog, and render readme.lhs to index.html using pandoc.
- css for styling
- using lhs as a valid haskell artform
- readme.lhs as a general purpose executable, tester, example holder and all-round centralising communication document.
- a long list of the most commonly-used, and mostly-benign language flags, NoImplicitPrelude & UnicodeSyntax.  I call this -XHaskell2016 in private.
- the stack-recommended .travis.yml
- Embedding [Protolude](https://www.stackage.org/package/protolude) as the replacement prelude.

Here's what I threw out:

- haddock. The design space of using markdown as a complete replacement is fresh territory.
- test directory. I looked through my test and example directories and noted the mess in many. I love to test - creating a realistic arbitrary instance is the best way to get to know your data structures - but tests and multiple examples didn't belong in the start-up phase.
- hakyll. For me, markdown created for a separate blogging process is better inside the actual  project as readme's - my hakyll site rots pretty quickly.
- app directory.  The boundary between what the app is and what the library is is very fluid for me, and a source of technical debt as concepts bounce between directories.

I then keep this repo around for phase 2, where a project takes a successful shape and and needs test and app boiler-plate.

Workflow
---

This design must be monadic cause I seem to get a hyper-efficient workflow for free! The template reproducably installs and runs out of the box, with zero TODO items littered through the structure.

~~~
stack install && readme

...

Copied executables to /Users/tonyday/.local/bin:
- readme
"ok"
~~~

I gather libraries I guess I'm needing, adding to .cabal and imports added to readme.lhs.  Firing up a repl there, I start hacking some code together, saving the good bits to readme.lhs, adding notes and links, and getting some initial learnings coded up in main, so I can eyeball results. I use the following command to build, run, and render an index.html to publish on github.

~~~
stack install && readme && pandoc -f markdown+lhs -i readme.lhs -t html -o index.html --filter pandoc-include --mathjax
~~~

todo:

- [ ] can stack --file-watch help?
- [ ] should pandoc be wrapped in the stack version ie `stack exec pandoc --`
- [ ] reproducible build instructions with stack-only environment (travis)

usage
---

To just use the stack template:

~~~
stack new project-name readme-lhs
cd project-name
stack install
readme
~~~

and it should chirp back a cheery "ok".

The [template file](other/readme-lhs.hsfiles) can always be edited, renamed etc and dropped into a directory, and stack will find it.


lhs
---
I use both .lhs and .hs styles, and often need to flip between the two. In particular, hlint didn't always play nice with .lhs, and for a while I needed to flip from .lhs to .hs, run hlint, and then flip back to .lhs.

lhs makes no attempt to account for haddock and will thus not be suitable for most people.

To install and use lhs:

~~~
git clone https://github.com/tonyday567/readme-lhs
cd readme-lhs
stack install
lhs readme.lhs
~~~

code
---

I tend to stick [protolude](http://www.stephendiehl.com/posts/protolude.html) in most modules, which really cuts down on boiler-plate.

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> import Protolude
> 
> main :: IO ()
> main = do
>   print "readme-lhs library"
>   let fac n = foldr (\x g n' -> g (x*n')) identity [1..n] 1
>   writeFile "other/example.md" $
>       "$\\prod_{x=1}^{20} x = " <> show (fac 20) <> "$\n"
>

Output from the code above appears in this readme.lhs when rendered with pandoc-include (except if you're reading this as the repo readme.md, sorry):

```include
other/example.md
```


