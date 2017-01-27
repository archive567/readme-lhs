```include
other/header.md
```

[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs)
===

 *a literate haskell workflow.*

 jk fs
 
~~~
stack build --copy-bins --exec "readme" --exec "pandoc -f markdown+lhs -i readme.lhs -t html -o index.html --filter pandoc-include --mathjax" --exec "pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md --filter pandoc-include --mathjax" --exec "echo Yah, it succeeded!" --file-watch
~~~

simple
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


design
---

Stripping away optional extras, readme-lhs tries for a minimalist design:

- markdown for all docs.  Anything outside what markdown can do is too fancy.
- pandoc for document conversion. `-f markdown+lhs` is a georgeous rendering of a .lhs file. I turn [github pages](https://help.github.com/articles/user-organization-and-project-pages/) on using the User Pages site method for each project I'd like to blog, and render readme.lhs to index.html using pandoc.
- css for styling, but also dealing with github markdown style which is where many users will read your lhs.
- using lhs as a valid haskell artform
- readme.lhs as a general purpose executable, tester, example holder and all-round centralising communication document.
- a long list of the most commonly-used, and mostly-benign language flags, NoImplicitPrelude & UnicodeSyntax.  I call this -XHaskell2016 in private.
- the stack-recommended .travis.yml (the complictaed one, so I can keep track of busting old ghc and lts versions)
- Embedding [Protolude](https://www.stackage.org/package/protolude) as the replacement prelude.

Here's what I threw out:

- haddock. The design space of using markdown as a complete replacement is fresh territory.
- test directory. I looked through my test and example directories and noted the mess in many. I love to test - creating a realistic arbitrary instance is the best way to get to know your data structures - but tests and multiple examples didn't belong in the start-up phase.
- hakyll. For me, markdown created for a separate blogging process is better inside the actual  project as readme's - my hakyll site rots pretty quickly.
- app directory.  The boundary between what the app is and what the library is is very fluid for me, and a source of technical debt as concepts bounce between directories.

I then keep this repo around for phase 2, where a project takes a successful shape and and needs test and app boiler-plate.


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

[hoogle](https://www.stackage.org/package/hoogle)

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


