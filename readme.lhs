<link rel="stylesheet" href="lhs.css">

readme-lhs
===

[![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs)

This is an environment to test a new template that I want to pull request into stack - the candidate sits [here](./readme-lhs.hsfiles) and stack recognizes template files locally at the top level of the project.

This repo carries some extra goodies, such as css and markdown helpers.

dev version
---

~~~
git clone readme-

~~~

Once I've PRed to stack ...

~~~
stack new projectname readme-lhs
cd projectname
<<edit readme.lhs>>
stack build
~~~

protolude
---

> {-# LANGUAGE NoImplicitPrelude #-}
> import Protolude

> main :: IO ()
> main = print "readme.lhs template"
>


Like many, I've done my time in cabal hell - discovered that there was a gap in version coverage in one of the 127 dependencies throwing the resolver into delusions of downgrading lens to 0.0.1.  And doing this under the hammer of a closing sprint when you've spent months talking up haskell and promising it's ready for production... (cabal hell flashback).

The point being I am motivated to zeroise the technical debt that incurres with a sloppy startup phase.

Within this context, I had been thinking a lot about my workflow - from idea inspiration to social output.  A minor project starts, often refactored out from a major project, but sometimes as a blank page. And the very next step had become a `stack new`, followed by a gathering of code snippets, library lists, links, renaming of files, repository creation, readme, and then testing, testing, testing.

The concept of a project is often represented as a series of imperative steps with workflow arrows dividing tasks on a big picture to an in-production UI. The reality however is that priorities are orthoganal to this representation.  The idea may not be possible without proving a law, or testing that a technology is stable, so the priority may be immediate low-level ghci work. The idea might be crappy, and a smart first move is to twitter/blog/post/chat about it.

So I started to refactor my process, read up on templates, and have worked towards `readme-lhs.hsfiles` that I think represents a flexible sweet spot early note-taking, coding, experimentation and sharing projects.

The workflow that emerges from usage of the template has low margin cost to prioritise any point in the project space.  readme.lhs renders well in github, and is radiant as the readme.md that results from `pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md`. More people will read your readme.md's than any blog you write (if you're not one of the superstars).

readme.lhs is built as an executable, can be insta loaded into ghci, and comes armed with the [protolude](https://github.com/sdiehl/protolude). Never again will I use Strings, commit code with undefined, or have to check which `foldMap` is in scope.


~~~
filewatcher '**/*.{lhs,hs,cabal}' 'stack build && echo "built" && ./.stack-work/install/x86_64-osx/nightly-2016-04-29/7.10.3/bin/readme | pandoc -f markdown+lhs -t html -o readme-out.html && pandoc -f markdown+lhs -i readme.lhs -t html -o readme.html && echo "run"'
~~~

~~~
pandoc -f markdown+lhs -t markdown -i readme.lhs -o readme.md
~~~

usage
---

~~~
stack new projectname readme-lhs
cd projectname
<<edit readme.lhs>>
stack build
~~~

`stack solve` and `stack build` if anything goes wrong.

The resultant workflow tends to be conducive to very tight coding loops.

To build & run:

~~~
stack build && echo "built" && ./.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin/readme
~~~

Build, run, render output, and render readme.lhs on file changes. 
