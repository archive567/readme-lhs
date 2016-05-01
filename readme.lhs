<link rel="stylesheet" href="lhs.css">

readme-lhs
===

[![Build Status](https://travis-ci.org/tonyday567/readme-lhs.png)](https://travis-ci.org/tonyday567/readme-lhs)

This is (or has been rendered from) [readme.lhs](https://github.com/tonyday567/readme-lhs/blob/master/readme.lhs).  The project is an environment to test a new template that I want to pull request into stack.

This repo comes with extra goodies; some css and examples of connecting `stack new projectname readme-lhs` with a complete workflow.

usage (when the PR hits stack ...)
---

~~~
stack new projectname readme-lhs
cd projectname
<<edit readme.lhs>>
stack build
~~~

old-schoool development
---

~~~
git clone https://github.com/tonyday567/readme-lhs
cd readme-lhs
stack build
~~~

This gives you
- a readme executable based on readme.lhs inclined towards markdown-friendly stdout output
- a readme.lhs that will be well rendered

From there, there is flexibility. To build & run:

~~~
stack build && echo "built" && ./.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin/readme
~~~

Build, run, render output and readme.lhs, and send to blog.
 
~~~
filewatcher '**/*.{lhs,hs,cabal}' 'stack build && echo "built" && ./.stack-work/install/x86_64-osx/nightly-2016-04-30/7.10.3/bin/readme | pandoc -f markdown+lhs -t html -o ~/git/tonyday567.github.io/readme-lhs-out.html && pandoc -f markdown+lhs -i readme.lhs -t html -o ~/git/tonyday567.github.io/readme-lhs.html && echo "run"'
~~~
 
raw template
---

The template candidate sits [here](https://github.com/tonyday567/readme-lhs/blob/master/readme-lhs.hsfiles) and stack recognizes template files locally at the top level of the project.



code
---

> {-# LANGUAGE NoImplicitPrelude #-}
> import Protolude

> main :: IO ()
> main = do
>   print "<h1>readme.lhs output</h1>"
>   print ""
>   


Like many, I've done my time in cabal hell - discovered that there was a gap in version coverage in one of the 127 dependencies throwing the resolver into delusions of downgrading lens to 0.0.1.  And doing this under the hammer of a closing sprint when you've spent months talking up haskell and promising it's ready for production... (cabal hell flashback).

The point being I am motivated to zeroise the technical debt that incurres with a sloppy startup phase.

Within this context, I had been thinking a lot about my workflow - from idea inspiration to social output.  A minor project starts, often refactored out from a major project, but sometimes as a blank page. And the very next step had become a `stack new`, followed by a gathering of code snippets, library lists, links, renaming of files, repository creation, readme, and then testing, testing, testing.

The concept of a project is often represented as a series of imperative steps with workflow arrows dividing tasks on a big picture to an in-production UI. The reality however is that priorities are orthoganal to this representation.  The idea may not be possible without proving a law, or testing that a technology is stable, so the priority may be immediate low-level ghci work. The idea might be crappy, and a smart first move is to twitter/blog/post/chat about it.

So I started to refactor my process, read up on templates, and have worked towards `readme-lhs.hsfiles` that I think represents a flexible sweet spot early note-taking, coding, experimentation and sharing projects.

The workflow that emerges from usage of the template has low margin cost to prioritise any point in the project space.  readme.lhs renders well in github, and is radiant as the readme.md that results from `pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md`. More people will read your readme.md's than any blog you write (if you're not one of the superstars).

readme.lhs is built as an executable, can be insta loaded into ghci, and comes armed with the [protolude](https://github.com/sdiehl/protolude). Never again will I use Strings, commit code with undefined, or have to check which `foldMap` is in scope.
