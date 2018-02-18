[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs)
===

<blockquote cite>
The language in which we express our ideas has a strong influence on our thought processes. ~ Knuth
</blockquote>

A literate-programming friendly; tight work-flow stack template.

other/readme-lhs.hsfiles
---

```
stack new project-name readme-lhs
cd project-name
stack build
$(stack path --local-install-root)/bin/readme-lhs-example
$(stack path --local-bin)/pandoc -f markdown -i other/header.md readme.md example/example.lhs other/footer.md -t html -o index.html --filter pandoc-include --mathjax
```

Which:

- runs an executable, which places some output in a markdown file
- gathers together the readme.md and the example.lhs into an index.html

alternative templates
===

Or grab some of the other templates from this repo:

other/readme-hs.hsfiles
---

like the lhs version, but hs based

other/batteries.hsfiles
---

This is my latest working template, overly influenced by [lexi-lambda's opinionated guide](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/). The template includes:

- some minor tweaks to protolude
- lens, foldl, formatting & text as must have libraries
- generic-lens-labels


