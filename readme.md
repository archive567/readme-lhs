[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs)
===

See https://tonyday567.github.io/readme-lhs/index.html for project description.

compilation recipe
---

```
stack build --test --exec "$(stack path --local-install-root)/bin/readme-lhs-test-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i other/header.md example/example.lhs other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
```

The above `recipe` builds the project, runs the test, renders this file as html, and then watches for file changes.  Pandoc and pandoc-include are assumed to be installed via stack, so you might have to:

```
stack install pandoc
stack install pandoc-include
```

template
---

The bare bones of this process is available as a stack template:

```
cd project-name
stack build
$(stack path --local-install-root)/bin/readme-lhs-example
stack new project-name readme-lhs
```

Which should produce:

```
Examples: 2  Tried: 2  Errors: 0  Failures: 0

All 0 tests passed (0.00s)

3628800 👍
```
