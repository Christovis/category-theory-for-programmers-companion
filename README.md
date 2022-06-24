# A reading companion for "[Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)"


Releases
--------

This is a work in progress.

* [Part 1](https://github.com/Christovis/category-theory-for-programmers-companion/blob/main/docs/part1.pdf)
* [Part 2](https://github.com/Christovis/category-theory-for-programmers-companion/blob/main/docs/part2.md)

Building
--------

To generate the build scripts provision a cabal sandbox with pandoc in it. This
is done by the ``write-you-a-haskell.cabal`` and  ``stack.yaml`` files.

**Stack**

```bash
$ stack exec make
```

To generate the pdf, I use the [Eisvogel](https://github.com/Wandmalfarbe/pandoc-latex-template) [Pandoc](https://pandoc.org) [LaTex](https://www.latex-project.org) template. To set up the pandoc and latex environments execute

```bash
$ brew install openjdk
$ brew install texlive
$ mkdir /Users/[username]/.pandoc/
$ mkdir /Users/[username]/.pandoc/templates/
$ cd /Users/[username]/.pandoc/templates/
$ cp ~/Documents/Eisvogel-2/eisvogel.latex ./
```

Generation of the epub is also supported.

```bash
$ stack exec make epub
```
