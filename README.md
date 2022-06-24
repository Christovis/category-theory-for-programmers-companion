# A reading companion for  <br/>"[Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)"

These notes serve the following purpose. Firstly, they list and summaries
important concepts in Category Theory and how they can be expressed in Haskell.
Secondly, the book content is compared to other books I am reading in parallel.

In composing this text I will follow these rules:
- Notes: are for references to other books and papers
- Bold font: is for definitions and questions of challenges
- Italic: If things that still need to be defined appear in text


Releases
--------

This is a work in progress.

* [Part 1](https://github.com/Christovis/category-theory-for-programmers-companion/blob/main/docs/part1.pdf)
* [Part 2](https://github.com/Christovis/category-theory-for-programmers-companion/blob/main/docs/part2.md)
* Part 3

Dependencies
------------

- Homebrew == 3.5.2

To build the documents:
- Pandoc == 2.18
    - pandoc-types == 1.22.2
    - texmath == 0.12.5
    - skylighting == 0.12.3
    - citeproc == 0.7
    - ipynb == 0.2
    - hslua == 2.2.0
    - Lua == 5.4
- pdfTeX 3.141592653-2.6-1.40.24 (TeX Live 2022/Homebrew)
    - kpathsea version 6.3.4
- Eisvogel == 2.0.0

To run the code in /src/:
- GHC == 8.10.7


Building
--------

To generate the build scripts provision a cabal and stack with pandoc in it. This
is done by the `.cabal` and  `stack.yaml` files.

To generate the pdf, I use the [Eisvogel](https://github.com/Wandmalfarbe/pandoc-latex-template) [Pandoc](https://pandoc.org) [LaTex](https://www.latex-project.org) template. To set up the pandoc and latex environments execute

```bash
$ brew install openjdk
$ brew install texlive
$ mkdir /Users/[username]/.pandoc/
$ mkdir /Users/[username]/.pandoc/templates/
$ cd /Users/[username]/.pandoc/templates/
$ cp ~/Documents/Eisvogel-2/eisvogel.latex ./
```

Generation of the pdf from markdown by executing

```bash
$ make docs/part1.pdf
```
