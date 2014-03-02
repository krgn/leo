Leo
===

query [dict.leo.org](http://dict.leo.org) from your program/shell

Description
-----------

`leo` is a command-line program and library written in the
[haskell](http://www.haskell.org/haskellwiki/Haskell)
programming language for querying the [leo](http://dict.leo.org) translation
database. Its a pet project, intented to help me get into `haskell`, the most
beautiful programming language I know of as of the date of this writing. I
might make your life easier, or not. It certainly made mine fun for a few days.

Installation 
------------

```
cabal configure
cabal install
```

Usage
-----

The command line application specifies a few options. Find out about them using
the `--help` switch.


```
leo --help

```

Valid languages are:

```
en - English
fr - French
sp - Spanish
it - Italian
ch - Chinese
ru - Russian
pt - Portugese
pl - Polish
```

Todo:
----

* Implement Output formats (Stdout, CSV, JSON)

Disclaimer
----------
`leo` is basically a web scraper, so be prepared that it might break while I'm
on holiday. Drop me an email if that should happen and I don't notice it.
Otherwise, you know what to do...


