# actojat
A Cobol to Java Transpiler for automated code base transpilation.


## Motivation
Companies have large legacy code bases that are hard to migrate manually. The
*Amazing Cobol To Java Transpiler* is meant to explore one possible solution: automatic
code transpilation.

Right now this project is work in progress, focusing on COBOL85 code bases using
[ANTLR](http://www.antlr.org/) to do some of the heavy lifting.


## How to build it
Install a [Java SE Development Kit](https://www.oracle.com/technetwork/java/javase/downloads/index.html) (platform version
8 or higher) as well as [Apache Maven](https://maven.apache.org/). Clone or download this repository, and build by running
```bash
$ mvn package
```


## Using actojat as a framework
If you want to use actojat to transpile from a language of your choice to Java, here is
how it can be done:
* At the transpiler's heart lies a [grammar file](actojat-cobol-support/src/main/antlr4/de/netherspace/apps/actojat/cobol_grammar.g4)
that describes the source language in an ANTLR-specific dialect of common BNF. Create a
grammar file of your own that accepts your chosen source language

* Use [ANTLR's Maven plugin](https://www.antlr.org/api/maven-plugin/latest/) to generate
your parser and tokenizer classes

* Utilize actojat's [Abstract Source Transpiler class](actojat-transpiler/src/main/java/de/netherspace/apps/actojat/AbstractSourceTranspiler.java)
as well as its [Java Intermediate Representation](actojat-transpiler/src/main/java/de/netherspace/apps/actojat/JavaIrToSourceCodeTranslator.java)
to perform the actual transpilation step
