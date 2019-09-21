# actojat
[![Build Status](https://travis-ci.org/patrickp89/actojat.svg?branch=master)](https://travis-ci.org/patrickp89/actojat)

A Cobol to Java transpiler for automated code base transpilation.


## Motivation
Companies have large legacy code bases which are hard to migrate manually. The
*Amazing Cobol To Java Transpiler* is meant to explore one possible solution: automatic
code transpilation.

Right now this project is **work in progress**, focusing on COBOL85 code bases only. A module that implements support
for ANSI C transpilation was added only to guarantee that the transpiler infrastructure has at least some degree of
generality. This project uses [ANTLR](http://www.antlr.org/) for some of the heavy lifting.


## How to build it
Install a [Java SE Development Kit](https://www.oracle.com/technetwork/java/javase/downloads/index.html) as well
as [Apache Maven](https://maven.apache.org/). Clone or download this repository, and build by running:
```bash
$ mvn package
```

## How to run it
To transpile a single file of COBOL code run:
```bash
$ java -jar actojat-cli/target/actojat.jar /path/to/test-source-helloworld.cob TestName2 my.base.pckg COBOL /tmp/ false
```

## Which language features are supported?
Actojat is still work in progress. The following table provides you with an overview, which language features of the
COBOL85 language standard are already supported:

| Feature (Keyword)     | Description          | Implemented?    | Sample COBOL code  | Generated Java Code |
| --------------------- | -------------------- | --------------- | ------------------ | ------------------- |
| Sections              | Section declarations | yes             | ...                | ...                 |
| Conditional operators | Greater, lesser, ... | yes             | [conditional.cob](actojat-cli/src/test/resources/cobol-sources/conditional.cob)   | [IfThenElseAndConditions.java](actojat-cli/src/test/resources/expected-java-sources/IfThenElseAndConditions.java)   |
| IF .. THEN ... ELSE   | Common branching     | yes             | [conditional.cob](actojat-cli/src/test/resources/cobol-sources/conditional.cob)   | [IfThenElseAndConditions.java](actojat-cli/src/test/resources/expected-java-sources/IfThenElseAndConditions.java)   |
| PERFORM ... TIMES     | For-loops            | yes             | [performtimes.cob](actojat-cli/src/test/resources/cobol-sources/performtimes.cob)   | [PerformTimes.java](actojat-cli/src/test/resources/expected-java-sources/PerformTimes.java)   |
| PERFORM ... UNTIL     | While-loops          | yes (partially) | ...                | ...                 |
| PERFORM ... VARYING   | For-loops            | no              | ...                | ...                 |

...


## Using actojat as a framework
If you want to use actojat to transpile from a language of your choice to Java, here is how it can be done:
* At the transpiler's heart lies a [grammar file](actojat-cobol-support/src/main/antlr4/de/netherspace/apps/actojat/cobol_grammar.g4)
that describes the source language in an ANTLR-specific dialect of common BNF. Create a grammar file of your own that
accepts your chosen source language,

* use [ANTLR's Maven plugin](https://www.antlr.org/api/maven-plugin/latest/) to generate your parser and tokenizer classes, and

* utilize actojat's [Abstract Source Transpiler class](actojat-transpiler/src/main/kotlin/de/netherspace/apps/actojat/AbstractSourceTranspiler.kt)
as well as its [Java Intermediate Representation](actojat-transpiler/src/main/kotlin/de/netherspace/apps/actojat/JavaIrToSourceCodeTranslator.kt)
to perform the actual transpilation step
