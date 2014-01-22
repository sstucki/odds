Odds [![Build Status](https://travis-ci.org/sstucki/odds.png?branch=master)](https://travis-ci.org/sstucki/odds)
====

Probabilistic programming in Scala

*Odds* is a small domain-specific language (DSL) for
[probabilistic programming](http://probabilistic-programming.org/),
embedded in [Scala](http://scala-lang.org/).  Odds provides
first-class support for random variables and probabilistic choice,
within Scala's powerful type system.  Odds programs are actually Scala
programs, which means that all of Scala's abstraction and modularity
facilities can be used to compose probabilistic computations and to
specify deterministic program parts.


Installation
------------

1. Odds is written in Scala, which runs on the JVM.  So for starters,
   you'll need an up-to-date [JRE/JDK](http://openjdk.java.net/).  If
   you're on a Debian/Ubuntu system, this should do the trick:

        $ sudo apt-get install openjdk-7-jre

2. Install the [SBT](http://www.scala-sbt.org/) build tool.

3. Clone the [GitHub repository](https://github.com/sstucki/odds):

        $ git clone https://github.com/sstucki/odds.git

4. Go into the root directory and compile the source code using SBT:

        $ cd odds
        $ sbt compile

5. Run `sbt test` to run the test suite.

6. Run `sbt publish-local` to install the Odds library for use in
   other projects.


Examples
--------

Have a look at the various test cases in the `core/test/` directory.
Many of them are Odds example programs, illustrating how to write an
Odds program and perform inference on it.


Documentation
-------------

To build the API docs, run `sbt doc`.  You can find the generated API
docs in the `core/target/scala-2.10/api/` directory.

Our [Scala'13 paper](http://dx.doi.org/10.1145/2489837.2489848) also
contains an introduction to Odds and describes the principles it is
built on.  The source code of Odds has changed quite a bit since
Scala'13, but the original source code (on which the paper was based)
is available [here](https://github.com/sstucki/odds/tree/scala13).


Source code
-----------

Visit [Odds](https://github.com/sstucki/odds) on GitHub.
