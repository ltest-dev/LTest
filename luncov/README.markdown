Frama-C/LTest: LUncov
=====================
*Infeasible test requirement detection*

Frama-C/LTest (or LTest for short) is a generic and integrated toolkit for
automation of white-box testing of C programs. This toolkit provides a unified
support of many different testing criteria as well as an easy integration of
new criteria. *LUncov* is the module of LTest in charge of detecting infeasible
(or *uncov*erable) test requirements. As the rest of the toolkit, this module
requires that your test requirements are encoded as labels.

LUncov is a Frama-C plugin.

LUncov offers three ways to detect infeasible requirements:

  - Evolved Value Analysis: global analysis, done only once for every label
  - Weakest precondition: done once per label
  - Their combination

Installation
------------

LUncov requires Frama-C 21.1 (Scandium) and OCaml 4.10.0 to be installed.
To install it run the following two commands in LUncov's directory:

    autoconf
    ./configure
    make
    make install

Depending of your system and your Frama-C installation, the latter command may
need to be run as root or `sudo`-ed.

Usage
-----

### Three modes

  - `-luncov-eva` detects infeasible requirements by a
    global one-time analysis

  - `-luncov-wp` detects infeasible labels by a distinct
    analysis for each label

  - `-luncov-vwap` detects infeasible labels by combining
    a global analysis with a goal-oriented analysis
    (Desactivated for now, WIP)

### Running the detection

To start detecting infeasible (or uncoverable) labels simply run:

    frama-c -luncov-eva file.c -main fun

where `file.c` is the name of the file under test and `fun` is the name of the
function under test.

*N.B.* LUncov automatically updates (or creates one if missing) the label
coverage file (here `test.labels`).

### EVA-based detection

You may specify value analysis's parameters. See `frama-c -value-help` for
details. It may be use to limit the verbosity of the analysis
(`-value-verbose`), or to increase its precision, e.g.:

    frama-c -luncov-eva -value-verbose 0 -slevel 5 -val-ilevel 32 test.c

The label pruning will be based on a more precise value analysis, with:

  - `-slevel 5`: a superposition of up to 5 states for each program location
    (default 0),

  - `-val-ilevel 32`: and a precise representation for small set of integral
    values (up to 32 values, default 16).

Other useful parameters include :

  - `-context-width n`  indicates Frama-C to use `n` as the width of the default
    context for value analysis (default: 2). Note that if the entry function
    takes an array as input Frama-C will considers that its maximum size is `n`.

  - `-lib-entry` tells Frama-C to consider the entry function as a library call
    rather than a program main. In particular, when on, Frama-C does not
    initialize global variables.

  - `-no-warn-signed-overflow` to adopt two's complement as the semantics of
    integer signed overflow

### Weakest precondition-based detection

You may specify additional weakest precondition's parameters. The most useful
parameter is without a doubt `-wp-model model` to choose the actual model use by
WP, for instance `typed` or `hoare` with options such as `cint` (vs. `nat`) or
`cast`. Recommended value: `typed+cint`.
See `frama-c -wp-help` for details.

Note that LUncov considers every annotations already present in the code as valid.

### Hybrid approach: value analysis and weakest precondition

In addition from flags pertinent to both approaches, the hybrid approach use an
additional parameters `-luncov-strategy s`. Indeed, the hybrid approach
transfers some information computed by the value analysis to the weakest
precondition precondition. The strategy `s` specifies what information to
transfer. Accepted strategies are:

  - `none`: no information is provided to WP
  - `param`: information about function parameters
    (e.g. possible values of the parameters)
  - `label`: information about the label
    (e.g. possible values about the variable it uses)
  - `param+label`: function parameters are provided to WP
  - `function`: information at each instruction to WP
  - `all`: all of the above

### Shortcut

We also provide a simple way to run LUncov with a predefined set of useful
parameters:

    luncov -luncov-wp test.c -main my_fun

This script first prints the used Frama-C command line, then executes it.

### Label database initialization

LUncov assumes a label database if present. If you need to generate one from the
source file on-the-fly, simply add `-luncov-init` to the command line.
Alternatively, it can be run alone:

    frama-c -luncov-init myfile.c

### Debug info

    frama-c -luncov-value -luncov-debug 1 test.c

Authors
-------

  - Mickaël Delahaye
  - Robin David
  - Thibault Martin

Also many thanks to the rest of LTest's team:
  - Sébastien Bardin
  - Omar Chebaro
  - Nikolai Kosmatov
