Frama-C/LTest: LAnnotate
========================

Frama-C/LTest (or LTest for short) is a generic and integrated toolkit for
automation of white-box testing of C programs. This toolkit provides a unified
support of many different testing criteria as well as an easy integration of
new criteria. *LAnnotate* is the module of LTest in charge of adding coverage
objectives into program in the form of annotations.

LAnnotate is a Frama-C plugin.

Installation
------------
LAnnotate requires Frama-C (Chlorine or latter) to be installed. A patch (Chlorine.patch) is available for LUncov compatibility.
Once Frama-C is installed, compile and install LAnnotate:

    make
    make install

The second command may need to be run as root (or sudo) depending on your
Frama-C installation.

Usage
-----

    frama-c -lannot=CRITERIA file.c

where CRITERIA is a comma-separated list of criteria. It outputs a new
annotated file named `file_labels.c`, with labels for each selected criterion.

Implemented criteria are DC, CC, MCC, n-CC, GACC, GICC, WM, IDP and FC.
for more information, run `frama-c -lannot-list` and see the [criteria
documentation](doc/criteria.markdown).

Global options are :

  - `-lannot-simplify` enables the simplication of the generated label
    predicates before annotations. For the moment, it only reduce the Boolean
    expressions.

  - `-lannot-allbool` tells LAnnotate to consider all Boolean expressions,
    rather that just branching decision. Impacts DC, CC, MCC, n-CC, GACC, GICC.

  - `-lannot-functions funs` tells LAnnotate what functions to consider
    (all by default).

  - `-lannot-o outputfile.c` tells LAnnotate how to name the annoted file
    (by default, add `_labels` before the extension).

There exist other options that are specific to some criteria, see the criteria
doc. To get the full list of options, run `frama-c -lannot-help`.

Authors
-------

- Omar Chebaro
- Mickaël Delahaye
- Michaël Marcozzi
- Thibault Martin  
Also many thanks to the rest of LTest's team:
- Nikolai Kosmatov
- Sébastien Bardin

License
-------

This file is part of Frama-C

Copyright (C) 2013-2018
  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)

You may redistribute it and/or modify it under the terms of the GNU
Lesser General Public License as published by the Free Software
Foundation, version 3.

It is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

See the GNU Lesser General Public License version 3
for more details (enclosed in the file LICENSE).
