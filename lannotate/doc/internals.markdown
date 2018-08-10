(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You may redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      *)
(*  Public License for more details.                                      *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3 for more          *)
(*  details (enclosed in the file LICENSE).                               *)
(*                                                                        *)
(**************************************************************************)

Internals
=========

Core modules
------------

### `Annotators`
specifies what is an annotator and the mechanism to register new annotators.

It provides the `make_label` function to annotators.
The function builds the label statements and calls simplify if demanded.

TODO one day remove the actual annotation from the annotators to be done once
as single pass by Annotators.

### `Logical`
implements the Boolean expression annotators: DC, CC, MCC, NCC, GACC, GICC && LIMIT (WIP)

### `Partition`
implements the input partition domain (IPD) annotator.

### `Simplify`
contains the simplification passes (or pass for now).

Implemented: use Bes to simplify the purely Boolean part of expression, works
pretty well, but its mainly minimizes the DNF. Does not seem optimal for DSE?

### `Wm`
implements the Weak mutation annotator: WM

### `Function`
implements the Function annotators: FC, FCC

### `Dataflow`
implements the dataflow criterias: all-defs, all-uses & def-use

### `Context`
implements the dataflow criteria: context

### `Statement`
implements the statement criteria: STMT

### `Loop`
implements loops criterias: ELO & SLO

### `Utils`
some utilities but lots of legacy code.

TODO cleanup

### `Ast_const`
utilities to make AST elements, extend and replace somewhat Cil in this role

Frama-C's classics
------------------

### `Options`
defines the plug-in's command line options.

### `Register`
implements and registers the plug-in's main.
Too much code in there... in particular the writing of .labels.

TODO refactor to other modules
TODO unified management of .labels data between LTest's tools


External
--------

Bes: a Boolean expression simplifier, old with lots of unused variables but
hey it does work pretty well.
Note that it's really too much for our use, something simpler like BDDs
(RO but not minimized) may be more indicated.
