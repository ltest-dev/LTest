(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

let () = Plugin.is_share_visible ()

include Plugin.Register
    (struct
      let name = "luncov"
      let shortname = "luncov"
      let help = "Uncoverable label detection"
    end)

module Enabled = False (struct
    let option_name = "-luncov"
    let help = "enable uncoverable label detection (disabled by default)"
  end)

let inits = add_group "Label data initialization"

let () = Parameter_customize.set_group inits
module Init = False (struct
    let option_name = "-luncov-init"
    let help = "(re-)initialize label data from source file"
  end)
let () = Init.add_set_hook (fun _old b -> if b then Enabled.on ())

let () = Parameter_customize.set_group inits
module ForceInit = False (struct
    let option_name = "-luncov-force-init"
    let help = "initialize label data from source file \
                (clearing any previous data first)"
  end)
let () = ForceInit.add_set_hook (fun _old b -> if b then Enabled.on ())

let methods = add_group "Detection methods"

let () = Parameter_customize.set_group methods
module WP = False (struct
    let option_name = "-luncov-wp"
    let help = "enable weakest precondition-based detection \
                (disabled by default, implies -luncov)"
  end)
let () = WP.add_set_hook (fun _old b -> if b then Enabled.on ())

let () = Parameter_customize.set_group methods
module EVA = False (struct
    let option_name = "-luncov-eva"
    let help = "enable EVA-based detection \
                (enabled by default, implies -luncov)"
  end)
let () = EVA.add_aliases [ "-luncov-value" ]
let () = EVA.add_set_hook (fun _old b -> if b then Enabled.on ())

let () = Parameter_customize.set_group methods
let () = Parameter_customize.is_invisible ()
module VWAP = False (struct
    let option_name = "-luncov-vwap"
    let help = "enable EVA + WP computation \
                (disabled by default, implies -luncov)"
  end)
let () = VWAP.add_set_hook (fun _old b -> if b then Enabled.on ())

let () = Parameter_customize.set_group methods
module AlwaysTrue = False (struct
    let option_name = "-luncov-always-true"
    let help = "enable WP to detect always true labels \
                (disabled by default, implies -luncov)"
  end)
let () = AlwaysTrue.add_aliases [ "-luncov-at" ]
let () = AlwaysTrue.add_set_hook (fun _old b -> if b then Enabled.on ())

let general = add_group "General options"

let () = Parameter_customize.set_group general
module Force = False (struct
    let option_name = "-luncov-force"
    let help = "force the computation for all labels, \
                including those marked covered or uncoverable \
                (disabled by default)"
  end)

let () = Parameter_customize.set_group general
module Time = True (struct
    let option_name = "-luncov-show-time"
    let help = "Display execution time at the end (enabled by default)"
  end)

let () = Parameter_customize.set_group general
module WPQedOptim = True (struct
    let option_name = "-luncov-wp-qed-optim"
    let help = "enable formula optimizations in QED during WP call \
                (enabled by default)"
  end)

let () = Parameter_customize.set_group general
module WPMakeProofs = True (struct
    let option_name = "-luncov-wp-make-proofs"
    let help = "enable call to provers in WP (in such a case, the formula is generated but not solved, useful for debug) \
                (call to provers is enabled by default)"
  end)

let () = Parameter_customize.set_group general
module WPShowLogs = True (struct
    let option_name = "-luncov-wp-show-logs"
    let help = "prints the logs generated by WP and the called solvers \
                (enabled by default)"
  end)

let () = Parameter_customize.set_group general
module Rte = False (struct
    let option_name = "-luncov-rte"
    let help = "add runtime error annotations for WP-based detection \
                (disabled by default), \
                see -rte-help for RTE generation parameters"
  end)

let () = Parameter_customize.set_group general
module LabelsFile = String (struct
    let default = ""
    let option_name = "-luncov-labels"
    let arg_name = "f"
    let help = "set the filename of the label data \
                (by default <input file>.labels)"
  end)

let () = Parameter_customize.set_group general
module HyperlabelsFile = String (struct
    let default = ""
    let option_name = "-luncov-hyperlabels"
    let arg_name = "f"
    let help = "set the filename of the hyperlabel data \
                (by default <input file>.hyperlabels)"
  end)

let () = Parameter_customize.set_group general
module WPMaxNbLabelPerCall = Int (struct
    let option_name = "-luncov-wp-max-labels-per-call"
    let arg_name = "n"
    let default = 0
    let help = "set the maximal number of labels per call to WP (useful to prevent memory overflow) \
                (0 by default, i.e. automatic management)"
  end)

let () = Parameter_customize.set_group general
module Multicore = Int (struct
    let option_name = "-luncov-multicore"
    let arg_name = "n"
    let default = 1
    let help = "set the number of processes used for parallel WP-based detection \
                (1 by default, no parallelism)"
  end)

let () = Parameter_customize.set_group general
module WpMaxMemory = Int (struct
    let option_name = "-luncov-wp-max-memory"
    let arg_name = "t"
    let default = 1000000
    let help = "set the maximal redident memory usage (in kB) allowed for each complete WP run (run is aborded if it overflows). \
                (1GB by default)"
  end)

let () = Parameter_customize.set_group general
module WpTimeout = Int (struct
    let option_name = "-luncov-wp-timeout"
    let arg_name = "t"
    let default = 10800
    let help = "set the timeout (in seconds) for WP complete analysis. \
                (10800s (3h) by default)"
  end)

let () = Parameter_customize.set_group general
module WpKillTimeout = Int (struct
     let option_name = "-luncov-wp-kill-timeout"
     let arg_name = "t"
    let default = 10
    let help = "set the timeout (in seconds) for each complete WP run. \
                (10s by default)"
   end)

let () = Parameter_customize.set_group general
module SolverTimeout = String (struct
    let option_name = "-luncov-wp-solver-timeout"
    let arg_name = "t"
    let default = "1."
    let help = "set the timeout (in seconds) for every solver call by WP. \
                (1s by default)"
  end)

let () = Parameter_customize.set_group general
let strat_str = ["none";"all";"param";"function";"label";"label+param"]
module Strategy = String (struct
    let default = "label"
    let option_name = "-luncov-strategy"
    let arg_name = "strategy"
    let help = "set the strategy to use for annotating code ("^(List.fold_left (fun acc e -> acc^","^e) "" strat_str)^")"
  end)
let () = Strategy.set_possible_values strat_str

let () = Parameter_customize.set_group general
module Backup = False (struct
    let option_name = "-luncov-backup"
    let help = "Backup the .labels file before writting the new one (default : false)."
  end)
