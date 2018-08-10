##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2013-2018                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  You may redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 3.                                                #
#                                                                        #
#  It is distributed in the hope that it will be useful, but WITHOUT     #
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    #
#  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      #
#  Public License for more details.                                      #
#                                                                        #
#  See the GNU Lesser General Public License version 3 for more          #
#  details (enclosed in the file LICENSE).                               #
#                                                                        #
##########################################################################

ifndef FRAMAC_SHARE
FRAMAC_SHARE := $(shell frama-c -journal-disable -print-path)
endif
ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR := $(shell frama-c -journal-disable -print-libpath)
endif

LUNCOV_SHARE := $(FRAMAC_SHARE)/luncov
# plugin short name here

PLUGIN_DEPENDENCIES := Wp

PLUGIN_NAME = LUncov
PLUGIN_CMO = options commons instrument instrument_multicore data value wp_singlecore wp_multicore init vwap register

PLUGIN_TESTS_DIRS:= luncov_value

include $(FRAMAC_SHARE)/Makefile.dynamic

install-sharedir:
	$(MKDIR) -p $(LUNCOV_SHARE)

install:: install-sharedir
	$(PRINT_CP) $(LUNCOV_SHARE)
	$(CP) labels.h $(LUNCOV_SHARE)
	$(CP) luncov $(BINDIR)

uninstall::
	$(PRINT_RM) $(LUNCOV_SHARE)
	$(RM) -r $(LUNCOV_SHARE)

clean::
	$(RM) -rf top
	$(RM) -f META.*
	$(RM) -f .depend

test::
	$(RM) -rf /tests/luncov_value/result
