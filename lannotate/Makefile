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

FRAMAC_SHARE    :=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR   :=$(shell frama-c.byte -print-libpath)
PLUGIN_NAME      = LAnnotate

PLUGIN_BFLAGS += -warn-error -a
PLUGIN_OFLAGS += -warn-error -a

PLUGIN_TESTS_DIRS:= options criteria

PLUGIN_CMO = options utils ast_const bes simplify annotators wm \
             logical partition ldataflow context function statement lloop register
include $(FRAMAC_SHARE)/Makefile.dynamic

clean::
	$(RM) -rf top
	$(RM) -f META.*
	$(RM) -f .depend
