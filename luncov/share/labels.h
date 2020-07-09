/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2020                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __LABELS_H__
#define __LABELS_H__

#define __PC__VA_NUM_ARGS(...) __PC__VA_NUM_ARGS_IMPL(__VA_ARGS__,9,8,7,6,5,4,3,2,1)
#define __PC__VA_NUM_ARGS_IMPL(_1,_2,_3,_4,_5,_6,_7,_8,_9,N,...) N
#define __PC__VA_NUM_SWITCH(name, n, ...) __PC__VA_NUM_SWITCH_AUX(name, n, __VA_ARGS__)
#define __PC__VA_NUM_SWITCH_AUX(name, n, ...) name ## n (__VA_ARGS__)

#define pc_label(...) __PC__VA_NUM_SWITCH(pc_label, __PC__VA_NUM_ARGS(__VA_ARGS__),__VA_ARGS__)
#define pc_label1(cond) __PC__LABEL(__COUNTER__, cond, "")
#define pc_label2(cond, id) __PC__LABEL(id, cond, "")
#define pc_label3(cond, id, tag) __PC__LABEL(id, cond, tag)

#define pathcrawler_label(...) __PC__VA_NUM_SWITCH(pathcrawler_label, __PC__VA_NUM_ARGS(__VA_ARGS__), __VA_ARGS__)
#define pathcrawler_label1(cond) __PC__LABEL(__COUNTER__, cond, "")
#define pathcrawler_label2(cond,tag) __PC__LABEL(__COUNTER__, cond, tag)

/*@assigns \nothing;*/
void __PC__LABEL(unsigned int id, int cond, const char* tag) __attribute((FC_BUILTIN));

/*@assigns \nothing;*/
void __PC__LABEL_SEQ(int one, unsigned long ids, int pos, int len, const char* vid, int zero) __attribute((FC_BUILTIN));

/*@assigns \nothing;*/
void __PC__LABEL_SEQ_COND(int zero, const char* vid) __attribute((FC_BUILTIN));

/*@assigns \nothing;*/
void __PC__LABEL_BINDINGS(unsigned int id, int cond, const char* tag, unsigned int bindId, unsigned int nbBinds, ...) __attribute((FC_BUILTIN));


#define __PC__LABEL(id,cond,tag) do {           \
    __PC__LABEL(id, cond, tag);                 \
  } while(0)

#define pc_label_sequence(one,ids,pos,len,vid,zero) do {        \
    __PC__LABEL_SEQ(one,ids,pos,len,vid,zero); \
  } while(0)

#define pc_label_sequence_condition(zero,vid) do {      \
    __PC__LABEL_SEQ_COND(zero,vid);                     \
  } while(0)

#define pc_label_bindings(cond, id, tag, bindId, nbBinds, ...) do {     \
    __PC__LABEL_BINDINGS(id, cond, tag, bindId, nbBinds, __VA_ARGS__);  \
  } while(0)

#endif
