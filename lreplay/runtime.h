/**************************************************************************/
/*                                                                        */
/*  This file is part of LReplay.                                         */
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

#ifndef RUNTIME_H
#define RUNTIME_H

#ifndef LREPLAY_OUTPUT
#define LREPLAY_OUTPUT a.covlabels
#endif

#ifndef SREPLAY_OUTPUT
#define SREPLAY_OUTPUT b.covsequences
#endif

#ifndef LREPLAY_TESTDRIVER
#define LREPLAY_TESTDRIVER unknown
#endif

#define str(x) # x
#define xstr(x) str(x)


/******************************************************************************/

#undef pathcrawler_assume
#define pathcrawler_assume(cond) do { if (!(cond)) fprintf(stderr, "User assumption violated at %s:%d\n", __FILE__, __LINE__);} while(0)
#undef pathcrawler_assert
#define pathcrawler_assert(cond) do { if (!(cond)) fprintf(stderr, "User assertion violated at %s:%d\n", __FILE__, __LINE__);} while(0)

#undef pc_label
#undef pc_label1
#undef pc_label2
#undef pc_label3
#undef pathcrawler_label
#undef pathcrawler_label1
#undef pathcrawler_label2
#undef __PC__LABEL
#undef pc_label_bindings

#define pc_label_bindings(...) do {bi_replay_report(__FILE__,__LINE__,__VA_ARGS__);} while (0)
#define pc_label_sequence(...) do {seq_replay_report(__VA_ARGS__);} while (0)
#define pc_label_sequence_condition(...) do {seq_cond_replay_report(__VA_ARGS__);} while (0)

#define __PC__VA_NUM_ARGS(...) __PC__VA_NUM_ARGS_IMPL(__VA_ARGS__,9,8,7,6,5,4,3,2,1)
#define __PC__VA_NUM_ARGS_IMPL(_1,_2,_3,_4,_5,_6,_7,_8,_9,N,...) N
#define __PC__VA_NUM_SWITCH(name, n, ...) __PC__VA_NUM_SWITCH_AUX(name, n, __VA_ARGS__)
#define __PC__VA_NUM_SWITCH_AUX(name, n, ...) name ## n (__VA_ARGS__)

#define pc_label(...) __PC__VA_NUM_SWITCH(pc_label, __PC__VA_NUM_ARGS(__VA_ARGS__),__VA_ARGS__)
#define pc_label1(cond) __PC__LABEL(cond,__COUNTER__, "")
#define pc_label2(cond, id) __PC__LABEL(cond, id, "")
#define pc_label3(cond, id, tag) __PC__LABEL(cond, id, tag)

#define pathcrawler_label(...) __PC__VA_NUM_SWITCH(pathcrawler_label, __PC__VA_NUM_ARGS(__VA_ARGS__), __VA_ARGS__)
#define pathcrawler_label1(cond) __PC__LABEL(cond, __COUNTER__, "")
#define pathcrawler_label2(cond,tag) __PC__LABEL(cond, __COUNTER__, tag)

#define __PC__LABEL(cond, id, tag) lbl_replay_report(__FILE__, __LINE__, cond, id, tag)

/******************************************************************************/

#include <stdio.h>
#include <stdarg.h>

typedef struct element {
    unsigned long id;
    struct element *next;
} element;

typedef struct binding_state {
  struct binding_state *next;
  int s[];
} binding_state;

typedef struct data {
  const char* file;
  int line;
  const char* tag;
  int nbBinds;
  char *vars[];
} data;

struct hash1 {
    unsigned long key_idSeq;
    int idEl;
    UT_hash_handle hh;
};

struct hash2 {
    const char *key_tag;
    element *head;
    UT_hash_handle hh;
};

struct hash3 {
    int key_idLabel;
    int status;
    UT_hash_handle hh;
};

struct hash4 {
    int key_idBind;
    binding_state *head;
    UT_hash_handle hh;
};

struct hash5 {
    int key_idBind;
    data *d;
    UT_hash_handle hh;
};

void lreplay_exit();
__attribute__((constructor)) void lreplay_init ();
void lbl_replay_report(const char* file,int line, int cond, int id, const char* tag);
void bi_replay_report(const char* file,int line, int cond, int id, const char* tag, int bindId, int nbBindings, ...);
void seq_replay_report(int cond, unsigned long sid, int lid, int slength, const char* tag, int nbBindings, ...);
void seq_cond_replay_report(int cond, const char* tag);

#endif
