/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2013-2018                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  You may redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 3.                                                */
/*                                                                        */
/*  It is distributed in the hope that it will be useful, but WITHOUT     */
/*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    */
/*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      */
/*  Public License for more details.                                      */
/*                                                                        */
/*  See the GNU Lesser General Public License version 3 for more          */
/*  details (enclosed in the file LICENSE).                               */
/*                                                                        */
/**************************************************************************/

#ifndef RUNTIME_H
#define RUNTiME_H

#ifndef LREPLAY_OUTPUT
#define LREPLAY_OUTPUT a.covlabels
#endif

#ifndef HSREPLAY_OUTPUT
#define HSREPLAY_OUTPUT b.covsequences
#endif

#ifndef LREPLAY_TESTDRIVER
#define LREPLAY_TESTDRIVER unknown
#endif

#define LREPLAY_NBACCESS a.accessedseq

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

#define pc_label_bindings(...) do {lreplay_report(__FILE__,__LINE__,__VA_ARGS__);} while (0)
#define pc_label_sequence(...) do {hsreplay_report(__FILE__,__LINE__,__VA_ARGS__);} while (0)
#define pc_label_sequence_condition(...) do {hsreplay_cond_report(__FILE__,__LINE__,__VA_ARGS__);} while (0)

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

#define __PC__LABEL(id, cond, tag) lreplay_report(__FILE__, __LINE__, cond, id, tag, 0)

/******************************************************************************/

#include <stdio.h>
#include <stdarg.h>

extern int errno ;

static FILE* lreplay_file = (void*) 0;
static FILE* hsreplay_file = (void*) 0;
static FILE* accessed_file = (void*) 0;

unsigned long cpt_rexec_sequences;
unsigned long cpt_access_labels;

typedef struct element {
    unsigned long idSeq;
    struct element *next;
} element;

struct hash1 {
    unsigned long key_idSeq;
    int idEl;
    UT_hash_handle hh;
};

struct hash2 {
    char key_tag[10];
    element *head;
    UT_hash_handle hh;
};

struct hash3 {
    int key_idLabel;
    int status;
    UT_hash_handle hh;
};

struct hash1 *coveredSequences;
struct hash1 *partiallyCoveredSequences;
struct hash2 *invertedPartiallyCoveredSequences;
struct hash3 *labelStatus;


void lreplay_exit();
__attribute__((constructor))  void lreplay_init ();
void lreplay_report(const char* file,int line, int cond, int id, const char* tag, int nbBindings, ...);
void hsreplay_report(const char* file,int line, int cond, unsigned long sid, int lid, int slength, const char* tag, int nbBindings, ...);
void hsreplay_cond_report(const char* file,int line, int cond, const char* tag);

#endif
