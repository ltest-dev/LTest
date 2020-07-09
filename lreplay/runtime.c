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

static FILE* lreplay_file = (void*) 0;
static FILE* sreplay_file = (void*) 0;

struct hash1 *coveredSequences;
struct hash1 *partiallyCoveredSequences;
struct hash2 *invertedPartiallyCoveredSequences;
struct hash3 *labelStatus;
struct hash4 *bindingStatus;
struct hash5 *bindingInfos;

// will be called at exit, free/del all hashtbls
void lreplay_exit() {
  if (lreplay_file)  {
    struct hash3 *curr3, *tmp3;
    struct hash4 *curr4, *tmp4;
    HASH_ITER(hh,labelStatus,curr3,tmp3){
      HASH_DEL(labelStatus,curr3);
      free(curr3);
    }

    HASH_ITER(hh,bindingStatus,curr4,tmp4){
      struct hash5 *curr5;
      HASH_FIND_INT(bindingInfos,&curr4->key_idBind,curr5);
      if(curr5 != NULL){
        binding_state *st,*sttmp;
        LL_FOREACH_SAFE(curr4->head, st, sttmp){
          fprintf(lreplay_file, "%d,%d,%s,%s:%d,%d", curr5->key_idBind, 1, curr5->d->tag, curr5->d->file, curr5->d->line, curr5->d->nbBinds);
          for (int i = 0; i < curr5->d->nbBinds; i++) {
            fprintf(lreplay_file,",%s,%d", curr5->d->vars[i], st->s[i]);
          }
          fprintf(lreplay_file,"%s\n", "");
          LL_DELETE(curr4->head, st);
          free(st);
        }
        HASH_DEL(bindingInfos,curr5);
        free(curr5->d);
        free(curr5);
      }
      HASH_DEL(bindingStatus,curr4);
      free(curr4);
    }

    fclose(lreplay_file);
    lreplay_file  = (void*) 0;
  }

  if (sreplay_file) {
    struct hash1 *curr, *tmp;
    struct hash2 *curr2, *tmp2;
    struct element *elt,*elttmp;

    HASH_ITER(hh,coveredSequences,curr,tmp){
      fprintf(sreplay_file,"%lu\n",curr->key_idSeq);
      HASH_DEL(coveredSequences,curr);
      free(curr);
    }
    HASH_ITER(hh,partiallyCoveredSequences,curr,tmp){
      HASH_DEL(partiallyCoveredSequences,curr);
      free(curr);
    }
    HASH_ITER(hh,invertedPartiallyCoveredSequences,curr2,tmp2){
      LL_FOREACH_SAFE(curr2->head, elt, elttmp){
        LL_DELETE(curr2->head, elt);
        free(elt);
      }
      HASH_DEL(invertedPartiallyCoveredSequences,curr2);
      free(curr2);
    }
    fclose(sreplay_file);
    sreplay_file = (void*) 0;
  }
}

// will be called before main (GCC magic)
__attribute__((constructor))  void lreplay_init () {
  lreplay_file = fopen(xstr(LREPLAY_OUTPUT), "w");
  if (!lreplay_file) {
    return;
  }
  fprintf(lreplay_file, "# %s\n", xstr(LREPLAY_TESTDRIVER));
  fprintf(lreplay_file, "# id,condition value,file:line,tag\n");
  fflush(lreplay_file);

  sreplay_file = fopen(xstr(SREPLAY_OUTPUT), "w");
  if (!sreplay_file) {
    return;
  }
  fprintf(sreplay_file, "# %s\n", xstr(LREPLAY_TESTDRIVER));
  fprintf(sreplay_file, "# id\n");

  coveredSequences=NULL;
  partiallyCoveredSequences=NULL;
  invertedPartiallyCoveredSequences=NULL;
  labelStatus=NULL;
  bindingStatus=NULL;
  bindingInfos=NULL;

  atexit(lreplay_exit);
}

int compare(element *a, unsigned long b){
  return a->id != b;
}

// Fill inverted hash table
// For each tag seen at runtime, add its sequences
void add_inverted_partially(int idSeq, const char* tag){
  struct hash2 *inverted_partially_covered;
  HASH_FIND_STR(invertedPartiallyCoveredSequences,tag,inverted_partially_covered);
  // If tag already saved
  if (inverted_partially_covered != NULL){
    struct element *out;
    LL_SEARCH(inverted_partially_covered->head,out,idSeq,compare);
    // Sequence not saved for this tag, add it
    if(out == NULL){
      struct element *newEl;
      newEl = malloc(sizeof(struct element));
      newEl->id=idSeq;
      newEl->next=NULL;
      LL_APPEND(inverted_partially_covered->head,newEl);
    }
  }
  else{
    // Create tag and save it with the sequence
    struct hash2 *tmp = malloc(sizeof(struct hash2));
    struct element *newEl = malloc(sizeof(struct element));
    newEl->id=idSeq;
    newEl->next=NULL;
    tmp->key_tag=tag;
    tmp->head=newEl;
    HASH_ADD_STR(invertedPartiallyCoveredSequences,key_tag,tmp);
  }
}

// Called for functions pc_label_sequence
void seq_replay_report(int cond, unsigned long sid, int lid, int slength, const char* tag, int nbBindings, ...) {

  cond = !!cond;

  if (!cond) return;

  if (nbBindings > 0){
    fprintf(stderr,"LReplay currently handles sequences with no binding only.\n");
    return;
  }

  struct hash1 *already_covered;
  HASH_FIND_INT(coveredSequences,&sid,already_covered);
  //Sequence not covered
  if (already_covered == NULL) {
    //First member of the sequence
    if (lid == 1){
        struct hash1 *partially_covered;
        HASH_FIND_INT(partiallyCoveredSequences,&sid,partially_covered);
        //And sequence not started yet, start it
        if (partially_covered == NULL){
            struct hash1 *tmp;
            tmp = malloc(sizeof(struct hash1));
            tmp->key_idSeq=sid;
            tmp->idEl=1;
            HASH_ADD_INT(partiallyCoveredSequences,key_idSeq,tmp);
            add_inverted_partially(sid,tag);
        }
     }
    //Last member of the sequence
    else if (lid == slength) {
      struct hash1 *partially_covered;
      HASH_FIND_INT(partiallyCoveredSequences,&sid,partially_covered);
      // Last seen member was the previous one, sequence is covered
      if (partially_covered != NULL && partially_covered->idEl + 1 == slength){
        HASH_DEL(partiallyCoveredSequences,partially_covered);
        HASH_ADD_INT(coveredSequences,key_idSeq,partially_covered);
      }
    }
    // Else for all other members
    else {
      struct hash1 *partially_covered;
      HASH_FIND_INT(partiallyCoveredSequences,&sid,partially_covered);
      // If the sequence is partially covered
      // And the last seen member was the previous one, increase last seen member
      if (partially_covered != NULL && lid == partially_covered->idEl + 1){
          partially_covered->idEl = lid;
          add_inverted_partially(sid,tag);
      }
    }
  }
}

// Called for functions pc_label_sequence_condition
void seq_cond_replay_report(int cond, const char* tag) {

  cond = !!cond;

  if (cond) return;

  struct hash2 *partially_covered;
  HASH_FIND_STR(invertedPartiallyCoveredSequences,tag,partially_covered);
  // Find all sequences partially covered with this tag
  if (partially_covered != NULL) {
    struct hash1 *curr, *tmp;
    struct element *elt,*elttmp;
    // Remove them from both hashtbl
    LL_FOREACH_SAFE(partially_covered->head, elt, elttmp){
      HASH_ITER(hh, partiallyCoveredSequences,curr,tmp){
        if (curr->key_idSeq == elt->id){
          HASH_DEL(partiallyCoveredSequences,curr);
          free(curr);
        }
      }
      LL_DELETE(partially_covered->head, elt);
      free(elt);
    }
    HASH_DEL(invertedPartiallyCoveredSequences,partially_covered);
    free(partially_covered);
  }
}


// Called for functions pc_label
void lbl_replay_report(const char* file,int line, int cond, int id, const char* tag) {

  if (!lreplay_file) return;

  cond = !!cond;

  // Goal: prevent a lot of superfluous coverage line that may occur when
  // label are in loops
  // -> the output file will contains at most (Number of labels)*2 lines
  struct hash3 *lblStatus;
  HASH_FIND_INT(labelStatus,&id,lblStatus);
  if (lblStatus == NULL) {
    lblStatus = malloc(sizeof(struct hash3));
    lblStatus->key_idLabel=id;
    lblStatus->status = 1 + cond;
    HASH_ADD_INT(labelStatus,key_idLabel,lblStatus);
    fprintf(lreplay_file, "%u,%d,%s,%s:%u\n", id, cond, tag, file, line);
  }
  else{
    switch(lblStatus->status){
    case 1: // Reached negatively
      if (!cond) return; // nothing new here

      // Becomes covered!
      fprintf(lreplay_file, "%u,%d,%s,%s:%u\n", id, cond, tag, file, line);
      lblStatus->status = 3;
      break;

    case 2: // Reached positively (i.e. covered)
      if (cond) return; // nothing new here

      // Becomes reached negatively (are we interested in that?)
      fprintf(lreplay_file, "%u,%d,%s,%s:%u\n", id, cond, tag, file, line);
      lblStatus->status = 3;
      break;

    case 3: // Reached negatively and positively
      return;
    }
  }
  fflush(lreplay_file);

}

// Check if we already saw the state s in older states
int already_seen(binding_state *olders, int s[], int size){
  int is_same;
  struct binding_state *st;
  LL_FOREACH(olders, st){
    is_same = 1;
    for (int i = 0; i < size; i++){
      if (s[i] != st->s[i]) { is_same = 0; break;}
    }
    if (is_same) return 1;
  }
  return 0;
}


// Called for functions pc_label_bindings
void bi_replay_report(const char* file,int line, int cond, int id, const char* tag, int bindId, int nbBindings, ...) {

  if (nbBindings != 0) {
    cond = !!cond;
    if (!lreplay_file || !cond) return;
    int i;

    // Get variables and their values from va_list
    int s[nbBindings];
    char *vars[nbBindings];

    va_list ap;
    va_start(ap, nbBindings);
    /* fprintf(lreplay_file, "%d,%d,%s,%s:%d,%d", id, cond, tag, file, line, nbBindings); */
    for (i = 0; i < nbBindings; i++) {
      vars[i]=va_arg(ap, char*);
      s[i] = !!(va_arg(ap, int));
      /* fprintf(lreplay_file,",%s,%d", vars[i], s[i]); */
    }
    /* fprintf(lreplay_file,"%s\n", ""); */
    va_end(ap);
    /* return; */

    // Create a new state with binding values
    struct binding_state *newState;
    newState = malloc(sizeof(struct binding_state) + sizeof(s));
    newState->next=NULL;
    for(i = 0; i < nbBindings; i++){
      newState->s[i]=s[i];
    }

    // Save infos of this label for later
    struct hash5 *currentBindInfos;
    HASH_FIND_INT(bindingInfos,&id,currentBindInfos);
    if(currentBindInfos == NULL){
      struct data *d;
      d = malloc(sizeof(struct data)+sizeof(vars));
      d->file=file;
      d->line=line;
      d->tag=tag;
      d->nbBinds=nbBindings;
      for(i = 0; i < nbBindings; i++){
        d->vars[i]=vars[i];
      }
      currentBindInfos = malloc(sizeof(struct hash5));
      currentBindInfos->key_idBind=id;
      currentBindInfos->d=d;
      HASH_ADD_INT(bindingInfos,key_idBind,currentBindInfos);
    }

    // If this is the first time we see this label, then adds it, else
    struct hash4 *currentBindStatus;
    HASH_FIND_INT(bindingStatus,&id,currentBindStatus);
    if(currentBindStatus==NULL){
      currentBindStatus = malloc(sizeof(struct hash4));
      currentBindStatus->key_idBind=id;
      currentBindStatus->head=newState;
      HASH_ADD_INT(bindingStatus,key_idBind,currentBindStatus);
    }
    else {
      if (already_seen(currentBindStatus->head,s,nbBindings)) return;
      LL_APPEND(currentBindStatus->head,newState);
    }
  }
  else{
    lbl_replay_report(file,line, cond, id, tag);
  }

  return;
}
