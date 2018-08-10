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


// will be called at exit
void lreplay_exit() {
  if (lreplay_file)  { fclose(lreplay_file) ; lreplay_file  = (void*) 0; }

  accessed_file = fopen(xstr(LREPLAY_NBACCESS), "a+");
  if (!accessed_file) {
    return;
  }
  else{
    if (cpt_access_labels != 0)
      fprintf(accessed_file, "\t\t%s : %lu\n", xstr(LREPLAY_TESTDRIVER), cpt_access_labels);
    else if (cpt_rexec_sequences != 0 )
      fprintf(accessed_file, "\t\t%s : %lu\n", xstr(LREPLAY_TESTDRIVER), cpt_rexec_sequences);
    else
      fprintf(accessed_file, "\t\t%s : 0\n", xstr(LREPLAY_TESTDRIVER));
    fflush(lreplay_file);
    fclose(accessed_file);
    accessed_file  = (void*) 0;
  }

  if (hsreplay_file) {
    struct hash1 *curr, *tmp;
    struct hash2 *curr2, *tmp2;
    HASH_ITER(hh,coveredSequences,curr,tmp){
      fprintf(hsreplay_file,"%lu\n",curr->key_idSeq);
      HASH_DEL(coveredSequences,curr);
      free(curr);
    }
    HASH_ITER(hh,partiallyCoveredSequences,curr,tmp){
      HASH_DEL(partiallyCoveredSequences,curr);
      free(curr);
    }

    element *elt,*elttmp;
    HASH_ITER(hh,invertedPartiallyCoveredSequences,curr2,tmp2){
      LL_FOREACH_SAFE(curr2->head, elt, elttmp){
        LL_DELETE(curr2->head, elt);
        free(elt);
      }
      HASH_DEL(invertedPartiallyCoveredSequences,curr2);
      free(curr2);
    }
    fclose(hsreplay_file);
    hsreplay_file = (void*) 0;
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

  hsreplay_file = fopen(xstr(HSREPLAY_OUTPUT), "w");
  if (!hsreplay_file) {
    return;
  }
  fprintf(hsreplay_file, "# %s\n", xstr(LREPLAY_TESTDRIVER));
  fprintf(hsreplay_file, "# id\n");
  coveredSequences=NULL;
  partiallyCoveredSequences=NULL;
  invertedPartiallyCoveredSequences=NULL;

  //fprintf(hsreplay_file, "# id,file:line,tag,bindings\n");
  //fflush(hsreplay_file);

  atexit(lreplay_exit);
}

int compare(element *a, int b){
  return a->idSeq != b;
}

void add_inverted_partially(int idSeq, const char* tag){
  struct hash2 *inverted_partially_covered;
  HASH_FIND_STR(invertedPartiallyCoveredSequences,tag,inverted_partially_covered);
  if (inverted_partially_covered != NULL){
    struct element *out;
    LL_SEARCH(inverted_partially_covered->head,out,idSeq,compare);
    if(!out){
      element *newEl;
      newEl = malloc(sizeof(struct element));
      newEl->idSeq=idSeq;
      newEl->next=NULL;
      LL_APPEND(inverted_partially_covered->head,newEl);
    }
  }
  else{
    struct hash2 *tmp;
    element *newEl;
    tmp = malloc(sizeof(struct hash2));
    newEl = malloc(sizeof(struct element));
    newEl->idSeq=idSeq;
    newEl->next=NULL;
    strncpy(tmp->key_tag,tag,10);
    tmp->head=newEl;
    HASH_ADD_STR(invertedPartiallyCoveredSequences,key_tag,tmp);
  }
}

void hsreplay_report(const char* file,int line, int cond, unsigned long sid, int lid, int slength, const char* tag, int nbBindings, ...) {

  cond = !!cond;

  if (!hsreplay_file || !cond) return;

  if (nbBindings > 0){
    fprintf(stderr,"LReplay currently handles sequences with no binding only.\n");
    return;
  }

  /*fprintf(hsreplay_file, "1,%u:%u/%u,%s,%s:%u,%d", sid, lid, slength, tag, file, line, nbBindings);

    if (nbBindings > 0){
      va_list ap;
      va_start(ap, nbBindings);
      int j;
      for (j = 0; j < nbBindings; j++) {
        fprintf(hsreplay_file,",%s", va_arg(ap, char*));
        fprintf(hsreplay_file,",%d", va_arg(ap, int  ));
      }
      va_end(ap);
    }
    fprintf(hsreplay_file,"%s\n", "");
    fflush(hsreplay_file);*/

  /* Traitement en C plutot qu'en CAML BETA */
  struct hash1 *already_covered;
  HASH_FIND_INT(coveredSequences,&sid,already_covered);
  if (already_covered == NULL) {
    cpt_rexec_sequences+=1;
    //Si ce n'est pas la dernière occurence dans la chaine
    if (lid != slength){
      struct hash1 *partially_covered;
      HASH_FIND_INT(partiallyCoveredSequences,&sid,partially_covered);
      //Si cette occurence est déjà partiellement couverte
      if (partially_covered != NULL){
        //Si c'est la première occurence, la même que la précédente, ou la suivante dans la chaine
        if (lid != partially_covered->idEl && lid <= partially_covered->idEl + 1){
          partially_covered->idEl = lid;
        }
      }
      //Si elle n'est pas couverte et que c'est le premier élément de la chaine
      else if (lid == 1){
        struct hash1 *tmp;
        tmp = malloc(sizeof(struct hash1));
        tmp->key_idSeq=sid;
        tmp->idEl=1;
        HASH_ADD_INT(partiallyCoveredSequences,key_idSeq,tmp);
        add_inverted_partially(sid,tag);
      }
    }
    else{
      struct hash1 *partially_covered;
      HASH_FIND_INT(partiallyCoveredSequences,&sid,partially_covered);
      //Si l'élément précédent était bien l'avant-dernier
      if (partially_covered != NULL && partially_covered->idEl + 1 == slength){
        HASH_DEL(partiallyCoveredSequences,partially_covered);
        HASH_ADD_INT(coveredSequences,key_idSeq,partially_covered);
      }
    }
  }
}

void hsreplay_cond_report(const char* file,int line, int cond, const char* tag) {

  cond = !!cond;

  if (!hsreplay_file || cond) return;

  //fprintf(hsreplay_file, "0,%s,%s:%u\n", tag, file, line);
  //fflush(hsreplay_file);
  cpt_rexec_sequences+=1;

  /* Traitement en C plutot qu'en CAML BETA */
  struct hash2 *partially_covered;
  HASH_FIND_STR(invertedPartiallyCoveredSequences,tag,partially_covered);
  if (partially_covered != NULL) {
    struct hash1 *curr, *tmp;
    element *elt,*elttmp;
    LL_FOREACH_SAFE(partially_covered->head, elt, elttmp){
      HASH_ITER(hh, partiallyCoveredSequences,curr,tmp){
        if (curr->key_idSeq == elt->idSeq){
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


void lreplay_report(const char* file,int line, int cond, int id, const char* tag, int nbBindings, ...) {

  if (!lreplay_file) return;

  cond = !!cond;

  cpt_access_labels+=1;

  if (nbBindings != 0) {
    fprintf(lreplay_file, "%d,%d,%s,%s:%u,%d", id, cond, tag, file, line, nbBindings);
    va_list ap;
    va_start(ap, nbBindings);
    int j;
    for (j = 0; j < nbBindings; j++) {
      fprintf(lreplay_file,",%s", va_arg(ap, char*));
      fprintf(lreplay_file,",%d", va_arg(ap, int));
    }
    fprintf(lreplay_file,"%s\n", "");
    va_end(ap);
    fflush(lreplay_file);
    return;
  }

  // Goal: prevent a lot of superfluous coverage line that may occur when
  // label are in loops
  // -> the output file will contains at most LREPLAY_EXPECTED*2 lines
  struct hash3 *lblStatus;
  HASH_FIND_INT(labelStatus,&id,lblStatus);
  if (lblStatus == NULL) {
    lblStatus = malloc(sizeof(struct hash3));
    lblStatus->key_idLabel=id;
    lblStatus->status = 1 + cond;
    HASH_ADD_INT(labelStatus,key_idLabel,lblStatus);
    fprintf(lreplay_file, "%u,%d,%s,%s:%u,%d\n", id, cond, tag, file, line, nbBindings);
  }
  else{
    switch(lblStatus->status){
    case 1: // Reached negatively
      if (!cond) return; // nothing new here

      // Becomes covered!
      fprintf(lreplay_file, "%u,%d,%s,%s:%u,%d\n", id, cond, tag, file, line, nbBindings);
      lblStatus->status = 3;
      break;

    case 2: // Reached positively (i.e. covered)
      if (cond) return; // nothing new here

      // Becomes reached negatively (are we interested in that?)
      fprintf(lreplay_file, "%u,%d,%s,%s:%u,%d\n", id, cond, tag, file, line, nbBindings);
      lblStatus->status = 3;
      break;

    case 3: // Reached negatively and positively
      return;
    }
  }
  fflush(lreplay_file);

}
