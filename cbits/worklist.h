#ifndef HS_CARDANOPRELUDE_WORKLIST_H
#define HS_CARDANOPRELUDE_WORKLIST_H

#include <stdbool.h>

#include "hashset.h"

/*******************************************************************************
  Worklist

  We implement the worklist as a stack, in order to do a depth-first traversal
  (which requires O(height) space, as opposed to O(n) space for breadth-first).
  The stack grows upwards, and has a fixed maximum number of elements.

  We include a hashset to avoid pushing items onto the worklist more than
  once, and delegate keeping track of capacity and count to that hashset.
*******************************************************************************/

typedef struct WorkList_ {
  void const ** wlStack;
  HashSet* wlHashSet;
} WorkList;

WorkList*          hs_cardanoprelude_worklist_alloc(unsigned int const capacity);
bool               hs_cardanoprelude_worklist_push(WorkList* const work, void const * const p);
void const * const hs_cardanoprelude_worklist_pop(WorkList* const work);
void               hs_cardanoprelude_worklist_free(WorkList* const work);

#endif
