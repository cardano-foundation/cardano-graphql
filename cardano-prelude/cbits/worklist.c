#include "worklist.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Create new worklist with the specified capacity
 *
 * Returns NULL if out of memory.
 */
WorkList* hs_cardanoprelude_worklist_alloc(unsigned int capacity) {
  WorkList* work = malloc(sizeof(WorkList));
  if(work == NULL) {
    return NULL;
  };

  void const ** stack = calloc(capacity, sizeof(void*));
  if(stack == NULL) {
    free(work);
    return NULL;
  }

  HashSet* set = hs_cardanoprelude_hashset_alloc(capacity, capacity);
  if(set == NULL) {
    free(work);
    free(stack);
    return NULL;
  };

  work->wlStack   = stack;
  work->wlHashSet = set;
  return work;
}

/*
 * Push a new element onto the worklist
 *
 * Returns
 * - `true`  if the value was successfully added
 * - `true`  if the value was _already_ added (without adding it again)
 * - `false` if the worklist is full and the value was not already added
 */
bool hs_cardanoprelude_worklist_push(WorkList* const work, void const * const p) {
  unsigned int count = work->wlHashSet->hsCount;

  Inserted inserted = hs_cardanoprelude_hashset_insert(work->wlHashSet, p);
  switch(inserted) {
    case HASHSET_INSERT_OK:
    {
      work->wlStack[count] = p;
      return true;
    }

    case HASHSET_INSERT_ALREADY_PRESENT:
    {
      return true;
    }

    default:
    case HASHSET_INSERT_FULL:
    {
      return false;
    }
  }
}

/*
 * Pop an element off the worklist
 *
 * Returns `NULL` if the worklist is empty.
 */
void const * const hs_cardanoprelude_worklist_pop(WorkList* const work) {
  unsigned int count = work->wlHashSet->hsCount;

  if(count > 0) {
    void const * const p = work->wlStack[count - 1];
    work->wlStack[count - 1] = NULL; // to aid debugging
    hs_cardanoprelude_hashset_delete(work->wlHashSet, p);
    return p;
  } else {
    return NULL;
  }
}

/*
 * Deallocate the worklist
 */
void hs_cardanoprelude_worklist_free(WorkList* const work) {
  hs_cardanoprelude_hashset_free(work->wlHashSet);
  free(work->wlStack);
  free(work);
}
