#include "hashset.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/*******************************************************************************
  Hashset
*******************************************************************************/

/*
 * Deleted element
 *
 * See discussion of deletion in the header.
 */
#define DELETED ((void*) -1)

/*
 * Allocate hash set of the specified capacity
 *
 * Returns NULL if out of memory.
 */
HashSet* hs_cardanoprelude_hashset_alloc(unsigned int const capacity) {
  HashSet* set = malloc(sizeof(HashSet));

  if(set == NULL) {
    return NULL;
  }

  void const ** buffer = calloc(capacity, sizeof(void*));
  if(buffer == NULL) {
    free(set);
    return NULL;
  }

  set->hsBuffer   = buffer;
  set->hsCapacity = capacity;
  set->hsCount    = 0;
  return set;
}

/*
 * Compute preferred bucket for the given pointer
 *
 * This is modelled after hashWord in the Haskell RTS.
 */
static unsigned int preferredBucket(HashSet* const set, void const * const p) {
  intptr_t key = (intptr_t) p / sizeof(void*);
  return key % set->hsCapacity;
}

/*
 * Check if the value is a member of the hashset.
 */
bool hs_cardanoprelude_hashset_member(HashSet* const set, void const * const p) {
  unsigned int const preferred = preferredBucket(set, p);
  unsigned int       bucket    = preferred;

  do {
    void const * const q = set->hsBuffer[bucket];
    if(q == p) {
      return true;
    } else if(q == NULL)  {
      return false;
    } else {
      bucket = (bucket + 1) % set->hsCapacity;
    }
  } while(bucket != preferred);

  return false;
}

/*
 * Insert value into the hashset
 */
Inserted hs_cardanoprelude_hashset_insert(HashSet* const set, void const * const p) {
  unsigned int const preferred = preferredBucket(set, p);
  unsigned int       bucket    = preferred;

  // We do not check the size first, because if we find the value already
  // present, we return successfully without modifying the hashset.

  do {
    void const * const q = set->hsBuffer[bucket];
    if(q == p) {
      return HASHSET_INSERT_ALREADY_PRESENT;
    } else if(q == NULL || q == DELETED) {
      set->hsBuffer[bucket] = p;
      set->hsCount++;
      return HASHSET_INSERT_OK;
    } else {
      bucket = (bucket + 1) % set->hsCapacity;
    }
  } while(bucket != preferred);

  return HASHSET_INSERT_FULL;
}

/*
 * Delete value from the hashset
 */
void hs_cardanoprelude_hashset_delete(HashSet* const set, void const * const p) {
  unsigned int const preferred = preferredBucket(set, p);
  unsigned int       bucket    = preferred;

  do {
    void const * const q = set->hsBuffer[bucket];
    if(q == p) {
      set->hsBuffer[bucket] = DELETED;
      set->hsCount--;
      return;
    } else if(q == NULL) {
      return;
    } else {
      bucket = (bucket + 1) % set->hsCapacity;
    }
  } while(bucket != preferred);
}

/*
 * Deallocate the hashset
 */
void hs_cardanoprelude_hashset_free(HashSet* set) {
  free(set->hsBuffer);
  free(set);
}
