#include "hashset.h"

#include <assert.h>
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
HashSet* hs_cardanoprelude_hashset_alloc(unsigned int const initCapacity, unsigned int const maxCapacity) {
  HashSet* set = malloc(sizeof(HashSet));

  if(set == NULL) {
    return NULL;
  }

  void const ** buffer = calloc(initCapacity, sizeof(void*));
  if(buffer == NULL) {
    free(set);
    return NULL;
  }

  set->hsBuffer      = buffer;
  set->hsCapacity    = initCapacity;
  set->hsMaxCapacity = maxCapacity;
  set->hsCount       = 0;
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
 * Double the capacity of the hashset
 *
 * Returns `false` if out of memory.
 */
static bool doubleCapacity(HashSet* const set) {
  unsigned int const oldCapacity = set->hsCapacity;
  unsigned int const newCapacity = oldCapacity * 2;
  if(newCapacity > set->hsMaxCapacity) {
    return false;
  }

  void const ** oldBuffer = set->hsBuffer;
  void const ** newBuffer = calloc(newCapacity, sizeof(void*));
  if(newBuffer == NULL) {
    return false;
  }

  set->hsBuffer   = newBuffer;
  set->hsCapacity = newCapacity;
  set->hsCount    = 0; // Set count back to 0 until we fill it back up
  for(int i = 0; i < oldCapacity; i++) {
    void const * const q = oldBuffer[i];
    if(q != NULL && q != DELETED) {
      Inserted inserted = hs_cardanoprelude_hashset_insert(set, q);
      assert(inserted == HASHSET_INSERT_OK);
    }
  }
  free(oldBuffer);
  return true;
}

/*
 * Insert value into the hashset
 */
Inserted hs_cardanoprelude_hashset_insert(HashSet* const set, void const * const p) {
  // We can't simply check the size first, and error out if the hashset is full,
  // because if the value is already present, we should return successfully
  // (without modifying the hashset).

  // However, if the hashset is near capacity, its performance will degrade
  // significantly. Assuming an optimal distribution of keys, there will still
  // be a NULL value in between every pair of keys when the hashset is 50% full,
  // which is still optimal (lookup and insert are really O(1)). After that,
  // however, we start generating runs of keys and so performance starts to
  // degrade. For this reason, we double the size of the hashset eagerly, as
  // soon as we hit 50%.

  if(set->hsCount >= set->hsCapacity / 2) {
    doubleCapacity(set);
  }

  unsigned int const preferred = preferredBucket(set, p);
  unsigned int       bucket    = preferred;

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
