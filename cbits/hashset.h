#ifndef HS_CARDANOPRELUDE_HASHSET_H
#define HS_CARDANOPRELUDE_HASHSET_H

#include <stdbool.h>

/*
 * HashSet
 *
 * We use a [hash table](https://en.wikipedia.org/wiki/Hash_table) using
 * [open addressing](https://en.wikipedia.org/wiki/Open_addressing), with
 * simple [linear probing](https://en.wikipedia.org/wiki/Linear_probing).
 * The capacity of the hashset is fixed.
 *
 * By rights, hashsets that use open addressing should implement [lazy
 * deletion](https://en.wikipedia.org/wiki/Lazy_deletion), merely _marking_
 * elements as deleted when they are deleted and _later_ (during lookup) moving
 * elements to the first available marked-as-deleted spot. For simplicity's
 * sake, we only implement the first half of this scheme, marking but never
 * moving. This suffices for our purposes: we never delete elements from the
 * large hashset used for the "visited" set, so the problem is irrelevant; and
 * for the hashset used internally in the worklist (to determine if an element
 * has already been added) this optimization is not important because this
 * hashset is highly dynamic, so that new values will use the marked-as-deleted
 * locations.
 */

typedef struct HashSet_ {
  void const ** hsBuffer;
  unsigned int hsCapacity;
  unsigned int hsCount;
} HashSet;

typedef enum {
  HASHSET_INSERT_OK,
  HASHSET_INSERT_ALREADY_PRESENT,
  HASHSET_INSERT_FULL
} Inserted;

HashSet* hs_cardanoprelude_hashset_alloc(unsigned int const capacity);
bool     hs_cardanoprelude_hashset_member(HashSet* const set, void const * const p);
Inserted hs_cardanoprelude_hashset_insert(HashSet* const set, void const * const p);
void     hs_cardanoprelude_hashset_delete(HashSet* const set, void const * const p);
void     hs_cardanoprelude_hashset_free(HashSet* const set);

#endif
