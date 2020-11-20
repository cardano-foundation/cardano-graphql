/*
 * Compute the size of a closure in memory.
 *
 * IMPLEMENTATION NOTES:
 *
 * Relevant files from the RTS:
 * - includes/rts/storage/ClosureMacros.h
 * - includes/rts/storage/Closures.h
 * - includes/rts/storage/ClosureTypes.h
 * - includes/rts/storage/InfoTables.h
 *
 * closure_size_untagged itself is based loosely on `scavenge_one` from `Scav.c`.
 */

#include "Rts.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashset.h"
#include "worklist.h"

// DEBUG should be defined when using the debug RTS, and should be defined
// _before_ including the headers.
// #define DEBUG 1

#include "HsFFI.h"

#ifdef DEBUG
StgOffset checkClosure(const StgClosure* p);
#endif

/*******************************************************************************
  Failures

  Right now knowledge of these values is duplicated C-side and Haskell-side;
  we could avoid that by using c2hs.
*******************************************************************************/

typedef enum {
  NO_FAILURE = 0,
  // Capacity of the worklist was insufficient
  WORK_LIST_FULL = 1,
  // Capacity of the visited set was insufficient
  VISITED_FULL = 2,
  // We failed to allocate the worklist or the visited set
  OUT_OF_MEMORY = 3,
  // In case of an unsupported closure type, the actual closure type is
  // the error code - UNSUPPORTED_CLOSURE
  UNSUPPORTED_CLOSURE = 4
} CountFailure;

/*******************************************************************************
  Size computation proper
*******************************************************************************/

/*
 * Push a pointer to the worklist, making sure to untag it.
 *
 * Returns `true` on success, `false` is the worklist is full.
 * If we already visited the value, we return `true` immediately.
 *
 * NOTE: Although the worklist already ensures that it will never push an
 * item that has already been added, we nonetheless need to check the visited
 * set also. For example, suppose we start with
 *
 *    A
 *   /|\
 *  / | \
 * B  |  C
 *  \ | /
 *   \|/
 *    D
 *
 * Then the worklist evolves as follows:
 *
 *   [A]
 * { pop off A; add B, D, C }
 *   [B, D, C]
 * { pop off B; notice D already added, push nothing }
 *   [D, C]
 * { pop off D; no children, nothing to push }
 *   [C]
 * { pop off C; notice D already visited, nothing to push, we're done. }
 *   []
 */
static bool push(WorkList* const work, HashSet* const visited, StgClosure * const tagged) {
#ifdef DEBUG
  checkClosure(tagged);
#endif

  // We can't mark it as `const * const` due to `UNTAG_CLOSURE`.
  StgClosure * const untagged = UNTAG_CLOSURE(tagged);

  if(hs_cardanoprelude_hashset_member(visited, untagged)) {
    return true;
  } else {
    return hs_cardanoprelude_worklist_push(work, untagged);
  }
}

/*
 * Pop a pointer off the worklist, and mark it as visited.
 *
 * Returns NULL and sets *err if the visited set is full.
 *
 * NOTE: The item we pop off cannot already have been visited:
 *
 * 1. When we pushed it, we checked that it had not already been visited, nor
 *    had it already been pushed.
 * 2. Any subsequent calls to push won't have added the same item (because
 *    we never push the same item more than once); and even if it had, that
 *    new item would have been added /after/ this one.
 * 3. Hence, by the time we pop it off, we can't yet have visited it.
 *
 * For this to work, we do need to be careful with ordering; if we start with
 *
 *    A <---\
 *   / \    |
 *  /   \---/
 * B
 *
 * The worklist should evolve as follows:
 *
 *   [A]
 * { pop off A, _mark A as visited_, and B (but not A, since already visited) }
 *   [B]
 * { pop off B, done }
 *   []
 */
static StgClosure const * const pop(WorkList* const work, HashSet* const visited, unsigned int* const err) {
  StgClosure const * const p = hs_cardanoprelude_worklist_pop(work);

  if(p != NULL) {
    Inserted inserted = hs_cardanoprelude_hashset_insert(visited, p);
    switch(inserted) {
      case HASHSET_INSERT_OK:
        return p;

      case HASHSET_INSERT_ALREADY_PRESENT:
        ASSERT(0); // Impossible (see above)
        return NULL;

      default:
      case HASHSET_INSERT_FULL:
        *err = VISITED_FULL;
        return NULL;
    }
  } else {
    return NULL;
  }
}

/*
 * Add all nested pointers to the worklist (non-recursively).
 *
 * We push the nested pointers in reverse order to the stack, so that we will
 * then _process_ them from left to right. This means we can process data types
 * that are deeply nested "to the right", such as
 *
 * > data List a = Nil | Cons a (List a)
 *
 * with constant stack space (think of it as a tail call optimization).
 *
 * `p` must be an untagged pointer.
 */
static CountFailure pushNested(WorkList* const work, HashSet* const visited, StgClosure const * const p) {
  const StgInfoTable* info = get_itbl(p);

  switch (info->type) {
    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    {
      StgThunk const * const q = (StgThunk const * const) p;

      for(int i = info->layout.payload.ptrs - 1; i >= 0; i--) {
        if(!push(work, visited, q->payload[i])) {
          return WORK_LIST_FULL;
        }
      }

      break;
    }

    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case CONSTR:
    case CONSTR_NOCAF:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case PRIM:
    {
      for(int i = info->layout.payload.ptrs - 1; i >= 0; i--) {
        if(!push(work, visited, p->payload[i])) {
          return WORK_LIST_FULL;
        }
      }

      break;
    }

    case THUNK_SELECTOR:
    {
      StgSelector const * const q = (StgSelector const * const) p;
      if(!push(work, visited, q->selectee)) {
        return WORK_LIST_FULL;
      }
      break;
    }

    // rts/sm/Evac.c points out that a blackhole can never point to another
    // indirection node during GC, but that the mutator might observe this.
    // We don't rely on this invariant, however, so it doesn't matter for us.

    case IND:
    case BLACKHOLE:
    case IND_STATIC:
    {
      StgInd const * const q = (StgInd const * const) p;
      if(!push(work, visited, q->indirectee)) {
        return WORK_LIST_FULL;
      }
      break;
    }

    case ARR_WORDS:
    {
      // No nested pointers
      break;
    }

    default:
    {
      return (UNSUPPORTED_CLOSURE + info->type);
      break;
    }
  }

  return NO_FAILURE;
}

/*
 * Compute the total size of the give closure
 *
 * The caller should check *err for an error code.
 */
 uint64_t hs_cardanoprelude_closureSize(unsigned int const workListCapacity, unsigned int const visitedInitCapacity, unsigned int const visitedMaxCapacity, unsigned int* const err, StgStablePtr const sp) {
    StgPtr const root = deRefStablePtr(sp);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(root));

    // Start optimistic
    *err = NO_FAILURE;

    // Accumulator for total size
    uint64_t acc = 0;

    // Initialize the worklist
    WorkList* work = hs_cardanoprelude_worklist_alloc(workListCapacity);
    if(work == NULL) {
      *err = OUT_OF_MEMORY;
      return 0;
    }

    // Initialize the visited set
    HashSet* visited = hs_cardanoprelude_hashset_alloc(visitedInitCapacity, visitedMaxCapacity);
    if(visited == NULL) {
      *err = OUT_OF_MEMORY;
      hs_cardanoprelude_worklist_free(work);
      return 0;
    }

    // Keep processing until the worklist is empty
    push(work, visited, (StgClosure*)root);
    StgClosure const * p;
    while((p = pop(work, visited, err))) {
      acc += closure_sizeW(p);
      *err = pushNested(work, visited, p);
      if(*err != NO_FAILURE) {
        break;
      }
    }

    hs_cardanoprelude_worklist_free(work);
    hs_cardanoprelude_hashset_free(visited);

    return acc;
}
