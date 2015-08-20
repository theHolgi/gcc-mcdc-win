/*
 * profile_mcdc.h
 *
 *  Created on: 15.08.2015
 *      Author: holger
 */

#ifndef PROFILE_MCDC_H_
#define PROFILE_MCDC_H_

enum decision_type {
	decision_and,
	decision_or,
	decision_basic
};

typedef struct decision_s
{
	enum decision_type  op;
	int                 bits_from_lhs;
	int                 bits_from_rhs;
	struct decision_s  *lhs;
	struct decision_s  *rhs;
} decision_t;

typedef struct
{
	int           bits;
	int           nr_patterns_false;
	int           nr_patterns_true;
	unsigned long *patterns;   /* Array contains first all false patterns, then all true patterns. */
} decision_pattern_t;

typedef struct
{
	decision_t         tree;
	decision_pattern_t *patlist;
} decision_tree_t;

typedef struct decision_tree_list_s {
    location_t           loc;
    decision_tree_t     *decision_tree;
    function            *assoc_fun;
    int                  counter_start;
    struct decision_tree_list_s *next;
} decision_tree_list_t;

extern decision_tree_list_t *decision_tree_root;
// extern GTY(()) tree tree_mcdc_profiler_fn;

#define Decision_tree_root (decision_tree_root)

/* Walker */
extern void for_each_mcdc_advance_walker(function *func, decision_tree_list_t **walker);
#define FOR_EACH_MCDC_STRUCT(func, walker) \
  walker = Decision_tree_root; while (walker && walker->assoc_fun != func) walker = walker->next; \
  for (; walker; for_each_mcdc_advance_walker(func, &walker))

/** Iterate idx over pattern array x */
#define For_each_false_pattern(idx, x) for(idx=0; idx< x->nr_patterns_false; idx++)
#define For_each_true_pattern(idx, x)  for(idx=x->nr_patterns_false; idx< x->nr_patterns_false + x->nr_patterns_true; idx++)

/** Give a representation of the decision tree for gcno */
extern const char *decision_tree_serialize(decision_t *t);

extern void gimple_init_mcdc_profiler(tree func);
extern int get_instrument_mcdc__startid(void);

extern tree instrument_mcdc__callnode(tree expr);

// extern tree instrument_mcdc__bc(tree expr);

#endif /* PROFILE_MCDC_H_ */
