/*
 * profile-mcdc.c
 *
 *  Created on: 13.08.2015
 *      Author: holger
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "flags.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "coverage.h"
#include "value-prof.h"
#include "tree.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "dumpfile.h"
#include "cgraph.h"
#include "profile-mcdc.h"

#include "stringpool.h"
#include "tree-pretty-print.h"
#include "tree-ssanames.h"
#include "tree-nested.h"

#define RESULT_PATTERN(relevants, result) ((relevants << 16) | result)
#define PATT_MERGE(left, bits_l, right, bits_r) ((left << bits_r) | right)

#define PATT_RELEVANT(p, mask) !!(p & (mask << 16))
#define PATT_VALUE(p, mask)    !!(p & mask)

#define Create_mask(bits, pos) (((1<<(bits))-1)<< (pos))


decision_tree_list_t *decision_tree_root;
static GTY(()) tree gcov_type_node;
static GTY(()) tree tree_mcdc_profiler_fn;
static int profile_mcdc_counter_id;

void for_each_mcdc_advance_walker(function *func, decision_tree_list_t **walker)
{
   *walker = (*walker)->next;
   while (*walker && (*walker)->assoc_fun != func) *walker = (*walker)->next;
}
decision_tree_t *build_decision_tree(tree pred, int *counter)
{
   decision_tree_t *res = NULL;
   decision_tree_t *lhs, *rhs;
   decision_pattern_t    *patt;

   enum tree_code code = TREE_CODE(pred);
   switch (code)
   {
   case TRUTH_ANDIF_EXPR:
   case TRUTH_ORIF_EXPR:
   {
	   res  = XNEW(decision_tree_t);
	   res->patlist = patt = XNEW(decision_pattern_t);
       lhs = build_decision_tree(TREE_OPERAND(pred,0),counter);
       rhs = build_decision_tree(TREE_OPERAND(pred,1),counter);
       res->tree.lhs = &(lhs->tree);
       res->tree.rhs = &(rhs->tree);
       res->tree.bits_from_lhs = lhs->patlist->bits;
       res->tree.bits_from_rhs = rhs->patlist->bits;
	   patt->bits = lhs->patlist->bits + rhs->patlist->bits;

       /* Merge the decision patterns */
       if (TREE_CODE(pred) == TRUTH_ANDIF_EXPR)
       {
    	  int i_l, i_r, i_o;
    	  res->tree.op = decision_and;
          /* Number of results: (left false) + (left true * right all) */
          patt->nr_patterns_false = lhs->patlist->nr_patterns_false
       		                     + (lhs->patlist->nr_patterns_true  * rhs->patlist->nr_patterns_false);
          patt->nr_patterns_true  = lhs->patlist->nr_patterns_true  * rhs->patlist->nr_patterns_true;
          patt->patterns = XNEWVEC(unsigned long, patt->nr_patterns_false + patt->nr_patterns_true);

          i_o=0;
          For_each_false_pattern(i_l, lhs->patlist)
             patt->patterns[i_o++] = PATT_MERGE(lhs->patlist->patterns[i_l], lhs->patlist->bits, 0, rhs->patlist->bits);
          For_each_true_pattern(i_l, lhs->patlist)
        	 For_each_false_pattern(i_r, rhs->patlist)
        		 patt->patterns[i_o++] = PATT_MERGE(lhs->patlist->patterns[i_l], lhs->patlist->bits,
        				                            rhs->patlist->patterns[i_r], rhs->patlist->bits);
          For_each_true_pattern(i_l, lhs->patlist)
        	 For_each_true_pattern(i_r, rhs->patlist)
        		 patt->patterns[i_o++] = PATT_MERGE(lhs->patlist->patterns[i_l], lhs->patlist->bits,
        				                            rhs->patlist->patterns[i_r], rhs->patlist->bits);
       }
       else
       {
     	  int i_l, i_r, i_o;
     	   res->tree.op = decision_or;
           /* Number of results: (left true) + (left false * right all) */
           patt->nr_patterns_false = lhs->patlist->nr_patterns_false * rhs->patlist->nr_patterns_false;
           patt->nr_patterns_true  = lhs->patlist->nr_patterns_true  *(rhs->patlist->nr_patterns_false + rhs->patlist->nr_patterns_true);
           patt->patterns = XNEWVEC(unsigned long, patt->nr_patterns_false + patt->nr_patterns_true);

           i_o=0;
           For_each_false_pattern(i_l, lhs->patlist)
        	 For_each_false_pattern(i_r, rhs->patlist)
        		 patt->patterns[i_o++] = PATT_MERGE(lhs->patlist->patterns[i_l], lhs->patlist->bits,
         				                            rhs->patlist->patterns[i_r], rhs->patlist->bits);
           For_each_false_pattern(i_l, lhs->patlist)
         	 For_each_true_pattern(i_r, rhs->patlist)
         		 patt->patterns[i_o++] = PATT_MERGE(lhs->patlist->patterns[i_l], lhs->patlist->bits,
         				                            rhs->patlist->patterns[i_r], rhs->patlist->bits);
           For_each_true_pattern(i_l, lhs->patlist)
           	   patt->patterns[i_o++] = PATT_MERGE(lhs->patlist->patterns[i_l], lhs->patlist->bits, 0, rhs->patlist->bits);
       }
       /* Release l+r patternlist after merge */
       XDELETE(lhs->patlist);
       XDELETE(rhs->patlist);
       break;
   }
   case COND_EXPR:
   case NE_EXPR:
   case EQ_EXPR:
   case GT_EXPR:
   case GE_EXPR:
   case LT_EXPR:
   case LE_EXPR:
	   res = XNEW(decision_tree_t);
	   res->patlist = patt = XNEW(decision_pattern_t);
	   patt->nr_patterns_false = 1;
	   patt->nr_patterns_true  = 1;
	   patt->bits = 1;
	   patt->patterns = XNEWVEC(unsigned long, 2);
	   patt->patterns[0] = RESULT_PATTERN(1,FALSE);   /* Conditional expression generates 2 results T/F */
	   patt->patterns[1] = RESULT_PATTERN(1,TRUE);

	   res->tree.op = decision_basic;
	   res->tree.bits_from_lhs = 1;
	   res->tree.bits_from_rhs = 0;
	   res->tree.lhs = res->tree.rhs = NULL;
	   ++*counter;
	   break;
   default:
	   fprintf(stderr, "Unhandled conditional:");
	   print_generic_stmt(stderr, pred,0);
	   break;
   }
   return res;
}

/*
Create a human-readable string from a pattern, using the decision tree
e.g.
0xe0008 = (T&&F) || (F && *)

1)    mask = 1111
  2l / 2r
1.1)  mask = 1100
  1l / 1r
1.1.1) mask = 1000 1l/0r
1.1.2) mask = 0100 1l/0r
...

1.2)  mask = 0011
  1l / 1r
1.2.1 mask = 0010

*/
const char *decision_assemble_string(unsigned long patt, decision_t *t, unsigned mask_offset, unsigned mask_length)
{
   switch(t->op)
   {
   case decision_and:
   case decision_or:
   {
	   const char *left  = decision_assemble_string(patt, t->lhs, mask_offset+t->bits_from_rhs, mask_length - t->bits_from_rhs);
	   const char *right = decision_assemble_string(patt, t->rhs, mask_offset,                  mask_length - t->bits_from_lhs);
	   size_t length = strlen(left)+strlen(right)+9;
	   char *result = XNEWVEC(char, length);
	   snprintf(result, length, (t->op == decision_and)?"(%s) && (%s)":"(%s) || (%s)", left,right);
	   return result;
   }
   default: // case decision_basic
   {
	   unsigned long mask = 1 << mask_offset;  // mask should only be one bit now.
	   if (PATT_RELEVANT(patt, mask))
		   return (PATT_VALUE(patt, mask))?"T":"F";
	   else
		   return "*";
	   if (patt & mask) return "X"; else return "*";
   }
   }
}

const char *decision_tree_serialize(decision_t *t)
{
	switch(t->op)
	{
	case decision_and:
	case decision_or:
	{
		const char *left  = decision_tree_serialize(t->lhs);
		const char *right = decision_tree_serialize(t->rhs);
		size_t length = strlen(left)+strlen(right)+2;
		char *result = XNEWVEC(char, length);
		snprintf(result, length, (t->op == decision_and)?"%s%s&":"%s%s|",
				                    left,right);
		return result;
	}
	default: // case decision_basic
	{
		return "x";
	}
	}
}
void pp_decision_tree(pretty_printer *p, decision_tree_t *t)
{
	int idx;
	unsigned long patt;
	for (idx=0; idx < (t->patlist->nr_patterns_false + t->patlist->nr_patterns_true); idx++)
	{
	   patt = t->patlist->patterns[idx];
	   pp_printf(p, " #%d: %#x = %s = %c\n",idx, patt,
			   decision_assemble_string(patt, &t->tree,0, t->patlist->bits),  // == t->tree.bits_from_lhs+t->tree.bits_from_rhs
			   (idx >= t->patlist->nr_patterns_false) ?'T':'F'
			   );
	}
}

/* Called within: Gimplifier / shortcut_cond_expr which dissects complex expressions
 * Context of cfun is set
 */
void create_mcdc_expression(tree old_cond_node, tree new_cond_node, int counter)
{
	if ((TREE_CODE (old_cond_node) == TRUTH_ANDIF_EXPR) /* Require MC/DC instrumentation only for && and  || */
	|| (TREE_CODE (old_cond_node) == TRUTH_ORIF_EXPR))
	{
		/* TODO: Let the magic begin. */
		pretty_printer buffer;
		decision_tree_list_t *decision, *last;
		pp_needs_newline (&buffer) = true;
		buffer.buffer->stream = fopen("conditions.txt","a");

		pp_printf(&buffer, "Analyze condition:");
		dump_generic_node(&buffer, old_cond_node, 0,0,0);

		decision = XNEW(decision_tree_list_t);
		decision->counter_start = counter;
		decision->decision_tree = build_decision_tree(old_cond_node, &counter);
		decision->next = NULL;
		decision->loc  = EXPR_LOCATION(old_cond_node);
		decision->assoc_fun = cfun;

		pp_newline (&buffer);
		pp_printf(&buffer, "  converted into:");
		dump_generic_node(&buffer, new_cond_node, 0,0,0);
		pp_newline (&buffer);
		if (Decision_tree_root == NULL) Decision_tree_root = decision;
		else
		{
			for (last = Decision_tree_root; last->next != 0; last = last->next);
			last->next = decision;
		}
		/* dump */
		pp_decision_tree(&buffer, decision->decision_tree);
		pp_newline_and_flush (&buffer);
		fclose(buffer.buffer->stream);
	}
}

void gimple_init_mcdc_profiler(tree body)
{
#if 0
	tree mcdc_profile_fn_type;
	// Muß eigentlich nur 1x überhaupt gemacht werden.
	mcdc_profile_fn_type
	    = build_function_type_list(void_type_node,
              integer_type_node, integer_type_node,
              NULL_TREE);

    tree_mcdc_profiler_fn
        = build_fn_decl("__gcov_mcdc_case", mcdc_profile_fn_type);
    TREE_NOTHROW (tree_mcdc_profiler_fn) = 1;
    TREE_SIDE_EFFECTS (tree_mcdc_profiler_fn) = 1;
    DECL_ATTRIBUTES (tree_mcdc_profiler_fn)
             = tree_cons (get_identifier ("leaf"), NULL,
                    DECL_ATTRIBUTES (tree_mcdc_profiler_fn));
	DECL_ASSEMBLER_NAME (tree_mcdc_profiler_fn); //   notwendig ??
#endif
    profile_mcdc_counter_id = 0;
    coverage_counter_alloc (GCOV_COUNTER_MCDC, 3);
}

int get_instrument_mcdc__startid(void)
{
	return profile_mcdc_counter_id;
}
/* insert decision tree instrumentation into program tree
 * when called: gimplify_expr()
 * state of the program: tree
 */
#if 0
void gimple_instrument_mcdc__init(gimple_seq *pre_p, tree expr)
{
	tree path_var,mask1_var, mask2_var, zero,one,viel;
	gimple_stmt_iterator gsi = gsi_last (*pre_p);

//	if (!gcov_type_node)
//		gcov_type_node = get_gcov_type ();
	path_var  = build_decl(EXPR_LOCATION(expr), VAR_DECL, get_identifier("PROF_mcdc_path"), integer_type_node);
	mask1_var = build_decl(EXPR_LOCATION(expr), VAR_DECL, get_identifier("PROF_mcdc_mask1"), integer_type_node);
	mask2_var = build_decl(EXPR_LOCATION(expr), VAR_DECL, get_identifier("PROF_mcdc_mask2"), integer_type_node);
	zero = build_int_cst (integer_type_node, 0);
	one  = build_int_cst (integer_type_node, 1);
	viel = build_int_cst (integer_type_node, 0x10000);

	// need to get the last instruction before the start of expression
	gimple_seq_add_stmt (pre_p,gimple_build_assign (path_var, zero));
	gimple_seq_add_stmt (pre_p,gimple_build_assign (mask1_var, one));
	gimple_seq_add_stmt (pre_p,gimple_build_assign (mask2_var, viel));
}
#endif

#if 1
tree instrument_mcdc__callnode(tree expr)
{
   tree gcov_type_tmp_var;

   return build2(PREINCREMENT_EXPR,gcov_type_node,
            tree_coverage_counter_ref (GCOV_COUNTER_MCDC, profile_mcdc_counter_id++),
            build_int_cst(gcov_type_node,1));
   // return  build_call_nary(void_type_node, build_addr(tree_mcdc_profiler_fn, expr),2, build_int_cst(integer_type_node,0), build_int_cst(integer_type_node, profile_mcdc_counter_id++));
}
#else
void instrument_mcdc__callnode(tree expr, int case_id)
{
    gimple_gen_mcdc_profiler(expr, case_id);
}
#endif
void
gimple_gen_mcdc_profiler (gimple_stmt_iterator gsi, int case_id)
{
  tree ref, one, gcov_type_tmp_var;
  gimple stmt1, stmt2, stmt3;

  ref = tree_coverage_counter_ref (GCOV_COUNTER_MCDC, case_id);
  one = build_int_cst (gcov_type_node, 1);
  gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
					  NULL, "PROF_mcdc_counter");
  stmt1 = gimple_build_assign (gcov_type_tmp_var, ref);     // PROF_edge_counter = <arc counter #edgeno>
  gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
					  NULL, "PROF_mcdc_counter");
  stmt2 = gimple_build_assign_with_ops (PLUS_EXPR, gcov_type_tmp_var, // expr: PROF_edge_counter + 1
					gimple_assign_lhs (stmt1), one);
  stmt3 = gimple_build_assign (ref, gimple_assign_lhs (stmt2));  // unterschlagen: "unshare_expr" <arc counter #edgeno> = <expr>
  gsi_insert_before (&gsi, stmt3,GSI_NEW_STMT);
  gsi_insert_before (&gsi, stmt2,GSI_NEW_STMT);
  gsi_insert_before (&gsi, stmt1,GSI_NEW_STMT);
}
