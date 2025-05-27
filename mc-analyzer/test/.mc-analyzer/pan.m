#define rand	pan_rand
#define pthread_equal(a,b)	((a)==(b))
#if defined(HAS_CODE) && defined(VERBOSE)
	#ifdef BFS_PAR
		bfs_printf("Pr: %d Tr: %d\n", II, t->forw);
	#else
		cpu_printf("Pr: %d Tr: %d\n", II, t->forw);
	#endif
#endif
	switch (t->forw) {
	default: Uerror("bad forward move");
	case 0:	/* if without executable clauses */
		continue;
	case 1: /* generic 'goto' or 'skip' */
		IfNotBlocked
		_m = 3; goto P999;
	case 2: /* generic 'else' */
		IfNotBlocked
		if (trpt->o_pm&1) continue;
		_m = 3; goto P999;

		 /* CLAIM SEM6_cond0 */
	case 3: // STATE 1 - _spin_nvr.tmp:21 - [((!(!((false_count_cond0>0)))&&!((true_count_cond0>0))))] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[3][1] = 1;
		if (!(( !( !((now.false_count_cond0>0)))&& !((now.true_count_cond0>0)))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 4: // STATE 3 - _spin_nvr.tmp:22 - [((!(!((true_count_cond0>0)))&&!((false_count_cond0>0))))] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported3 = 0;
			if (verbose && !reported3)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported3 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported3 = 0;
			if (verbose && !reported3)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported3 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[3][3] = 1;
		if (!(( !( !((now.true_count_cond0>0)))&& !((now.false_count_cond0>0)))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 5: // STATE 10 - _spin_nvr.tmp:27 - [(!((true_count_cond0>0)))] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported10 = 0;
			if (verbose && !reported10)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported10 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported10 = 0;
			if (verbose && !reported10)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported10 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[3][10] = 1;
		if (!( !((now.true_count_cond0>0))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 6: // STATE 15 - _spin_nvr.tmp:31 - [(!((false_count_cond0>0)))] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported15 = 0;
			if (verbose && !reported15)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported15 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported15 = 0;
			if (verbose && !reported15)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported15 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[3][15] = 1;
		if (!( !((now.false_count_cond0>0))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 7: // STATE 20 - _spin_nvr.tmp:33 - [-end-] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported20 = 0;
			if (verbose && !reported20)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported20 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported20 = 0;
			if (verbose && !reported20)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported20 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[3][20] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* CLAIM SEM4_var0 */
	case 8: // STATE 1 - _spin_nvr.tmp:12 - [(!((!((main._p==enddef_var0))||(!((init_count_var0>0))||(use_count_var0>0)))))] (6:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[2][1] = 1;
		if (!( !(( !((((int)((P0 *)Pptr(f_pid(0)))->_p)==19))||( !((now.init_count_var0>0))||(now.use_count_var0>0))))))
			continue;
		/* merge: assert(!(!((!((main._p==enddef_var0))||(!((init_count_var0>0))||(use_count_var0>0))))))(0, 2, 6) */
		reached[2][2] = 1;
		spin_assert( !( !(( !((((int)((P0 *)Pptr(f_pid(0)))->_p)==19))||( !((now.init_count_var0>0))||(now.use_count_var0>0))))), " !( !(( !((main._p==enddef_var0))||( !((init_count_var0>0))||(use_count_var0>0)))))", II, tt, t);
		/* merge: .(goto)(0, 7, 6) */
		reached[2][7] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 9: // STATE 10 - _spin_nvr.tmp:17 - [-end-] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported10 = 0;
			if (verbose && !reported10)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported10 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported10 = 0;
			if (verbose && !reported10)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported10 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[2][10] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* CLAIM SEM3_1_var0 */
	case 10: // STATE 1 - _spin_nvr.tmp:3 - [(!((!((main._p==enddef_var0))||(!((use_count_var0>0))||(init_count_var0>0)))))] (6:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[1][1] = 1;
		if (!( !(( !((((int)((P0 *)Pptr(f_pid(0)))->_p)==19))||( !((now.use_count_var0>0))||(now.init_count_var0>0))))))
			continue;
		/* merge: assert(!(!((!((main._p==enddef_var0))||(!((use_count_var0>0))||(init_count_var0>0))))))(0, 2, 6) */
		reached[1][2] = 1;
		spin_assert( !( !(( !((((int)((P0 *)Pptr(f_pid(0)))->_p)==19))||( !((now.use_count_var0>0))||(now.init_count_var0>0))))), " !( !(( !((main._p==enddef_var0))||( !((use_count_var0>0))||(init_count_var0>0)))))", II, tt, t);
		/* merge: .(goto)(0, 7, 6) */
		reached[1][7] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 11: // STATE 10 - _spin_nvr.tmp:8 - [-end-] (0:0:0 - 1)
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported10 = 0;
			if (verbose && !reported10)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported10 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported10 = 0;
			if (verbose && !reported10)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported10 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[1][10] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC main */
	case 12: // STATE 1 - verification.pml:22 - [cond0 = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[0][1] = 1;
		(trpt+1)->bup.oval = cond0;
		cond0 = 0;
#ifdef VAR_RANGES
		logval("cond0", cond0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 13: // STATE 2 - verification.pml:23 - [cond0 = 1] (0:0:1 - 1)
		IfNotBlocked
		reached[0][2] = 1;
		(trpt+1)->bup.oval = cond0;
		cond0 = 1;
#ifdef VAR_RANGES
		logval("cond0", cond0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 14: // STATE 5 - verification.pml:25 - [isdef_var0 = 1] (0:0:1 - 3)
		IfNotBlocked
		reached[0][5] = 1;
		(trpt+1)->bup.oval = ((int)isdef_var0);
		isdef_var0 = 1;
#ifdef VAR_RANGES
		logval("isdef_var0", ((int)isdef_var0));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 15: // STATE 7 - verification.pml:26 - [init_count_var0 = (init_count_var0+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][7] = 1;
		(trpt+1)->bup.oval = now.init_count_var0;
		now.init_count_var0 = (now.init_count_var0+1);
#ifdef VAR_RANGES
		logval("init_count_var0", now.init_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 16: // STATE 8 - verification.pml:26 - [init_count_var0 = init_count_var0] (0:0:1 - 1)
		IfNotBlocked
		reached[0][8] = 1;
		(trpt+1)->bup.oval = now.init_count_var0;
		now.init_count_var0 = now.init_count_var0;
#ifdef VAR_RANGES
		logval("init_count_var0", now.init_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 17: // STATE 10 - verification.pml:28 - [true_count_cond0 = (true_count_cond0+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][10] = 1;
		(trpt+1)->bup.oval = now.true_count_cond0;
		now.true_count_cond0 = (now.true_count_cond0+1);
#ifdef VAR_RANGES
		logval("true_count_cond0", now.true_count_cond0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 18: // STATE 11 - verification.pml:29 - [use_count_var0 = (use_count_var0+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][11] = 1;
		(trpt+1)->bup.oval = now.use_count_var0;
		now.use_count_var0 = (now.use_count_var0+1);
#ifdef VAR_RANGES
		logval("use_count_var0", now.use_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 19: // STATE 15 - verification.pml:32 - [true_count_cond0 = (true_count_cond0+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][15] = 1;
		(trpt+1)->bup.oval = now.true_count_cond0;
		now.true_count_cond0 = (now.true_count_cond0+1);
#ifdef VAR_RANGES
		logval("true_count_cond0", now.true_count_cond0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 20: // STATE 16 - verification.pml:33 - [use_count_var0 = (use_count_var0+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][16] = 1;
		(trpt+1)->bup.oval = now.use_count_var0;
		now.use_count_var0 = (now.use_count_var0+1);
#ifdef VAR_RANGES
		logval("use_count_var0", now.use_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 21: // STATE 19 - verification.pml:35 - [isdef_var0 = 0] (0:0:1 - 2)
		IfNotBlocked
		reached[0][19] = 1;
		(trpt+1)->bup.oval = ((int)isdef_var0);
		isdef_var0 = 0;
#ifdef VAR_RANGES
		logval("isdef_var0", ((int)isdef_var0));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 22: // STATE 20 - verification.pml:35 - [use_count_var0 = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[0][20] = 1;
		(trpt+1)->bup.oval = now.use_count_var0;
		now.use_count_var0 = 0;
#ifdef VAR_RANGES
		logval("use_count_var0", now.use_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 23: // STATE 21 - verification.pml:35 - [init_count_var0 = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[0][21] = 1;
		(trpt+1)->bup.oval = now.init_count_var0;
		now.init_count_var0 = 0;
#ifdef VAR_RANGES
		logval("init_count_var0", now.init_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 24: // STATE 22 - verification.pml:35 - [destroy_count_var0 = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[0][22] = 1;
		(trpt+1)->bup.oval = destroy_count_var0;
		destroy_count_var0 = 0;
#ifdef VAR_RANGES
		logval("destroy_count_var0", destroy_count_var0);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 25: // STATE 23 - verification.pml:36 - [-end-] (0:0:0 - 1)
		IfNotBlocked
		reached[0][23] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */
	case  _T5:	/* np_ */
		if (!((!(trpt->o_pm&4) && !(trpt->tau&128))))
			continue;
		/* else fall through */
	case  _T2:	/* true */
		_m = 3; goto P999;
#undef rand
	}

