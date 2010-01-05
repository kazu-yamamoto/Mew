/*
 * pattern.c
 *
 * Author:  Kazu Yamamoto <Kazu@Mew.org>
 * Created: Oct 17, 2000
 *
 * Code:
 */

#include "mew.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/****************************************************************
 * 
 * Structures
 *
 */

enum ptype {
	P_NUL, /* terminator */
	P_EXP, /* abstracted expression */
	P_STR, /* str, "str", "\"" */
	P_EQ,  /* =   case-insensitive */
	P_CEQ, /* ==  case-sensitive */
	P_NEQ, /* !=  case-insensitive */
	P_CNE ,/* !== case-sensitive */
	P_AND, /* &, && */
	P_OR,  /* |, || */
	P_NOT, /* ! */
	P_LSQ, /* ( */
	P_RSQ  /* ) */
};

struct pat {
	enum ptype typ;  /* See above */
	struct pat *prv; /* Doubly linked list */
	struct pat *nxt; /* Doubly linked list */
	struct pat *dwn; /* For P_EXP and P_NOT */
	char *key;       /* For P_STR, P_EQ, P_NEQ */
	char *val;       /* P_EQ, P_NEQ */
};

/****************************************************************
 *
 * prototype
 *
 */

private void Pinit(char *);
private int  Getc(void);
private void Ungetc(void);
private int  token(char **);
private struct pat *get_pat(void);
private struct pat *exp_eq(struct pat *);
private struct pat *exp_sq(struct pat *);
private struct pat *exp_not(struct pat *);
private struct pat *exp_lao(struct pat *, enum ptype);
private struct pat *exp_exp(struct pat *);
private struct pat *parse_pattern(char *);
private int eval_exp(struct pat *);

/****************************************************************
 * 
 * Utilities for lexical analyzer
 *
 */

char *Pvec;
int  Ppos;
int  Plim;

private void
Pinit(char *p) {
	Pvec = p;
	Ppos = 0;
	Plim = strlen(Pvec);
}


private int
Getc() {
	if (Ppos < Plim)
		return Pvec[Ppos++];
	else 
		return EOF;
	/* Getc() is to be called even after Getc() returned EOF. */
}

private void
Ungetc() {
	if (Ppos > 0)
		Ppos--;
	else
		warn_exit("ungetc is impossible.");
}

/****************************************************************
 * 
 * Lexical analyzer
 *
 */

private char Keyval[BUFSIZ];

private int
token(char **ret) {
	int c;
	int type = P_NUL, i = 0, quoted = NO;
	*ret = NULL;
	
	while ((c = Getc()) != EOF) {
		switch (c) {
		case SP:
		case TAB:
			continue;
		case '&':
			type = P_AND;
			break;
		case '|':
			type = P_OR;
			break;
		case '!':
			type = P_NOT;
			break;
		case EQ:
			type = P_EQ;
			break;
		case '(':
			return P_LSQ;
		case ')':
			return P_RSQ;
			break;
		case '"':
			type = P_STR;
			quoted = YES;
			break;
		case '\\':
			if ((c = Getc()) == EOF)
				warn_exit("broken escape.");
			/* fall through */
		default:
			type = P_STR;
			Keyval[i++] = c;
			break;
		}
		break;
	}

	if (quoted == YES) {
		while ((c = Getc()) != EOF) {
			switch (c) {
			case '\\':
				if ((c = Getc()) == EOF)
					warn_exit("broken escape.");
				Keyval[i++] = c;
				break;
			case '"':
				goto allocstr;
			default:
				Keyval[i++] = c;
				break;
			}
		}
		warn_exit("broken quoted string.");
	}

	while ((c = Getc()) != EOF) {
		switch (c) {
		case SP:
		case TAB:
			break;
		case '&':
			if (type != P_AND)
				Ungetc();
			break;
		case '|':
			if (type != P_OR)
				Ungetc();
			break;
		case '!':
			Ungetc();
			break;
		case EQ:
			if (type == P_EQ) {
				type = P_CEQ;
				break;
			} else if (type == P_NOT) {
				if ((c = Getc()) == EQ) {
					type = P_CNE;
				} else {
					type = P_NEQ;
					Ungetc();
				}
				break;
			} else {
				Ungetc();
				break;
			}
		case '(':
			Ungetc();
			break;
		case ')':
			Ungetc();
			break;
		case '\\':
			if (type != P_STR) {
				Ungetc();
				break;
			}
			if ((c = Getc()) == EOF)
				warn_exit("broken escape.");
			/* fall through */
		default:
			if (type != P_STR) {
				Ungetc();
				break;
			}
			Keyval[i++] = c;
			continue;
		}
		break;
	}

 allocstr:
	if (type == P_STR) {
		Keyval[i] = NUL;
		STRDUP(*ret, Keyval);
	}
	return type;
}

/****************************************************************
 * 
 * Utilities for parser
 *
 */

private struct pat *
get_pat() {
	struct pat *p;
	MALLOC(p, sizeof(struct pat)); 
	memset(p, 0, sizeof(struct pat));
	return p;
}

/****************************************************************
 * 
 * Parser
 *
 */

/*
 * Both key = val and key != val is the most strong. 
 * This function assumes to be called just once as pre-process
 * before exp_exp().
 *
 *  P_STR P_EQ P_STR  =>  P_EXP
 *   key   val              v
 *                        P_EQ
 *                       key val
 */

private struct pat *
exp_eq(struct pat *top) {
	struct pat *n, *p, *q = top;

	while (q != NULL)
		if (q->typ == P_EQ  || q->typ == P_CEQ ||
		    q->typ == P_NEQ || q->typ == P_CNE) {
			if (q->prv == NULL || q->nxt == NULL)
				warn_exit("broken eq/neq.");
			if (q->prv->typ != P_STR || q->nxt->typ != P_STR)
				warn_exit("broken eq/neq.");
			n = get_pat();
			p = q->prv;
			n->typ = q->typ;
			n->key = p->key;
			n->val = q->nxt->key;
			p->typ = P_EXP;
			p->dwn = n;
			p->key = NULL;
			q = q->nxt->nxt;
			if (q != NULL) q->prv = p;
			free(p->nxt->nxt);
			free(p->nxt);
			p->nxt = q;
		} else
			q = q->nxt;
	return top;
}

/*
 * Making square bracket to one P_EXP.
 *
 * X P_LSQ Y Z P_RSQ Z => X P_EXP Z
 *                            v
 *                            Y Z   (will be parsed by exp_exp())
 */

private struct pat *
exp_sq(struct pat *top) {
	struct pat *p = NULL, *q = top, *n;
	int level = 0;
	while (q != NULL) {
		switch (q->typ) {
		case P_LSQ:
			if (level == 0) p = q;
			level++;
			break;
		case P_RSQ:
			level--;
			if (level < 0) warn_exit("too many ')'.");
			if (level == 0) {
				if (p->nxt == q)
					warn_exit("empty '()'.");
				n = p->nxt;
				n->prv = NULL;
				q->prv->nxt = NULL;
				n = exp_exp(n);
				if (p->prv == NULL)
					top = n;
				else {
					p->prv->nxt = n;
					n->prv = p->prv;
					free(p);
				}
				if (q->nxt == NULL)
					;
				else {
					q->nxt->prv = n;
					n->nxt = q->nxt;
					free(q);
				}
				q = n->nxt;
				continue;
			}
			break;
		default:
			break;
		}
		q = q->nxt;
	}

	if (level > 0)
		warn_exit("too many '('.");
	return top;
}

/*
 * Binding logical not to the next one
 *
 *  P_NOT P_EXP  =>  P_EXP'
 *                     v
 *                   P_NOT
 *                     v
 *                   P_EXP
 * P_EXP is a result of exp_sq() or exp_eq().
 */

private struct pat *
exp_not(struct pat *top) {
	struct pat *n, *m, *p = top, *q;
	while (p != NULL)
		if (p->typ == P_NOT) {
			if ((q = p->nxt) == NULL)
				warn_exit("broken not.");
			if (q->typ != P_EXP)
				warn_exit("broken not.");
			n = get_pat();
			m = get_pat();
			p->dwn = n;
			n->typ = P_NOT;
			n->dwn = m;
			m->typ = P_EXP;
			m->dwn = q->dwn;
			p->typ = P_EXP;
			q = q->nxt;
			free(p->nxt);
			if (q != NULL) q->prv = p;
			p->nxt = q;
			p = q;
		} else
			p = p->nxt;
	return top;
}

/* Binding logical and/or to the previous one and the next one.
 * This is processed barkward from the tail so that the result
 * P_EXP can be evaluated naturally.
 *
 * P_EXP1 P_ANDa P_EXP2 P_ANDb P_EXP3
 *
 *              v
 *              v
 *
 * P_EXP5
 *  v
 * P_ANDa P_EXP1 P_EXP4
 *                v
 *               P_ANDb P_EXP2 P_EXP3
 */
 
private struct pat *
exp_lao(struct pat *top, enum ptype lao) {
	struct pat *p = top, *q, *r, *n;
	int count = 0;

	do {
		if (p->typ == lao) count++;
		q = p;
		p = p->nxt;
	} while (p != NULL);
	if (count == 0)
		return top;
	while (q != NULL)
		if (q->typ == lao) {
			p = q->prv;
			r = q->nxt;
			if (p == NULL || r == NULL)
				warn_exit("broken and/or.");
			if (p->typ != P_EXP || r->typ != P_EXP)
				warn_exit("broken and/or.");
			n = get_pat();
			n->typ = P_EXP;
			n->prv = p->prv;
			n->nxt = r->nxt;
			n->dwn = p;
			if (p->prv != NULL) p->prv->nxt = n;
			if (r->nxt != NULL) r->nxt->prv = n;
			p->prv = NULL;
			r->nxt = NULL;
			/* swap p and q */
			q->typ = P_EXP;
			q->dwn = p->dwn;
			p->typ = lao;
			p->dwn = NULL;
			top = n;
			q = n->prv;
		} else {
			top = q;
			q = q->prv;
		}
	return top;
}

/*
 * Creating an expression tree.
 *
 */

private struct pat *
exp_exp(struct pat *top) {
	if (top == NULL)
		warn_exit("invalid pattern.");
	top = exp_sq(top);
	top = exp_not(top);
	top = exp_lao(top, P_AND);
	top = exp_lao(top, P_OR);
	return top;
}

/****************************************************************
 * 
 * Starter of the parser
 *
 */

private struct pat *
parse_pattern(char *pattern) {
	enum ptype type;
	char *str;
	struct pat *top = NULL, *p = NULL, *q = NULL;

	Pinit(pattern);
	while ((type = token(&str)) != P_NUL) {
		p = get_pat();
		p->typ = type;
		if (type == P_STR)
			p->key = str;
		if (top == NULL)
			top = p;
		else {
			q->nxt = p;
			p->prv = q;
		}
		q = p;
	}

	top = exp_eq(top);
	top = exp_exp(top);
	return top;
}

/****************************************************************
 * 
 * Evaluator of an expression tree
 *
 */

private int
eval_exp(struct pat *p) {
	switch (p->typ) {
	case P_NOT:
		if (eval_exp(p->dwn) == TRUE)
			return FALSE;
		else
			return TRUE;
	case P_AND:
		if (eval_exp(p->nxt) == FALSE)
			return FALSE;
		if (eval_exp(p->nxt->nxt) == FALSE)
			return FALSE;
		return TRUE;
	case P_OR:
		if (eval_exp(p->nxt) == TRUE)
			return TRUE;
		if (eval_exp(p->nxt->nxt) == TRUE)
			return TRUE;
		return FALSE;
#ifndef DEBUG
	case P_EQ:
		if ((search_string(p->key, p->val, NO)) == TRUE)
			return TRUE;
		else
			return FALSE;
	case P_CEQ:
		if ((search_string(p->key, p->val, YES)) == TRUE)
			return TRUE;
		else
			return FALSE;
	case P_NEQ:
		if ((search_string(p->key, p->val, NO)) == TRUE)
			return FALSE;
		else
			return TRUE;
	case P_CNE:
		if ((search_string(p->key, p->val, YES)) == TRUE)
			return FALSE;
		else
			return TRUE;
#endif /* !DEBUG */
	case P_EXP:
		return eval_exp(p->dwn);
		break;
	default:
		warn_exit("broken tree.");
	}
	return TRUE; /* never reached */
}

/****************************************************************
 * 
 * External interface
 *
 */

private struct pat *Parsed_pattern;

public void
pattern_init(char *pattern) {
	Parsed_pattern = parse_pattern(pattern);
}

public int
pattern_match() {
	return eval_exp(Parsed_pattern);
}

#ifdef DEBUG
/****************************************************************
 * 
 * Visualizing parsed pattern
 *
 */

private void print_exp(struct pat *, int);

#define leveling(n) {int i; for(i = 0; i < (n); i++) putchar(SP);}

private void
print_exp(struct pat *p, int level){
	switch (p->typ) {
	case P_NOT:
		leveling(level);
		printf("!\n");
		print_exp(p->dwn, level + 1);
		break;
	case P_AND:
		leveling(level);
		printf("&\n");
		print_exp(p->nxt, level + 1);
		print_exp(p->nxt->nxt, level + 1);
		break;
	case P_OR:
		leveling(level);
		printf("|\n");
		print_exp(p->nxt, level + 1);
		print_exp(p->nxt->nxt, level + 1);
		break;
	case P_EQ:
		leveling(level);
		printf("'%s' = '%s'\n", p->key, p->val);
		break;
	case P_NEQ:
		leveling(level);
		printf("'%s' != '%s'\n", p->key, p->val);
		break;
	case P_CEQ:
		leveling(level);
		printf("'%s' == '%s'\n", p->key, p->val);
		break;
	case P_CNE:
		leveling(level);
		printf("'%s' !== '%s'\n", p->key, p->val);
		break;
	case P_EXP:
		print_exp(p->dwn, level + 1);
		break;
	default:
		warn_exit("broken tree.");
	}
}

int
main(int argc, char *argv[])
{
	char *progname = getprognm(argv[0]);
	struct pat *top;

	warn_prog = progname;
	if (argc != 2)
		warn_exit("argc != 2.");
	top = parse_pattern(argv[1]);
	print_exp(top, 0);
	exit(EXIT_SUCCESS);
}
#endif /* DEBUG */

/* 
 * Copyright (C) 2000-2005 Mew developing team.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the team nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * pattern.c ends here
 */
