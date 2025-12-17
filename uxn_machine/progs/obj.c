#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAB "    "

#define TAG_INT 1000
// #define TAG_STRING 252

// Ormund : 

/****************************************/
/*                                      */
/*    ######    ########            ##  */
/*    ######    ########            ##  */
/*  ##      ##  ##      ##          ##  */
/*  ##      ##  ##      ##          ##  */
/*  ##      ##  ##      ##          ##  */
/*  ##      ##  ##      ##          ##  */
/*  ##      ##  ########            ##  */
/*  ##      ##  ########            ##  */
/*  ##      ##  ##      ##  ##      ##  */
/*  ##      ##  ##      ##  ##      ##  */
/*  ##      ##  ##      ##  ##      ##  */
/*  ##      ##  ##      ##  ##      ##  */
/*    ######      ######      ######    */
/*    ######      ######      ######    */
/*                                      */
/****************************************/

typedef struct obj {
    long tag;
    long size;
    struct obj **block;
} obj_t;

void dump_aux(obj_t *o, int tab) {
    switch (o->tag) {
    case TAG_INT:
        printf("{addr=%#x | tag=%ld | size=%ld | int=%ld}",
               (unsigned)(long)o->block, o->tag, o->size, (long)o->block);
        break;
    default:
        printf("{addr=%#x | tag=%ld | size=%ld | block=\n",
               (unsigned)(long)o->block, o->tag, o->size);
        for (int i = 0; i < o->size; i++) {
            for (int j = 0; j < tab + 1; j++)
                printf(TAB);
            printf("[%d]", i);
            dump_aux(o->block[i], tab + 1);
            printf("\n");
        }
        for (int j = 0; j < tab; j++)
            printf(TAB);
        printf("}");
        break;
    }
}
void dump(obj_t *o) { dump_aux(o, 0); }

obj_t *int_repr(int i) {
    obj_t *o = malloc(sizeof(obj_t));
    o->tag   = TAG_INT;
    o->size  = 0;
    o->block = (obj_t **)(long)i;
    return o;
}

bool obj_is_int(obj_t *o) { return (o->tag == TAG_INT); }

/****************************************************************************/
/*                                                                          */
/*    ######    ##      ##  ##      ##  ##########    ######    ##      ##  */
/*    ######    ##      ##  ##      ##  ##########    ######    ##      ##  */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  */
/*  ##          ##      ##  ####    ##      ##      ##      ##  ##      ##  */
/*  ##          ##      ##  ####    ##      ##      ##      ##  ##      ##  */
/*    ######      ########  ##  ##  ##      ##      ##      ##    ######    */
/*    ######      ########  ##  ##  ##      ##      ##      ##    ######    */
/*          ##          ##  ##    ####      ##      ##########  ##      ##  */
/*          ##          ##  ##    ####      ##      ##########  ##      ##  */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  */
/*    ######      ######    ##      ##      ##      ##      ##  ##      ##  */
/*    ######      ######    ##      ##      ##      ##      ##  ##      ##  */
/*                                                                          */
/****************************************************************************/

enum syntax_type { Int = TAG_INT, Add = 0, Sub = 1, Mul = 2 };
typedef struct syntax {
    enum syntax_type t;
    union {
        /* If t = Int : int */
        int v;
        /* Else : (Add | Sub | Mul) of (syntax*, syntax*)*/
        struct {
            struct syntax *fst;
            struct syntax *snd;
        } of;
    };
} syntax_t;

obj_t *syntax_repr(syntax_t *s) {
    obj_t *o = NULL;
    switch (s->t) {
    case Int:
        o = int_repr(s->v);
        break;
    case Add:
    case Sub:
    case Mul:
        o           = malloc(sizeof(obj_t));
        o->size     = 2;
        o->tag      = s->t;
        o->block    = malloc(2 * sizeof(obj_t));
        o->block[0] = syntax_repr(s->of.fst);
        o->block[1] = syntax_repr(s->of.snd);
        break;
    }
    return o;
}

/****************************************************/
/*                                                  */
/*    ######      ######    ##      ##  ##########  */
/*    ######      ######    ##      ##  ##########  */
/*  ##      ##  ##      ##  ##      ##      ##      */
/*  ##      ##  ##      ##  ##      ##      ##      */
/*  ##          ##      ##  ####    ##      ##      */
/*  ##          ##      ##  ####    ##      ##      */
/*  ##          ##      ##  ##  ##  ##      ##      */
/*  ##          ##      ##  ##  ##  ##      ##      */
/*  ##          ##      ##  ##    ####      ##      */
/*  ##          ##      ##  ##    ####      ##      */
/*  ##      ##  ##      ##  ##      ##      ##      */
/*  ##      ##  ##      ##  ##      ##      ##      */
/*    ######      ######    ##      ##      ##      */
/*    ######      ######    ##      ##      ##      */
/*                                                  */
/****************************************************/

enum cont_type {
    Id     = TAG_INT,
    Fn_add = 0,
    Fn_sub = 1,
    Fn_mul = 2,
    Ar_add = 3,
    Ar_sub = 4,
    Ar_mul = 5
};
typedef struct cont {
    enum cont_type t;
    union {
        /* If t = Id : void* */
        void *none;
        /* Elif t = Fn_... : of (int, cont*) */
        struct {
            int i;
            struct cont *k;
        } fn;
        /* Else (t = Ar_...) of (syntax*, cont*) */
        struct {
            syntax_t *t;
            struct cont *k;
        } ar;
    } of;
} cont_t;

obj_t *cont_repr(cont_t *k) {
    obj_t *o = NULL;
    switch (k->t) {
    case Id:
        o = int_repr(0);
        break;
    case Fn_add:
    case Fn_sub:
    case Fn_mul:
        o           = malloc(sizeof(obj_t));
        o->size     = 2;
        o->tag      = k->t;
        o->block    = malloc(2 * sizeof(obj_t));
        o->block[0] = int_repr(k->of.fn.i);
        o->block[1] = cont_repr(k->of.fn.k);
        break;
    case Ar_add:
    case Ar_sub:
    case Ar_mul:
        o           = malloc(sizeof(obj_t));
        o->size     = 2;
        o->tag      = k->t;
        o->block    = malloc(2 * sizeof(obj_t));
        o->block[0] = syntax_repr(k->of.ar.t);
        o->block[1] = cont_repr(k->of.ar.k);
        break;
    }
    return o;
}

/****************************************************************************************************/
/*                                                                                                  */
/*    ######    ########      ######    ##########  ########      ######      ######    ##########  */
/*    ######    ########      ######    ##########  ########      ######      ######    ##########  */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      */
/*  ##      ##  ##      ##  ##              ##      ##      ##  ##      ##  ##              ##      */
/*  ##      ##  ##      ##  ##              ##      ##      ##  ##      ##  ##              ##      */
/*  ##      ##  ########      ######        ##      ########    ##      ##  ##              ##      */
/*  ##      ##  ########      ######        ##      ########    ##      ##  ##              ##      */
/*  ##########  ##      ##          ##      ##      ##      ##  ##########  ##              ##      */
/*  ##########  ##      ##          ##      ##      ##      ##  ##########  ##              ##      */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      */
/*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      */
/*  ##      ##  ########      ######        ##      ##      ##  ##      ##    ######        ##      */
/*  ##      ##  ########      ######        ##      ##      ##  ##      ##    ######        ##      */
/*                                                                                                  */
/*                                                                                                  */
/*  ##      ##    ######      ######    ##      ##      ##      ##      ##  ##########              */
/*  ##      ##    ######      ######    ##      ##      ##      ##      ##  ##########              */
/*  ####  ####  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      */
/*  ####  ####  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      */
/*  ##  ##  ##  ##      ##  ##          ##      ##      ##      ####    ##  ##                      */
/*  ##  ##  ##  ##      ##  ##          ##      ##      ##      ####    ##  ##                      */
/*  ##      ##  ##      ##  ##          ##########      ##      ##  ##  ##  ########                */
/*  ##      ##  ##      ##  ##          ##########      ##      ##  ##  ##  ########                */
/*  ##      ##  ##########  ##          ##      ##      ##      ##    ####  ##                      */
/*  ##      ##  ##########  ##          ##      ##      ##      ##    ####  ##                      */
/*  ##      ##  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      */
/*  ##      ##  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      */
/*  ##      ##  ##      ##    ######    ##      ##      ##      ##      ##  ##########              */
/*  ##      ##  ##      ##    ######    ##      ##      ##      ##      ##  ##########              */
/*                                                                                                  */
/****************************************************************************************************/

obj_t *apply(obj_t *k, obj_t *i, bool show);

obj_t *eval(obj_t *s, obj_t *k, bool show) {
    if (show) {
        printf("s = ");
        dump(s);
        printf("\nk = ");
        dump(k);
        printf("\n<eval>\n");
    }
    obj_t *k_bis    = NULL;
    obj_t *res_eval = NULL;
    switch (s->tag) {
    case TAG_INT:
        res_eval = apply(k, s, show);
        break;
    case Add:
        k_bis      = malloc(sizeof(obj_t));
        k_bis->tag = Ar_add;
        goto follow;
    case Sub:
        k_bis      = malloc(sizeof(obj_t));
        k_bis->tag = Ar_sub;
        goto follow;
    case Mul:
        k_bis      = malloc(sizeof(obj_t));
        k_bis->tag = Ar_mul;
    follow:
        k_bis->size     = 2;
        k_bis->block    = malloc(2 * sizeof(obj_t *));
        k_bis->block[0] = s->block[1];
        k_bis->block[1] = k;
        res_eval        = eval(s->block[0], k_bis, show);
        break;
    };
    if (s->tag != TAG_INT)
        free(s->block);
    free(s);
    return res_eval;
}

obj_t *apply(obj_t *k, obj_t *i, bool show) {
    if (show) {
        printf("k = ");
        dump(k);
        printf("\ni = ");
        dump(i);
        printf("\n<apply>\n");
    }
    obj_t *k_bis = NULL;
    obj_t *s     = NULL;
    obj_t *res_apply;
    switch (k->tag) {
    case TAG_INT:
        free(s);
        res_apply = int_repr((int)(long)i->block);
        break;
    case Ar_add:
        free(s);
        k_bis      = malloc(sizeof(obj_t));
        k_bis->tag = Fn_add;
        goto ar;
    case Ar_sub:
        free(s);
        k_bis      = malloc(sizeof(obj_t));
        k_bis->tag = Fn_sub;
        goto ar;
    case Ar_mul:
        free(s);
        k_bis      = malloc(sizeof(obj_t));
        k_bis->tag = Fn_mul;
    ar:
        k_bis->size     = 2;
        k_bis->block    = malloc(2 * sizeof(obj_t));
        k_bis->block[0] = i;
        k_bis->block[1] = k->block[1];
        res_apply       = eval(k->block[0], k_bis, show);
        break;
    case Fn_add:
        s         = int_repr((long)k->block[0]->block + (long)i->block);
        res_apply = eval(s, k->block[1], show);
        break;
    case Fn_sub:
        s         = int_repr((long)k->block[0]->block - (long)i->block);
        res_apply = eval(s, k->block[1], show);
        break;
    case Fn_mul:
        s         = int_repr((long)k->block[0]->block * (long)i->block);
        res_apply = eval(s, k->block[1], show);
        break;
    }
    if (k->tag != TAG_INT)
        free(k->block);
    free(k);
    return res_apply;
}

/**************************************************/
/*                                                */
/*  ##      ##    ######      ##      ##      ##  */
/*  ##      ##    ######      ##      ##      ##  */
/*  ####  ####  ##      ##    ##      ##      ##  */
/*  ####  ####  ##      ##    ##      ##      ##  */
/*  ##  ##  ##  ##      ##    ##      ####    ##  */
/*  ##  ##  ##  ##      ##    ##      ####    ##  */
/*  ##      ##  ##      ##    ##      ##  ##  ##  */
/*  ##      ##  ##      ##    ##      ##  ##  ##  */
/*  ##      ##  ##########    ##      ##    ####  */
/*  ##      ##  ##########    ##      ##    ####  */
/*  ##      ##  ##      ##    ##      ##      ##  */
/*  ##      ##  ##      ##    ##      ##      ##  */
/*  ##      ##  ##      ##    ##      ##      ##  */
/*  ##      ##  ##      ##    ##      ##      ##  */
/*                                                */
/**************************************************/

int main() {
    /**
    Commandes de compilation pour avoir des infos sur les mems leak
    et exection de l'executable :

    -   Mac only :
        $ gcc -Wall -Wextra -g obj.c -o obj.leak
        $ leaks --atExit -- ./obj.leak

    -   Any system :
        $ gcc -fsanitize=address -Wall -Wextra -g obj.c -o obj.asan
        $ ./obj.asan
    */

    syntax_t expr0 = {Add, .of = {
                        &(syntax_t){Int, {1}},
                        &(syntax_t){Int, {2}}
                    }};
    syntax_t expr1 = {Sub, .of = {
                        &(syntax_t){Int, {1}},
                        &(syntax_t){Int, {2}}
                    }};
    syntax_t expr2 = {Add, .of = {
                        &(syntax_t){Int, {1}},
                        &(syntax_t){Mul, .of = {
                            &(syntax_t){Int, {2}},
                            &(syntax_t){Int, {3}}
                        }}
                    }};
    syntax_t expr3 = {Add, .of = {
                        &((syntax_t){Mul, .of = {
                            &(syntax_t){Int, {3}},
                            &(syntax_t){Int, {-2}}
                        }}),
                        &((syntax_t){Mul, .of = {
                            &(syntax_t){Int, {7}},
                            &(syntax_t){Int, {3}}
                        }})
                    }};

    syntax_t exprs[4] = {expr0, expr1, expr2, expr3};
    cont_t k          = {Id, .of.none = NULL};

    for (int i = 0; i < 4; i++) {
        obj_t *os = syntax_repr(&exprs[i]);
        obj_t *ok = cont_repr(&k);
        printf("\n----------------------------------<");
        printf(" %d ", i);
        printf(">----------------------------------\n");
        obj_t *res = eval(os, ok, false);
        printf("expr_%d ~> ", i);
        if (obj_is_int(res)) {
            dump(res);
        } else {
            printf("fail");
        }
        printf("\n");
        free(res);
    }
    printf("\n");
    return 0;
}