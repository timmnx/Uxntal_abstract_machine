#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAG_INT 1000
// #define TAG_STRING 252

struct obj {
  int tag;
  int size;
  struct obj *block;
};
typedef struct obj obj_t;

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

enum syntax_type { Int, Add, Sub, Mul };
struct syntax {
    enum syntax_type t;
    union {
        /* If t = Int : int */
        int v;
        /* Else : (Add | Sub | Mul) of (syntax*, syntax*)*/
        struct {
            struct syntax* fst;
            struct syntax* snd;
        } of;
    };
};

typedef struct syntax syntax_t;

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

enum cont_type {Id, Fn_add, Fn_sub, Fn_mul, Ar_add, Ar_sub, Ar_mul};
struct cont {
    enum cont_type t;
    union {
        /* If t = Id : void* */
        void* none;
        /* Elif t = Fn_... : of (int, cont*) */
        struct {
            int i;
            struct cont* k;
        } fn;
        /* Else (t = Ar_...) of (syntax*, cont*) */
        struct {
            syntax_t *t;
            struct cont* k;
        } ar;
    } of;
};

typedef struct cont cont_t;

int eval(syntax_t* s, cont_t* k);
int apply(cont_t* k, int i);

int eval(syntax_t* s, cont_t* k){
    cont_t *k_bis = malloc(sizeof(cont_t));
    k_bis->of.ar.t = s->of.snd;
    k_bis->of.ar.k = k;
    int res_eval;
    switch (s->t) {
        case Int:
            res_eval = apply(k, s->v);
            break;
        case Add:
            k_bis->t = Ar_add;
            res_eval = eval (s->of.fst, k_bis);
            break;
        case Sub:
            k_bis->t = Ar_sub;
            res_eval = eval (s->of.fst, k_bis);
            break;
        case Mul:
            k_bis->t = Ar_mul;
            res_eval = eval (s->of.fst, k_bis);
            break;
    }
    free(k_bis);
    return res_eval;
}

int apply(cont_t* k, int i){
    cont_t *k_bis = malloc(sizeof(cont_t));
    syntax_t *s = malloc(sizeof(syntax_t));
    s->t = Int;
    int res_apply;
    switch (k->t) {
        case Id:
            res_apply = i;
            break;
        case Ar_add:
            k_bis->t = Fn_add;
            k_bis->of.fn.i = i;
            k_bis->of.fn.k = k->of.ar.k;
            res_apply = eval(k->of.ar.t, k_bis);
            break;
        case Ar_sub:
            k_bis->t = Fn_sub;
            k_bis->of.fn.i = i;
            k_bis->of.fn.k = k->of.ar.k;
            res_apply = eval(k->of.ar.t, k_bis);
            break;
        case Ar_mul:
            k_bis->t = Fn_mul;
            k_bis->of.fn.i = i;
            k_bis->of.fn.k = k->of.ar.k;
            res_apply = eval(k->of.ar.t, k_bis);
            break;
        case Fn_add:
            s->v = k->of.fn.i + i;
            res_apply = eval(s, k->of.fn.k);
            break;
        case Fn_sub:
            s->v = k->of.fn.i - i;
            res_apply = eval(s, k->of.fn.k);
            break;
        case Fn_mul:
            s->v = k->of.fn.i * i;
            res_apply = eval(s, k->of.fn.k);
            break;
    }
    free(k_bis);
    free(s);
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
int main(){
    /**
    Commandes de compilation pour avoir des infos sur les mems leak
    et exection de l'executable :

    -   Mac only :
        $ gcc -Wall -Wextra -g main.c -o main.leak
        $ leaks --atExit -- ./main.leak

    -   Any system :
        $ gcc -fsanitize=address -Wall -Wextra -g main.c -o main.asan
        $ ./obj.asan
    */

    syntax_t int1 = {Int,{1}};
    syntax_t int2 = {Int,{2}};
    syntax_t int3 = {Int,{3}};
    syntax_t int7 = {Int,{7}};
    syntax_t int_2 = {Int,{-2}};
    syntax_t expr0 = {Add, .of={&int1, &int2}};
    syntax_t expr1 = {Sub, .of={&int1, &int2}};
    syntax_t mul23 = {Mul, .of={&int2, &int3}};
    syntax_t expr2 = {Add, .of={&int1, &mul23}};
    syntax_t expr3 = {Add, .of={
        &((syntax_t) {Mul, .of={&int3, &int_2}}),
        &((syntax_t) {Mul, .of={&int7, &int3}})
    }};
    syntax_t exprs[4] = {expr0, expr1, expr2, expr3};
    cont_t* k = malloc(sizeof(cont_t));
    for (int i=0; i<4; i++){
        k->t = Id;
        k->of.none = NULL;
        printf("expr_%d ~> %d\n", i, eval(&exprs[i], k));
    }
    free(k);
    return 0;
}