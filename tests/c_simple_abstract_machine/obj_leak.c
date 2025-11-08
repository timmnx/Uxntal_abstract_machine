#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define TAB "    "

#define TAG_INT 1000
// #define TAG_STRING 252

struct obj {
  int tag;
  int size;
  struct obj **block;
};
typedef struct obj obj_t;

void dump_aux(obj_t* o, int tab){
    switch (o->tag) {
        case TAG_INT:
            printf("{addr=%#x | tag=%d | size=%d | int=%d}",
                (unsigned) (long) o->block, o->tag, o->size, (int) (long) o->block);
            break;
        default:
            printf("{addr=%#x | tag=%d | size=%d | block=\n",
                (unsigned) (long) o->block, o->tag, o->size);
            for(int i=0; i<o->size; i++){
                for(int j=0; j<tab+1; j++) printf(TAB);
                printf("[%d]", i);
                dump_aux(o->block[i], tab+1);
                printf("\n");
            }
            for(int j=0; j<tab; j++) printf(TAB);
            printf("}");
            break;
    }
}
void dump(obj_t* o){
    dump_aux(o, 0);
}
void drop(obj_t* o){
    // return;
    if (o != NULL) {
        switch (o->tag) {
            case TAG_INT:
                break;
            default:
                for (int i=0; i < o->size; i++){
                    drop(o->block[i]);
                }
                free(o->block);
                break;
        }
        free(o);
    }
}
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

enum syntax_type { Int=TAG_INT, Add=0, Sub=1, Mul=2 };
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

obj_t* syntax_repr(syntax_t* s){
    obj_t* o = malloc(sizeof(obj_t));
    obj_t** block = NULL;
    o->tag = (int) s->t;
    switch (s->t) {
        case Int:
            o->tag = TAG_INT;
            o->size = 0;
            o->block = (obj_t**) (long) s->v;
            break;
        default:
            o->size = 2;
            block = malloc(2*sizeof(obj_t));
            block[0] = syntax_repr(s->of.fst);
            block[1] = syntax_repr(s->of.snd);
            o->block = block;
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

enum cont_type {Id=TAG_INT, Fn_add=0, Fn_sub=1, Fn_mul=2, Ar_add=3, Ar_sub=4, Ar_mul=5};
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

obj_t* cont_repr(cont_t* k){
    obj_t* o = malloc(sizeof(obj_t));
    obj_t** block = NULL;
    o->tag = (int) (k->t);
    switch (k->t) {
        case Id:
            o->tag = (int) Id;
            o->size = 0;
            o->block = block;
            break;
        case Fn_add:
        case Fn_sub:
        case Fn_mul:
            o->size = 2;
            block = malloc(2*sizeof(block));
            block[0]->tag = TAG_INT;
            block[0]->size = 0;
            block[0]->block = (obj_t**) (long) k->of.fn.i;
            block[1] = cont_repr(k->of.fn.k);
            o->block = block;
            break;
        case Ar_add:
        case Ar_sub:
        case Ar_mul:
            o->size = 2;
            block = malloc(2*sizeof(block));
            block[0] = syntax_repr(k->of.ar.t);
            block[1] = cont_repr(k->of.ar.k);
            o->block = block;
            break;
    }
    return o;
}

// typedef obj_t* my_int;

int apply(obj_t* k, obj_t* i, bool show);

int eval(obj_t* s, obj_t* k, bool show){
    if (show) {
        printf("s = ");
        dump(s);
        printf("\nk = ");
        dump(k);
        printf("\n<eval>\n");
    }
    obj_t* k_bis = malloc(sizeof(obj_t));
    obj_t** block = NULL;
    int res_eval;
    switch (s->tag) {
        case TAG_INT:
            res_eval = apply(k, s, show);
            break;
        case Add:
            k_bis->tag = Ar_add;
            k_bis->size = 2;
            block = malloc(2*sizeof(obj_t*));
            block[0] = s->block[1];
            block[1] = k;
            k_bis->block = block;
            res_eval = eval(s->block[0], k_bis, show);
            // drop(block[0]);
            // drop(block[1]);
            // free(block);
            break;
        case Sub:
            k_bis->tag = Ar_sub;
            k_bis->size = 2;
            block = malloc(2*sizeof(obj_t*));
            block[0] = s->block[1];
            block[1] = k;
            k_bis->block = block;
            res_eval = eval(s->block[0], k_bis, show);
            // drop(block[0]);
            // drop(block[1]);
            // free(block);
            break;
        case Mul:
            k_bis->tag = Ar_mul;
            k_bis->size = 2;
            block = malloc(2*sizeof(obj_t*));
            block[0] = s->block[1];
            block[1] = k;
            k_bis->block = block;
            res_eval = eval(s->block[0], k_bis, show);
            // drop(block[0]);
            // drop(block[1]);
            // free(block);
            break;
    };
    // drop(s);
    if (s->tag != TAG_INT) free(s->block);
    free(s);
    // drop(k_bis);
    return res_eval;
}

int apply(obj_t* k, obj_t* i, bool show){
    if (show) {
        printf("k = ");
        dump(k);
        printf("\ni = ");
        dump(i);
        printf("\n<apply>\n");
    }
    obj_t* k_bis = malloc(sizeof(obj_t));
    obj_t** block = NULL;
    obj_t* s = malloc(sizeof(obj_t));
    s->tag = TAG_INT;
    s->size = 0;
    int res_apply;
    switch (k->tag) {
        case TAG_INT:
            res_apply = (int) (long) i->block;
            
            break;
        case Ar_add:
            k_bis->tag = Fn_add;
            k_bis->size = 2;
            block = malloc(2*sizeof(obj_t));
            block[0] = i;
            block[1] = k->block[1];
            k_bis->block = block;
            res_apply = eval(k->block[0], k_bis, show);
            // drop(block[0]);
            // drop(block[1]);
            // free(block);
            break;
        case Ar_sub:
            k_bis->tag = Fn_sub;
            k_bis->size = 2;
            block = malloc(2*sizeof(obj_t));
            block[0] = i;
            block[1] = k->block[1];
            k_bis->block = block;
            res_apply = eval(k->block[0], k_bis, show);
            // drop(block[0]);
            // drop(block[1]);
            // free(block);
            break;
        case Ar_mul:
            k_bis->tag = Fn_mul;
            k_bis->size = 2;
            block = malloc(2*sizeof(obj_t));
            block[0] = i;
            block[1] = k->block[1];
            k_bis->block = block;
            res_apply = eval(k->block[0], k_bis, show);
            // drop(block[0]);
            // drop(block[1]);
            // free(block);
            break;
        case Fn_add:
            s->block = (obj_t**) ((long) k->block[0]->block + (long) i->block);
            res_apply = eval(s, k->block[1], show);
            break;
        case Fn_sub:
            s->block = (obj_t**) ((long) k->block[0]->block - (long) i->block);
            res_apply = eval(s, k->block[1], show);
            break;
        case Fn_mul:
            s->block = (obj_t**) ((long) k->block[0]->block * (long) i->block);
            res_apply = eval(s, k->block[1], show);
            break;
    }
    // drop(k_bis);
    // drop(s);
    if (k->tag != TAG_INT) free(k->block);
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
int main(){
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
    syntax_t exprs[5] = {expr0, expr1, expr2, expr3};
    cont_t* k = malloc(sizeof(cont_t));

    // dump(syntax_repr(&int2));
    
    for (int i=0; i<1; i++){
        obj_t* os = syntax_repr(&exprs[i]);
        k->t = Id;
        k->of.none = NULL;
        obj_t* ok = cont_repr(k);
        printf("\n------------------------< %d >------------------------\n", i);
        printf("os = ");
        dump(os);
        printf("\nok = ");
        dump(ok);
        printf("\n\n");
        int res = eval(os, ok, false);
        printf("\nexpr_%d ~> %d\n", i, res);
        // drop(os);
        // drop(ok);
    }
    printf("\n%lu %lu\n", sizeof(int), sizeof(long));
    free(k);
    return 0;
}