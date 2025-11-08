#include <stdio.h>

void test(int n){
    switch (n) {
        case 0:
            printf("0\n");
        case 1:
            printf("1\n");
            break;
        case 2:
        case 3:
            printf("2&3\n");
            break;
        default:
            printf("default\n");
            break;
    }
    printf("\n");
}

typedef struct {
    int x;
    int y;
} obj_t;

obj_t create(int x, int y){
    obj_t o = {x, y};
    return o; 
}

void print_obt_t(obj_t o){
    printf("o = {x=%d, y%d}\n", o.x, o.y);
    o.x ++;
    printf("o = {x=%d, y%d}\n", o.x, o.y);
}

int main(){
    for(int i=0; i<=5; i++){
        printf("---< %d >---\n", i);
        test(i);
    }

    obj_t o = create(1,2);
    print_obt_t(o);
    print_obt_t(o);

    return 0;
}