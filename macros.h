/* macros.h
 *
 * Author: The Hellermeister 2017
*/

#define ENVCOPIER(major) {                                     \
            int i,j;                                            \
            for (i=0,j=1;i<major;++i,++j){                      \
              MOV(INDD(R2,j),INDD(R1,i));                       \
            }                                                   \
            MOV(R3,FPARG(1));                                   \
            PUSH(R3);                                           \
            CALL(MALLOC);                                       \
            DROP(1);                                            \
            MOV(INDD(R2,0),R0);                                 \
            MOV(R4,INDD(R2,0));                                 \
            for(i=0,j=2;i<R3;++i,++j){                          \
              MOV(INDD(R4,i),FPARG(j));                         \
            }                                                   \
        }

#define TCAPPLIC(n,m){                                      \
    int i,j;\
    MOV(R4,IMM(n));                                                \
    for(i=n, j=m; j>=-2; --j, --i){                              \
        MOV(FPARG(1+i),STARG(1+j));                         \
    }\
        DROP(R4+4);\
    MOV(FP,R1);\
}

#define ALIGNEFRAME(n, params, t) {                          \
            MOV(FPARG(1), params + 1);                    \
            int i,j;                                      \
            for (i=2+params,j=n+1; i>=-2 ;--i, --j){      \
              MOV(FPARG(j),FPARG(i));                     \
            }                       \
            DROP(n-params-t);                             \
            MOV(FP,SP);                                   \
        }        


#define OPTIONAL(n, params) {                           \
            int i;                                      \
            if (n==params){                             \
                for (i=-2;i<params+2;++i){              \
                    MOV(FPARG(i-1),FPARG(i));           \
                }                                       \
                MOV(FPARG(i-1),IMM(SOB_NIL));\
                PUSH(FP);\
                MOV(FP,SP);\
                MOV(FPARG(1), params + 1);\
            }                                           \
            else{                                       \
                MOV(R1,IMM(SOB_NIL));                       \
                for (i=n+1;i>params+1;--i){                 \
                  PUSH(R1);                                 \
                  PUSH(FPARG(i));                           \
                  CALL(MAKE_SOB_PAIR);                      \
                  DROP(2);                                  \
                  MOV(R1,R0);                               \
                }                                           \
                MOV(FPARG(2+params), R1);                   \
                ALIGNEFRAME(n,params,1);                      \
             }                                              \
    }


#define VARIADIC(n,params) {                           \
            PUSH(IMM(99));\
            int i;                                      \
            if (n==0){                              \
              if (FPARG(2)==IMM(SOB_NIL)){\
                DROP(1);\
                MOV(FPARG(1),IMM(1));}\
                else{\
                  for (i=-2;i<params+2;++i){              \
                      MOV(FPARG(i-1),FPARG(i));           \
                  }                                       \
                  MOV(FP,SP);\
                  MOV(FPARG(1),IMM(1));\
                  MOV(FPARG(2),IMM(SOB_NIL));}\
            }                                           \
            else{                                       \
                MOV(R1,IMM(SOB_NIL));                       \
                for (i=n+1;i>1;--i){                 \
                  PUSH(R1);                                 \
                  PUSH(FPARG(i));                           \
                  CALL(MAKE_SOB_PAIR);                      \
                  DROP(2);                                  \
                  MOV(R1,R0);                               \
                }                                           \
                MOV(FPARG(2), R1);                   \
                ALIGNEFRAME(n,0,0);                      \
             }                                              \
    }

