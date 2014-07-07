#ifndef IR_H_INCLUDED
#define IR_H_INCLUDED

void printIR(BaseSequentialStream *chp, int argc, char *argv[]);
void readIR(void);
void ADCinit(void);
msg_t irThread(void *arg);
int getIR1(void);
int getIR2(void);
int getIR3(void);
void getIR(int*);

#endif // IR_H_INCLUDED
