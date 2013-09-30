#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <iostream>
#include <unistd.h>
#include <linux/input.h>
//#include <time.h>
#include <sys/time.h>

#define yPx2mm 200/815//42400 //6300
#define xPx2mm 200/770//42800 //6350 //13650
//7454
//7113
#define MOUSEFILE "/dev/input/mouse2"

using namespace std;

int main(void)
{
int fd;
int x=0;
int y=0;
struct input_event ie;
FILE *file;
timeval  first, next;
//struct timeval  new;
if((fd = open(MOUSEFILE, O_RDONLY)) == -1) {
    perror("opening device");
    exit(EXIT_FAILURE);
}

file = fopen("1.txt","w+"); 
fprintf(file,"0#0",x,y);
fclose(file);


while(read(fd, &ie, sizeof(struct input_event))) {
    signed char *ptr = (signed char*)&ie; //ptr >> 16 & 0xFFFF;
    gettimeofday(&next, NULL);
    
    for(int i=0; i<sizeof(ie); i++)
        printf("%03d ", *ptr++);
    printf("\n");
    signed char *ptr2 = (signed char*)&ie;
    //int test = (a << 24) & *(ptr2+23)) + (r << 16) & *(ptr2+22)) + ((g << 8) & *(ptr2+21)) + (b & *(ptr2+20));
    int test = (*(ptr2+16) & 0xFF000000) + (*(ptr2+15) & 0x00FF0000) + (*(ptr2+14) & 0x0000FF00) + (*(ptr2+13) & 0x000000FF);
    //if((2 == *(ptr2+8)) && (0 == *(ptr2+10))) 
    x+= *(ptr2+1);
    //if((2 == *(ptr2+8)) && (1 == *(ptr2+10))) 
    y+= *(ptr2+2);
    //int test = *(ie) >> 16 & 0xFFFF;
    gettimeofday(&first, NULL);
    printf("xPx:__%d  yPx:__%d  xmm:__%.2f  ymm:__%.2f     TEST:%d=%d",x,y,(double)x*xPx2mm,(double)y*yPx2mm,test,*(ptr2+12));
    cout << (first.tv_usec - next.tv_usec) << " ms.";
    printf("\n");
    //file = fopen("CurrentPos.txt","w"); 
    //fprintf(file,"%d#%d",x,y);//(int)*(ptr2+1)*xPx2mm*100,(int)*(ptr2+2)*yPx2mm*100);
    //fclose(file);
    file = fopen("1.txt","w+"); 
    fprintf(file,"%d#%d",x,y);//(int)*(ptr2+1)*xPx2mm*100,(int)*(ptr2+2)*yPx2mm*100);
    fclose(file);

}

}


