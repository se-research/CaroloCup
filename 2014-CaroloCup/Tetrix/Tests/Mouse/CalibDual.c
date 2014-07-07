#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <iostream>
#include <unistd.h>
#include <linux/input.h>
#include <sys/time.h>
#include <math.h>
//#include <conio.h>

#define PI M_PI//3.141592653589793

using namespace std;

int main(void){
int i;
int x1Buff=0, y1Buff=0, x2Buff=0, y2Buff=0, j=0, k=0 ,x1=0,y1=0,x2=0,y2=0;

int Fx1Tot=0,Fy1Tot=0,Fx2Tot=0,Fy2Tot=0,
Sx1Tot=0,Sy1Tot=0,Sx2Tot=0,Sy2Tot=0, offsetAng1=0, offsetAng2=0, totY1=0, totX1=0, totY2=0, totX2=0;

FILE *file1;
FILE *file2;

//clrscr();
cout << "Calibration Started!\n";

file1 = fopen("1.txt","r");
j=fscanf(file1,"%d#%d",&x1Buff,&y1Buff);
fclose(file1);
file2 = fopen("2.txt","r");
k=fscanf(file2,"%d#%d",&x2Buff,&y2Buff);
fclose(file2);

if( (j == 2) && (k == 2) ){

    printf("Move FORWARD 3 meters and press any key.\n");//X:_%f  Y:_%f  Ang:_%f", Xcurr, Ycurr, THETAcurr);
    printf("\n");

    cin.get();//getch();
    file1 = fopen("1.txt","r");
    j=fscanf(file1,"%d#%d",&x1,&y1);
    fclose(file1);
    file2 = fopen("2.txt","r");
    k=fscanf(file2,"%d#%d",&x2,&y2);
    fclose(file2);

    Fx1Tot += x1 - x1Buff;
    Fy1Tot += y1 - y1Buff;
    Fx2Tot += x2 - x2Buff;
    Fy2Tot += y2 - y2Buff;
    
    
    
    printf("X:_%d  Y:_%d  X2:_%d  Y2:_%d\n\n", Fx1Tot, Fy1Tot, Fx2Tot, Fy2Tot);
    printf("Position sideways and hit a key.\n");
    
    cin.get();
    
    file1 = fopen("1.txt","r");
    j=fscanf(file1,"%d#%d",&x1Buff,&y1Buff);
    fclose(file1);
    file2 = fopen("2.txt","r");
    k=fscanf(file2,"%d#%d",&x2Buff,&y2Buff);
    fclose(file2);
    
    
    
    printf("Move sideways 3 meters and hit a key.\n");
    
    cin.get();
    
    file1 = fopen("1.txt","r");
    j=fscanf(file1,"%d#%d",&x1,&y1);
    fclose(file1);
    file2 = fopen("2.txt","r");
    k=fscanf(file2,"%d#%d",&x2,&y2);
    fclose(file2);
    
    
    Sx1Tot += x1 - x1Buff;
    Sy1Tot += y1 - y1Buff;
    Sx2Tot += x2 - x2Buff;
    Sy2Tot += y2 - y2Buff;
	
    
    //printf("X:_%f  Y:_%f  X2:_%f  Y2:_%f\n\n", Sx1Tot, Sy1Tot, Sx2Tot, Sy2Tot);
    
    //offsetAng1 = acos(Fx1Tot/Fy1Tot);
    //totY1 = (Fy1Tot/cos(offsetAng1))/3000;
    //totX1 = (Sx1Tot*cos(offsetAng1))/3000;
    
    //offsetAng2 = acos(Fx2Tot/Fy2Tot);
    //totY2 = (Fy2Tot/cos(offsetAng2))/3000;
    //totX2 = (Sx2Tot*cos(offsetAng2))/3000;
    
    
    
    file1 = fopen("Calibration.txt","w+");
    fprintf(file1,"%d#%d#%d#%d#%d#%d#%d#%d\n", Fx1Tot, Fy1Tot, Fx2Tot, Fy2Tot, Sx1Tot, Sy1Tot, Sx2Tot, Sy2Tot);
    //fprintf(file1,"%f#%f#%f/n",offsetAng1,totY1,totX1);
    //fprintf(file1,"%f#%f#%f/n",offsetAng2,totY2,totX2);
    fclose(file1);
    
} else printf("Could not read file.\n");

printf("\nCalibration ended.\n");

return 0;
}

