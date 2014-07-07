#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <iostream>
#include <unistd.h>
#include <linux/input.h>
//#include <time.h>
#include <sys/time.h>
#include <math.h>

#define PI M_PI//3.141592653589793
//3.141593

#define MIN(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })
using namespace std;

double X_R_curr=0, Y_R_curr=0, X_L_curr=0, Y_L_curr=0, THETAcurr=0;
double m1y;
double m2y;
double m1x;
double m2x;

double mouse_left_tilt; 
double mouse_right_tilt;

double sign(double a){
  if(0 == (int)(a*1000000) ) return 1;
  return (a/fabs(a));
}

bool isPI(double x){
  if( ((int)(x*1000)) == ((int)(PI*1000)) ) true;
  else false;
}

double getAng(double x1, double y1, double x2, double y2){
  return atan2(y2-y1, x2-x1);
}

double getDistance(double x1, double y1, double x2, double y2){
  // Using pythagoras statement to get the distance between two points
  return sqrt(pow(y2-y1,2)+pow(x2-x1,2));
}

double calibrateY(double x, double y, double tilt){
  return (getDistance(0,0,x,y) * sin( getAng(0,0,x,y) - tilt ) );
}

double calibrateX(double x, double y, double tilt){
  return (getDistance(0,0,x,y) * cos(getAng(0,0,x,y) - tilt) );
}

double pyt(double x1, double y1, double x2, double y2){
  return sqrt(pow((x2-x1),2) + pow((y2-y1),2));
}

void localToGlobal(double right_x, double right_y, double left_x, double left_y){
  // translating the local points to global
  //  double X_R_curr=0, Y_R_curr=0, X_L_curr=0, Y_L_curr=0, THETAcurr=0;
  
  X_R_curr += (getDistance(0,0,right_x,right_y) * (cos(THETAcurr + (getAng(0, 0, right_x, right_y)))));
  Y_R_curr += (getDistance(0,0,right_x,right_y) * (sin(THETAcurr + (getAng(0, 0, right_x, right_y)))));

  X_L_curr += (getDistance(0,0,left_x,left_y) * (cos(THETAcurr + (getAng(0, 0, left_x, left_y)))));
  Y_L_curr += (getDistance(0,0,left_x,left_y) * (sin(THETAcurr + (getAng(0, 0, left_x, left_y)))));
  
  double car_x = (X_R_curr/m2x + X_L_curr/m1x)/2;
  double car_y = (Y_R_curr/m2y + Y_L_curr/m1y)/2;
  printf("%f,%f\n", car_x,car_y);

 
}

double newX(double xRatio, double x1, double y1, double offsetAng){
  return (pyt(0,0,x1*xRatio,y1) * cos(getAng(0,0,y1,x1*xRatio)+offsetAng));

  //double distM1 = pyt(0,0,x1*xRatio1,y1);
  //double angM1 = getAng(y1,x1*xRatio1);

  //double newCordM1X = dist * cos(ang+offsetAng);
  //double newCordM1Y = dist * sin(ang+offsetAng);

}

double newY(double xRatio, double x1, double y1, double offsetAng){
  return (pyt(0,0,x1*xRatio,y1) * sin(getAng(0,0,y1,x1*xRatio)+offsetAng));
}

int main(void){
  int i;
  int x1Buff=0;
  int y1Buff=0;
  int x2Buff=0;
  int y2Buff=0;
  int j;
  int k;
  int x1=0;
  int y1=0;
  int x2=0;
  int y2=0;
  int x1Tot=0;
  int y1Tot=0;
  int x2Tot=0;
  int y2Tot=0;

  double ALPHAv=0, ALPHAh=0, lv=0, Xv=0, XvCurr=0, Yv=0, YvCurr=0, lh=0, Xh=0, XhCurr=0, Yh=0, YhCurr=0, GAMMA=0, Rv=0, Rh=0, DELTA_THETA=0, XDELTAh=0, YDELTAh=0, XDELTAv=0, YDELTAv=0, DELTAX=0, DELTAY=0, D=100, xRatioM1=0, offsetAngM1=0, xRatioM2=0, offsetAngM2=0, m2Ratio=0;

  int m1y1Cal=0, m1x1Cal=0, m1y2Cal=0, m1x2Cal=0, m2y1Cal=0, m2x1Cal=0, m2y2Cal=0, m2x2Cal=0;



  FILE *file1;
  FILE *file2;
  FILE *file3;

  timeval  first, next;
  //if((fd = open(MOUSEFILE, O_RDONLY)) == -1) {
  //    perror("opening device");
  //    exit(EXIT_FAILURE);
  //}
  gettimeofday(&first, NULL);

  cout << " Started!\n";
  i=0;
  file1 = fopen("1.txt","r");
  file2 = fopen("2.txt","r");

  fscanf(file1,"%d#%d",&x1Buff,&y1Buff);
  fclose(file1);
  fscanf(file2,"%d#%d",&x2Buff,&y2Buff);
  fclose(file2);

  file3 = fopen("Calibration.txt","r");
  fscanf(file3,"%d#%d#%d#%d#%d#%d#%d#%d", &m1x1Cal, &m1y1Cal, &m2x1Cal, &m2y1Cal, &m1x2Cal, &m1y2Cal, &m2x2Cal, &m2y2Cal);
  fclose(file3);

  mouse_left_tilt = atan2(m1y1Cal,m1x1Cal) - PI/2;
  mouse_right_tilt = atan2(m2y1Cal,m2x1Cal) - PI/2;





  printf("%d \n%d \n%d \n%d \n%d \n%d \n%d \n%d \n\n", m1x1Cal, m1y1Cal, m2x1Cal, m2y1Cal, m1x2Cal, m1y2Cal, m2x2Cal, m2y2Cal);

  xRatioM1 = abs(m1y1Cal)/abs(m1x2Cal);
  offsetAngM1 = getAng(0,0,m1y1Cal,abs(m1x1Cal*xRatioM1));

  xRatioM2 = m2y1Cal/m2x2Cal;
  offsetAngM2 = getAng(0,0,m2y1Cal,abs(m2x1Cal*xRatioM2));
  m2Ratio = abs(m1y1Cal)/abs(m2y1Cal);

  // printf("%d \n%d \n%d \n%d \n%d \n%d \n%d \n%d \n\n", m1y1Cal, m1x1Cal, m2y1Cal, m2x1Cal, m1y2Cal, m1x2Cal, m2y2Cal, m2x2Cal);

  m1y = m1y1Cal/850;
  m2y = m2y1Cal/850;
  m1x = fabs(m1x2Cal)/745;
  m2x = fabs(m2x2Cal)/745;

  j=0;
  k=0;
  while(1){
    i++;
    if( (i % 100) == 0){ //read(fd, &ie, sizeof(struct input_event))) {
      //cout << " \n" << i%10 << "loops.\n" << i;
      file1 = fopen("1.txt","r");
      j=fscanf(file1,"%d#%d",&x1,&y1);
      fclose(file1);
      file2 = fopen("2.txt","r");
      k=fscanf(file2,"%d#%d",&x2,&y2);
      fclose(file2);


      if( (j == 2) && ( k == 2) ){
	
    	x1Tot += (XvCurr = x1 - x1Buff);
    	y1Tot += (YvCurr = y1 - y1Buff);
    	x2Tot += (XhCurr = x2 - x2Buff);
    	y2Tot += (YhCurr = y2 - y2Buff);
    	
    	x1Buff = x1;
    	y1Buff = y1;
    	x2Buff = x2;
    	y2Buff = y2;
    	
    	Xv += (XvCurr/1.0);
    	Yv += (YvCurr/1.0);
    	Xh += (XhCurr/1.0);
    	Yh += (YhCurr/1.0);
	
      	
      	
      	if((abs(Xv)>1) && (abs(Yv)>1) && (abs(Xh)>1) && (abs(Yh)>1)){
	  gettimeofday(&next, NULL);
	  //cout << (next.tv_usec - first.tv_usec) << " ms.";
    	
	  //Xv *= m1x; //newX(xRatioM1, Xv, Yv, offsetAngM1);
	  //Yv *= m1y; //newY(xRatioM1, Xv, Yv, offsetAngM1);
	  //Xh *= m2x; //newX(xRatioM2, Xh, Yh, offsetAngM2)*m2Ratio;
	  //	  Yh *= m2y; //newY(xRatioM2, Xh, Yh, offsetAngM2)*m2Ratio;
		
	  //Xcurr=0, Ycurr=0, THETAcurr=0;

	  Yv = calibrateY(Xv, Yv, mouse_left_tilt);
	  Xv = calibrateX(Xv, Yv, mouse_left_tilt);
	  Yh = calibrateY(Xh, Yh, mouse_right_tilt);
	  Xh = calibrateX(Xh, Yh, mouse_right_tilt);
	  
	
	  double start_right_x = 50;
	  double start_right_y = 0;
	  double start_left_x = -50;
	  double start_left_y = 0;
	  
	  //double D=200, Xv=100, Xh=100, Yv=100, Yh=150, X_R_curr=0, Y_R_curr=0, X_L_curr=0, Y_L_curr=0;
	  
    	
	  double traveld_X = MIN(fabs(Xv), fabs(Xh)) * (fabs(Xv)/Xv);
	  double traveld_Y = MIN(fabs(Yv), fabs(Yh)) * (fabs(Yv)/Yv);
    	
	  double curr_right_X = traveld_X + start_right_x;
	  double curr_right_Y = traveld_Y + start_right_y;
	  double curr_left_X = traveld_X + start_left_x;
	  double curr_left_Y = traveld_Y + start_left_y;
	  

	  cout << Xv << " , " << Yv << " ---- " << Xh << " , " << Yh << endl;

	  //double angle = Math.atan2(Y_R_curr - Y_L_curr, X_R_curr - X_L_curr);
	  if(fabs(Yv) > fabs(Yh))
	    {
	      double delta_Y = Yv - Yh;
	      curr_left_X = curr_right_X - D * cos(((delta_Y/D)* PI));
	      curr_left_Y = curr_right_Y + D * sin(((delta_Y/D)* PI));    		
	    } 
	  if(fabs(Yv) < fabs(Yh))
	    {
	      double delta_Y = Yh - Yv;
	      curr_right_X = curr_left_X - D * cos(PI - ((delta_Y/D) * PI));
	      curr_right_Y = curr_left_Y + D * sin(PI - ((delta_Y/D) * PI));
	    }
	  


	  double end_right_x = curr_right_X;
	  double end_right_y = curr_right_Y;
	  double end_left_x = curr_left_X;
	  double end_left_y = curr_left_Y;

	  double center_x = (end_right_x  + end_left_x) / 2;
	  double center_y = (end_right_y  + end_left_y) / 2;


	  //localToGlobal(end_right_x, end_right_y, end_left_x, end_left_y);
	  

	  X_R_curr += Xh; 
	  Y_R_curr += Yh; 

	  X_L_curr += Xv; 
	  Y_L_curr += Yv; 


	  double car_x = (X_R_curr + X_L_curr)/2;
	  double car_y = (Y_R_curr + Y_L_curr)/2;
	  printf("%f,%f\n", car_x,car_y); 
	  

	  //double angle = Math.atan2(Y_R_curr - Y_L_curr, X_R_curr - X_L_curr);

	  Xv = 0;
	  Yv = 0;
	  Xh = 0;
	  Yh = 0;
	  first = next;
      	}
      }
    }
  }

  return 0;
}

