#include <stdlib.h>
#include <stdio.h>
#include "msv/include/protocol_byte.h"
#include <string.h>
#include <stdint.h>

#define TERM 0x01

/***************************** RAZOR DATA MAP *******************************
 *                [0] = Yaw    [1] = Pitch   [2] = Roll 
 * Magnetometer:  [3] = X      [4] = Y       [5] = Z
 * Gyroscope:     [6] = X      [7] = Y       [8] = Z
 * Accelerometer: [9] = X     [10] = Y      [11] = Z
 */

int round(float num){
  if((num + 0.5) >= ((int)(num) + 1))
    return ((int)(num)+1);
  else
    return ((int)(num));
}

uint32_t yaw(int razorData[12]){
  uint32_t encoded = 0;

  if(razorData[0] < -180 || razorData[0] > 180){
    encoded |= 0x0100;
    return encoded;
  }
  
  if(razorData[0] < 0){
    encoded |= 0x0100;
  }
  
  encoded |= razorData[0];
  
  return encoded;
}

uint32_t magnetometer(int razorData[12]){
  uint32_t encoded = 0;
  if(razorData[3] < -250 || razorData[3] > 250 || razorData[4] < -250 || razorData[4] > 250){
    encoded |= 0x0100;
    return encoded;
  }

  if(razorData[3] < 0){
    encoded |= 0x0100;
  }
  if(razorData[4] < 0){
    encoded |= 0x0100 << 9;
  }
  
  encoded |= razorData[3];
  encoded |= razorData[4] << 9;

  return encoded;
}

uint32_t gyroscope(int razorData[12]){
  uint32_t encoded = 0;
  if(razorData[6] < -250 || razorData[6] > 250 || razorData[7] < -250 || razorData[7] > 250){
    encoded |= 0x0100;
    return encoded;
  }
  
  if(razorData[6] < 0){
    encoded |= 0x0100;
  }
  if(razorData[7] < 0){
    encoded |= 0x0100 << 9;
  }
  
  encoded |= razorData[6];
  encoded |= razorData[7] << 9;
  
  return encoded;
}

uint32_t accelerometer_razor(int razorData[12]){
  uint32_t encoded = 0;

  if(razorData[9] < -250 || razorData[9] > 250 || razorData[10] < -250 || razorData[10] > 250){
    encoded |= 0x0100;
    return encoded;
  }

  if(razorData[9] < 0){
    encoded |= 0x0100;
  }
  if(razorData[10] < 0){
    encoded |= 0x0100 << 9;
  }
  
  encoded |= razorData[9];
  encoded |= razorData[10] << 9;
  
  return encoded;
}

uint32_t accelerometer_discovery(int8_t discoveryData[2]){
  uint32_t encoded = 0;

  if(discoveryData[0] < -250 || discoveryData[0] > 250 || discoveryData[1] < -250 || discoveryData[1] > 250){
    encoded |= 0x0100;
    return encoded;
  }
  
  if(discoveryData[0] < 0){
    encoded |= 0x0100;
  }
  if(discoveryData[1] < 0){
    encoded |= 0x0100 << 9;
  }
  
  encoded |= discoveryData[0];
  encoded |= discoveryData[1] << 9;

  return encoded;
}

uint8_t ultrasonic(int value){
  uint8_t encoded;
  if(value <= 63){
    encoded = value;
  }else if(value <= 189){
    encoded = value / 3;
    encoded |= 1 << 6;
  }else if(value <= 315){
    encoded = value / 5;
    encoded |= 1 << 7;
  }else if(value <= 630){
    encoded = value / 10;
    encoded |= 1 << 6;
    encoded |= 1 << 7;
  }else{
    encoded = 0;
  }
  return encoded;
}
uint32_t infrared(int ir1, int ir2, int ir3){
  uint32_t encoded = 0;
  encoded |= ir1;
  encoded |= ir2 << 5;
  encoded |= ir3 << 10;
  return encoded;
}

uint32_t currentPos(float imuInfo[7]){
  uint32_t encoded = 0;

  if(imuInfo[1] < -25 || imuInfo[1] > 25 || imuInfo[2] < -25 || imuInfo[2] > 25){
    encoded |= 0x1000;
    return encoded;
  }
  
  if(imuInfo[1] < 0){
    encoded |= 0x1000;
  }
  if(imuInfo[2] < 0){
    encoded |= 0x1000 << 13;
  }
  
  encoded |= (int)(imuInfo[1]*100);
  encoded |= (int)(imuInfo[2]*100) << 13;

  return encoded;
}

uint32_t traveled_path(float imuInfo[7]){
  uint32_t encoded = 0;
  
  encoded |= (int)((imuInfo[3]/2)*100);
  encoded |= (int)((imuInfo[4]/2)*100) << 6;
 
  return encoded;
}

uint32_t current_velocity(float imuInfo[7]){
  uint32_t encoded = 0;

  if(imuInfo[5] < -511 || imuInfo[5] > 511 || imuInfo[6] < -511 || imuInfo[6] > 511){
    encoded |= 0x0200;
    return encoded;
  }
  
  if(imuInfo[5] < 0){
    encoded |= 0x0200;
  }
  if(imuInfo[6] < 0){
    encoded |= 0x1000 << 10;
  }
  
  encoded |= (int)(imuInfo[5]);
  encoded |= (int)(imuInfo[6]) << 10;

  return encoded;
}

uint32_t current_orientation(float imuInfo[7]){
  uint32_t encoded = 0;

  encoded |=  round((imuInfo[7]/180)*1000);
  
  return encoded;
}

void translate(int request, int ir_data[3], int16_t us_data[3],int razorData[12], float imuInfo[7], int8_t discoveryAccelData[2], uint8_t data[4]){ 
  uint32_t package = 0;
  package |= request;
  
  switch(request){
  case 1:
    package |= infrared(ir_data[0],ir_data[1],ir_data[2]) << 4;
    break;
  case 2:
    package |= ultrasonic(us_data[0]) << 4;
    package |= ultrasonic(us_data[1]) << 12;
    package |= ultrasonic(us_data[2]) << 20;
    break;
  case 3: 
    package |= yaw(razorData) << 4;
    break;
  case 4:
    package |= magnetometer(razorData) << 4;
    break;
  case 5:
    package |= gyroscope(razorData) << 4;
    break;
  case 6:
    package |= accelerometer_razor(razorData) << 4;
    break;
  case 7:
    package |= accelerometer_discovery(discoveryAccelData) << 4;
    break;
  case 10:
    package |= currentPos(imuInfo) << 4;
    break;
  case 11:
    package |= traveled_path(imuInfo) << 4;
    break;
  case 12:
    package |= current_velocity(imuInfo) << 4;
    break;
  case 13:
    package |= current_orientation(imuInfo) << 4;
    break;
  }
  package |= TERM << 30;

  data[0]= (package >> 24);
  data[1]= (package >> 16) & 0xFF;
  data[2]= (package >> 8) & 0xFF;
  data[3]= package & 0xFF;
}
