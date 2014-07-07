/* Contains functions for accessing the uEye wide angle camera lense. Functions
 * included are for initializing the camera, retrieving an image, and
 * deinitializing the camera
 *
 */


#include <iostream>
#include <opencv/cv.h>
#include <opencv/highgui.h>
#include "ueye.h"

using namespace std;
using namespace cv;

// Global variables for camera functions
HIDS hCam = 0;
char* ppcImgMem;
int pid;

/* Initializes the uEye camera. If camera initialization is successful, it
 * returns true, otherwise returns false */
bool init_camera()
{
  int nRet = is_InitCamera (&hCam, NULL);

  is_AllocImageMem(hCam,752, 480, 1 ,&ppcImgMem, &pid);  
  is_SetImageMem(hCam, ppcImgMem, pid);
  is_SetDisplayMode (hCam, IS_SET_DM_DIB);
  is_SetColorMode (hCam, IS_CM_MONO8);
  int pnCol , pnColMode;
  is_GetColorDepth(hCam, &pnCol , &pnColMode);

  is_CaptureVideo(hCam, IS_WAIT);

  if (nRet != IS_SUCCESS)
    {
      if (nRet == IS_STARTER_FW_UPLOAD_NEEDED)
      {
        hCam = hCam | IS_ALLOW_STARTER_FW_UPLOAD;
        nRet = is_InitCamera (&hCam, NULL);
      }
      cout << "camera failed to initialize " << endl;
      return false;
    }
  else
    return true;
}

/* Deinitializes the uEye camera. Returns true if deinitialization is
* successful, otherwise returns false */
bool deinit_camera()
{

  // Free the allocated memory
  is_FreeImageMem(hCam, ppcImgMem, pid);

  // Try to deinitialize camera. If successful, return true, otherwise return
  // false
  if(is_ExitCamera(hCam) == IS_SUCCESS)
    return true;
  else
    return false;
}

/* Retrieves an image from the camera, in the form of a signed char pointer. Signed
 * char is meant for IPL images. If the data is to be used with a Mat, the char
 * should be cast into an unsigned char i.e. (uchar* ). 
 *
 * Arguments: char* to be passed in by reference, which will contain the 
 * image. I.e. char* imgPointer; get_image(imgPointer);
 * Returns a boolean, true if image is retrieved, false if it fails.
*/
bool get_image(char*& img)
{
  void *pMemVoid; //pointer to where the image is stored

	// Takes an image from the camera. If successful, returns true, otherwise
	// returns false
  if (is_GetImageMem(hCam, &pMemVoid) == IS_SUCCESS){

    img = (char*) pMemVoid;
    return true;
  }
  else
    return false;
}
 
