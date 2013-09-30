/* Contains functions for accessing the uEye wide angle camera lense. Functions
 * included are for initializing the camera, retrieving an image, and
 * deinitializing the camera
*/

#include <iostream>
#include <opencv/cv.h>
#include <opencv/highgui.h>
#include "/usr/include/ueye.h"

using namespace std;
using namespace cv;
HIDS hCam = 0;

/* Initializes the uEye camera */
void init_camera()
{
    int nRet = is_InitCamera (&hCam, NULL);

    if (nRet != IS_SUCCESS)
    {
        if (nRet == IS_STARTER_FW_UPLOAD_NEEDED)
        {
            hCam = hCam | IS_ALLOW_STARTER_FW_UPLOAD;
            nRet = is_InitCamera (&hCam, NULL);
        }
        cout << "camera failed to initialize " << endl;
     }

     cout << "NR is " << nRet << endl;

}

/* Deinitializes the uEye camera */
void deinit_camera()
{
    is_ExitCamera(hCam);
}

/* Retrieves an image from the camera, in the form of a signed char. Signed
 * char is meant for IPL images. If the data is to be used with a Mat, the char
 * should be cast into an unsigned char i.e. (uchar* )
*/
char* get_image()
{
    char* ppcImgMem;
    int pid;
    cout << " AllocMem result: " << is_AllocImageMem(hCam,752, 480, 1 ,&ppcImgMem, &pid) << endl; 
    cout << " SET IMAGE result " << is_SetImageMem(hCam, ppcImgMem, pid) << endl;

    is_SetDisplayMode (hCam, IS_SET_DM_DIB);
    is_SetColorMode (hCam, IS_CM_MONO8);
    int pnCol , pnColMode;
    is_GetColorDepth(hCam, &pnCol , &pnColMode);
    int result;
    int m_nSizeX = 752;
    int m_nSizeY = 480;

    Mat resultMat(480,752,CV_8U);

	// Takes an image from the camera
    if (is_FreezeVideo(hCam, IS_WAIT) == IS_SUCCESS){

        void *pMemVoid; //pointer to where the image is stored
        is_GetImageMem(hCam, &pMemVoid);

        return (char*)pMemVoid;
    }
}
