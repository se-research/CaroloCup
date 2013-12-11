#include "Transforms.h"

Point2f ipm(Point img_pt, CameraStruct cam) {
    float f = cam.focal;
    float u0 = cam.u0;
    float v0 = cam.v0;
    float theta0 = cam.theta0;
    float gamma0 = cam.gamma0;
    float h = cam.height;
    int img_height = cam.size.height;
    int img_width = cam.size.width;
    float alpha_u = atan(u0/f);
    float alpha_v = atan(v0/f);
    float x = h/tan((theta0-alpha_u) + img_pt[0]*2*alpha_u/(img_height-1))*
            cos((gamma0-alpha_v) + img_pt[1]*2*alpha_v/(img_width-1));
    float z = h/tan((theta0-alpha_u) + img_pt[0]*2*alpha_u/(img_height-1))*
            sin((gamma0-alpha_v) + img_pt[1]*2*alpha_v/(img_width-1));
    return Point2f(x, z);
}