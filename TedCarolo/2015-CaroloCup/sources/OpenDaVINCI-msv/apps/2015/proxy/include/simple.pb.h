/* Automatically generated nanopb header */
/* Generated by nanopb-0.2.7 at Tue May 13 16:26:16 2014. */

#ifndef _PB_SIMPLE_PB_H_
#define _PB_SIMPLE_PB_H_
#include <pb.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Enum definitions */
/* Struct definitions */
typedef struct _arduinoToUdoo {
    int32_t us_left;
    int32_t us_center;
    int32_t us_right;
    int32_t ir_left;
    int32_t ir_right_front;
    int32_t ir_right_back;
} arduinoToUdoo;

typedef struct _udooToArduino {
    bool has_speed;
    int32_t speed;
    bool has_steering;
    int32_t steering;
    bool has_leftBlink;
    bool leftBlink;
    bool has_rightBlink;
    bool rightBlink;
    bool has_stopLight;
    bool stopLight;
} udooToArduino;

/* Default values for struct fields */

/* Field tags (for use in manual encoding/decoding) */
#define arduinoToUdoo_us_left_tag                1
#define arduinoToUdoo_us_center_tag              2
#define arduinoToUdoo_us_right_tag               3
#define arduinoToUdoo_ir_left_tag                4
#define arduinoToUdoo_ir_right_front_tag         5
#define arduinoToUdoo_ir_right_back_tag          6
#define udooToArduino_speed_tag                  1
#define udooToArduino_steering_tag               2
#define udooToArduino_leftBlink_tag              3
#define udooToArduino_rightBlink_tag             4
#define udooToArduino_stopLight_tag              5

/* Struct field encoding specification for nanopb */
extern const pb_field_t arduinoToUdoo_fields[7];
extern const pb_field_t udooToArduino_fields[6];

/* Maximum encoded size of messages (where known) */
#define arduinoToUdoo_size                       66
#define udooToArduino_size                       28

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif