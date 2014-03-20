#ifndef __USBD_CDC_VCP_H
#define __USBD_CDC_VCP_H

// Includes
#include "stm32f4xx_conf.h"
#include "usbd_cdc_core.h"
#include "usbd_conf.h"
#include <stdint.h>

typedef struct {
  uint32_t bitrate;
  uint8_t  format;
  uint8_t  paritytype;
  uint8_t  datatype;
} LINE_CODING;

// Functions
void VCP_put_char(uint8_t buf);
void VCP_send_str(uint8_t* buf);
void VCP_send_buffer(uint8_t* buf, int len);
void VCP_send_packet(unsigned char *data, unsigned char len);
void VCP_timerfunc();

#endif /* __USBD_CDC_VCP_H */

