#ifdef USB_OTG_HS_INTERNAL_DMA_ENABLED 
#pragma     data_alignment = 4 
#endif /* USB_OTG_HS_INTERNAL_DMA_ENABLED */

#include "usbd_cdc_vcp.h"
#include "stm32f4xx_conf.h"
#include "crc.h"
#include "main.h"
#include "comm.h"

// Settings
#define RX_TIMEOUT				2

// Private variables
LINE_CODING linecoding = {
		115200, /* baud rate*/
		0x00, /* stop bits-1*/
		0x00, /* parity - none*/
		0x08 /* nb. of bits 8*/
};

// External variables
extern uint8_t APP_Rx_Buffer[];
extern uint32_t APP_Rx_ptr_in;

// Packet variables
static volatile unsigned char rx_state = 0;
static volatile unsigned char rx_timeout = 0;

// Private function prototypes
static uint16_t VCP_Init(void);
static uint16_t VCP_DeInit(void);
static uint16_t VCP_Ctrl(uint32_t Cmd, uint8_t* Buf, uint32_t Len);
static uint16_t VCP_DataTx(uint8_t* Buf, uint32_t Len);
static uint16_t VCP_DataRx(uint8_t* Buf, uint32_t Len);
static void process_packet(unsigned char *data, unsigned char len);

CDC_IF_Prop_TypeDef VCP_fops = { VCP_Init, VCP_DeInit, VCP_Ctrl, VCP_DataTx,
		VCP_DataRx };

/* Private functions ---------------------------------------------------------*/
/**
 * @brief  VCP_Init
 *         Initializes the Media on the STM32
 * @param  None
 * @retval Result of the opeartion (USBD_OK in all cases)
 */
static uint16_t VCP_Init(void) {
	return USBD_OK;
}

/**
 * @brief  VCP_DeInit
 *         DeInitializes the Media on the STM32
 * @param  None
 * @retval Result of the opeartion (USBD_OK in all cases)
 */
static uint16_t VCP_DeInit(void) {
	return USBD_OK;
}

/**
 * @brief  VCP_Ctrl
 *         Manage the CDC class requests
 * @param  Cmd: Command code
 * @param  Buf: Buffer containing command data (request parameters)
 * @param  Len: Number of data to be sent (in bytes)
 * @retval Result of the opeartion (USBD_OK in all cases)
 */
static uint16_t VCP_Ctrl(uint32_t Cmd, uint8_t* Buf, uint32_t Len) {
	switch (Cmd) {
	case SEND_ENCAPSULATED_COMMAND:
		/* Not  needed for this driver */
		break;

	case GET_ENCAPSULATED_RESPONSE:
		/* Not  needed for this driver */
		break;

	case SET_COMM_FEATURE:
		/* Not  needed for this driver */
		break;

	case GET_COMM_FEATURE:
		/* Not  needed for this driver */
		break;

	case CLEAR_COMM_FEATURE:
		/* Not  needed for this driver */
		break;

	case SET_LINE_CODING:
		/* Not  needed for this driver */
		break;

	case GET_LINE_CODING:
		Buf[0] = (uint8_t) (linecoding.bitrate);
		Buf[1] = (uint8_t) (linecoding.bitrate >> 8);
		Buf[2] = (uint8_t) (linecoding.bitrate >> 16);
		Buf[3] = (uint8_t) (linecoding.bitrate >> 24);
		Buf[4] = linecoding.format;
		Buf[5] = linecoding.paritytype;
		Buf[6] = linecoding.datatype;
		break;

	case SET_CONTROL_LINE_STATE:
		/* Not  needed for this driver */
		break;

	case SEND_BREAK:
		/* Not  needed for this driver */
		break;

	default:
		break;
	}

	return USBD_OK;
}

/**
 * @brief  putchar
 *         Sends one char over the USB serial link.
 * @param  buf: char to be sent
 * @retval none
 */
void VCP_put_char(uint8_t buf) {
	VCP_DataTx(&buf, 1);
}

void VCP_send_str(uint8_t* buf) {
	uint32_t i = 0;
	while (*(buf + i)) {
		i++;
	}
	VCP_DataTx(buf, i);
}

void VCP_send_buffer(uint8_t* buf, int len) {
	VCP_DataTx(buf, len);
}

/**
 * @brief  VCP_DataTx
 *         CDC received data to be send over USB IN endpoint are managed in
 *         this function.
 * @param  Buf: Buffer of data to be sent
 * @param  Len: Number of data to be sent (in bytes)
 * @retval Result of the opeartion: USBD_OK if all operations are OK else VCP_FAIL
 */
static uint16_t VCP_DataTx(uint8_t* Buf, uint32_t Len) {
	uint32_t i = 0;
	while (i < Len) {
		APP_Rx_Buffer[APP_Rx_ptr_in] = *(Buf + i);
		APP_Rx_ptr_in++;
		i++;
		/* To avoid buffer overflow */
		if (APP_Rx_ptr_in == APP_RX_DATA_SIZE) {
			APP_Rx_ptr_in = 0;
		}
	}

	return USBD_OK;
}

void VCP_send_packet(unsigned char *data, unsigned char len) {
	VCP_put_char(2);
	VCP_put_char(len);
	VCP_DataTx(data, len);
	unsigned short crc = crc16(data, len);
	VCP_put_char(crc >> 8);
	VCP_put_char(crc);
	VCP_put_char(3);
}

/*
 * Call this function every millisecond
 */
void VCP_timerfunc() {
	if (rx_timeout) {
		rx_timeout--;
	} else {
		rx_timeout = 0;
	}
}

/**
 * @brief  VCP_DataRx
 *         Data received over USB OUT endpoint are sent over CDC interface
 *         through this function.
 *
 *         @note
 *         This function will block any OUT packet reception on USB endpoint
 *         until exiting this function. If you exit this function before transfer
 *         is complete on CDC interface (ie. using DMA controller) it will result
 *         in receiving more data while previous ones are still not sent.
 *
 * @param  Buf: Buffer of data to be received
 * @param  Len: Number of data received (in bytes)
 * @retval Result of the opeartion: USBD_OK if all operations are OK else VCP_FAIL
 */
static uint16_t VCP_DataRx(uint8_t* Buf, uint32_t Len) {
	static unsigned char payload_length = 0;
	static unsigned char rx_buffer[256];
	static unsigned char rx_data_ptr = 0;
	static unsigned char crc_low = 0;
	static unsigned char crc_high = 0;
	unsigned char rx_data;

	for(int i = 0;i < Len;i++) {
		rx_data = Buf[i];

		switch (rx_state) {
		case 0:
			if (rx_data == 2) {
				rx_state++;
				rx_timeout = RX_TIMEOUT
						;
				rx_data_ptr = 0;
			} else {
				rx_state = 0;
			}
			break;

		case 1:
			payload_length = rx_data;
			rx_state++;
			rx_timeout = RX_TIMEOUT
					;
			break;

		case 2:
			rx_buffer[rx_data_ptr++] = rx_data;
			if (rx_data_ptr == payload_length) {
				rx_state++;
			}
			rx_timeout = RX_TIMEOUT
					;
			break;

		case 3:
			crc_high = rx_data;
			rx_state++;
			rx_timeout = RX_TIMEOUT
					;
			break;

		case 4:
			crc_low = rx_data;
			rx_state++;
			rx_timeout = RX_TIMEOUT
					;
			break;

		case 5:
			if (rx_data == 3) {
				if (crc16(rx_buffer, payload_length)
						== ((unsigned short) crc_high << 8
								| (unsigned short) crc_low)) {
					// Packet received!
					process_packet(rx_buffer, payload_length);
				}
			}

			rx_state = 0;
			break;

		default:
			rx_state = 0;
			break;
		}
	}

	return USBD_OK;
}

static void process_packet(unsigned char *data, unsigned char len) {
	if (!len) {
		return;
	}

	switch (data[0]) {
	case 0:
		// Send received data to main loop
		main_process_packet(data + 1, len - 1);
		break;

	case 1:
		/*
		 * Packet that expects response
		 */
		comm_handle_res_packet(data + 1, len - 1);
		break;

	case 2:
		/*
		 * Packet that expects no response
		 */
		comm_handle_nores_packet(data + 1, len - 1);
		break;

	default:
		break;
	}
}
