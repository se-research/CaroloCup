/*
	Copyright 2012 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#include "stm32f4xx_it.h"
#include "stm32f4xx_conf.h"
#include "main.h"
#include "mcpwm.h"
#include "usbd_cdc_vcp.h"
#include "servo.h"
#include "dead_reckoning.h"
#include "comm.h"
#include "servo_dec.h"

#include "usb_core.h"
#include "usbd_core.h"
#include "usbd_cdc_core.h"

extern uint32_t USBD_OTG_ISR_Handler(USB_OTG_CORE_HANDLE *pdev);
extern USB_OTG_CORE_HANDLE USB_OTG_dev;

#ifdef USB_OTG_HS_DEDICATED_EP1_ENABLED
extern uint32_t USBD_OTG_EP1IN_ISR_Handler (USB_OTG_CORE_HANDLE *pdev);
extern uint32_t USBD_OTG_EP1OUT_ISR_Handler (USB_OTG_CORE_HANDLE *pdev);
#endif

/**
 * @brief   This function handles NMI exception.
 * @param  None
 * @retval None
 */
void NMI_Handler(void) {
}

/**
 * @brief  This function handles Hard Fault exception.
 * @param  None
 * @retval None
 */
void HardFault_Handler(void) {
	/* Go to infinite loop when Hard Fault exception occurs */
	while (1) {
	}
}

/**
 * @brief  This function handles Memory Manage exception.
 * @param  None
 * @retval None
 */
void MemManage_Handler(void) {
	/* Go to infinite loop when Memory Manage exception occurs */
	while (1) {
	}
}

/**
 * @brief  This function handles Bus Fault exception.
 * @param  None
 * @retval None
 */
void BusFault_Handler(void) {
	/* Go to infinite loop when Bus Fault exception occurs */
	while (1) {
	}
}

/**
 * @brief  This function handles Usage Fault exception.
 * @param  None
 * @retval None
 */
void UsageFault_Handler(void) {
	/* Go to infinite loop when Usage Fault exception occurs */
	while (1) {
	}
}

/**
 * @brief  This function handles SVCall exception.
 * @param  None
 * @retval None
 */
void SVC_Handler(void) {

}

/**
 * @brief  This function handles Debug Monitor exception.
 * @param  None
 * @retval None
 */
void DebugMon_Handler(void) {

}

/**
 * @brief  This function handles PendSVC exception.
 * @param  None
 * @retval None
 */
void PendSV_Handler(void) {
}

/**
 * @brief  This function handles SysTick Handler.
 * @param  None
 * @retval None
 */
void SysTick_Handler(void) {
	timing_handler();
	mcpwm_time_int_handler();
	VCP_timerfunc();
	dr_update_func();
	servodec_timerfunc();
}

#ifdef USE_USB_OTG_FS
void OTG_FS_WKUP_IRQHandler(void) {
	if (USB_OTG_dev.cfg.low_power) {
		*(uint32_t *) (0xE000ED10) &= 0xFFFFFFF9;
		SystemInit();
		USB_OTG_UngateClock(&USB_OTG_dev);
	}
	EXTI_ClearITPendingBit(EXTI_Line18 );
}
#endif

#ifdef USE_USB_OTG_HS
void OTG_HS_WKUP_IRQHandler(void)
{
	if(USB_OTG_dev.cfg.low_power)
	{
		*(uint32_t *)(0xE000ED10) &= 0xFFFFFFF9;
		SystemInit();
		USB_OTG_UngateClock(&USB_OTG_dev);
	}
	EXTI_ClearITPendingBit(EXTI_Line20);
}
#endif

/**
 * @brief  This function handles OTG_HS Handler.
 * @param  None
 * @retval None
 */
#ifdef USE_USB_OTG_HS
void OTG_HS_IRQHandler(void)
#else
void OTG_FS_IRQHandler(void)
#endif
{
	USBD_OTG_ISR_Handler(&USB_OTG_dev);
}

#ifdef USB_OTG_HS_DEDICATED_EP1_ENABLED
/**
 * @brief  This function handles EP1_IN Handler.
 * @param  None
 * @retval None
 */
void OTG_HS_EP1_IN_IRQHandler(void)
{
	USBD_OTG_EP1IN_ISR_Handler (&USB_OTG_dev);
}

/**
 * @brief  This function handles EP1_OUT Handler.
 * @param  None
 * @retval None
 */
void OTG_HS_EP1_OUT_IRQHandler(void)
{
	USBD_OTG_EP1OUT_ISR_Handler (&USB_OTG_dev);
}
#endif

void TIM1_TRG_COM_TIM11_IRQHandler(void) {
	TIM_ClearITPendingBit(TIM1, TIM_IT_COM );
	mcpwm_comm_int_handler();
}

void DMA2_Stream0_IRQHandler(void) {
	if (DMA_GetITStatus(DMA2_Stream0, DMA_IT_TCIF0 )) {
		DMA_ClearITPendingBit(DMA2_Stream0, DMA_IT_TCIF0 );
		mcpwm_adc_int_handler();
		dma_adc_handler();
	}
}

void ADC_IRQHandler(void) {
	ADC_ClearITPendingBit(ADC1, ADC_IT_JEOC);
	mcpwm_adc_inj_int_handler();
}

void TIM7_IRQHandler(void) {
	TIM_ClearITPendingBit(TIM7, TIM_IT_Update);
	servo_tim_isr();
}

void EXTI3_IRQHandler(void) {
	if (EXTI_GetITStatus(EXTI_Line3) != RESET) {
		servodec_int_handler();
		EXTI_ClearITPendingBit(EXTI_Line3);
	}
}

void EXTI15_10_IRQHandler(void) {
	if (EXTI_GetITStatus(EXTI_Line13) != RESET) {
		servodec_int_handler();
		EXTI_ClearITPendingBit(EXTI_Line13);
	}

	if (EXTI_GetITStatus(EXTI_Line14) != RESET) {
		servodec_int_handler();
		EXTI_ClearITPendingBit(EXTI_Line14);
	}
}
