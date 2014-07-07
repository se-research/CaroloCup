/*
 * STM32F4 LED1 example.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "stm32f4xx_conf.h"

GPIO_InitTypeDef  GPIO_InitStructure;

void Delay(__IO uint32_t nCount) {
    while(nCount--) {}
}

int main(void) {
    /* GPIOD Periph clock enable */
    RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

    /* Configure PD13 and PD15 in output pushpull mode */
    GPIO_InitStructure.GPIO_Pin = GPIO_Pin_13 | GPIO_Pin_15;
    GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
    GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
    GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
    GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
    GPIO_Init(GPIOD, &GPIO_InitStructure);

    while (1) {
        /* Set PD13 Orange */
        GPIOD->BSRRL = GPIO_Pin_13;
        /* Reset PD15 Blue */
        GPIOD->BSRRH = GPIO_Pin_15;

        Delay(10000000L);

        /* Set PD15 Blue */
        GPIOD->BSRRL = GPIO_Pin_15;
        /* Reset PD13 Orange */
        GPIOD->BSRRH = GPIO_Pin_13;

        Delay(10000000L);
    }
}

