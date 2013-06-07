/*
 * STM32F4 LED2 example.
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

    /* Configure PD12 and PD14 in output pushpull mode */
    GPIO_InitStructure.GPIO_Pin = GPIO_Pin_12 | GPIO_Pin_14;
    GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
    GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
    GPIO_InitStructure.GPIO_Speed = GPIO_Speed_100MHz;
    GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
    GPIO_Init(GPIOD, &GPIO_InitStructure);

    while (1) {
        /* Set PD12 Green */
        GPIOD->BSRRL = GPIO_Pin_12;
        /* Reset PD14 Red */
        GPIOD->BSRRH = GPIO_Pin_14;

        Delay(10000000L);

        /* Set PD14 Red */
        GPIOD->BSRRL = GPIO_Pin_14;
        /* Reset PD12 Green */
        GPIOD->BSRRH = GPIO_Pin_12;

        Delay(10000000L);
    }
}

