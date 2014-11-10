/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

/**
 * Simplified implementation of sscanf supporting %d, %x, %c, %n:
 *
 * %d dec integer (e.g.: 42)
 * %x hex integer (e.g.: 0xa0)
 * %b bin integer (e.g.: b1010100010)
 * %n hex, dec or bin integer (e.g: 42, 0xa0, b1010100010)
 * %c any character
 */
int rsscanf(const char* str, const char* format, ...);

