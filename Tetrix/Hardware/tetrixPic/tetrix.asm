
_Interrupt:

	CALL        _USB_Interrupt_Proc+0, 0
L__Interrupt109:
	RETFIE      1
; end of _Interrupt

_pharse:

	CLRF        pharse_x+0 
	CLRF        pharse_z+0 
	CLRF        pharse_i+0 
L__pharse3:
	MOVF        pharse_i+0, 0 
	ADDWF       FARG_pharse_buff+0, 0 
	MOVWF       FSR0L 
	MOVLW       0
	ADDWFC      FARG_pharse_buff+1, 0 
	MOVWF       FSR0H 
	MOVF        POSTINC0+0, 0 
	XORLW       44
	BTFSS       STATUS+0, 2 
	GOTO        L__pharse8
	MOVLW       pharse_words+0
	MOVWF       FLOC__pharse+0 
	MOVLW       hi_addr(pharse_words+0)
	MOVWF       FLOC__pharse+1 
	MOVF        pharse_x+0, 0 
	MOVWF       R0 
	MOVLW       0
	MOVWF       R1 
	MOVLW       34
	MOVWF       R4 
	MOVLW       0
	MOVWF       R5 
	CALL        _Mul_16x16_U+0, 0
	MOVF        R0, 0 
	ADDWF       FLOC__pharse+0, 0 
	MOVWF       R2 
	MOVF        R1, 0 
	ADDWFC      FLOC__pharse+1, 0 
	MOVWF       R3 
	MOVF        pharse_z+0, 0 
	SUBWF       pharse_i+0, 0 
	MOVWF       R0 
	MOVF        R0, 0 
	ADDWF       R2, 0 
	MOVWF       FSR1L 
	MOVLW       0
	ADDWFC      R3, 0 
	MOVWF       FSR1H 
	CLRF        POSTINC1+0 
	MOVF        pharse_i+0, 0 
	ADDLW       1
	MOVWF       pharse_z+0 
	INCF        pharse_x+0, 1 
	GOTO        L__pharse9
L__pharse8:
	MOVLW       pharse_words+0
	MOVWF       FLOC__pharse+0 
	MOVLW       hi_addr(pharse_words+0)
	MOVWF       FLOC__pharse+1 
	MOVF        pharse_x+0, 0 
	MOVWF       R0 
	MOVLW       0
	MOVWF       R1 
	MOVLW       34
	MOVWF       R4 
	MOVLW       0
	MOVWF       R5 
	CALL        _Mul_16x16_U+0, 0
	MOVF        R0, 0 
	ADDWF       FLOC__pharse+0, 0 
	MOVWF       R2 
	MOVF        R1, 0 
	ADDWFC      FLOC__pharse+1, 0 
	MOVWF       R3 
	MOVF        pharse_z+0, 0 
	SUBWF       pharse_i+0, 0 
	MOVWF       R0 
	MOVF        R0, 0 
	ADDWF       R2, 0 
	MOVWF       FSR1L 
	MOVLW       0
	ADDWFC      R3, 0 
	MOVWF       FSR1H 
	MOVF        pharse_i+0, 0 
	ADDWF       FARG_pharse_buff+0, 0 
	MOVWF       FSR0L 
	MOVLW       0
	ADDWFC      FARG_pharse_buff+1, 0 
	MOVWF       FSR0H 
	MOVF        POSTINC0+0, 0 
	MOVWF       POSTINC1+0 
L__pharse9:
	MOVF        pharse_i+0, 0 
	XORLW       64
	BTFSC       STATUS+0, 2 
	GOTO        L__pharse6
	INCF        pharse_i+0, 1 
	GOTO        L__pharse3
L__pharse6:
	MOVF        FARG_pharse_speedAngle+0, 0 
	MOVWF       FARG_strcpy_s1+0 
	MOVF        FARG_pharse_speedAngle+1, 0 
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       37
	MOVWF       ?LocalText_pharse+0 
	CLRF        ?LocalText_pharse+1 
	MOVLW       ?LocalText_pharse+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_pharse+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVF        FARG_pharse_speedAngle+0, 0 
	MOVWF       FARG_strcat_s1+0 
	MOVF        FARG_pharse_speedAngle+1, 0 
	MOVWF       FARG_strcat_s1+1 
	MOVLW       pharse_words+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(pharse_words+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVF        FARG_pharse_speedAngle+0, 0 
	MOVWF       FARG_strcat_s1+0 
	MOVF        FARG_pharse_speedAngle+1, 0 
	MOVWF       FARG_strcat_s1+1 
	MOVLW       44
	MOVWF       ?LocalText_pharse+0 
	CLRF        ?LocalText_pharse+1 
	MOVLW       ?LocalText_pharse+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(?LocalText_pharse+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVF        FARG_pharse_speedAngle+0, 0 
	MOVWF       FARG_strcat_s1+0 
	MOVF        FARG_pharse_speedAngle+1, 0 
	MOVWF       FARG_strcat_s1+1 
	MOVLW       pharse_words+34
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(pharse_words+34)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVF        FARG_pharse_speedAngle+0, 0 
	MOVWF       FARG_strcat_s1+0 
	MOVF        FARG_pharse_speedAngle+1, 0 
	MOVWF       FARG_strcat_s1+1 
	MOVLW       124
	MOVWF       ?LocalText_pharse+0 
	CLRF        ?LocalText_pharse+1 
	MOVLW       ?LocalText_pharse+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(?LocalText_pharse+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVF        FARG_pharse_text1+0, 0 
	MOVWF       FSR1L 
	MOVF        FARG_pharse_text1+1, 0 
	MOVWF       FSR1H 
	MOVLW       pharse_words+68
	MOVWF       FSR2L 
	MOVLW       hi_addr(pharse_words+68)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	MOVF        FARG_pharse_text2+0, 0 
	MOVWF       FSR1L 
	MOVF        FARG_pharse_text2+1, 0 
	MOVWF       FSR1H 
	MOVLW       pharse_words+102
	MOVWF       FSR2L 
	MOVLW       hi_addr(pharse_words+102)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	MOVLW       pharse_words+136
	MOVWF       FARG_StrToInt_input+0 
	MOVLW       hi_addr(pharse_words+136)
	MOVWF       FARG_StrToInt_input+1 
	CALL        _StrToInt+0, 0
	MOVFF       FARG_pharse_led+0, FSR1L
	MOVFF       FARG_pharse_led+1, FSR1H
	MOVF        R0, 0 
	MOVWF       POSTINC1+0 
	RETURN      0
; end of _pharse

_ledsActivity:

	MOVF        FARG_ledsActivity_led+0, 0 
	MOVWF       R0 
	RRCF        R0, 1 
	BCF         R0, 7 
	RRCF        R0, 1 
	BCF         R0, 7 
	MOVLW       1
	ANDWF       R0, 0 
	MOVWF       R1 
	MOVF        FARG_ledsActivity_led+0, 0 
	MOVWF       R3 
	RRCF        R3, 1 
	BCF         R3, 7 
	MOVLW       1
	ANDWF       R3, 1 
	MOVLW       1
	ANDWF       FARG_ledsActivity_led+0, 0 
	MOVWF       R2 
	MOVF        R1, 0 
	XORLW       1
	BTFSS       STATUS+0, 2 
	GOTO        L__ledsActivity12
	BSF         PORTD+0, 0 
	GOTO        L__ledsActivity13
L__ledsActivity12:
	MOVLW       254
	ANDWF       PORTD+0, 1 
L__ledsActivity13:
	MOVF        R3, 0 
	XORLW       1
	BTFSS       STATUS+0, 2 
	GOTO        L__ledsActivity15
	BSF         PORTD+0, 1 
	GOTO        L__ledsActivity16
L__ledsActivity15:
	MOVLW       253
	ANDWF       PORTD+0, 1 
L__ledsActivity16:
	MOVF        R2, 0 
	XORLW       1
	BTFSS       STATUS+0, 2 
	GOTO        L__ledsActivity18
	BSF         PORTD+0, 2 
	GOTO        L__ledsActivity19
L__ledsActivity18:
	MOVLW       251
	ANDWF       PORTD+0, 1 
L__ledsActivity19:
	RETURN      0
; end of _ledsActivity

_delSpaces:

	MOVF        R0, 0 
	MOVWF       delSpaces_local_result+0 
	MOVF        R1, 0 
	MOVWF       delSpaces_local_result+1 
	CLRF        delSpaces_j+0 
	CLRF        delSpaces_i+0 
L__delSpaces21:
	MOVLW       128
	XORWF       FARG_delSpaces_size+0, 0 
	MOVWF       R0 
	MOVLW       128
	XORWF       delSpaces_i+0, 0 
	SUBWF       R0, 0 
	BTFSS       STATUS+0, 0 
	GOTO        L__delSpaces25
	MOVF        delSpaces_i+0, 0 
	ADDWF       FARG_delSpaces_source+0, 0 
	MOVWF       FSR0L 
	MOVLW       0
	BTFSC       delSpaces_i+0, 7 
	MOVLW       255
	ADDWFC      FARG_delSpaces_source+1, 0 
	MOVWF       FSR0H 
	MOVF        POSTINC0+0, 0 
	XORLW       32
	BTFSC       STATUS+0, 2 
	GOTO        L__delSpaces27
	MOVLW       delSpaces_res+0
	MOVWF       R0 
	MOVLW       hi_addr(delSpaces_res+0)
	MOVWF       R1 
	MOVF        delSpaces_j+0, 0 
	ADDWF       R0, 0 
	MOVWF       FSR1L 
	MOVLW       0
	BTFSC       delSpaces_j+0, 7 
	MOVLW       255
	ADDWFC      R1, 0 
	MOVWF       FSR1H 
	MOVF        delSpaces_i+0, 0 
	ADDWF       FARG_delSpaces_source+0, 0 
	MOVWF       FSR0L 
	MOVLW       0
	BTFSC       delSpaces_i+0, 7 
	MOVLW       255
	ADDWFC      FARG_delSpaces_source+1, 0 
	MOVWF       FSR0H 
	MOVF        POSTINC0+0, 0 
	MOVWF       POSTINC1+0 
	INCF        delSpaces_j+0, 1 
L__delSpaces27:
	MOVF        delSpaces_i+0, 0 
	XORWF       FARG_delSpaces_size+0, 0 
	BTFSC       STATUS+0, 2 
	GOTO        L__delSpaces25
	INCF        delSpaces_i+0, 1 
	GOTO        L__delSpaces21
L__delSpaces25:
	MOVF        delSpaces_local_result+0, 0 
	MOVWF       FSR1L 
	MOVF        delSpaces_local_result+1, 0 
	MOVWF       FSR1H 
	MOVLW       delSpaces_res+0
	MOVWF       FSR2L 
	MOVLW       hi_addr(delSpaces_res+0)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	RETURN      0
; end of _delSpaces

_findBegining:

	MOVF        R0, 0 
	MOVWF       findBegining_local_result+0 
	MOVF        R1, 0 
	MOVWF       findBegining_local_result+1 
	CLRF        findBegining_j+0 
	CLRF        findBegining_found+0 
	CLRF        findBegining_i+0 
L__findBegining31:
	DECF        findBegining_i+0, 0 
	MOVWF       R0 
	MOVF        R0, 0 
	ADDWF       FARG_findBegining_source+0, 0 
	MOVWF       FSR0L 
	MOVLW       0
	BTFSC       R0, 7 
	MOVLW       255
	ADDWFC      FARG_findBegining_source+1, 0 
	MOVWF       FSR0H 
	MOVF        POSTINC0+0, 0 
	XORLW       37
	BTFSS       STATUS+0, 2 
	GOTO        L__findBegining36
	MOVF        findBegining_i+0, 0 
	MOVWF       findBegining_j+0 
	MOVLW       255
	MOVWF       findBegining_found+0 
L__findBegining36:
	MOVF        findBegining_found+0, 0 
	XORLW       255
	BTFSS       STATUS+0, 2 
	GOTO        L__findBegining39
	MOVF        findBegining_j+0, 0 
	SUBWF       findBegining_i+0, 0 
	MOVWF       R2 
	MOVLW       findBegining_res+0
	MOVWF       R0 
	MOVLW       hi_addr(findBegining_res+0)
	MOVWF       R1 
	MOVF        R2, 0 
	ADDWF       R0, 0 
	MOVWF       FSR1L 
	MOVLW       0
	BTFSC       R2, 7 
	MOVLW       255
	ADDWFC      R1, 0 
	MOVWF       FSR1H 
	MOVF        findBegining_i+0, 0 
	ADDWF       FARG_findBegining_source+0, 0 
	MOVWF       FSR0L 
	MOVLW       0
	BTFSC       findBegining_i+0, 7 
	MOVLW       255
	ADDWFC      FARG_findBegining_source+1, 0 
	MOVWF       FSR0H 
	MOVF        POSTINC0+0, 0 
	MOVWF       POSTINC1+0 
L__findBegining39:
	MOVF        findBegining_i+0, 0 
	XORLW       63
	BTFSC       STATUS+0, 2 
	GOTO        L__findBegining34
	INCF        findBegining_i+0, 1 
	GOTO        L__findBegining31
L__findBegining34:
	MOVF        findBegining_local_result+0, 0 
	MOVWF       FSR1L 
	MOVF        findBegining_local_result+1, 0 
	MOVWF       FSR1H 
	MOVLW       findBegining_res+0
	MOVWF       FSR2L 
	MOVLW       hi_addr(findBegining_res+0)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	RETURN      0
; end of _findBegining

_startDisplay:

	CLRF        _menuMode+0 
	CLRF        _underMenuMode+0 
	CLRF        _carTmpMode+0 
	MOVLW       0
	MOVWF       _carMode+0 
	MOVLW       _carMode+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_carMode+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       48
	MOVWF       ?LocalText_startDisplay+0 
	CLRF        ?LocalText_startDisplay+1 
	MOVLW       ?LocalText_startDisplay+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_startDisplay+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _newDisp1+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       84
	MOVWF       ?LocalText_startDisplay+0 
	MOVLW       101
	MOVWF       ?LocalText_startDisplay+1 
	MOVLW       116
	MOVWF       ?LocalText_startDisplay+2 
	MOVLW       114
	MOVWF       ?LocalText_startDisplay+3 
	MOVLW       105
	MOVWF       ?LocalText_startDisplay+4 
	MOVLW       120
	MOVWF       ?LocalText_startDisplay+5 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+6 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+7 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+8 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+9 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+10 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+11 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+12 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+13 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+14 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+15 
	CLRF        ?LocalText_startDisplay+16 
	MOVLW       ?LocalText_startDisplay+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_startDisplay+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       119
	MOVWF       ?LocalText_startDisplay+0 
	MOVLW       101
	MOVWF       ?LocalText_startDisplay+1 
	MOVLW       108
	MOVWF       ?LocalText_startDisplay+2 
	MOVLW       99
	MOVWF       ?LocalText_startDisplay+3 
	MOVLW       111
	MOVWF       ?LocalText_startDisplay+4 
	MOVLW       109
	MOVWF       ?LocalText_startDisplay+5 
	MOVLW       101
	MOVWF       ?LocalText_startDisplay+6 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+7 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+8 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+9 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+10 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+11 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+12 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+13 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+14 
	MOVLW       32
	MOVWF       ?LocalText_startDisplay+15 
	CLRF        ?LocalText_startDisplay+16 
	MOVLW       ?LocalText_startDisplay+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_startDisplay+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _oldDisp1+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_oldDisp1+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _newDisp1+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _oldDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_oldDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _tmpDisp1+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_tmpDisp1+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _newDisp1+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _tmpDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_tmpDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	CALL        _Lcd_Init+0, 0
	MOVLW       1
	MOVWF       FARG_Lcd_Cmd_out_char+0 
	CALL        _Lcd_Cmd+0, 0
	MOVLW       12
	MOVWF       FARG_Lcd_Cmd_out_char+0 
	CALL        _Lcd_Cmd+0, 0
	MOVLW       1
	MOVWF       FARG_Lcd_Out_row+0 
	MOVLW       1
	MOVWF       FARG_Lcd_Out_column+0 
	MOVLW       _newDisp1+0
	MOVWF       FARG_Lcd_Out_text+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_Lcd_Out_text+1 
	CALL        _Lcd_Out+0, 0
	MOVLW       2
	MOVWF       FARG_Lcd_Out_row+0 
	MOVLW       1
	MOVWF       FARG_Lcd_Out_column+0 
	MOVLW       _newDisp2+0
	MOVWF       FARG_Lcd_Out_text+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_Lcd_Out_text+1 
	CALL        _Lcd_Out+0, 0
	RETURN      0
; end of _startDisplay

_setDisplay:

	MOVLW       _newDisp1+0
	MOVWF       FARG_strcmp_s1+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcmp_s1+1 
	MOVLW       _oldDisp1+0
	MOVWF       FARG_strcmp_s2+0 
	MOVLW       hi_addr(_oldDisp1+0)
	MOVWF       FARG_strcmp_s2+1 
	CALL        _strcmp+0, 0
	MOVLW       0
	XORWF       R1, 0 
	BTFSS       STATUS+0, 2 
	GOTO        L__setDisplay110
	MOVLW       0
	XORWF       R0, 0 
L__setDisplay110:
	MOVLW       0
	BTFSS       STATUS+0, 2 
	MOVLW       255
	MOVWF       FLOC__setDisplay+0 
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcmp_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcmp_s1+1 
	MOVLW       _oldDisp2+0
	MOVWF       FARG_strcmp_s2+0 
	MOVLW       hi_addr(_oldDisp2+0)
	MOVWF       FARG_strcmp_s2+1 
	CALL        _strcmp+0, 0
	MOVLW       0
	XORWF       R1, 0 
	BTFSS       STATUS+0, 2 
	GOTO        L__setDisplay111
	MOVLW       0
	XORWF       R0, 0 
L__setDisplay111:
	MOVLW       0
	BTFSS       STATUS+0, 2 
	MOVLW       255
	MOVWF       R2 
	MOVF        R2, 0 
	IORWF       FLOC__setDisplay+0, 0 
	MOVWF       R0 
	BTFSC       STATUS+0, 2 
	GOTO        L__setDisplay44
	MOVLW       1
	MOVWF       FARG_Lcd_Cmd_out_char+0 
	CALL        _Lcd_Cmd+0, 0
	MOVLW       12
	MOVWF       FARG_Lcd_Cmd_out_char+0 
	CALL        _Lcd_Cmd+0, 0
	MOVLW       1
	MOVWF       FARG_Lcd_Out_row+0 
	MOVLW       1
	MOVWF       FARG_Lcd_Out_column+0 
	MOVLW       _newDisp1+0
	MOVWF       FARG_Lcd_Out_text+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_Lcd_Out_text+1 
	CALL        _Lcd_Out+0, 0
	MOVLW       2
	MOVWF       FARG_Lcd_Out_row+0 
	MOVLW       1
	MOVWF       FARG_Lcd_Out_column+0 
	MOVLW       _newDisp2+0
	MOVWF       FARG_Lcd_Out_text+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_Lcd_Out_text+1 
	CALL        _Lcd_Out+0, 0
	MOVLW       _oldDisp1+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_oldDisp1+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _newDisp1+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _oldDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_oldDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
L__setDisplay44:
	RETURN      0
; end of _setDisplay

_readIR:

	CLRF        FARG_ADC_Read_channel+0 
	CALL        _ADC_Read+0, 0
	MOVF        R0, 0 
	MOVWF       readIR_analogRead0+0 
	MOVF        R1, 0 
	MOVWF       readIR_analogRead0+1 
	MOVLW       1
	MOVWF       FARG_ADC_Read_channel+0 
	CALL        _ADC_Read+0, 0
	MOVF        R0, 0 
	MOVWF       readIR_analogRead1+0 
	MOVF        R1, 0 
	MOVWF       readIR_analogRead1+1 
	MOVF        readIR_analogRead0+0, 0 
	MOVWF       FARG_WordToStr_input+0 
	MOVF        readIR_analogRead0+1, 0 
	MOVWF       FARG_WordToStr_input+1 
	MOVLW       _strIr1+0
	MOVWF       FARG_WordToStr_output+0 
	MOVLW       hi_addr(_strIr1+0)
	MOVWF       FARG_WordToStr_output+1 
	CALL        _WordToStr+0, 0
	MOVLW       _strIr1+0
	MOVWF       FARG_delSpaces_source+0 
	MOVLW       hi_addr(_strIr1+0)
	MOVWF       FARG_delSpaces_source+1 
	MOVLW       10
	MOVWF       FARG_delSpaces_size+0 
	MOVLW       FLOC__readIR+0
	MOVWF       R0 
	MOVLW       hi_addr(FLOC__readIR+0)
	MOVWF       R1 
	CALL        _delSpaces+0, 0
	MOVLW       _strIr1+0
	MOVWF       FSR1L 
	MOVLW       hi_addr(_strIr1+0)
	MOVWF       FSR1H 
	MOVLW       FLOC__readIR+0
	MOVWF       FSR2L 
	MOVLW       hi_addr(FLOC__readIR+0)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	MOVF        readIR_analogRead1+0, 0 
	MOVWF       FARG_WordToStr_input+0 
	MOVF        readIR_analogRead1+1, 0 
	MOVWF       FARG_WordToStr_input+1 
	MOVLW       _strIr2+0
	MOVWF       FARG_WordToStr_output+0 
	MOVLW       hi_addr(_strIr2+0)
	MOVWF       FARG_WordToStr_output+1 
	CALL        _WordToStr+0, 0
	MOVLW       _strIr2+0
	MOVWF       FARG_delSpaces_source+0 
	MOVLW       hi_addr(_strIr2+0)
	MOVWF       FARG_delSpaces_source+1 
	MOVLW       10
	MOVWF       FARG_delSpaces_size+0 
	MOVLW       FLOC__readIR+0
	MOVWF       R0 
	MOVLW       hi_addr(FLOC__readIR+0)
	MOVWF       R1 
	CALL        _delSpaces+0, 0
	MOVLW       _strIr2+0
	MOVWF       FSR1L 
	MOVLW       hi_addr(_strIr2+0)
	MOVWF       FSR1H 
	MOVLW       FLOC__readIR+0
	MOVWF       FSR2L 
	MOVLW       hi_addr(FLOC__readIR+0)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	RETURN      0
; end of _readIR

_showMode:

	MOVF        FARG_showMode_chose+0, 0 
	XORLW       1
	BTFSS       STATUS+0, 2 
	GOTO        L__showMode51
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       49
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	GOTO        L__showMode48
L__showMode51:
	MOVF        FARG_showMode_chose+0, 0 
	XORLW       2
	BTFSS       STATUS+0, 2 
	GOTO        L__showMode54
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       50
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	GOTO        L__showMode48
L__showMode54:
	MOVF        FARG_showMode_chose+0, 0 
	XORLW       3
	BTFSS       STATUS+0, 2 
	GOTO        L__showMode57
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       51
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	GOTO        L__showMode48
L__showMode57:
	MOVF        FARG_showMode_chose+0, 0 
	XORLW       4
	BTFSS       STATUS+0, 2 
	GOTO        L__showMode60
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       52
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	GOTO        L__showMode48
L__showMode60:
	MOVF        FARG_showMode_chose+0, 0 
	XORLW       5
	BTFSS       STATUS+0, 2 
	GOTO        L__showMode63
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       53
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	GOTO        L__showMode48
L__showMode63:
	MOVF        FARG_showMode_chose+0, 0 
	XORLW       6
	BTFSS       STATUS+0, 2 
	GOTO        L__showMode66
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       54
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	GOTO        L__showMode48
L__showMode66:
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       77
	MOVWF       ?LocalText_showMode+0 
	MOVLW       111
	MOVWF       ?LocalText_showMode+1 
	MOVLW       100
	MOVWF       ?LocalText_showMode+2 
	MOVLW       101
	MOVWF       ?LocalText_showMode+3 
	MOVLW       32
	MOVWF       ?LocalText_showMode+4 
	MOVLW       48
	MOVWF       ?LocalText_showMode+5 
	MOVLW       32
	MOVWF       ?LocalText_showMode+6 
	MOVLW       32
	MOVWF       ?LocalText_showMode+7 
	MOVLW       32
	MOVWF       ?LocalText_showMode+8 
	MOVLW       32
	MOVWF       ?LocalText_showMode+9 
	MOVLW       32
	MOVWF       ?LocalText_showMode+10 
	MOVLW       32
	MOVWF       ?LocalText_showMode+11 
	MOVLW       32
	MOVWF       ?LocalText_showMode+12 
	MOVLW       32
	MOVWF       ?LocalText_showMode+13 
	MOVLW       32
	MOVWF       ?LocalText_showMode+14 
	MOVLW       32
	MOVWF       ?LocalText_showMode+15 
	CLRF        ?LocalText_showMode+16 
	MOVLW       ?LocalText_showMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_showMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
L__showMode48:
	RETURN      0
; end of _showMode

_ChoseMode:

	BTFSC       PORTB+0, 6 
	GOTO        L__ChoseMode112
	BCF         R2, 0 
	GOTO        L__ChoseMode113
L__ChoseMode112:
	BSF         R2, 0 
L__ChoseMode113:
	MOVF        _menuMode+0, 0 
	XORLW       0
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode69
	MOVLW       1
	MOVWF       _menuMode+0 
L__ChoseMode69:
	BTFSC       PORTB+0, 6 
	GOTO        L__ChoseMode114
	BSF         R2, 0 
	GOTO        L__ChoseMode115
L__ChoseMode114:
	BCF         R2, 0 
L__ChoseMode115:
	MOVF        _menuMode+0, 0 
	XORLW       1
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode72
	MOVLW       2
	MOVWF       _menuMode+0 
	MOVLW       _newDisp1+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       67
	MOVWF       ?LocalText_ChoseMode+0 
	MOVLW       104
	MOVWF       ?LocalText_ChoseMode+1 
	MOVLW       111
	MOVWF       ?LocalText_ChoseMode+2 
	MOVLW       115
	MOVWF       ?LocalText_ChoseMode+3 
	MOVLW       101
	MOVWF       ?LocalText_ChoseMode+4 
	MOVLW       32
	MOVWF       ?LocalText_ChoseMode+5 
	MOVLW       109
	MOVWF       ?LocalText_ChoseMode+6 
	MOVLW       111
	MOVWF       ?LocalText_ChoseMode+7 
	MOVLW       100
	MOVWF       ?LocalText_ChoseMode+8 
	MOVLW       101
	MOVWF       ?LocalText_ChoseMode+9 
	MOVLW       58
	MOVWF       ?LocalText_ChoseMode+10 
	MOVLW       48
	MOVWF       ?LocalText_ChoseMode+11 
	MOVLW       45
	MOVWF       ?LocalText_ChoseMode+12 
	MOVLW       54
	MOVWF       ?LocalText_ChoseMode+13 
	MOVLW       32
	MOVWF       ?LocalText_ChoseMode+14 
	MOVLW       32
	MOVWF       ?LocalText_ChoseMode+15 
	CLRF        ?LocalText_ChoseMode+16 
	MOVLW       ?LocalText_ChoseMode+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(?LocalText_ChoseMode+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVF        _carTmpMode+0, 0 
	MOVWF       FARG_showMode_chose+0 
	CALL        _showMode+0, 0
L__ChoseMode72:
	BTFSC       PORTB+0, 6 
	GOTO        L__ChoseMode116
	BCF         R2, 0 
	GOTO        L__ChoseMode117
L__ChoseMode116:
	BSF         R2, 0 
L__ChoseMode117:
	MOVF        _menuMode+0, 0 
	XORLW       2
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R0, 0 
	ANDWF       R1, 1 
	MOVF        _underMenuMode+0, 0 
	XORLW       0
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R0 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode75
	MOVLW       1
	MOVWF       _underMenuMode+0 
L__ChoseMode75:
	BTFSC       PORTB+0, 6 
	GOTO        L__ChoseMode118
	BSF         R2, 0 
	GOTO        L__ChoseMode119
L__ChoseMode118:
	BCF         R2, 0 
L__ChoseMode119:
	MOVF        _menuMode+0, 0 
	XORLW       2
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R0, 0 
	ANDWF       R1, 1 
	MOVF        _underMenuMode+0, 0 
	XORLW       1
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R0 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode78
	INCF        _carTmpMode+0, 1 
	MOVF        _carTmpMode+0, 0 
	SUBLW       6
	BTFSC       STATUS+0, 0 
	GOTO        L__ChoseMode81
	CLRF        _carTmpMode+0 
L__ChoseMode81:
	MOVF        _carTmpMode+0, 0 
	MOVWF       FARG_showMode_chose+0 
	CALL        _showMode+0, 0
	CLRF        _underMenuMode+0 
L__ChoseMode78:
	BTFSC       PORTB+0, 6 
	GOTO        L__ChoseMode120
	BSF         R2, 0 
	GOTO        L__ChoseMode121
L__ChoseMode120:
	BCF         R2, 0 
L__ChoseMode121:
	MOVF        _menuMode+0, 0 
	XORLW       0
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode84
	MOVLW       _newDisp1+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp1+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _tmpDisp1+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_tmpDisp1+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
	MOVLW       _newDisp2+0
	MOVWF       FARG_strcpy_s1+0 
	MOVLW       hi_addr(_newDisp2+0)
	MOVWF       FARG_strcpy_s1+1 
	MOVLW       _tmpDisp2+0
	MOVWF       FARG_strcpy_s2+0 
	MOVLW       hi_addr(_tmpDisp2+0)
	MOVWF       FARG_strcpy_s2+1 
	CALL        _strcpy+0, 0
L__ChoseMode84:
	BTFSC       PORTB+0, 7 
	GOTO        L__ChoseMode122
	BCF         R2, 0 
	GOTO        L__ChoseMode123
L__ChoseMode122:
	BSF         R2, 0 
L__ChoseMode123:
	MOVF        _menuMode+0, 0 
	XORLW       2
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode87
	MOVLW       3
	MOVWF       _menuMode+0 
L__ChoseMode87:
	BTFSC       PORTB+0, 7 
	GOTO        L__ChoseMode124
	BSF         R2, 0 
	GOTO        L__ChoseMode125
L__ChoseMode124:
	BCF         R2, 0 
L__ChoseMode125:
	MOVF        _menuMode+0, 0 
	XORLW       3
	MOVLW       255
	BTFSS       STATUS+0, 2 
	MOVLW       0
	MOVWF       R1 
	CLRF        R0 
	BTFSC       R2, 0 
	INCF        R0, 1 
	MOVF        R1, 0 
	ANDWF       R0, 1 
	BTFSC       STATUS+0, 2 
	GOTO        L__ChoseMode90
	MOVF        _carTmpMode+0, 0 
	MOVWF       FARG_ShortToStr_input+0 
	MOVLW       _carMode+0
	MOVWF       FARG_ShortToStr_output+0 
	MOVLW       hi_addr(_carMode+0)
	MOVWF       FARG_ShortToStr_output+1 
	CALL        _ShortToStr+0, 0
	MOVLW       _carMode+0
	MOVWF       FARG_delSpaces_source+0 
	MOVLW       hi_addr(_carMode+0)
	MOVWF       FARG_delSpaces_source+1 
	MOVLW       10
	MOVWF       FARG_delSpaces_size+0 
	MOVLW       FLOC__ChoseMode+0
	MOVWF       R0 
	MOVLW       hi_addr(FLOC__ChoseMode+0)
	MOVWF       R1 
	CALL        _delSpaces+0, 0
	MOVLW       _carMode+0
	MOVWF       FSR1L 
	MOVLW       hi_addr(_carMode+0)
	MOVWF       FSR1H 
	MOVLW       FLOC__ChoseMode+0
	MOVWF       FSR2L 
	MOVLW       hi_addr(FLOC__ChoseMode+0)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	CLRF        _menuMode+0 
L__ChoseMode90:
	CALL        _setDisplay+0, 0
	RETURN      0
; end of _ChoseMode

_main:

	MOVLW       12
	IORWF       ADCON1+0, 1 
	MOVLW       7
	IORWF       CMCON+0, 1 
	MOVLW       255
	MOVWF       TRISA+0 
	CLRF        TRISD+0 
	CLRF        PORTD+0 
	MOVLW       63
	MOVWF       PORTB+0 
	MOVLW       63
	MOVWF       TRISB+0 
	CALL        _startDisplay+0, 0
	MOVLW       _readbuff+0
	MOVWF       FARG_HID_Enable_readbuff+0 
	MOVLW       hi_addr(_readbuff+0)
	MOVWF       FARG_HID_Enable_readbuff+1 
	MOVLW       _writebuff+0
	MOVWF       FARG_HID_Enable_writebuff+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_HID_Enable_writebuff+1 
	CALL        _HID_Enable+0, 0
	MOVLW       25
	MOVWF       SPBRG+0 
	BSF         TXSTA+0, 2, 0
	CALL        _UART1_Init+0, 0
L__main94:
	CALL        _ChoseMode+0, 0
	CALL        _readIR+0, 0
	CALL        _HID_Read+0, 0
	MOVF        R0, 0 
	XORLW       0
	BTFSC       STATUS+0, 2 
	GOTO        L__main99
	MOVLW       _readbuff+0
	MOVWF       FARG_pharse_buff+0 
	MOVLW       hi_addr(_readbuff+0)
	MOVWF       FARG_pharse_buff+1 
	MOVLW       main_led+0
	MOVWF       FARG_pharse_led+0 
	MOVLW       hi_addr(main_led+0)
	MOVWF       FARG_pharse_led+1 
	MOVLW       main_speedAngle+0
	MOVWF       FARG_pharse_speedAngle+0 
	MOVLW       hi_addr(main_speedAngle+0)
	MOVWF       FARG_pharse_speedAngle+1 
	MOVLW       _tmpDisp1+0
	MOVWF       FARG_pharse_text1+0 
	MOVLW       hi_addr(_tmpDisp1+0)
	MOVWF       FARG_pharse_text1+1 
	MOVLW       _tmpDisp2+0
	MOVWF       FARG_pharse_text2+0 
	MOVLW       hi_addr(_tmpDisp2+0)
	MOVWF       FARG_pharse_text2+1 
	CALL        _pharse+0, 0
	MOVF        main_led+0, 0 
	MOVWF       FARG_ledsActivity_led+0 
	CALL        _ledsActivity+0, 0
	MOVLW       main_speedAngle+0
	MOVWF       FARG_UART1_Write_Text_uart_text+0 
	MOVLW       hi_addr(main_speedAngle+0)
	MOVWF       FARG_UART1_Write_Text_uart_text+1 
	CALL        _UART1_Write_Text+0, 0
L__main99:
	CALL        _UART1_Data_Ready+0, 0
	MOVF        R0, 0 
	XORLW       1
	BTFSS       STATUS+0, 2 
	GOTO        L__main102
	MOVLW       main_serialRead+0
	MOVWF       FARG_UART1_Read_Text_Output+0 
	MOVLW       hi_addr(main_serialRead+0)
	MOVWF       FARG_UART1_Read_Text_Output+1 
	MOVLW       124
	MOVWF       ?LocalText_main+0 
	CLRF        ?LocalText_main+1 
	MOVLW       ?LocalText_main+0
	MOVWF       FARG_UART1_Read_Text_Delimiter+0 
	MOVLW       hi_addr(?LocalText_main+0)
	MOVWF       FARG_UART1_Read_Text_Delimiter+1 
	MOVLW       64
	MOVWF       FARG_UART1_Read_Text_Attempts+0 
	CALL        _UART1_Read_Text+0, 0
	MOVLW       main_serialRead+0
	MOVWF       FARG_findBegining_source+0 
	MOVLW       hi_addr(main_serialRead+0)
	MOVWF       FARG_findBegining_source+1 
	MOVLW       FLOC__main+0
	MOVWF       R0 
	MOVLW       hi_addr(FLOC__main+0)
	MOVWF       R1 
	CALL        _findBegining+0, 0
	MOVLW       64
	MOVWF       FSR1L 
	MOVLW       5
	MOVWF       FSR1H 
	MOVLW       FLOC__main+0
	MOVWF       FSR2L 
	MOVLW       hi_addr(FLOC__main+0)
	MOVWF       FSR2H 
	CALL        ___CS2S+0, 0
	CLRF        POSTINC1+0 
	MOVLW       _writebuff+0
	MOVWF       FARG_strcat_s1+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_strcat_s1+1 
	MOVLW       44
	MOVWF       ?LocalText_main+0 
	CLRF        ?LocalText_main+1 
	MOVLW       ?LocalText_main+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(?LocalText_main+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVLW       _writebuff+0
	MOVWF       FARG_strcat_s1+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_strcat_s1+1 
	MOVLW       _carMode+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(_carMode+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVLW       _writebuff+0
	MOVWF       FARG_strcat_s1+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_strcat_s1+1 
	MOVLW       44
	MOVWF       ?LocalText_main+0 
	CLRF        ?LocalText_main+1 
	MOVLW       ?LocalText_main+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(?LocalText_main+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVLW       _writebuff+0
	MOVWF       FARG_strcat_s1+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_strcat_s1+1 
	MOVLW       _strIr1+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(_strIr1+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVLW       _writebuff+0
	MOVWF       FARG_strcat_s1+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_strcat_s1+1 
	MOVLW       44
	MOVWF       ?LocalText_main+0 
	CLRF        ?LocalText_main+1 
	MOVLW       ?LocalText_main+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(?LocalText_main+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
	MOVLW       _writebuff+0
	MOVWF       FARG_strcat_s1+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_strcat_s1+1 
	MOVLW       _strIr2+0
	MOVWF       FARG_strcat_s2+0 
	MOVLW       hi_addr(_strIr2+0)
	MOVWF       FARG_strcat_s2+1 
	CALL        _strcat+0, 0
L__main105:
	MOVLW       _writebuff+0
	MOVWF       FARG_HID_Write_writebuff+0 
	MOVLW       hi_addr(_writebuff+0)
	MOVWF       FARG_HID_Write_writebuff+1 
	MOVLW       64
	MOVWF       FARG_HID_Write_len+0 
	CALL        _HID_Write+0, 0
	MOVF        R0, 0 
	XORLW       0
	BTFSC       STATUS+0, 2 
	GOTO        L__main105
L__main102:
	GOTO        L__main94
	GOTO        $+0
; end of _main
