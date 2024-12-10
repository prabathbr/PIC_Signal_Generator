;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EE322 Project
;Group No:26
;....................................................................
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Initial EEPROM data : 01 01 E1 01 01 01
;*************************************************************

__config _HS_OSC & _LVP_OFF & _WDT_OFF & _PWRTE_OFF & _BODEN_OFF & _MCLRE_OFF
;configuration bits for programming

;let's include PIC16F628A headers
#include "p16f628a.inc"

;program begins at 0x00
org 0x00

;*************************************************************
;Initilization
;*************************************************************

;variables define
count1 equ 0x20
count2 equ 0x21
count3 equ 0x22 ;delays
value	equ 0x23	;sin/trg lookup table row
mode equ 0x24 ;mode set
count4 equ 0x25
count5 equ 0x26
count_pos equ 0x27
count6 equ 0x28
count7 equ 0x29
delay_0 equ 0x30					      
delay_1 equ 0x31
delay_2 equ 0x32 ;delays				              
lcd_temp	equ 0x33	;lcd buffer
volt	equ 0xA0	;voltage value
volt1	equ 0x34	;voltage value buffer
count_sq	equ 0x35	;square delay
count_si	equ 0x36	;sine/trg delay
;end of variable set

;disable comparators and make PORTA as digital input
movlw 0x07
movwf CMCON


;LCD control lines define
#define lcd_4	PORTB, 0
#define lcd_5	PORTB, 1	
#define lcd_6	PORTB, 2
#define lcd_7	PORTB, 3	;data lines for 4bit mode
#define	lcd_rs	PORTB, 4	;command line
#define lcd_e	PORTA, 0	;clock line
;END LCD control lines

;set I/O pins
bsf STATUS,RP0	;bank1

bcf TRISA,RA0	;lcd clock line as output
bsf TRISA,RA1
bsf TRISA,RA3
bsf TRISA,RA4
bsf TRISA,RA5	;RA1,3,4,5 as input

clrf TRISB	;PORTB as ouput
movlw b'11100000
movwf VRCON	;enalbe internal voltage reference module
bcf STATUS,RP0	;bank0

clrf value	;clear lookup table row to 0
movlw 0x05
movwf count3	;init some delay


call	lcd_init	;init lcd

;Loading previous state from EEPROM

bsf STATUS,RP0 ;bank1
movlw 0x05
movwf EEADR	;set EEPROM address
bsf EECON1,RD	;read EEPROM
movfw EEDATA	
bcf STATUS,RP0 ;bank0
movwf 0x41 ;copy read data to buffer
movfw 0x41
movwf count_pos	;copy data to count position 
;mode
;load mode
bsf STATUS,RP0 ;bank0
movlw 0x00
movwf EEADR	;set address
bsf EECON1,RD	;read
movfw EEDATA
bcf STATUS,RP0	;bank0
movwf mode	;copy read data to mode reg
btfsc  mode,0  ;if mode is sqr
call set_0	;run sqr
btfsc  mode,1 ;if mode is trg
call set_1	;run trg
btfsc  mode,2 ;if mode is sine
call set_2	;run sine
;/mode

movfw 0x41
movwf count_pos	;load count position from buffer

;volt
;load amplitude
bsf STATUS,RP0	;bank1
movlw 0x02	
movwf EEADR	;set address
bsf EECON1,RD	;read
movfw EEDATA	
movwf VRCON	;copy read data to VRCON
bcf STATUS,RP0	;bank0

bsf STATUS,RP0	;bank1
movlw 0x03
movwf EEADR	;set address
bsf EECON1,RD	;read
movfw EEDATA
bcf STATUS,RP0	;bank0
movwf volt1	;copy read data to voltage level reg

;Dummy LCD routine to show it works
;movlw	POSITION  = set the position on LCD                                
;call	lcd_addr  = send it to LCD			
;movlw	'CHAR'	  = set the char to show	
;call	lcd_data  = send it to LCD
;this for lines are run for each LCD display and not repeated in comments 	


;Show Amplitude in LCD
;High digit
movlw	0x08 ;LCD address                                 
call	lcd_addr ;set it on LCD
movlw high volt_h ; get location of volt_h table
movwf PCLATH ; set PCLATH as needed
movfw volt1	; set row value 
call volt_h	; get data on that row
call	lcd_data ; send that data to LCD to show

;;Similar procedure is used whenever we needed to get data from table and show on LCD	

;low digit
movlw	0x0A                                  
call	lcd_addr
movlw high volt_l
movwf PCLATH
movfw volt1
call volt_l
call	lcd_data	
;/volt



;freq
movfw 0x41
movwf count_pos	;load count position from buffer
call disp_sqr ;display frequency
;/freq


;Show default charcters on LCD


movlw	0x00                                  
call	lcd_addr		
movlw	'M'
call	lcd_data	

;Show "M:"
movlw	0x01                                  
call	lcd_addr		
movlw	':'
call	lcd_data

;Show "F:"
movlw	0x40                                  
call	lcd_addr		
movlw	'F'
call	lcd_data	
movlw	0x41                                  
call	lcd_addr		
movlw	':'
call	lcd_data

;Show "Hz"
movlw	0x48                                  
call	lcd_addr		
movlw	'H'
call	lcd_data
movlw	0x49                                  
call	lcd_addr		
movlw	'z'
call	lcd_data

;Show "A:"
movlw	0x06                                  
call	lcd_addr		
movlw	'A'
call	lcd_data	
movlw	0x07                                  
call	lcd_addr		
movlw	':'
call	lcd_data

;Show "."
movlw	0x09                                  
call	lcd_addr
movlw '.'
call	lcd_data	

;Show "Vpp"
movlw	0x0B                                  
call	lcd_addr
movlw 'V'
call	lcd_data	
movlw	0x0C                                  
call	lcd_addr
movlw 'p'
call	lcd_data	
movlw	0x0D                                  
call	lcd_addr
movlw 'p'
call	lcd_data	


;Show "G26" (Group number)
movlw	0x4D                                  
call	lcd_addr
movlw 'G'
call	lcd_data
movlw	0x4E                                  
call	lcd_addr
movlw '2'
call	lcd_data
movlw	0x4F                                  
call	lcd_addr
movlw '6'
call	lcd_data



;*************************************************************
; Main Loop
;*************************************************************
loop

	btfsc  mode,0  ;if mode is sqr
	call square_run	;run sqr

	btfsc  mode,1  ;if mode is trg
	call trangle_run	;run trg

	btfsc  mode,2  ;if mode is sine
	call sine_run	;run sine

goto loop


;*************************************************************
; Button polling Function
;*************************************************************
poll
	clrf value ;clead lookuptable row to 0

	btfss PORTA,RA1 ;if RA0 button on
	goto chkm1	;goto chkm1
	goto chkm2	;else goto chkm2

	chkm1
		btfss PORTA,RA3 ;if bot RA3 & RA1 buttons on
		call change_mode	;change mode

	chkm2	;if only RA0 is on continue
		btfss PORTA,RA1	;if RA1 button on 
		call inc ;increase amplitude

		btfss PORTA,RA3	;if RA3 button on
		call dec	;derease amplitude

		btfss PORTA,RA5 ;if RA5 button on
		call dec1	;decrease delay(increase freq)

		btfss PORTA,RA4	;if RA$ button on
		call inc1	;increase delay(decrease freq)

		btfsc  mode,0  ;if mode sqr
		goto square_run	;run sqr again

		btfsc  mode,1  ;if mode trg
		goto trangle_run	;run trg again

		btfsc  mode,2  ;if mode sine
		goto sine_run	;run sine again


return

;*************************************************************
; Change mode Function
;*************************************************************

change_mode

	call delay3 ;delay some time
	btfsc  mode,0  ;if current square 
	goto set_1	;set trangle

 
	btfsc  mode,1 ;if current trangle
	goto set_2	;set sine

	btfsc  mode,2 ;if current sine
	goto set_0	;set square

	set_1	;set trangle on mode reg
		bcf mode,0
		bcf mode,2
		bsf mode,1

;display TRG on LCD
		movlw	0x02                                  
		call	lcd_addr		
		movlw	'T'
		call	lcd_data	
		movlw	0x03                                  
		call	lcd_addr		
		movlw	'R'
		call	lcd_data	
		movlw	0x04                                  
		call	lcd_addr		
		movlw	'G'
		call	lcd_data	
		call no_sqr	;load appropriate freq and show/generate
		goto set_ok ;set ok


	set_2 ;set sine on mode reg
		bcf mode,0
		bcf mode,1
		bsf mode,2

		;display SIN on LCD
		movlw	0x02                                  
		call	lcd_addr		
		movlw	'S'
		call	lcd_data	
		movlw	0x03                                  
		call	lcd_addr		
		movlw	'I'
		call	lcd_data	
		movlw	0x04                                  
		call	lcd_addr		
		movlw	'N'
		call	lcd_data	
		call no_sqr ;load appropriate freq and show/generate
		goto set_ok	;set ok

	set_0	;set square on mode reg
		bcf mode,1
		bcf mode,2
		bsf mode,0
		;display SIN on LCD
		movlw	0x02                                  
		call	lcd_addr		
		movlw	'S'
		call	lcd_data	
		movlw	0x03                                  
		call	lcd_addr		
		movlw	'Q'
		call	lcd_data	
		movlw	0x04                                  
		call	lcd_addr		
		movlw	'R'
		call	lcd_data	
		call disp_sqr ;load appropriate freq and show/generate
		goto set_ok ;set ok



	set_ok
		;writing mode to EEPROM
		movfw mode	;copy mode to W
		bsf STATUS, RP0 ;bank1
		movwf EEDATA ;copy W to EEDATA
		movlw 0x00
		movwf EEADR ;set address
		bcf STATUS, RP0	;bank0 
		call write_eeprom 	;write EEPROM

return

;*************************************************************
; Generate Sine wave Function
;*************************************************************

sine_run
	movlw  .252
	subwf  value,W  
	btfsc  STATUS,Z  ;check for end of table
	call poll	;poll buttons
	movlw  HIGH sine	
	movwf  PCLATH
	movfw value
	call sine	;load value from table
	movwf PORTB	;output value
	incf value,f	;increase row of table
	call delay2	;delay for next row
return

;*************************************************************
; Generate Trangle wave Function
;*************************************************************

trangle_run

	movlw  .252
	subwf  value,W  
	btfsc  STATUS,Z  ;check for end of table
	call poll	;polling for buttons
	movlw  HIGH trangle
	movwf  PCLATH
	movfw value 
	call trangle	;loading value from table
	movwf PORTB	;output value
	incf value,f	;incrase row of table
	call delay2	;delay for next row

return

;*************************************************************
; Generate Square wave Function
;*************************************************************
square_run
	;timer for polling buttons
	movlw  .252 ; set time out value
	subwf  value,W  ;substrace timeout from current timer value
	btfsc  STATUS,Z ;check result for zero
	call poll	; if so call button poll routine
	incf value,f	;increase timer value by 1

	movlw 0xFF 
	movwf PORTB ;set portb to high
	call delaysq	;delay
	movlw 0x00	
	movwf PORTB		;set portb to low
	call delaysq	;delay

return

;*************************************************************
; Amplitude Increment Function
;*************************************************************
inc
	call delay3 ;delay sometime
	bsf STATUS,RP0 ;bank1
	
	movlw  b'11101111
	subwf  VRCON,W  
	btfsc  STATUS,Z ;check for max amplitude
	goto volt_max	;if so goto to volt_max

	incf  VRCON,1 ;else incemnet by one step
	movlw b'11100000
	iorwf VRCON,1 ;make correct value for VRCON

	volt_max
		movfw VRCON
		sublw .32
		sublw 0
		movwf volt
		bcf volt,4
		bcf volt,5
		bcf volt,6
		bcf volt,7
		movfw volt ;get lower nibble from VRCON,as step
		bcf STATUS,RP0 ;bank0
		movwf volt1 ;store amplitude step

		;Load voltage value digits from table and show on lcd
		;for high char
		movlw	0x08                                  
		call	lcd_addr
		movlw high volt_h
		movwf PCLATH
		movfw volt1
		call volt_h
		call	lcd_data	

		;for low char
		movlw	0x0A                                  
		call	lcd_addr
		movlw high volt_l
		movwf PCLATH
		movfw volt1
		call volt_l
		call	lcd_data	

		;writing vrcon to eeprom
		bsf STATUS, RP0 ;bank1
		movfw  VRCON 
		movwf EEDATA ;copy VRCON to EEDATA
		movlw 0x02
		movwf EEADR ;set address
		bcf STATUS, RP0 ;bank0
		call write_eeprom ;write EEPROM

		;writing amplitude step to eeprom
		movfw volt1 ;copy step to W reg
		bsf STATUS, RP0 
		movwf EEDATA ;copy W to EEDATA
		movlw 0x03
		movwf EEADR ;set address
		bcf STATUS, RP0 ;bank0
		call write_eeprom ;write EEPROM

return

;*************************************************************
; Amplitude Decrement Function
;*************************************************************
dec
	call delay3 ;wait some time
	bsf STATUS,RP0	;goto bank1
	movlw  b'11100000 
	subwf  VRCON,W  
	btfsc  STATUS,Z	;check for min amplitude
	goto volt_min	;if min goto volt_min
	decf VRCON,1	;else decremnt by 1 step
	movlw b'11100000 
	iorwf VRCON,1	;make correct value for VRCON

	volt_min
		movfw VRCON
		sublw .32
		sublw 0
		movwf volt
		bcf volt,4
		bcf volt,5
		bcf volt,6
		bcf volt,7
		movfw volt		;get lower nibble from VRCON as voltage value
		bcf STATUS,RP0	;goto bank0
		movwf volt1		;store amplitde value

		;Load voltage value digits from table and show on lcd
		;for high char
		movlw	0x08                                  
		call	lcd_addr
		movlw high volt_h
		movwf PCLATH
		movfw volt1
		call volt_h
		call	lcd_data	

		;for low char
		movlw	0x0A                                  
		call	lcd_addr
		movlw high volt_l
		movwf PCLATH
		movfw volt1
		call volt_l
		call	lcd_data	

		;writing vrcon to eeprom
		;write VRCON
		bsf STATUS, RP0 ;bank1
		movfw  VRCON
		movwf EEDATA ;copy VRCON to EEDATA
		movlw 0x02	;set address
		movwf EEADR 
		bcf STATUS, RP0 ;bank 1
		call write_eeprom ;write EEPROM

		;write Voltage value
		movfw volt1	;copy voltage value to W reg
		bsf STATUS, RP0 ;bank1
		movwf EEDATA ;copy W to EEDATA
		movlw 0x03	
		movwf EEADR ;set address
		bcf STATUS, RP0 ;goto bank0
		call write_eeprom ;write eeprom
return

;*************************************************************
; Delay value increment(Frequncy Decrement) Function
;*************************************************************
inc1
	call delay3	;wait some time

	movlw  0x16
	subwf  count_pos,W  
	btfss  STATUS,Z	;check for max delay state of 0x16
	incf count_pos	;if delay state < 0x16;increment

disp_sqr

	btfss mode,0
	goto no_sqr		;check for square mode;if not goto no_sqr

	;if in square mode;load delay value from table and send to count_sq
	movlw high sqr_delay
	movwf PCLATH
	movfw count_pos
	call sqr_delay
	movwf count_sq

	;send . & k chars to LCD for appropriate postions
	movlw	0x43 ;set DDRAM address                                 
	call	lcd_addr		
	movlw	'.'	;data sent to W reg
	call	lcd_data
	movlw	0x47   ;set DDRAM address                                  
	call	lcd_addr		
	movlw	'k'	;data sent to W reg	
	call	lcd_data

	;Frequency values,load from table and sent to LCD for appropriate postions
	;load high digit
	movlw	0x42                                  
	call	lcd_addr
	movlw high sqr_high
	movwf PCLATH
	movfw count_pos
	call sqr_high
	call	lcd_data	

	;load low digit
	movlw	0x44                                  
	call	lcd_addr
	movlw high sqr_low
	movwf PCLATH
	movfw count_pos
	call sqr_low
	call	lcd_data	

	goto done_sqr	;goto end if functions for square wave finishes

no_sqr	;if sine/trangle

	;if in square mode;load delay value from table and send to count_si
	movlw high sin_delay
	movwf PCLATH
	movfw count_pos
	call sin_delay
	movwf count_si

	;Frequency values,load from table and sent to LCD for appropriate postions
	;load and send high char
	movlw	0x42                                  
	call	lcd_addr
	movlw high sin_high
	movwf PCLATH
	movfw count_pos
	call sin_high
	call	lcd_data	

	;load and send mid char
	movlw	0x43                                  
	call	lcd_addr
	movlw high sin_mid
	movwf PCLATH
	movfw count_pos
	call sin_mid
	call	lcd_data	

	;load and send low char
	movlw	0x44                                  
	call	lcd_addr
	movlw high sin_low
	movwf PCLATH
	movfw count_pos
	call sin_low
	call	lcd_data	

	;erase the 'k' to obtain Hz display only
	movlw	0x47                                  
	call	lcd_addr		
	movlw	' '
	call	lcd_data

done_sqr

	;writing freq to eeprom
	movfw count_pos	;load delay postion to W reg
	bsf STATUS, RP0 ;bank 1
	movwf EEDATA 	;set EEDATA to delay
	movlw 0x05		;set address to write
	movwf EEADR 	;set EEADR to address
	bcf STATUS, RP0 ;bank 0
	call write_eeprom	;write EEPROM

return

;*************************************************************
; Delay value decrement(Frequncy Increment) Function
;*************************************************************
dec1
	call delay3	;wait some time
	movlw  .1
	subwf  count_pos,W  
	btfss  STATUS,Z		;check for min state of 1
	decf count_pos	;if delay state > 1;decrement
	call disp_sqr	;show freqency ..etc on display
return


;*************************************************************
; Write EEPROM Function
;*************************************************************
write_eeprom 
	bsf STATUS, RP0	;goto bank1
	bsf EECON1, WREN	;write enable
	movlw 0x55 
	movwf EECON2
	movlw 0xAA 
	movwf EECON2	;give the sequnce to enter
	bsf EECON1, WR 	;write
	btfsc EECON1, WR
	goto $-1	;wait till writing finishes
	bcf EECON1, WREN ;write disable
	bcf STATUS, RP0 ;goto bank0
	;return after small delay in next routine
;*************************************************************
; Delay loop for Sine/Trangular signals
;*************************************************************
delay2	
		movfw count_si	
		movwf count4	;load delay value from count_si
		loop0
			decfsz	count4
			goto 	loop0	;delay loop
return

;*************************************************************
; Delay short time,pre defined
;*************************************************************
delay2x	
		movfw .72
		movwf count6
		movlw .5
		movwf count7
		

		loop0x
			loop11x
				decfsz	count7
				goto 	loop11x	;delay loop1
			decfsz	count6
			goto 	loop0x	;delay loop2
		
return

;*************************************************************
; Delay loop for Square signal
;*************************************************************
delaysq	
	movlw 0x06	;set calibration
	movwf count5
sq_s
	movfw count_sq

	movwf	count4
delaysq_0
	decfsz	count4, f
	goto	delaysq_0	;delay loop 1
	decfsz	count5
goto sq_s	;delay loop2

		
return

;*************************************************************
; Delay delay2x for n times
;*************************************************************
delay3
		movlw .10
		movwf count3
		loop3
		call delay2x
		decfsz	count3 ;delay loop
		
		goto loop3
call delay_1ms	; 1ms delay
return

;*************************************************************
; Initialize LCD 
;*************************************************************
lcd_init
	call	delay ;delay sometime
	
	bcf	lcd_rs			;command mode
	bcf	lcd_e
	bsf	lcd_4
	bsf	lcd_5
	bcf	lcd_6
	bcf	lcd_7			;set pins as 001100 to init LCD
	call	lcd_clock	;give clock 
	
	movlw	0x04
	call	delay	;delay 4ms
	
	call	lcd_clock	;give cloc
	call	short_dly
	call	short_dly	;short delay ~100us
	call	lcd_clock	;give clock

	
	bcf	lcd_4	;set 4-bit mode
	call	lcd_clock	;give clock

	
	movlw	b'00101000'		;function set command
	call	lcd_data		;send as data
	
	movlw	b'00001100'		;display control set
	call	lcd_data		;send as data
	
	movlw	b'00000110'		;entry mode set
	call	lcd_data		;send as data

	movlw	b'00010000'		;display shift set
	call	lcd_data		;send as data

	movlw	b'00000001'		;clear screen command
	call	lcd_data		;send as data

	bsf	lcd_rs		        ;data mode
	return


;*************************************************************
; Goto position
;*************************************************************
lcd_addr
	bcf	lcd_rs			;command mode
	iorlw	0x80		;set DDRAM address + postion
	call	lcd_data	;send to lcd
	bsf	lcd_rs			;switch to data mode
	call	delay_1ms	;wait some time
	return


;*************************************************************
; Send data as 2 nibbles to LCD
;*************************************************************
lcd_data	
	movwf	lcd_temp	;buffer to store lcd data
	bcf	lcd_4
	bcf	lcd_5
	bcf	lcd_6
	bcf	lcd_7			;clear data lines

	btfsc	lcd_temp, 7
		bsf	lcd_7
	btfsc	lcd_temp, 6
		bsf	lcd_6
	btfsc	lcd_temp, 5
		bsf	lcd_5
	btfsc	lcd_temp, 4
		bsf	lcd_4			;set higher nibble to output lines
	call	lcd_clock		;clock signal
	

	bcf	lcd_4
	bcf	lcd_5
	bcf	lcd_6
	bcf	lcd_7			;clear data lines

	btfsc	lcd_temp, 3
		bsf	lcd_7
	btfsc	lcd_temp, 2
		bsf	lcd_6
	btfsc	lcd_temp, 1
		bsf	lcd_5
	btfsc	lcd_temp, 0
		bsf	lcd_4			;set lower nibble to data lines
	call	lcd_clock		;clock signal

	return


;*************************************************************
; Clock signal for LCD
;*************************************************************
lcd_clock 
	bsf	lcd_e	;clock enable
	nop			
	nop				
	nop				 
    nop		;pulse		
	bcf	lcd_e ;clock disable
	call	delay_1ms	
	call	delay_1ms	;wait some time
	return



;*************************************************************
; 1ms * W delay
;*************************************************************
delay
	movwf	delay_0
delay_w
	call	delay_1ms
	decfsz	delay_0, F
	goto	delay_w
	return


;*************************************************************	
; 1ms delay.
;*************************************************************
delay_1ms
			
	movlw	0xE6
	movwf	delay_1
	movlw	0x04
	movwf	delay_2
delay_1ms_0
	decfsz	delay_1, f
	goto	$+2
	decfsz	delay_2, f
	goto	delay_1ms_0
		
	goto	$+1
	nop
			
	return

;*************************************************************	
; 40us delay.
;*************************************************************
short_dly

	movlw	0x41
	movwf	delay_1
short_dly_0
	decfsz	delay_1, f
	goto	short_dly_0
	return

;*************************************************************
; Table for Amplitude-High digit
;*************************************************************
org 0x400
volt_h
	addwf PCL,f
	dt .48,.48,.49,.49,.50,.50,.51,.51,.52,.52,.53,.53,.54,.54,.55,'M'

;*************************************************************
; Table for Amplitude-Low digit
;*************************************************************
org 0x414
volt_l
	addwf PCL,f
	dt .48,.53,.48,.53,.48,.53,.48,.53,.48,.53,.48,.53,.48,.53,.48,'X'

;*************************************************************
; Table for Delay in Square mode
;*************************************************************
org 0x430
sqr_delay
	addwf PCL,f
	dt 0x0A,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x15,0x17,0x1A,0x1D,0x20,0x25,0x2C,0x35,0x43,0x5A,0x88,0xFF

;*************************************************************
; Table for Frequncy-Square-High digit
;*************************************************************
org 0x44E
sqr_high
	addwf PCL,f
	dt 0x0A,'B','A','9','9','8','8','7','7','6','6','6','5','5','4','4','3','3','2','2','1','1','0'

;*************************************************************
; Table for Frequncy-Square-Low digit
;*************************************************************
org 0x46C
sqr_low
	addwf PCL,f
	dt 0x0A,'5','5','8','1','6','1','6','2','9','5','0','5','0','5','0','5','0','5','0','5','0','5'

;*************************************************************
; Table for Delay in Sine/Trangle mode
;*************************************************************
org 0x48A
sin_delay
	addwf PCL,f
	dt .1,.1,.2,.4,.5,.6,.7,.8,.9,.10,.11,.12,.13,.14,.16,.19,.23,.27,.34,.42,.56,.78,.96

;*************************************************************
; Table for Frequncy-Sine/Trangle-High digit
;*************************************************************
org 0x4A8
sin_high
	addwf PCL,f
	dt 0x0A,'B','A','9','8','8','7','7','6','6','6','5','5','5','5','4','4','3','3','2','2','1','1'

;*************************************************************
; Table for Frequncy-Sine/Trangle-Mid digit
;*************************************************************
org 0x4C6
sin_mid
	addwf PCL,f
	dt 0x0A,'6','7','2','6','0','6','2','8','5','2','9','7','4','0','5','0','5','0','5','0','5','2'

;*************************************************************
; Table for Frequncy-Sine/Trangle-Low digit
;*************************************************************
org 0x4E2
sin_low
	addwf PCL,f
	dt 0x0A,'5','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','5'

;*************************************************************
; Table for Sine signal
;*************************************************************
org 0x700
sine
	 addwf PCL,1	
  	 retlw .128 
	 retlw .134 
	 retlw .141 
	 retlw .147 
	 retlw .153 
	 retlw .159 
	 retlw .165 
	 retlw .171 
	 retlw .177 
	 retlw .183 
	 retlw .189 
	 retlw .194 
	 retlw .200 
	 retlw .205 
	 retlw .210 
	 retlw .214 
	 retlw .219 
	 retlw .223 
	 retlw .227 
	 retlw .231 
	 retlw .235 
	 retlw .238 
	 retlw .241 
	 retlw .244 
	 retlw .246 
	 retlw .248 
	 retlw .250 
	 retlw .252 
	 retlw .253 
	 retlw .254 
	 retlw .255 
	 retlw .255 
	 retlw .255 
	 retlw .255 
	 retlw .254 
	 retlw .253 
	 retlw .252 
	 retlw .250 
	 retlw .248 
	 retlw .246 
	 retlw .244 
	 retlw .241 
	 retlw .238 
	 retlw .235 
	 retlw .231 
	 retlw .227 
	 retlw .223 
	 retlw .219 
	 retlw .214 
	 retlw .210 
	 retlw .205 
	 retlw .200 
	 retlw .194 
	 retlw .189 
	 retlw .183 
	 retlw .177 
	 retlw .171 
	 retlw .165 
	 retlw .159 
	 retlw .153 
	 retlw .147 
	 retlw .141 
	 retlw .134 
	 retlw .128 
	 retlw .122 
	 retlw .115 
	 retlw .109 
	 retlw .103 
	 retlw .97 
	 retlw .91 
	 retlw .85 
	 retlw .79 
	 retlw .73 
	 retlw .67 
	 retlw .62 
	 retlw .56 
	 retlw .51 
	 retlw .46 
	 retlw .42 
	 retlw .37 
	 retlw .33 
	 retlw .29 
	 retlw .25 
	 retlw .21 
	 retlw .18 
	 retlw .15 
	 retlw .12 
	 retlw .10 
	 retlw .8 
	 retlw .6 
	 retlw .4 
	 retlw .3 
	 retlw .2 
	 retlw .1 
	 retlw .1 
	 retlw .1 
	 retlw .1 
	 retlw .2 
	 retlw .3 
	 retlw .4 
	 retlw .6 
	 retlw .8 
	 retlw .10 
	 retlw .12 
	 retlw .15 
	 retlw .18 
	 retlw .21 
	 retlw .25 
	 retlw .29 
	 retlw .33 
	 retlw .37 
	 retlw .42 
	 retlw .46 
	 retlw .51 
	 retlw .57 
	 retlw .62 
	 retlw .67 
	 retlw .73 
	 retlw .79 
	 retlw .85 
	 retlw .91 
	 retlw .97 
	 retlw .103 
	 retlw .109 
	 retlw .115 
	 retlw .122 
	 retlw .128 
	 retlw .134 
	 retlw .141 
	 retlw .147 
	 retlw .153 
	 retlw .159 
	 retlw .166 
	 retlw .172 
	 retlw .177 
	 retlw .183 
	 retlw .189 
	 retlw .194 
	 retlw .200 
	 retlw .205 
	 retlw .210 
	 retlw .214 
	 retlw .219 
	 retlw .223 
	 retlw .227 
	 retlw .231 
	 retlw .235 
	 retlw .238 
	 retlw .241 
	 retlw .244 
	 retlw .246 
	 retlw .248 
	 retlw .250 
	 retlw .252 
	 retlw .253 
	 retlw .254 
	 retlw .255 
	 retlw .255 
	 retlw .255 
	 retlw .255 
	 retlw .254 
	 retlw .253 
	 retlw .252 
	 retlw .250 
	 retlw .248 
	 retlw .246 
	 retlw .244 
	 retlw .241 
	 retlw .238 
	 retlw .235 
	 retlw .231 
	 retlw .227 
	 retlw .223 
	 retlw .219 
	 retlw .214 
	 retlw .210 
	 retlw .205 
	 retlw .199 
	 retlw .194 
	 retlw .189 
	 retlw .183 
	 retlw .177 
	 retlw .171 
	 retlw .165 
	 retlw .159 
	 retlw .153 
	 retlw .147 
	 retlw .140 
	 retlw .134 
	 retlw .128 
	 retlw .122 
	 retlw .115 
	 retlw .109 
	 retlw .103 
	 retlw .97 
	 retlw .90 
	 retlw .84 
	 retlw .79 
	 retlw .73 
	 retlw .67 
	 retlw .62 
	 retlw .56 
	 retlw .51 
	 retlw .46 
	 retlw .41 
	 retlw .37 
	 retlw .33 
	 retlw .29 
	 retlw .25 
	 retlw .21 
	 retlw .18 
	 retlw .15 
	 retlw .12 
	 retlw .10 
	 retlw .8 
	 retlw .6 
	 retlw .4 
	 retlw .3 
	 retlw .2 
	 retlw .1 
	 retlw .1 
	 retlw .1 
	 retlw .1 
	 retlw .2 
	 retlw .3 
	 retlw .4 
	 retlw .6 
	 retlw .8 
	 retlw .10 
	 retlw .12 
	 retlw .15 
	 retlw .18 
	 retlw .21 
	 retlw .25 
	 retlw .29 
	 retlw .33 
	 retlw .37 
	 retlw .42 
	 retlw .47 
	 retlw .51 
	 retlw .57 
	 retlw .62 
	 retlw .67 
	 retlw .73 
	 retlw .79 
	 retlw .85 
	 retlw .91 
	 retlw .97 
	 retlw .103 
	 retlw .109 
	 retlw .116 
	 retlw .122 

;*************************************************************
; Table for Trangle signal
;*************************************************************
org 0x600
trangle
	 addwf PCL,1
	 retlw .0 
	 retlw .4 
	 retlw .8 
	 retlw .12 
	 retlw .16 
	 retlw .20 
	 retlw .24 
	 retlw .28 
	 retlw .32 
	 retlw .36 
	 retlw .40 
	 retlw .44 
	 retlw .48 
	 retlw .52 
	 retlw .56 
	 retlw .60 
	 retlw .64 
	 retlw .68 
	 retlw .72 
	 retlw .76 
	 retlw .80 
	 retlw .85 
	 retlw .89 
	 retlw .93 
	 retlw .97 
	 retlw .101 
	 retlw .105 
	 retlw .109 
	 retlw .113 
	 retlw .117 
	 retlw .121 
	 retlw .125 
	 retlw .129 
	 retlw .133 
	 retlw .137 
	 retlw .141 
	 retlw .145 
	 retlw .149 
	 retlw .153 
	 retlw .157 
	 retlw .161 
	 retlw .165 
	 retlw .170 
	 retlw .174 
	 retlw .178 
	 retlw .182 
	 retlw .186 
	 retlw .190 
	 retlw .194 
	 retlw .198 
	 retlw .202 
	 retlw .206 
	 retlw .210 
	 retlw .214 
	 retlw .218 
	 retlw .222 
	 retlw .226 
	 retlw .230 
	 retlw .234 
	 retlw .238 
	 retlw .242 
	 retlw .246 
	 retlw .250 
	 retlw .255 
	 retlw .251 
	 retlw .247 
	 retlw .243 
	 retlw .239 
	 retlw .235 
	 retlw .231 
	 retlw .227 
	 retlw .223 
	 retlw .219 
	 retlw .215 
	 retlw .211 
	 retlw .207 
	 retlw .203 
	 retlw .199 
	 retlw .195 
	 retlw .191 
	 retlw .187 
	 retlw .183 
	 retlw .179 
	 retlw .175 
	 retlw .170 
	 retlw .166 
	 retlw .162 
	 retlw .158 
	 retlw .154 
	 retlw .150 
	 retlw .146 
	 retlw .142 
	 retlw .138 
	 retlw .134 
	 retlw .130 
	 retlw .126 
	 retlw .122 
	 retlw .118 
	 retlw .114 
	 retlw .110 
	 retlw .106 
	 retlw .102 
	 retlw .98 
	 retlw .94 
	 retlw .90 
	 retlw .85 
	 retlw .81 
	 retlw .77 
	 retlw .73 
	 retlw .69 
	 retlw .65 
	 retlw .61 
	 retlw .57 
	 retlw .53 
	 retlw .49 
	 retlw .45 
	 retlw .41 
	 retlw .37 
	 retlw .33 
	 retlw .29 
	 retlw .25 
	 retlw .21 
	 retlw .17 
	 retlw .13 
	 retlw .9 
	 retlw .5 
	 retlw .0 
	 retlw .4 
	 retlw .8 
	 retlw .12 
	 retlw .16 
	 retlw .20 
	 retlw .24 
	 retlw .28 
	 retlw .32 
	 retlw .36 
	 retlw .40 
	 retlw .44 
	 retlw .48 
	 retlw .52 
	 retlw .56 
	 retlw .60 
	 retlw .64 
	 retlw .68 
	 retlw .72 
	 retlw .76 
	 retlw .80 
	 retlw .85 
	 retlw .89 
	 retlw .93 
	 retlw .97 
	 retlw .101 
	 retlw .105 
	 retlw .109 
	 retlw .113 
	 retlw .117 
	 retlw .121 
	 retlw .125 
	 retlw .129 
	 retlw .133 
	 retlw .137 
	 retlw .141 
	 retlw .145 
	 retlw .149 
	 retlw .153 
	 retlw .157 
	 retlw .161 
	 retlw .165 
	 retlw .170 
	 retlw .174 
	 retlw .178 
	 retlw .182 
	 retlw .186 
	 retlw .190 
	 retlw .194 
	 retlw .198 
	 retlw .202 
	 retlw .206 
	 retlw .210 
	 retlw .214 
	 retlw .218 
	 retlw .222 
	 retlw .226 
	 retlw .230 
	 retlw .234 
	 retlw .238 
	 retlw .242 
	 retlw .246 
	 retlw .250 
	 retlw .255 
	 retlw .251 
	 retlw .247 
	 retlw .243 
	 retlw .239 
	 retlw .235 
	 retlw .231 
	 retlw .227 
	 retlw .223 
	 retlw .219 
	 retlw .215 
	 retlw .211 
	 retlw .207 
	 retlw .203 
	 retlw .199 
	 retlw .195 
	 retlw .191 
	 retlw .187 
	 retlw .183 
	 retlw .179 
	 retlw .175 
	 retlw .170 
	 retlw .166 
	 retlw .162 
	 retlw .158 
	 retlw .154 
	 retlw .150 
	 retlw .146 
	 retlw .142 
	 retlw .138 
	 retlw .134 
	 retlw .130 
	 retlw .126 
	 retlw .122 
	 retlw .118 
	 retlw .114 
	 retlw .110 
	 retlw .106 
	 retlw .102 
	 retlw .98 
	 retlw .94 
	 retlw .90 
	 retlw .85 
	 retlw .81 
	 retlw .77 
	 retlw .73 
	 retlw .69 
	 retlw .65 
	 retlw .61 
	 retlw .57 
	 retlw .53 
	 retlw .49 
	 retlw .45 
	 retlw .41 
	 retlw .37 
	 retlw .33 
	 retlw .29 
	 retlw .25 
	 retlw .21 
	 retlw .17 
	 retlw .13 
	 retlw .9 
	 retlw .5 



org 0x500
square
	 addwf PCL,1	
	 dt 0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF,0x00,0xFF
		


end