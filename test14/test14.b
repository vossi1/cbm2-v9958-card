; *************************************************************************************************
;                  CBM2-V9958-Card Yamaha V9938-V9958 CBM2-Test / Vossi 01/2019
; *************************************************************************************************
; basic copys init-code to bank 15 and starts at $0400
!cpu 6502	; 6502, 6510, 65c02, 65816
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
; VDP write speed: Reg->Reg(Port 1,2,3) 2us, Reg->VRAM 8us, VRAM->VRAM 8us, VRAM->Reg 8us
; 	sta(zp),Y = 6 cycles @ 2MHz = 3us -> no wait nessesary
;	lda# + sta(zp),y = 8 cycles @ 2MHz = 4us -> add 5us wait
;	lda addr,x + sta(zp),y + inx + bne = 15 cycles @2MHz = 7.5us -> OK
; pass parameters to subroutine: AA = lowbyte, XX = highbyte / return AA or XXAA 

; switches
DEBUG = 1		; selects DEBUG code
PAL = 0			; PAL=1, NTSC=0		selects V9938/58 PAL RGB-output, NTSC has a higher picture
LINES = 192		; lines = 192 / 212
!source "macros_6502.b"
; ***************************************** CONSTANTS *********************************************
FILL					= $00		; fills free memory areas with $00
V_NULL					= $ff		; VDP string End
VDPREG1					= $00		; VDP reg 1 value (mode bits M1+M2, screen disabled)
VDPREG18				= $0d		; VDP reg 18 value (V/H screen adjust, $0d = Sony PVM 9")
!if LINES=192 {VDPREG9	= $00|PAL*2	; VDP reg 9 value ($00 = NTSC, $02 = PAL)
	}else {VDPREG9		= $80|PAL*2}
VDPTUNE					= 0			; tune vdp waits in 1us steps
CLEARBANKS				= 1			; number of 16kB VRAM banks to clear
COLS					= 32		; screen columns
ROWS					= 24		; used lines
; ***************************************** ADDRESSES *********************************************
VDPAddress				= $d900		; Port#0 RamWrite, #1 Control, #2 Palette, #4 RamRead, #5 Status
PatternTable			= $0000
ColorTable				= $2000
ScreenTable				= $1800
!ifndef DUMMYADDRESS{!addr DUMMYADDRESS = $0000}
!addr vdpcopy16_data	= VdpCopy16CodePointer+1	; 16bit pointer to LDA-address in VdpCopy16
!addr vdpcopy_data		= VdpCopyCodePointer+1		; 16bit pointer to LDA-address in VdpCopy
; ***************************************** ZERO PAGE *********************************************
!addr CodeBank			= $00	; *** bank select register	
!addr IndirectBank		= $01
ZP = $33						; *** start zero page pointers
!addr VDPRamWrite		= ZP			; VDP pointer
!addr VDPControl		= ZP+$02
!addr VDPPalette		= ZP+$04
!addr VDPIndirect		= ZP+$06
!addr VDPRamRead		= ZP+$08
!addr VDPStatus			= ZP+$0a
										; IO pointers
VZP = $40						; *** start zero page VDP parameter
!addr vdp_counter		= VZP			; 8bit universal counter
!addr vdp_counter2		= VZP+$01		; 8bit universal counter
!addr vdp_calc			= VZP+$02		; 8bit universal calc memory
!addr vdp_bgcolor		= VZP+$03		; 8bit background color
!addr vdp_color			= VZP+$04		; 8bit color
!addr vdp_pointer		= VZP+$05		; 16bit universal pointer
!addr vdp_pointer2		= VZP+$07		; 16bit universal pointer
!addr vdp_data			= VZP+$09		; 16bit pointer to source data (bitmap or string)
!addr vdp_size			= VZP+$0b		; 16bit size for VdpCopy + VdpCopy16
!addr vdp_size_x		= VZP+$0d		; 16bit x size for VDP subroutines
!addr vdp_size_y		= VZP+$0f		; 16bit y size for VDP sobroutines
BZP = $c0						; *** start zero page byte-variables
!addr vdpid				= BZP			; VDP ID
!addr counter			= BZP+$01		; 8bit universal counter
WZP = $d0						; *** start zero page word variables	
!addr counter16			= WZP			; 16bit universal counter
!addr pointer			= WZP+$02		; universal pointer
;						= $f8-$fc		; reserved console source
!addr debug_fd			= $fd;-$ff		; 3 byte debug register for A, X, Y			
; ******************************************* MACROS **********************************************
!ifdef DEBUG{
!macro DEBUG{ jsr ConsoleDebug }
}
!macro VDPWAIT .t{			; *** t x 1 us wait for VRAM write
	!do while .t > 0{
		nop								; each nop needs 1 us @ 2MHz
		!set .t = .t -1}
}
!macro VdpSetReg .r{			; *** set VDP Register
	ldy #$00
	sta(VDPControl),y					; first writes data in A to control port #1
	lda # .r | $80						; writes register no. with bit#7 = 1 to Port #1
	sta(VDPControl),y
}
; ********************* Y register must be $00 for all Vdp-Address subroutines ********************
!macro VdpWriteAddress{			; *** set VDP write vram address-pointer to XXAA
	sta(VDPControl),y
	txa
	ora # $40							; bit#6 = 1 write
	sta(VDPControl),y
} 
!macro VdpReadAddress{					; *** set VDP read vram address-pointer to XXAA
	sta(VDPControl),y
	txa
	sta(VDPControl),y
}
; **************************************** BASIC LOADER *******************************************
!initmem FILL
*= $0003
	!byte $2f,$00,$0a,$00,$81,$49,$b2,$31,$30,$32,$34,$a4,$31,$30,$33,$39
	!byte $3a,$dc,$31,$3a,$41,$b2,$c2,$28,$49,$29,$3a,$dc,$31,$35,$3a,$97
	!byte $49,$2c,$41,$3a,$82,$3a,$9e,$31,$30,$32,$34,$00,$00,$00,$00,$00
	; 10 fori=1024to1039:bank1:a=peek(i):bank15:pokei,a:next:sys1024
	; $0033 - $00FF	zero page
	; $0100 - $01FF	cpu-stack
	; $0200 - $03FF	start-code
	; $0400 - $040F	init-code -> mirror in bank 15
	; $0410 - 		code / data
; ***************************************** ZONE INIT *********************************************
!zone init
*= $0400
init:							; *** initialize bank regs and start main code ***
	sei									; disable interupts
	lda #$01							; switch to bank 1
	sta CodeBank
	jmp start
end:							; *** terminate program and jump back to basic ***
	lda #$0f							; switch completely back to bank 15
	sta CodeBank
	sta IndirectBank
	cli									; enable interupts
	rts									; back to basic
; ***************************************** ZONE MAIN *********************************************
!zone main
*= $0200
start:							; *** main code starts here ***	
	lda #$0f							; set bank indirect reg to bank 15
	sta IndirectBank
	jsr ConsoleClear					; start console
	jsr InitZeroPage					; initialize all zero page pointers
	+ldax16i message_start
	jsr ConsoleText
	+ldax16i message_copyright
	jsr ConsoleText
	lda # 1								; read status register 1
	jsr VdpStatus
	lsr
	and #$1f							; isolate VDP identification bit #1-5
	sta vdpid
	bne +
	+ldax16i message_v9938				; ID = 0 -> V9938
	jsr ConsoleText
	jmp ++
+	cmp #$02
	bne +
	+ldax16i message_v9958				; ID = 1 -> V9958
	jsr ConsoleText
	jmp ++
+	+ldax16i message_novdp				; no vdp detected
	jsr ConsoleText
++	+ldax16i message_clearvram
	jsr ConsoleText
	
	jsr VdpInit						; VDP init - returns 16k VRAM banks detected
	bne +
	+ldax16i message_nomemory
	jsr ConsoleText
	jmp ++
+	asl
	asl
	jsr ConsoleByte
	+ldax16i message_memory
	jsr ConsoleText
++	lda vdpid						; ready message only with id ID 0 or 2 
	beq +
	cmp #$02
	bne ++
+	+ldax16i message_vdpready
	jsr ConsoleText
	jmp +
++	+ldax16i message_vdpunknown
	jsr ConsoleText

+	jsr VdpClear					; clear screen

	lda # 8							; set counter to start line
	sta counter
	+st16i vdp_data, vdp_message	; string address
-	ldy counter						; line in Y
	ldx # 1							; column in X
+DEBUG
	jsr VdpText
	inc counter
	lda counter
	cmp # 16						; last line reached
	bne -

+	jsr VdpOn						; switch display on
	jmp end							; end program
; ************************************* ZONE SUBROUTINES ******************************************
!zone subroutines
InitZeroPage:					; *** init zero page addresses
	lda # >VDPAddress
	sta VDPRamWrite+1
	sta VDPControl+1
	sta VDPPalette+1
	sta VDPIndirect+1
	sta VDPRamRead+1
	sta VDPStatus+1
	ldx # <VDPAddress
	stx VDPRamWrite
	inx
	stx VDPControl
	inx
	stx VDPPalette
	inx
	stx VDPIndirect
	inx
	stx VDPRamRead
	inx
	stx VDPStatus
	rts
; *********************************** ZONE VDP_SUBROUTINES ****************************************
*= $0410
!zone vdp_subroutines
VdpInit:						; *** initialize VDP ***
	lda #$00
	tax
	+VdpSetReg 17						; write VDP regs fast indirect
	+VDPWAIT 4
-	lda VdpInitData,x
	sta(VDPIndirect),y
	inx
	cpx # VdpInitDataEnd - VdpInitData
	bne -
	lda # VDPREG18
	+VdpSetReg 18						; set register 18 V/H display adjust L 7-1,0,f-8 R
									; * clear 128kB VRAM (with bank switching 0-7)
	tya									; all regs $00
	tax
	+VdpWriteAddress			 		; set VRAM write address to $XXAA = $0000
	+st16i counter16, $0000
	ldx #00
-	tya									; VRAM init value = $00, X already $00
	sta(VDPRamWrite),y
	inc counter16
	bne -
	inc counter16+1
	lda counter16+1
	cmp #40								; 16kB bank end reached ?
	bne -
	inx
	cpx # CLEARBANKS					; last 16kB bank reached ?							
	beq +
	txa
	+VdpSetReg 14						; set bank to X
	jmp -
+	tya
	tax
	+VdpSetReg 14						; reset to bank 0

	sty counter							; start test at bank 0
-	lda counter
	+VdpSetReg 14 						
	inc counter
	+VDPWAIT 4
	tya
	tax
	+VdpReadAddress						; set VRAM adress to $0000
	+VDPWAIT 6
	lda(VDPRamRead),y
	bne +								; cleared VRAM not $00 ?
	lda counter
	cmp #$08
	beq ++
	jmp -
+	dec counter
++	lda counter
	pha
	tya									; switch back to VRAM bank 0
	+VdpSetReg 14
									; * copy font to pattern generator table
	+st16i vdpcopy16_data, FontData
	+st16i vdp_size, FontDataEnd - FontData
	+ldax16i PatternTable				; VRAM address in XXAA
	jsr VdpCopy16
									; * copy color-palette
	tya
	+VdpSetReg  16						; set VDP register 16 = palette pointer to $00 
	ldx #$00
-	lda PaletteData,x					; load palette-color to write
	sta(VDPPalette),y
	inx
	cpx # PaletteDataEnd - PaletteData
	bne -

	+st16i vdpcopy_data, ColorData	; * copy color-table
	lda # ColorDataEnd - ColorData
	sta vdp_size
	+ldax16i ColorTable					; load color-table address
	jsr VdpCopy
	pla									; return vram banks
	rts

VdpOn:							; *** enable screen ***
	lda # VDPREG1 | $40					; set mode reg 1 (M1+M2), bit#6 = 1 enables screen
	+VdpSetReg 1
	rts

VdpOff:							; *** disable screen ***
	lda # VDPREG1 & $bf					; set mode reg 1 (M1+M2), bit#6 = 1 enables screen
	+VdpSetReg 1
	rts

VdpStatus:						; *** read status register in A - return status in A
	lda # 1
	+VdpSetReg 15						; reg 15 = 1 initiates read status-reg 1
	+VDPWAIT 6						; wait for DVP
	lda(VDPStatus),y					; read status
	rts

VdpCopy:						; *** copy vdp_size bytes from pointer to VRAM at $XXAA ***
	ldy #$00
	+VdpWriteAddress
	ldx #$00
VdpCopyCodePointer:						; +1,+2 = data address in memory
-	lda DUMMYADDRESS,x					; load data
	sta(VDPRamWrite),y
	inx
	cpx vdp_size
	bne -
	rts

VdpCopy16:						; *** copy vdp_size bytes from pointer to VRAM at $XXAA ***
	lda #$00
	+VdpWriteAddress
	inc vdp_size+1						; add 1 to count-highbyte because nessasary dec count+1
	ldx vdp_size
VdpCopy16CodePointer:					; +1,+2 = data address in memory
-	lda DUMMYADDRESS					; load data
	sta(VDPRamWrite),y
	+inc16 VdpCopy16CodePointer+1		; increase lda address
	dex
	bne -
	dec vdp_size+1
	bne -
	rts

VdpClear:
	ldy #$00
	+ldax16i ScreenTable
	+VdpWriteAddress			 		; set VRAM write address to $XXAA = $0000
	lda # (>(27*COLS))+1				; 27 lines with 32 characters =$360 (+1 run for $60 in X)
	sta counter
	ldx # <(27*COLS)
	lda # ' '							; space
-	sta(VDPRamWrite),y
	+VDPWAIT 3-VDPTUNE
	dex
	bne -
	dec counter
	bne -
	rts

VdpText:						; *** copy string vdp_data to VRAM at $XXAA ***
	stx vdp_calc						; safe column
	+ldax16i ScreenTable				; load screen table base in AX
-	cpy #$00
	beq +
	dey
	clc
	adc # COLS							; add 32 for each line
	bcc -
	inx
	bcs -
+	clc
	adc vdp_calc						; add column
	bcc +
	inx
+	+VdpWriteAddress					; Y already $00
	sty vdp_counter						; init string counter
-	tya									; clear A for ORA = LDA
	ldy vdp_counter
	ora(vdp_data),y						; load source data
	cmp # V_NULL
	beq +								; V_NULL = end of string
	ldy #$00
	sta(VDPRamWrite),y
	inc vdp_counter						; next character from string
	bne -
+	rts

; ***************************************** ZONE DEBUG ********************************************
!zone debug_subroutine
!ifdef DEBUG{
ConsoleDebug:							; optional debug code prints A, X, Y
	php
	sta debug_fd
	stx debug_fd+1
	sty debug_fd+2
	+ldax16i string_debug_axy
	jsr ConsoleText
	lda debug_fd
	jsr ConsoleByte
	+ldax16i string_space
	jsr ConsoleText
	lda debug_fd+1
	jsr ConsoleByte
	+ldax16i string_space
	jsr ConsoleText
	lda debug_fd+2
	jsr ConsoleByte
	+ldax16i string_cr
	jsr ConsoleText
	lda debug_fd
	ldx debug_fd+1
	ldy debug_fd+2
	plp
	rts
}
; ********************************** ZONE CONSOLE_SUBROUTINES *************************************
!zone console_subroutines
!source "console.b"
; ****************************************** ZONE DATA ********************************************
!zone data
message_start !scr "Commodore-CBM2 with V9958 Hires-Color-Card!", C_CR, C_NULL
message_copyright !scr "Design (c) Vossi 01/2019 in Hamburg/Germany", C_CR, C_CR, C_NULL
message_clearvram !scr "Initializing VDP and clearing VRAM... ", C_CR, C_NULL
message_memory !scr "000 kB graphics memory detected", C_CR, C_NULL
message_nomemory !scr "No graphics memory detected!", C_CR, C_NULL
message_v9938 !scr "V9938 detected", C_CR, C_NULL
message_v9958 !scr "V9958 detected", C_CR, C_NULL
message_novdp !scr "No VDP detected! -> trying to initialize:", C_CR, C_NULL
message_vdpready !scr "VDP ready.", C_CR, C_NULL
message_vdpunknown !scr "VDP ?", C_CR, C_NULL
vdp_message !scr "CBM2 V9958-card (c) vossi 2019", V_NULL

VdpInitData:	; graphics1-mode
!byte $00,VDPREG1,$06,$80,$00,$36,$07,$00,$08,VDPREG9,$00,$00,$10,$f0,$00
	; reg  0: $00 mode control 1: graphics mode 1 (bit#1-3 = M3 - M5)
	; reg  1: $00 mode control 2: bit#1 16x16 sprites, bit#3-4 = M2-M1, #6 =1: display enable)
	; reg  2: $06 name (screen) table base address $1800 (* $400)
	; reg  3: $80 color table base address $2000 (* $40)
	; reg  4: $00 pattern (character) generator table base address $0000 (* $800)
	; reg  5: $36 sprite attribute table base address $1B00 (* $80)
	; reg  6: $07 sprite pattern (data) generator base address = $3800 (* $800)
	; reg  7: $00 text/overscan-backdrop color 
	; reg  8: $08 bit#3 = 1: 64k VRAM chips, bit#1 = 0 sprites disable, bit#5 0=transparent
	; reg  9: $00 bit#1 = NTSC/PAL, #2 = EVEN/ODD, #3 = interlace, #7 = 192/212 lines
	; reg 10: $00 color table base address $0000 bit#0-2 = A14-A16
	; reg 11: $00 sprite attribute table base address bit#0-1 = A15-A16
	; reg 12: $10 text/background blink color
	; reg 13: $f0 blink periods ON/OFF - f0 = blinking off
	; reg 14: $00 VRAM write addresss bit#0-2 = A14-A16
VdpInitDataEnd:

; ***** Color Palette - 16 colors, 2 byte/color: RB, 0G each 3bit -> C64 VICII-colors *****
PaletteData:
	!byte $00,$00,$77,$07,$70,$01,$17,$06	;	0=black		1=white		2=red		3=cyan
	!byte $56,$02,$32,$06,$06,$02,$72,$07	;	4=violet	5=green		6=blue		7=yellow
	!byte $70,$03,$60,$02,$72,$03,$11,$01	;	8=orange	9=brown		a=lightred	b=darkgrey
	!byte $33,$03,$54,$07,$27,$04,$55,$05	;	c=grey		d=litegreen	e=lightblue	f=lightgrey
PaletteDataEnd:

ColorData:
	!byte $1b,$1b,$1b,$1b,$7b,$eb,$2b,$2b	; each byte = color/background for a group of 8 tiles
	!byte $6b,$6b,$2b,$2b,$1b,$1b,$1b,$1b
	!byte $1b,$1b,$1b,$1b,$1b,$1b,$1b,$1b
	!byte $1b,$1b,$1b,$1b,$1b,$1b,$1b,$1b
ColorDataEnd:

FontData:
	!binary "cbm2-8x8.fon"
FontDataEnd: