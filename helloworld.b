; *************************************************************************************************
;               CBM2-V9958-Card Yamaha V9938-V9958 CBM2-Hello World! / Vossi 03/2019
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
PAL = 0			; PAL=1, NTSC=0		selects V9938/58 PAL RGB-output, NTSC has a higher picture
LINES = 192		; lines = 192 / 212
; ***************************************** CONSTANTS *********************************************
FILL					= $00		; fills free memory areas with $00
V_NULL					= $ff		; VDP string End
VDPREG1					= $00		; VDP reg 1 value (mode bits M1+M2, screen disabled)
VDPREG18				= $0d		; VDP reg 18 value (V/H screen adjust, $0d = Sony PVM 9")
!if LINES=192 {VDPREG9	= $00|PAL*2	; VDP reg 9 value ($00 = NTSC, $02 = PAL)
	}else {VDPREG9		= $80|PAL*2}
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
										; IO pointers
VZP = $40						; *** start zero page VDP parameter
!addr vdp_counter		= VZP			; 8bit universal counter
!addr vdp_calc			= VZP+$02		; 8bit universal calc memory
!addr vdp_data			= VZP+$09		; 16bit pointer to source data (bitmap or string)
!addr vdp_size			= VZP+$0b		; 16bit size for VdpCopy + VdpCopy16

; ******************************************* MACROS **********************************************
!macro inc16 .a{			; *** increase 16bit
	inc .a
	bne .j
	inc .a+1
.j}
!macro ldax16i .v{			; *** loads 16bit immediate to XXAA
	lda # <.v
	ldx # >.v
}
!macro st16i .a, .v{		; *** store 16bit immediate to address
	lda # <.v
	sta .a
	lda # >.v
	sta .a+1
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
	jsr InitZeroPage					; initialize all zero page pointers
	jsr VdpClear					; clear screen

	+st16i vdp_data, vdp_message	; string address
	ldy # 0							; line in Y
	ldx # 0							; column in X
	jsr VdpText

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
	ldx # <VDPAddress
	stx VDPRamWrite
	inx
	stx VDPControl
	inx
	stx VDPPalette
	inx
	stx VDPIndirect
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
									; * clear 16kB VRAM
	tya									; all regs $00
	tax
	+VdpWriteAddress			 		; set VRAM write address to $XXAA = $0000
	lda #40								; set counter to clear $40 x $100 bytes
	sta vdp_counter
	tya									; VRAM init value = $00, X already $00
-	+VDPWAIT 3							; wait 3us - each 8us is a VRAM write allowed
	sta(VDPRamWrite),y
	inx
	bne -
	dec vdp_counter
	bne -								; 16kB reached ?
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
	sta vdp_counter
	ldx # <(27*COLS)
	lda # ' '							; space
-	sta(VDPRamWrite),y
	+VDPWAIT 3
	dex
	bne -
	dec vdp_counter
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
; ****************************************** ZONE DATA ********************************************
!zone data
vdp_message !scr "Hello World!", V_NULL

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
; ***** color data for tiles: each byte = color/background for a group of 8 tiles *****
ColorData:
	!byte $16,$16,$16,$16,$16,$16,$16,$16	; text = white, background = blue
	!byte $16,$16,$16,$16,$16,$16,$16,$16
	!byte $16,$16,$16,$16,$16,$16,$16,$16
	!byte $16,$16,$16,$16,$16,$16,$16,$16
ColorDataEnd:
; ***** CBM2 8x8 font - characters 0-90 only *****
FontData:
	!byte $1c, $22, $4a, $56, $4c, $20, $1e, $00
	!byte $00, $00, $38, $04, $3c, $44, $3a, $00
	!byte $40, $40, $5c, $62, $42, $62, $5c, $00
	!byte $00, $00, $3c, $42, $40, $42, $3c, $00
	!byte $02, $02, $3a, $46, $42, $46, $3a, $00
	!byte $00, $00, $3c, $42, $7e, $40, $3c, $00
	!byte $0c, $12, $10, $7c, $10, $10, $10, $00
	!byte $00, $00, $3a, $46, $46, $3a, $02, $3c
	!byte $40, $40, $5c, $62, $42, $42, $42, $00
	!byte $08, $00, $18, $08, $08, $08, $1c, $00
	!byte $04, $00, $0c, $04, $04, $04, $44, $38
	!byte $40, $40, $44, $48, $50, $68, $44, $00
	!byte $18, $08, $08, $08, $08, $08, $1c, $00
	!byte $00, $00, $76, $49, $49, $49, $49, $00
	!byte $00, $00, $5c, $62, $42, $42, $42, $00
	!byte $00, $00, $3c, $42, $42, $42, $3c, $00
	!byte $00, $00, $5c, $62, $62, $5c, $40, $40
	!byte $00, $00, $3a, $46, $46, $3a, $02, $02
	!byte $00, $00, $5c, $62, $40, $40, $40, $00
	!byte $00, $00, $3e, $40, $3c, $02, $7c, $00
	!byte $10, $10, $7c, $10, $10, $12, $0c, $00
	!byte $00, $00, $42, $42, $42, $46, $3a, $00
	!byte $00, $00, $42, $42, $42, $24, $18, $00
	!byte $00, $00, $41, $49, $49, $49, $36, $00
	!byte $00, $00, $42, $24, $18, $24, $42, $00
	!byte $00, $00, $42, $42, $46, $3a, $02, $3c
	!byte $00, $00, $7e, $04, $18, $20, $7e, $00
	!byte $3c, $20, $20, $20, $20, $20, $3c, $00
	!byte $1c, $22, $20, $f8, $20, $40, $fe, $00
	!byte $3c, $04, $04, $04, $04, $04, $3c, $00
	!byte $00, $08, $1c, $2a, $08, $08, $08, $00
	!byte $00, $00, $10, $20, $7f, $20, $10, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $08, $08, $08, $08, $00, $00, $08, $00
	!byte $24, $24, $24, $00, $00, $00, $00, $00
	!byte $24, $24, $7e, $24, $7e, $24, $24, $00
	!byte $08, $1e, $28, $1c, $0a, $3c, $08, $00
	!byte $00, $62, $64, $08, $10, $26, $46, $00
	!byte $30, $48, $48, $30, $4a, $44, $3a, $00
	!byte $04, $08, $10, $00, $00, $00, $00, $00
	!byte $04, $08, $10, $10, $10, $08, $04, $00
	!byte $20, $10, $08, $08, $08, $10, $20, $00
	!byte $08, $2a, $1c, $3e, $1c, $2a, $08, $00
	!byte $00, $08, $08, $3e, $08, $08, $00, $00
	!byte $00, $00, $00, $00, $08, $08, $10, $00
	!byte $00, $00, $00, $7e, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $18, $18, $00
	!byte $00, $02, $04, $08, $10, $20, $40, $00
	!byte $3c, $42, $46, $5a, $62, $42, $3c, $00
	!byte $08, $18, $28, $08, $08, $08, $3e, $00
	!byte $3c, $42, $02, $0c, $30, $40, $7e, $00
	!byte $3c, $42, $02, $1c, $02, $42, $3c, $00
	!byte $04, $0c, $14, $24, $7e, $04, $04, $00
	!byte $7e, $40, $78, $04, $02, $44, $38, $00
	!byte $1c, $20, $40, $7c, $42, $42, $3c, $00
	!byte $7e, $42, $04, $08, $10, $10, $10, $00
	!byte $3c, $42, $42, $3c, $42, $42, $3c, $00
	!byte $3c, $42, $42, $3e, $02, $04, $38, $00
	!byte $00, $00, $08, $00, $00, $08, $00, $00
	!byte $00, $00, $08, $00, $08, $08, $10, $00
	!byte $0e, $18, $30, $60, $30, $18, $0e, $00
	!byte $00, $00, $7e, $00, $7e, $00, $00, $00
	!byte $70, $18, $0c, $06, $0c, $18, $70, $00
	!byte $3c, $42, $02, $0c, $10, $00, $10, $00
	!byte $00, $00, $00, $00, $ff, $00, $00, $00
	!byte $18, $24, $42, $7e, $42, $42, $42, $00
	!byte $7c, $22, $22, $3c, $22, $22, $7c, $00
	!byte $1c, $22, $40, $40, $40, $22, $1c, $00
	!byte $78, $24, $22, $22, $22, $24, $78, $00
	!byte $7e, $40, $40, $78, $40, $40, $7e, $00
	!byte $7e, $40, $40, $78, $40, $40, $40, $00
	!byte $1c, $22, $40, $4e, $42, $22, $1c, $00
	!byte $42, $42, $42, $7e, $42, $42, $42, $00
	!byte $1c, $08, $08, $08, $08, $08, $1c, $00
	!byte $0e, $04, $04, $04, $04, $44, $38, $00
	!byte $42, $44, $48, $70, $48, $44, $42, $00
	!byte $40, $40, $40, $40, $40, $40, $7e, $00
	!byte $42, $66, $5a, $5a, $42, $42, $42, $00
	!byte $42, $62, $52, $4a, $46, $42, $42, $00
	!byte $18, $24, $42, $42, $42, $24, $18, $00
	!byte $7c, $42, $42, $7c, $40, $40, $40, $00
	!byte $18, $24, $42, $42, $4a, $24, $1a, $00
	!byte $7c, $42, $42, $7c, $48, $44, $42, $00
	!byte $3c, $42, $40, $3c, $02, $42, $3c, $00
	!byte $3e, $08, $08, $08, $08, $08, $08, $00
	!byte $42, $42, $42, $42, $42, $42, $3c, $00
	!byte $42, $42, $42, $24, $24, $18, $18, $00
	!byte $42, $42, $42, $5a, $5a, $66, $42, $00
	!byte $42, $42, $24, $18, $24, $42, $42, $00
	!byte $22, $22, $22, $1c, $08, $08, $08, $00
	!byte $7e, $02, $04, $18, $20, $40, $7e, $00
FontDataEnd: