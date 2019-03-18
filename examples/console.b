; **************************************************************
;            CBM2 Console Subroutines / Vossi 01/2019
; **************************************************************
; constants
C_ROWS				= 25		; console rows on screen
C_COLS				= 80		; console columns on screen
C_CR				= $80		; console CR
C_NULL				= $ff		; console string End
; addresses
ConsoleAddress		= $d000		; CRTC screen-memory
!addr c_textpointer	= ConsoleTextCodePointer+1	; 16bit pointer to LDA-address in ConsoleText
!addr c_scrpointer	= $f8;+$f9	; CRTC-memory pointer to actual position
!addr c_cursor_x	= $fa		; cursor-position for console output
!addr c_cursor_y	= $fb
!addr c_pointer		= $fc		; helper_pointer
; subroutines
ConsoleInit:					; *** inits console pointer, cursor home
	+st16i c_scrpointer, ConsoleAddress	; set write-pointer to screen base address
	ldy #$00
	sty c_cursor_x					; set cursor to 0, 0
	sty c_cursor_y
	rts
	
ConsoleClear:					; *** console clear + cursor home
	jsr ConsoleInit
	ldx # (>(C_ROWS*C_COLS))+1		; calculate screen memory size 
	lda # " "						; clear character = space 
-	sta(c_scrpointer),y				; Y is $00 from ConsoleInit
	iny
	bne -
	dex
	beq +
	inc c_scrpointer+1
	bne -
+	lda # >ConsoleAddress
	sta c_scrpointer+1					; reset highbyte write-pointer to screen base address
	rts
	
ConsoleCursor:					; *** set cursor position to X, Y
	sty c_cursor_y
	stx c_cursor_x
	+st16i c_scrpointer, ConsoleAddress	; reset write-pointer to screen base address
-	cpy # 0
	beq +
	dey
	lda c_scrpointer
	clc
	adc # C_COLS					; add 80 bytes for each line to position
	sta c_scrpointer
	bcc -
	inc c_scrpointer+1
	bne -							; = jmp
+	lda c_scrpointer
	clc
	adc c_cursor_x					; add x-position
	sta c_scrpointer
	bcc +
	inc c_scrpointer+1
+	rts

ConsoleText:					; *** writes string at cursor to screen and moves cursor
	+stax16 c_textpointer
	ldy #$00
	ldx #$00
ConsoleTextCodePointer:				; +1,+2 = text-data address in memory
-	lda DUMMYADDRESS,x				; load data, max 256 bytes
	cmp # C_NULL					; string end?
	beq +++
	cmp # C_CR
	bne ++
	lda # C_COLS
	sbc c_cursor_x
	clc
	adc c_scrpointer
	sta c_scrpointer
	bcc +
	inc c_scrpointer+1
+	sty c_cursor_x
	inc c_cursor_y
	lda c_cursor_y
	cmp # C_ROWS
	bne +
	jsr ConsoleScroll
+	inx
	bne -
	jmp +++
++	sta(c_scrpointer),y
	+inc16 c_scrpointer
	inc c_cursor_x
	lda c_cursor_x
	cmp # C_COLS
	bne +
	tya								; c_cursor_x to 0
	sta c_cursor_x
	inc c_cursor_y
	lda c_cursor_y
	cmp # C_ROWS
	bne +
	jsr ConsoleScroll
+	inx
	bne -
+++	rts

ConsoleByte:					; *** print AA as hexbyte to console
	tax								; duplicate to X
	and # $f0						; isolate low nibble
	lsr
	lsr
	lsr
	lsr
	adc # $30
	cmp # $3a
	bmi +
	adc # $06
+	sta string_hexbyte+1
	txa
	and # $0f
	adc # $30
	cmp # $3a
	bmi +
	adc # $06
+	sta string_hexbyte+2
	+ldax16i string_hexbyte
	jsr ConsoleText
	rts

ConsoleScroll:					; *** scrolls console screen one line up
	pha
	txa
	pha
	+st16i c_pointer, ConsoleAddress
-	ldy # C_COLS
	lda(c_pointer),y					; load data
	ldy # 0
	sta(c_pointer),y					; write data one line above
	+inc16 c_pointer
	lda c_pointer
	cmp # <(ConsoleAddress+(C_ROWS-1)*C_COLS)
	bne -
	lda c_pointer+1
	cmp # >(ConsoleAddress+(C_ROWS-1)*C_COLS)
	bne -
	lda # " "
-	sta(c_pointer),y
	iny
	cpy # C_COLS
	bne -
+	dec c_cursor_y
	lda c_pointer
	sta c_scrpointer
	lda c_pointer+1
	sta c_scrpointer+1
	ldy #$00
	pla
	tax
	pla
	rts
; strings
string_hexbyte !scr "$  ", C_NULL
string_space !scr " ", C_NULL
string_cr !raw C_CR, C_NULL
string_debug_axy !scr ".debug A, X, Y: ", C_NULL
