; **************************************************************
;             Universal 6502 Macros / Vossi 01/2019
; **************************************************************
!ifndef DUMMYADDRESS{!addr DUMMYADDRESS = $0000}	; addressdummy for selfmod code
!macro inc16 .a{			; *** increase 16bit
	inc .a
	bne .j
	inc .a+1
.j}
!macro dec16 .a{			; *** decrease 16bit - destroys A
	lda .a
	bne .j
	dec .a+1
.j	dec .a
}
!macro ldax16 .a{			; *** loads 16bit from address to XXAA
	lda .a
	ldx .a+1
}
!macro ldax16i .v{			; *** loads 16bit immediate to XXAA
	lda # <.v
	ldx # >.v
}
!macro ldaindax{			; *** loads (XXAA) indirect
	sta .c+1
	stx .c+2
	ldx #00
.c:	lda DUMMYADDRESS,x
}
!macro ldaindzpy .zp{		; *** loads (ZP-address),Y indirect
	lda#00
	ora(.zp),y
}
!macro stax16 .a{			; *** stores 16bit in XXAA to address
	sta .a
	stx .a+1
}
!macro st16i .a, .v{		; *** store 16bit immediate to address
	lda # <.v
	sta .a
	lda # >.v
	sta .a+1
}
!macro mov16 .a, .b{		; *** moves 16bit from address to address
	lda .a
	sta .b
	lda .a+1
	sta .b+1
}
!macro bcc .a{				; *** far branches
bcs .j
jmp .a
.j}
!macro bcs .a{
bcc .j
jmp .a
.j}
!macro bne .a{
beq .j
jmp .a
.j}
!macro beq .a{
bne .j
jmp .a
.j}