# cbm2-v9958-card

**Copyright (c) 2019 Vossi - v 1.0**
**Basic-Cartridge (c) 2024 Vossi**
**www.mos6502.net**

## License
This work is licensed under a Creative Commons Attribution-ShareAlike 4.0
International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).

:thumbsup: final pcb version 1.0
cbm2 color-graphics-card for the LP and HP models with the Yamaha V9958 (V9938) VDP and 128kB dedicated video ram.

**[Schematic](https://github.com/vossi1/cbm2-v9958-card/blob/master/doc/Schematics.png)**

**[Parts](https://github.com/vossi1/cbm2-v9958-card/blob/master/doc/partlist)**

**description:**

    This is a color-graphics-card that plugs in one of the two 60 pin expansion connectors
    of a cbm2-machine.
    It is based on the Yamaha MSX2+ video display processor (VDP) V9958 (MSX2: V9938).
    Both VDP's are backward compatible to the TMS9918/9928 used in many old home computers.
    The V9958 has some additional features compared to the V9938 like vertical soft scrolling
    and some 19k color modes.

    The VDP has a RGB output programmable for PAL or NTSC. You need an analog RGB monitor
    like the 1084 or a scart TV. The V9958-card has 128kB dedicated RAM - only reachable
    through the VDP-ports. It has screen resolutions from 256x192 up to 512x212 / 512x424i
    and 16/256/19k colors with 32 sprites, soft scrolling and hardware commands like: fill,
    line, copy. Screen modes up to 5 need only 64kB vram, mode 6 and 7 need 128kB ram,
    because of faster bank interleave access.

![V9958-card photo](https://github.com/vossi1/cbm2-v9958-card/blob/master/pictures/card10-front.jpg)

**details:**

    I placed the V9958 at $D900 (not used disk rom i/o), so the write-ports are $D900-$D903.
    I had some trouble because the 6509 does a read access in cycle 5 of the STA(ZP),Y
    instruction before it writes. This increased sometimes the address pointer of the VDP.
    Thats why I splittet the read and write access ports. The read access ports are $D904-$D905.
    The programmable logic chip (GAL) does the splitting and outputs the CSR/CSW signals
    for the VDP. I also added a 4kB 2732 eprom in the not used disk-rom area $1000-$1FFF.
    The upper half of this eprom holds the two cbm2 font sets (2x 128 chars non reverse).
    The reverse characters will be created in the font-copy routine of my modified color-kernal.
    I converted the complete cbm2-font to 6x8 matrix because the VDP has only 512 pixels width.
    The lower half of the eprom is unused till now. I aslo have a nice intro for that ;)
    If you only need the fonts, you can use a 2716 or 2816.

![V9958-card back photo](https://github.com/vossi1/cbm2-v9958-card/blob/master/pictures/card10-back.jpg)

**assembling hints:**

    You can solder the rams directly to the board, but I prefer soj-sockets. These sockets
    are SMD ! I carefully cut out the bottom of the sockets and solder the sockets with a
    small 0,4mm tip from the inside.
    After that you should measure all socket connections from socket to socket. All used pins
    are parallel connected except lcas. After doublechecking insert the bootom-plates with a
    little bit glue in the middle of the sockets - so the rams have the right heigh!!!
    You should plug the drams and not pull them, if not nessesary, because the sockets are
    very sensitive!
    If you need to pull a dram, do it carefully with a PLCC-extractor to not damage the socket.

**comments:**

    For mode 6 and 7 it needs 128kB because it uses bank interleave access to get the speed!
    The card uses one dram for bank0 and one for bank 1.
    I tried everything to use only one chip - but without success. I tried to use the lower
    byte for cas0 and the upper byte for cas1.
    I also tried to use cas0/1 to switch an address line to use 128kB in one chip - no success.
    So I decided to use two common drams - easy to find in china or on old 1/2 MB pc-vga-cards.

My Prototypy:
![V9958-card prototype](https://github.com/vossi1/cbm2-v9958-card/blob/master/pictures/prototype.jpg)

**kernal:**

    My special cbm2-color-kernal is also available on github.
    With it you can use nearly all software with the color card.
    Its possible to switch text/background colors with an ESC-key sequence.
    Most cbm2-software uses the kernal routines to print, because of the memory management.
    But "Space Chase" does not work - it write directly into the screen memory!

**examples: helloworld.b / test14.b**

    My assembly-sources for the cbm2-series have the ".b" extension!
    Use the great ACME assembler to assemble the files. It is available for OSX, linux, windofs.
    Helloworld is small "one file" testcode for the card. It uses some 6502 and vdp macros.
    It has a 91 character-font (in hex bytes) and the c64-color-palette ;)
    Test14 is a bigger test - also in graphics mode 1.
    The 6502 macros, console subroutines and the cbm2-font are in external files.

BASIC:
![V9958-basic-demo](https://github.com/vossi1/cbm2-v9958-card/blob/master/pictures/basic-demo.jpg)

**basic cartridge / vdemo**

    The binary is for an 8KB eprom at $6000 on a cartridge module.
    It adds many great new statements to Basic128 or Basic256.
    All new graphics commands are shown in txt file.
    My extension only use mode 7 with 256x212 pixel, 256 fixed colors, 32 16x16 sprites!
    PAL/NTSC is selectable with the MODE command. CLEAR, LINE, FRAME, BLOCK, VCOPY statements
    and the scrolling subroutine use the very fast internal V9958 commands.
    The upper left corner is 0,0 because the sprites also use these orientation.
    RAM are in bank 15 $600-$7FF is used for buffering sprite colors for each of the 64 pattern!
    The V9958 stores the sprite colorlines for each sprite and not for each pattern - But my
    extension does that ;) If you select one of the 64 pattern for a sprite, SPRITE also copies
    the colorlines from my buffer to the colortable for this specific sprite.
    It's also possible to override the color with SCOLOR temporary - but only unicolor.
    You can convert 24 bit RGB BMP-pictures with up to 256x212 (only even x values) to 8bit RGB
    for the IMAGE command with my BMP2RGB8 tool here on git.
    The Basic program vdemo.prg shows how to use the new statements.
    In the tiny draw-tool vplot.prg yue also see use of the new TEST-function.

    If you find a bug or have a question? -> leave me a note...

**version 1.0 changes:**

- fixed two missing eprom traces
- added dedicated second dram for bank 1 on the bottom side
- replaced 7400 with a gal:
    split address-space for read / write access with A2
    eprom access only with READ
