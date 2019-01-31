defint h-i
SUB ColorPrint (text$, row%, col%, fc%, bc%) STATIC
SHARED igraph
'goto jump
IF igraph < 12 THEN
IF bc% < 8 THEN
COLOR bc% + 8
ELSE
COLOR bc%
END IF
LOCATE row%, col%: PRINT text$;
COLOR 15
EXIT SUB
END IF
jump:
'This creates the background
c$ = STRING$(LEN(text$), 219)'Defines a Big Block BackGround(really foreground)
reg 1, &H1300
reg 2, bc%
reg 3, LEN(text$)
reg 4, 256 * (row% - 1) + (col% - 1)
reg 9, strseg(c$)     'es
reg 7, strptr(c$)     'bp
call interrupt &h10
reg 1, &H1300
reg 2, (fc% XOR bc%) + &H80 'xor the previous color block with new characters
reg 3, LEN(text$)
reg 4, 256 * (row% - 1) + (col% - 1)
reg 9, strseg(text$)
reg 7, strptr(text$)
call interrupt &h10
END SUB

SUB lightgray (iwhatcol, ilevelr, ilevelg, ilevelb)
call setDACVal (GetDAC%(iwhatcol),ilevelr *4.04, ilevelg * 4.04, ilevelb * 4.04)
'PALETTE iwhatcol, 65536 * ilevelb + 256 * ilevelg + ilevelr
END SUB

SUB screencheck (igraph, lastline, lastrow, ivgashift, ifree)
ifree = 2 '= QB
SELECT CASE igraph
CASE 0: SCREEN 0: ivgashift = 0:
COLOR 1, 15
EXIT SUB
CASE 9
SCREEN igraph
  ivgashift = 0
  lastline = 25
  CASE ELSE
    igraph = 12
    SCREEN igraph
    lastline = 30
  END SELECT
  WINDOW (1, 1)-(640, 480)
  LINE (0, 0)-(640, 480), 15, BF
  lastrow = 80: ivgashift = lastline - 25
END SUB




