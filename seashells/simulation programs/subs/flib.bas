SUB apnp (ihpp, xal, yal, lp, iwidth, icol, ityploc)
  PSET (xal, yal), icol
END SUB


SUB backfront (inz, x0, x1, y4, y1, s0r, s1r, s0l, s1l, icol, iwil, px, p4, pyy)

IF y4 > s0l THEN 'line can start
  IF y1 > s1r THEN 'full
    CALL linep(ihp, x0, y4, x1, y1, 0, iwil, icol, 0)
  ELSE 'becomes invisible
    IF inz < 5 OR inz > 6 THEN
      p4 = px / (y4 - y1 + s1r - s0l)
      xs = (s1r - y1) * p4
      ys = y1 + xs * (y4 - y1) / px
    CALL linep(ihp, x0, y4, x1 + xs, ys, 0, iwil, icol, 0)
    ELSE
      xs = (s1r - y1) * p4
      ys = y1 + xs * pyy / px
      CALL linep(ihp, x0, y4, x1 + xs, ys, 0, iwil, icol, 0)
    END IF
  END IF
END IF
EXIT SUB
END SUB



SUB clearscreen
LINE (0, 0)-(1600, 1450), 15, BF 'erease frame
END SUB

SUB clsscr (ipcs, ipcx, ibackcol)
LINE (0, 0)-(1600, 1480), ibackcol, BF 'erease frame
' this is in DOS for frame records for animated gif's
IF ipcx >= 0 THEN EXIT SUB ' frame size should not be changed when record runs
IF ipcs > -2 THEN idxr = 1: idxl = 640: idyu = 479: idyo = 15 ' real windows is set later
END SUB


SUB cmessage (itype, iline, irow, ilettercol, iletterback, ibackc, text$, resp$)
''$INCLUDE: 'SHARED.bi'
' Prints a message on the screen,
' itype >3: centered
' itype = 2 or 4: a RETURN is required
' itype = 6 or 7: test is displayed for 2 or .3 sec.
' iline: The line on which the message appears, VGA-Corrected
' if iline <0 'counted from lower border
ilinea = iline: IF iline < 0 THEN ilinea = 26 + ivgashift + iline
'IF ilinea < ivgashift + 25 THEN linea = ivgashift + 25
IF itype > 2 THEN
iblank = 81: IF ilinea = 25 + ivgashift THEN iblank = 80
CALL ColorPrint(SPACE$(iblank - irow), ilinea, 1, ilettercol, iletterback)
END IF
tex$ = LEFT$(text$, 80 - irow)
IF itype > 3 THEN 'centered
   isp = LEN(tex$)
   IF isp + irow < 72 THEN
   isp = (79 - isp) / 2:
   tex$ = LEFT$(SPACE$(isp) + tex$, 80)
   END IF
   END IF
CALL ColorPrint(tex$, ilinea, irow, ilettercol, iletterback)
IF itype = 2 OR itype = 4 THEN
   IF resp$ > "" THEN
   CALL ColorPrint("[" + resp$ + "]", ilinea, 78 - LEN(resp$), 15, 13)
   END IF
   resp$ = ""
   DO UNTIL resp$ > "": resp$ = INKEY$: LOOP
   CALL ColorPrint(SPACE$(79), ilinea, 1, ibackcol, ibackcol)
   END IF
   IF itype = 6 OR itype = 7 THEN
      deltime = 2: IF itype = 7 THEN deltime = .3
      resp$ = ""
      t1 = TIMER
      DO UNTIL TIMER - t1 > deltime
      resp$ = INKEY$
      IF resp$ > "" THEN EXIT DO

      LOOP
      CALL ColorPrint(SPACE$(79), ilinea, 1, ibackcol, ibackcol)
      END IF
END SUB




SUB eingabe (igt, inz, iaf(), f(), label$(), text$, ianf, ianz)
'''SHARED ivgashift, displaywas$, igtdelay, ibackcol
'Input routine for mupltiple changes of the plot parameters
CALL cmessage(1, -1, 1, 15, 15, ibackcol, SPACE$(79), resp$)
twas$ = "sabcdefgh"
FOR i = ianf TO ianz
  ipu = 1: ipu = INSTR(ipu, displaywas$, MID$(twas$, i + 1, 1))
  IF ipu > 0 THEN
    textprint$ = "Substance  : " + text$ + "  OLD = "
  IF inz = 1 THEN textprint$ = textprint$ + STR$(iaf(i))
  IF inz = 2 THEN textprint$ = textprint$ + formata$(4,f(i))
  IF inz = 3 THEN textprint$ = textprint$ + label$(i)
  textprint$ = textprint$ + "  NEW = "
CALL cmessage(1, -1, 1, 2, 15, ibackcol, textprint$, resp$)
CALL cmessage(1, -1, 11, 4, 15, ibackcol, MID$(twas$, i + 1, 1), resp$)
ilength = LEN(textprint$) + 1
fac$ = ""
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_ ", resp$)

facinput:
SELECT CASE igt
CASE 0, -1 'No GUIDED TOUR active
IF inz = 3 THEN
LINE INPUT ; " "; fac$
ELSE
fac$ = ""
DO
dummy$ = INKEY$
REM
SELECT CASE dummy$
CASE ""
CASE "g", "G", "-", "+", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
fac$ = fac$ + dummy$
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
CASE CHR$(8)
lentext = LEN(fac$)
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
IF LEN(fac$) > 0 THEN fac$ = LEFT$(fac$, LEN(fac$) - 1)
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
CASE "x", CHR$(27):
CALL cmessage(1, -1, 1, 12, 15, ibackcol, SPACE$(79), "")
GOTO nexti
CASE "q": GOTO einende
CASE CHR$(13):
CALL cmessage(1, -1, 1, 12, 15, ibackcol, SPACE$(79), "")
EXIT DO
CASE ELSE: BEEP
END SELECT
LOOP
END IF

CASE 1 TO 1000: LINE INPUT #11, fac$
IF igtdelay > 1 THEN CALL gtreturn(5, igt)
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_    ", resp$)
IF igtdelay > 1 THEN CALL gtreturn(5, igt)
IF fac$ = "q" GOTO einende
IF fac$ = "x" GOTO nexti
END SELECT
    IF LEFT$(fac$, 1) = "g" OR LEFT$(fac$, 1) = "G" THEN
      fac$ = MID$(fac$, 2, 20)
      FOR il = i TO ianz
	IF inz = 1 THEN iaf(il) = VAL(fac$)'      Global change
	IF inz = 2 THEN f(il) = VAL(fac$)
	IF inz = 3 THEN label$(il) = fac$
      NEXT il
      GOTO einende
    END IF

IF inz = 1 THEN iaf(i) = VAL(fac$)
IF inz = 2 THEN f(i) = VAL(fac$)
IF inz = 3 THEN label$(i) = fac$
nexti:
  END IF
NEXT i
einende:
CALL cmessage(1, -1, 1, 15, 15, ibackcol, SPACE$(79), resp$)
color 1,15
END SUB

SUB gmanage (wasnun$) STATIC
''$INCLUDE: 'SHARED.bi'
'for changes of the graphic parameters
ianf = VAL(RIGHT$(wasnun$, 1))
IF ianf > 0 THEN
  lenwas = LEN(wasnun$)
  wasnun$ = MID$(wasnun$, 1, lenwas - 1)
END IF

SELECT CASE wasnun$
CASE "PER": CALL zeingabe(igt, 1, 2, i, per, resp$, "per")
CASE "KROT", "IMODUS": CALL zeingabe(igt, 1, 1, krot, d, resp$, "krot")
CASE "F": CALL eingabe(igt, 2, itypus(), gfa(), label$(), "Faktor", ianf, kn)
CASE "X": CALL eingabe(igt, 2, itypus(), gxa(), label$(), "xa", ianf, kn)
CASE "Y": CALL eingabe(igt, 2, itypus(), gya(), label$(), "ya", ianf, kn)

CASE "ITYP", "ITYPE": CALL eingabe(igt, 1, itypus(), gfa(), label$(), "ityp", ianf, kn)
CASE "ICOL": CALL eingabe(igt, 1, ico(), gfa(), label$(), "icol", ianf, kn)
CASE "IWI": CALL eingabe(igt, 1, iwi(), gfa(), label$(), "iwi", ianf, kn)
CASE "DXP":
 mess$ = "Shift in X-direction in multiple displays"
 CALL zeingabe(igt, 1, 2, i, dxp, resp$, mess$)
 mess$ = "Shift in y or y-expansion"
 CALL zeingabe(igt, 1, 2, i, dyp, resp$, mess$)
CASE "DYP":
 mess$ = "Shift in y or y-expansion"
 CALL zeingabe(igt, 1, 2, i, dyp, resp$, mess$)


CASE "<", ">"
factor = 4 / 3: IF wasnun$ = ">" THEN factor = 3 / 4' scaling for different screens

FOR il = 0 TO kn
gxa(il) = factor * gxa(il)
gya(il) = factor * gya(il)
gfa(il) = factor * gfa(il)
NEXT
dx = INT(factor * dx)

CASE "IGTDELAY"
	 mess$ = "Delay in the GUIDED TOURS, 10 = 1sec"
	 CALL zeingabe(igt, 1, 1, igtdelay, dyp, resp$, mess$)
CASE "ISTELZ", "IST":
 mess$ = "1=vertical A-B in 3D, Kringel in kd=28, use IH for winkel"
 CALL zeingabe(igt, 1, 1, istelz, dxp, resp$, mess$)

CASE "IH", "IHINTR":
 mess$ = "ihintr: 1=hidden frame, 2=last doddet, shift KD=28"
 CALL zeingabe(igt, 1, 1, ihintr, dyp, resp$, mess$)

CASE "PCS"
 ipcx = -1
 mess$ = "Filename of the PCX-file"
 CALL zeingabe(igt, 0, 3, istelz, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 ipcs = -2
 ELSE
 pcx$ = resp$ + "-": ipcs = 1
 igraph = 12: CALL screencheck
 END IF


CASE "PCOFF", "P0": ipcx = -1: ipcs = -2'frame out"
 CALL cmessage(5, -1, 1, 4, 15, ibackcol, " PCX off", dummy$)


CASE "PCXDRIVE", "DRIVE"
 mess$ = "drive for pcx and parameter [ " + dirtemp$ + " ];   c,d,e...."
 resp$ = dirtemp$
 CALL zeingabe(igt, 0, 3, ipcx, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 else
  resp$ = left$(resp$,1)
  IF resp$ > "b" AND resp$ < "z" THEN
  dirtemp$ = resp$ + ":\atemp\"
  ELSE
  BEEP
 END IF
end if


CASE "PCB" 'just a blanck screen, e.g. end of animated gif
   LINE (0, 0)-(640, 480), 15, BF
   CALL PCXOUT(ipcx, ipcs)

CASE "PCFULL" 'take the whole screen
  idyo = 479: idyu = 10: idxl = 3: idxra = 635

CASE "HP" ' this allow to write the picture in HPGL on a file
  IF ihpused < 2 THEN
    iwidthm = 0
    lpt$ = lptmem$
    resp$ = ""
    mess$ = "directory is " + dirtemp$ + "; filename, ohne .hgl; RETURN = skip]"
    CALL zeingabe(igt, 0, 3, ihp, dxp, resp$, mess$)
    IF resp$ = "" THEN EXIT SUB
    icomem = 0
    ihp = 9'####################
    resp$ = dirtemp$ + resp$ + ".hgl"
    OPEN resp$ FOR OUTPUT AS #77
    CALL cmessage(3, -1, 1, 4, 15, ibackcol, "         will be written to disk, file = " + resp$, dummy$)
    PRINT #77, CHR$(27); "%1B"
    lptmem$ = lpt$
   PRINT #77, "IN,IP10900,7650,0,0,SC -100,1039,0,800,sp1, PA"
      PRINT #77, "NP 8 PW .1,1 PW .2,2 PW .3,3 PW .4,4 "'line width plotter
      IF ihp = 9 THEN PRINT #77, "RO 180"
    ihpused = ihp
  ELSE
    ihp = ihpused 'for the next plot on the same page
  END IF
  wasnun$ = "W"

CASE "HPCLOSE", "HPC"
  ihp = 1
  CLOSE #77
  ihpused = 0

CASE "23", "VR"
  FOR il = 0 TO kn
    gxa(il) = gxa(il) + dxp
  NEXT
CASE "24", "VL"
  FOR il = 0 TO kn
    gxa(il) = gxa(il) - dxp
  NEXT
CASE "25", "VU"
  FOR il = 0 TO kn
    gya(il) = gya(il) - dyp
  NEXT
CASE "26", "VO"
  FOR il = 0 TO kn
    gya(il) = gya(il) + dyp
  NEXT

CASE "VH"
FOR il = 1 TO kn
FOR ix = lax(il) TO lx(il)
FOR iy = lay(il) TO ly(il) / 2
jyz = ly(il) + 1 - iy
zwischenwert = axy(il, ix, jyz)
axy(il, ix, jyz) = axy(il, ix, iy)
axy(il, ix, iy) = zwischenwert
NEXT iy: NEXT ix: NEXT il

CASE "VRL"
FOR il = 1 TO kn
FOR ix = lax(il) TO lx(il) / 2
FOR iy = lay(il) TO ly(il)
jxz = lx(il) + 1 - ix
zwischenwert = axy(il, jxz, iy)
axy(il, jxz, iy) = axy(il, ix, iy)
axy(il, ix, iy) = zwischenwert
NEXT iy: NEXT ix: NEXT il


CASE "VROT"
IF lx(1) > imy OR ly(1) > imx THEN
mess$ = "              Array to large for rotation, imx= " + STR$(imx) + "; imy= " + STR$(imy)
CALL cmessage(3, ii - i - 1, 1, 4, 15, ibackcol, mess$, "OK")
ELSE
FOR il = 1 TO ilm ' (axy(0) dient als Zwischenspeicher)
FOR ix = lax(il) TO lx(il)
FOR iy = lay(il) TO ly(il)
axy(0, iy, ix) = axy(il, ix, iy)
NEXT: NEXT
imem = lax(il): lax(il) = lay(il): lay(il) = imem
imem = lx(il): lx(il) = ly(il): ly(il) = imem
FOR ix = lax(il) TO lx(il)
FOR iy = lay(il) TO ly(il)
axy(il, ix, iy) = axy(0, ix, iy)
NEXT: NEXT
NEXT il
END IF

CASE ELSE: EXIT SUB
END SELECT
wasnun$ = ""
EXIT SUB
END SUB


SUB gtourcontrol (pcontrol$, igt, label$()) STATIC
'control of GUIDED TOURS

DO WHILE igt > 0
IF istop = 1 GOTO istopagain' used if calculation is continued
readnextgt:
ON EOF(11) GOTO ENDGT
LINE INPUT #11, PCONTROL$
SELECT CASE LEFT$(PCONTROL$, 1)
  CASE " "
    label$(0) = PCONTROL$
    CALL titles(1, title$, PCONTROL$)
    GOTO readnextgt
  CASE "%":  GOTO readnextgt
  CASE "<"
    idelay = VAL(MID$(PCONTROL$, 2, 7))
    CALL gtreturn(idelay * 10, igt)
    IF igt > 0 GOTO readnextgt
  CASE "+":
istopagain: ' used if calculation has been continued
    istop = 0
	IF igtdelay < 0 THEN
	CALL gtreturn(-igtdelay, igt)' delay if igtdelay is negative
	ELSE
	CALL gtreturn(100, igt)
	IF PCONTROL$ = "C" OR PCONTROL$ = "S" OR PCONTROL$ = "N" THEN
	istop = 1: EXIT SUB
	END IF
	END IF
    IF igt > 0 GOTO readnextgt
END SELECT
SELECT CASE PCONTROL$
    CASE ""
    PCONTROL$ = "": EXIT SUB
    CASE "-", "_": label$(0) = "": title$ = ""

    CASE "#":
	  LOCATE 25 + ivgashift - 5, 1
	  FOR i = 1 TO 4: LINE INPUT #11, label$(i)
	  ON EOF(11) GOTO ENDGT
	  IF label$(i) = "" THEN EXIT FOR
	  IF label$(i) = "." THEN
	  label$(i) = ""
	  END IF
	  NEXT i
	  IF igtdelay > 0 THEN
	  FOR ii = 1 TO i - 1
	  CALL cmessage(3, ii - i - 1, 1, 4, 15, ibackcol, " " + label$(ii), "")
	  NEXT
	  CALL cmessage(3, ii - i - 1, 1, 4, 15, ibackcol, SPACE$(78), "")
	  CALL gtreturn(100, igt)
	  END IF
	  label$(0) = ""
	IF PCONTROL$ = "C" OR PCONTROL$ = "S" OR PCONTROL$ = "N" THEN
	istop = 1: EXIT SUB
	END IF

    CASE "&":
	  INPUT #11, ierease, izeile, idelay, inewscreen
	  ON EOF(11) GOTO ENDGT
	  IF ierease = 1 THEN
	  CALL screencheck
	  CALL init(1, igraph, 0, 15, ivgashift)
	  LINE (1, 1)-(640, 480), 15, BF
	  END IF
	  FOR i = 1 TO 4: LINE INPUT #11, label$(i)
	  ON EOF(11) GOTO ENDGT
	  IF label$(i) = "" THEN EXIT FOR
	  IF label$(i) = "." THEN label$(i) = ""
	  NEXT i
	  FOR ii = 1 TO i - 1
	  CALL cmessage(5, ii - i - 1, 1, 4, 15, ibackcol, " " + label$(ii), "")
	  NEXT ii
	  label$(0) = ""
	  CALL gtreturn(idelay * 10, igt)
    CASE "EOF-GT"
ENDGT:
	 igt = 0: CLOSE (11): label$(0) = "": PCONTROL$ = "": fdelay = 0
	  mess$ = "-------   End of the GUIDED TOUR  ------"
	  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
    CASE ELSE:
	IF igtdelay > 5 AND igtdelay < 80 THEN
	CALL cmessage(1, -1, 1, 15, 1, ibackcol, PCONTROL$ + "      ", "")
	CALL gtreturn(5, igt) 'short delay
	END IF
    EXIT SUB
END SELECT
LOOP
END SUB


SUB gtreturn (inz, igt)
'makes delays or asks for <RETURN> in GUIDED TOURS

SELECT CASE inz
CASE 0
CASE 1 TO 50  'delay in .1 seconds
  t1 = TIMER
  DO UNTIL TIMER - t1 > .1 * inz
  resp$ = INKEY$
  IF resp$ = "q" THEN GOTO terminateTour
  IF resp$ > "" THEN EXIT DO
  LOOP
CASE 51 TO 1000 'requires RETURN
  PCONTROL$ = MID$(PCONTROL$, 3, 80)
  IF PCONTROL$ > "" THEN
  PCONTROL$ = MID$(PCONTROL$ + SPACE$(77), 1, 80)
  CALL ColorPrint(PCONTROL$, 25 + ivgashift, 1, ibackcol, ibackcol)
  END IF
   CALL ColorPrint("[ok]", 25 + ivgashift, 76, 15, 13)
   resp$ = "":   DO UNTIL resp$ > "": resp$ = INKEY$: LOOP
   CALL ColorPrint(SPACE$(79), 25 + ivgashift, 1, 15, 15)

IF igt > 0 THEN
SELECT CASE resp$
CASE "c", "C": PCONTROL$ = "C"
CASE "s", "S": PCONTROL$ = "S"
CASE "n", "N": PCONTROL$ = "N"
'CASE "i": igtdelay = 0
CASE "z", "Z": PCONTROL$ = "Z"
CASE "q"
GOTO terminateTour
CASE "-", "_"
  mess$ = " GUIDED TOUR suspended, back with command GT"
  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
  igt = -1: PCONTROL$ = ""
CASE ELSE: PCONTROL$ = ""
END SELECT
END IF
END SELECT
EXIT SUB

terminateTour:
  mess$ = "GUIDED TOUR terminated"
  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
  CLOSE (11)
  igt = 0: fdelay = 0
'RETURN

END SUB

'DEFBL A-G, O-Z
SUB init (inewscreen, igraph, ivorcol, ibackcol, ivgashift) STATIC
CLS
END SUB

'DEFBL A-G, O-Z
SUB leftright (x1, x2, x3, y1, y2, y3, s1r, s2r, s3r, s1l, s2l, s3l, icol, iwil, px, p4)
IF y1 > s1l THEN 'line starts, point 1 (or 4) is visible
  IF y3 > s3r THEN 'point 3 (or 6) is visible
    CALL linep(ihp, x1, y1, x3, y3, 0, iwil, icol, 0)
  ELSE '3 not visible
    IF y2 > s2r THEN 'point 2 (or 5) is visible
      CALL linep(ihp, x1, y1, x2, y2, 0, iwil, icol, 0)
    END IF
  END IF

ELSE 'point 1 is not visible,
  IF y2 > s2r THEN 'point 2 (or 5) is visible
    IF y2 > s2l THEN
      xs = (s1l - y1) * p4
      ys = y1 + xs * py / px
    ELSE
      xs = px: ys = y2
    END IF
    IF y3 > s3r THEN 'point 3 (or 6) is visible
      CALL linep(ihp, x1 - xs, ys, x3, y3, 0, iwil, icol, 0)
    ELSE
      CALL linep(ihp, x1 - xs, ys, x2, y2, 0, iwil, icol,  0)
    END IF
  ELSE 'point 2 was not visible
    IF y3 > s3r THEN 'point 3 (or 6) is visible
      IF y2 > s2l THEN
	CALL linep(ihp, x2, y2, x3, y3, 0, iwil, icol,  0)
      ELSE
	xs = (s2l - y2) * p4
	ys = y2 + xs * py / px
	CALL linep(ihp, x2 - xs, ys, x3, y3, 0, iwil, icol,  0)
      END IF
    END IF
  END IF
END IF
EXIT SUB
END SUB

'DEFBL A-G, O-Z
SUB linep (ihp, x1, y1, x2, y2, ipat, iwidth, icol,  ityp) STATIC
' allows to draw line with different width and writing in HPGL to disk
IF ipcs > -2 THEN
	IF x1 < idxl THEN idxl = x1
	IF x2 < idxl THEN idxl = x2
	IF x1 > idxr THEN idxr = x1
	IF x2 > idxr THEN idxr = x2
	IF y1 < idyu THEN idyu = y1
	IF y2 < idyu THEN idyu = y2
	IF y1 > idyo THEN idyo = y1
	IF y2 > idyo THEN idyo = y2
END IF
SELECT CASE ipat
CASE 0' line
  SELECT CASE ityp'linetype
  CASE 2: LINE (x1, y1)-(x2, y2), icol,  , &H4040
  CASE 3: LINE (x1, y1)-(x2, y2), icol,  , &HF0F0
  CASE 4, 5:  'LINE (x1, y1)-(x2, y2), icol,  , &H8888
'  CASE 5: 'dottet line
    xhypoten = SQR((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
    xdl = 6
    xr = xdl * (x2 - x1) / (xhypoten + .00001)
    yr = xdl * (y2 - y1) / (xhypoten + .00001)
    xr5 = xr / 4
    yr5 = yr / 4
    idanz = xhypoten / xdl
    xl = x1: yl = y1
    FOR i = 1 TO idanz
      PSET (xl, yl), icol
      xl = xl + xr: yl = yl + yr
    NEXT i
    PSET (x2, y2), icol
  CASE ELSE   'normal line
SELECT CASE iwidth
CASE 0, 1
    LINE (x1, y1)-(x2, y2), icol
CASE 2, 3
xiwi = iwidth - 1
IF ABS(y2 - y1) < ABS(x2 - x1) THEN  '< 45ø
IF ABS(x2 - x1) < 1 THEN EXIT SUB
yl = y1
astep = 1
IF x2 < x1 THEN astep = -1
delta = (y2 - y1) / ABS(x2 - x1)
FOR xl = x1 TO x2 + xiwi / 2 STEP astep
LINE (xl, yl)-(xl+astep, yl + xiwi), icol, bf
yl = yl + delta
NEXT
ELSE   '> 45ø
astep = 1
xl = x1
IF y2 < y1 THEN astep = -1
IF ABS(y2 - y1) < 1 THEN EXIT SUB
delta = (x2 - x1) / ABS(y2 - y1)
FOR yl = y1 TO y2 STEP astep
LINE (xl, yl)-(xl + xiwi, yl + 1), icol, bf
xl = xl + delta
NEXT
END IF
CASE 4 TO 10
     wi = ATN((x2 - x1) / (y2 - y1 + .00000012#))
 pix = .5 * COS(wi): piy = -.5 * SIN(wi)
 pimax = iwidth * pix
 pimay = iwidth * piy
 xa1 = x1 - pimax: xa2 = x2 - pimax: ya1 = y1 - pimay: ya2 = y2 - pimay


    xb1 = x1 + pimax: xb2 = x2 + pimax: yb1 = y1 + pimay: yb2 = y2 + pimay
    LINE (xa1, ya1)-(xa2, ya2), icol,
    LINE (xa2, ya2)-(xb2, yb2), icol
    LINE (xb2, yb2)-(xb1, yb1), icol
    LINE (xb1, yb1)-(xa1, ya1), icol
    PAINT (x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2), icol
 END SELECT 'iwidth
 END SELECT 'ityp

CASE 1
  SELECT CASE ityp'Rechteck
  CASE 1: LINE (x1, y1)-(x2, y2), icol,  B
  CASE 2: LINE (x1, y1)-(x2, y2), icol,  B, &H4040
  CASE 3: LINE (x1, y1)-(x2, y2), icol,  B, &HF0F0
  CASE 4: LINE (x1, y1)-(x2, y2), icol,  B, &H8888
  END SELECT

CASE 2'   rectangle filled
  LINE (x1, y1)-(x2, y2), icol,  BF
    '  IF n < krot THEN n = 0
CASE 3
  ddy = y2 - y1
  idx = x2 - x1
  idy = ddy
  idx1 = idy - 1
  idx2 = idy - 2
  idx3 = idy - 3
  x21 = x2 - 1
  SELECT CASE ityp
      CASE 10 TO 32000
	LINE (x1, y1)-(x21, y2), icol,  BF
      CASE 7 TO 9
	FOR iyd = y1 TO y1 + idx1 STEP 1
	  LINE (x1, iyd)-(x21, iyd), icol,  , &H5555
	  iyd = iyd + 1
	  LINE (x1, iyd)-(x21, iyd), icol,  , &HAAAA
	NEXT iyd
      CASE 4 TO 6
	FOR iyd = y1 TO y1 + idx2 STEP 2
	  LINE (x1, iyd)-(x21, iyd), icol,  , &H5555
	  iyd = iyd + 2
	  LINE (x1, iyd)-(x21, iyd), icol,  , &HAAAA
	NEXT iyd
      CASE 1 TO 3
	FOR iyd = y1 TO y1 + idx2 STEP 2
	  LINE (x1, iyd)-(x21, iyd), icol,  , &H1111
	  iyd = iyd + 2
	  LINE (x1, iyd)-(x21, iyd), icol,  , &H4444
	NEXT iyd
      END SELECT 'shading

END SELECT'ipat

' for writing HPGL files:
IF ihp = 0 OR ihp = 1 THEN EXIT SUB
  IF iwidth = 0 THEN iwidth = 1
  IF iwidth <> iwidthm THEN
  penwidth = .1 * iwidth
  PRINT #77, "PW"; penwidth
  iwidthm = iwidth
  END IF

 IF icol <> icomem THEN
' icohp = ico: GOTO weiter
    SELECT CASE icol
    CASE 0, 8: icohp = 1 'scharz
    CASE 1, 9: icohp = 5'blau
    CASE 2, 10: icohp = 3'grn
    CASE 12, 4, 6, 13: icohp = 2'rot
    CASE 3, 11: icohp = 7
    CASE 14: icohp = 4'gelb
    CASE ELSE
    icohp = 1
    END SELECT
    PRINT #77, "SP"; icohp
    icomem = icol
END IF

SELECT CASE ipat
CASE 0  'line
SELECT CASE ityp
CASE 4, 5
    idanz = xhypoten / xdl
    xl = x1: yl = y1
    FOR i = 1 TO idanz
  PRINT #77, "PAPU "; : PRINT #77, USING "####.#"; xl; : PRINT #77, ", ";
  PRINT #77, USING "####.#"; yl; : PRINT #77, "PD "; : PRINT #77, USING "####.#"; xl + xr5;
  PRINT #77, ","; : PRINT #77, USING "####.#"; yl + yr5
      xl = xl + xr: yl = yl + yr
    NEXT i
CASE 1 TO 3
    ilaa = ityp
    iilp = 1: PRINT #77, "LT", ilaa, filp
  PRINT #77, "PAPU "; : PRINT #77, USING "####.#"; x1; : PRINT #77, ", "; : PRINT #77, USING "####.#"; y1; : PRINT #77, "PD "; :
  PRINT #77, USING "####.#"; x2; : PRINT #77, ","; : PRINT #77, USING "####.#"; y2
 PRINT #77, "LT " 'sets line type back
CASE ELSE
  PRINT #77, "PAPU "; : PRINT #77, USING "####.#"; x1; : PRINT #77, ", "; : PRINT #77, USING "####.#"; y1; :
  PRINT #77, "PD "; : PRINT #77, USING "####.#"; x2; : PRINT #77, ",";
  PRINT #77, USING "####.#"; y2
END SELECT 'itype
CASE 1  'open rectancle
  PRINT #77, "PAPU"; x1; ","; y1; "PD";
  WRITE #77, x2, y1, x2, y2, x1, y2, x1, y1
CASE 2 'closed rectangle
  PRINT #77, "PUPA"; x1; ","; y1; "RA"; x2; ","; y2
CASE 3
  PRINT #77, "PUPA"; x1; ","; y1; "PM0 PU"
  WRITE #77, x2, y1, x2, y2, x1, y2, x1, y1
  PRINT #77, "PM 2"
 IF ityp > 9 THEN
 PRINT #77, "FT 1"
 ELSE
 PRINT #77, "FT 10,", 10 * ityp
 END IF
  PRINT #77, "FP"
END SELECT
END SUB



SUB paramenu STATIC'=======================================================-
'Allows to change floating point parameters (for calculation)
'               or of integer parameters (for program flow)
'  or inputting a command. Control variable:   PCONTROL$
'  e.g. name of the parameter to be changed or command
'--------------------------------------------------------------------------
'   inv(): temporary storage of integer values KT, KP .. KG, K1..
'   flv():        ..    floating point values DA, RA ..
'   parnam$(): contains the names of the variables for identification
Dim s$(250), sl$(60)
DIM printeq$(50)
  a$ = "KT,KP,KX,KY,KD,KI,KE,KR,KN,KG,K1,K2,K3,K4,K5,K6,K8,K8,K9,K0,"
  FOR i = 1 TO 20: iparnam$(i) = MID$(a$, (i - 1) * 3 + 1, 2): NEXT i
  a$ = "DRBSCAGQTU"     ' generating strings for parameter names
  FOR il = 1 TO 10
  FOR i = 1 TO 10 '
  fparnam$(il, i) = MID$(a$, i, 1) + CHR$(64 + il)
  NEXT i: NEXT il
  fparnam$(0, 1) = "DX": fparnam$(0, 2) = "DY": fparnam$(0, 3) = "DZ"
resp$ = ""
ifound = 0

IF kn = 0 THEN 'initialization after program start
 iparam = 1:  t1 = TIMER
   irfilenr$ = programname$ + ptype$ + "1" + ".prm"
   filenametmp$ = apardir$ + programname$ + ptype$ + "1" + ".prm"
   progrtmp$ = programname$
color 1,15
GOTO readfilenow
END IF

totaltime = timer - t1
parametermenu:
' show most used commands
IF iparam < 1 THEN LINE (0, 0)-(1600, 1450), 15, BF: CALL prtscreen("iniscr.txt")

IF iparam <= 1 THEN CALL rwparameterdisk("PS", 8)
showparameterlist: '--------- formatted print of parameters on screen
parameinput:
pdirectinput = -1 'to find and input of the type "KT = 1"
IF igt > 0 THEN
CALL gtourcontrol(PCONTROL$, igt, label$())  'if GUIDED TOUR is on
ELSE
    IF PCOMMAND$ > "" THEN
     PCONTROL$ = PCOMMAND$
     PCOMMAND$ = ""
    ELSE
     PCONTROL$ = ""
     CALL zeingabe(igt, 0, 3, i, dummy, PCONTROL$, "->")
  END IF
END IF
usepcontrol:
PCONTROL$ = UCASE$(PCONTROL$)'------------------ all in uppercase letters
pcleft$ = "": t1 = TIMER
INS = 0: ipcontrol = 0: pcontrol2$ = ""
ipu = INSTR(PCONTROL$, "=")
IF ipu > 0 THEN  'e.g., KT=12 is also a legal input, used for menu
pdirectinput = VAL(MID$(PCONTROL$, ipu + 1, 12))
PCONTROL$ = MID$(PCONTROL$, 1, ipu - 1)
END IF

IF PCONTROL$ = "" THEN
iparam = iparam - 1: GOTO parametermenu

END IF

'intparam ?
FOR i = 1 TO 20'----- Find the parameter name in parnam$(INS)
  IF PCONTROL$ = iparnam$(i) THEN
  IF pdirectinput = -1 THEN
    CALL zeingabe(igt, 1, 1, inv(i), dummy, dummy$, iparnam$(i))
    ELSE
    inv(i) = pdirectinput
  END IF
  modified$ = ", modified             ": comment$ = ""
  IF i = 9 THEN iparam = 1: 'number of substances has been changed

  GOTO setparameter
  END IF
NEXT i

'floatparam?
  FOR il = 0 TO 10
  FOR i = 1 TO 10 '
  IF PCONTROL$ = fparnam$(il, i) THEN
   IF pdirectinput = -1 THEN
    CALL zeingabe(igt, 1, 2, idummy, flv(il, i), dummy$, fparnam$(il, i))

    ELSE
    flv(il, i) = pdirectinput
    END IF
    IF LEFT$(PCONTROL$, 1) = "D" AND flv(il, i) > diffmax AND il > 0 THEN
       mess$ = "to avoid numerical instability, diffusion must be < " + STR$(diffmax) + ", corrected"
       CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
       flv(il, i) = diffmax
      END IF
    modified$ = ", modified             ": comment$ = ""
  GOTO setparameter
  END IF
  NEXT i: NEXT il


SELECT CASE PCONTROL$
CASE ""'--- input "RETURN" for full menu
  iparam = iparam - 1: GOTO parametermenu



CASE "NT": ipcx = 0: ipcs = -1: igraph = 12
CALL screencheck''(igraph, lastline, lastrow, ivgashift, ifree)
idxr = 1: idxl = 640: idyu = 479: idyo = 15
PCONTROL$ = "N": GOTO leave

CASE "NC"
 igraph = 12: mess$ = "Filename of the PCX-file"
 CALL zeingabe(igt, 0, 3, istelz, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 ELSE
 pcx$ = resp$ + "-": ipcx = 1: ipcs = -2
 END IF
PCONTROL$ = "N": GOTO leave

CASE "P"
      CALL PCXOUT(ipcx, ipcs)

CASE "S", "SM", "C", "N", "I", "II", "M", "CS", "D", "DP", "A"
t1 = TIMER
  GOTO leave' fixes parameters, jump into the main program
CASE "", " " 'ignore
CASE "QQ": END
CASE "Q"'That is the END
     resp$ = "Y / N": mess$ = " You like to terminate the program?"
     CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, resp$)
     IF resp$ = "y" OR resp$ = "Y" OR resp$ = "j" THEN CLS : END
CASE "PT", "PTYPE"
  CALL zeingabe(igt, 1, 3, i, D, ptype$, "ptype")
  ptype$ = LCASE$(ptype$)
CASE "DW"
  CALL zeingabe(igt, 1, 3, i, D, displaywas$, "display what (sab...) ")
    displaywas$ = LCASE$(displaywas$)
    GOTO setparameter

CASE "FDELAY", "DELAY"
	 mess$ = "Delay in the between the displays, 0.1 is reasonable"
	 CALL zeingabe(igt, 1, 2, i, fdelay, resp$, mess$)
         if fdelay = 0 then
           idosgraph = 12          ' there will be no delay after saving/reading
         else                    ' this parameter set again
           idosgraph = 9 ' there will be A delay after saving/reading
         end if

CASE "IGTDELAY"
    CALL zeingabe(igt, 1, 1, igtdelay, D, dummy$, "Delay in the GUIDED TOUR; in .1 seconds")

CASE "M4", "M5"
	SELECT CASE programname$
	CASE "sp", "xt", "SP", "XT"
	    IF PCONTROL$ = "M4" THEN ipcontrol = 104
	    IF PCONTROL$ = "M5" THEN ipcontrol = 105
	  PCONTROL$ = "D"
	  EXIT SUB

	CASE ELSE
	    displaywastmp$ = ""
	    IF PCONTROL$ = "M4" THEN ipcontrol = 24
	    IF PCONTROL$ = "M5" THEN ipcontrol = 25
	    PCONTROL$ = "D"
	    GOTO leave
	END SELECT


CASE "T": CALL cmessage(6, -1, 1, 15, 4, ibackcol, "Execution time was " + STR$(totaltime), "")

CASE "VGA"
  igraph = 12: CALL screencheck''(igraph, lastline, lastrow, ivgashift, ifree)
CASE "EGA", "ID"
  igraph = 9: idosgraph = 9: fdelay = .08: CALL screencheck ''(igraph, lastline, lastrow, ivgashift, ifree)
CASE "ID" : idosgraph = 9: fdelay = .08

CASE "1", "2", "3", "4", "5", "6", "7", "8", "9", "12"
igraph = VAL(PCONTROL$):  CALL screencheck'  1 = lowerst resolution (VGA)

CASE "SCREEN", "SCR"
IF pdirectinput > 0 THEN
igraph = pdirectinput:
ELSE
mess$ = "Screen, 1:VGA, 2:XGA, 3:SXGA; 4:1280*1024"
IF ifree = 3 THEN mess$ = "Screen: [return]: = VGA; 9: = EGA"
  CALL zeingabe(igt, 1, 1, igraph, d, dummy$, mess$)
END IF
  pdirectinput = 0: IF igraph = 0 THEN igraph = 12
  CALL screencheck

CASE "PE", "PEF" 'PrintEquation, on screen or file
  such$ = "SELECT CASE KE'EQUATION"
  GOTO FindEquation
equationfound:
  IF i > 1 THEN
   CLS : LOCATE 1, 1
   FOR k = 1 TO i: PRINT printeq$(k): NEXT k
    IF i > 21 + ivgashift - kn THEN
     CALL cmessage(4, -1, 1, 15, 1, ibackcol, " ", "RETURN")
    END IF
  END IF
  iparam = 1
IF PCONTROL$ = "PEF" OR PCONTROL$ = "PEP" THEN
  CALL zeingabe(igt, 0, 3, i, D, resp$, "filename for saving equation and parameter " + dirtemp$)
  IF resp$ = "" OR resp$ = CHR$(27) GOTO parametermenu
  icheckerror = 0
  ON ERROR GOTO checkerror
  OPEN dirtemp$ + resp$ + ".TXT" FOR OUTPUT AS #8
	  IF icheckerror = 1 THEN
	    CLOSE (8):
	    mess$ = " could not write on " + dirtemp$ + resp$ + ".TXT"
	    resp$ = "ok": CALL cmessage(2, -1, 1, 15, 4, ibackcol, mess$, resp$)
	   GOTO parametermenu
	  END IF
  CALL rwparameterdisk("PS", 8)
   PRINT #8, : PRINT #8,
   FOR k = 1 TO i: PRINT #8, printeq$(k): NEXT k
     PRINT #8,
     CALL rwparameterdisk("PE", 8)
     PRINT #8, :  CLOSE (8)
     iparam = 1: GOTO parametermenu
END IF

CASE "GT"
  IF igt < 0 THEN     ' a suspended GUIDED TOUR will be continued
  igt = 1
  ELSE
gtlist:
  CALL whatprmfiles(s$(), programname$, programname$, "GT", pdir$, irfilenr$, ilineparam)
  iparam = 0
  IF irfilenr$ = "q" OR irfilenr$ = "p" OR irfilenr$ = "" GOTO parametermenu
  gtfilename$ = pdir$ + irfilenr$ + ".GT"
readgtnow:
  ON ERROR GOTO checkerror
  icheckerror = 0
  OPEN gtfilename$ FOR INPUT AS #11
  IF icheckerror = 1 THEN
    mess$ = " the file " + gtfilename$ + " does not exist. You can get a list"
    resp$ = "y/n": CALL cmessage(2, -1, 1, 15, 4, ibackcol, mess$, resp$)
    IF resp$ = "y" GOTO gtlist
    GOTO parametermenu
  END IF
  LINE INPUT #11, resp$
  LINE (0, 0)-(1640, 1480), ibackcol, BF 'erease frame
    igt = 1
END IF
iparam = 2

CASE "PDIR"
  CALL zeingabe(igt, 1, 3, i, D, pdir$, "Directory of parameter files ")
  IF RIGHT$(pdir$, 1) <> "\" THEN pdir$ = pdir$ + "\"

CASE "CLS":  iparam = 2:
  LINE (1, 1)-(1640, 1480), ibackcol, BF


CASE "MENU"
  PCONTROL$ = ""
   CALL whatprmfiles(sl$(), programname$, "*", "hlm", aprodir$ + "help/", irfilenr$, ilineparamL)
  IF irfilenr$ = "q" THEN GOTO parametermenu
  OPEN aprodir$ + "help/" + irfilenr$ + ".hlm" FOR INPUT AS #2
 LINE INPUT #2, mess$: i = 1
  DO UNTIL EOF(2)
    LINE INPUT #2, sl$(i)
    IF sl$(i) = "" THEN EXIT DO
    i = i + 1
  LOOP
  CLOSE #2
  sl$(i) = ""
  CALL smenu(sl$(), PCONTROL$, iline, mess$)
  IF PCONTROL$ = "q" THEN PCONTROL$ = ""
  IF RIGHT$(PCONTROL$, 1) = "." THEN
	CALL zeingabe(igt, 0, 3, i, dummy, resp$, " substance to which the change should apply, a,b,...    ")
    CLS
    iparam = 0
    PCONTROL$ = LEFT$(PCONTROL$, 1) + resp$
  END IF
  LINE (0, 0)-(1600, 1450), 15, BF 'erease frame
  iparam = 1
  GOTO usepcontrol

CASE ELSE
	pcleft$ = LEFT$(PCONTROL$, 1)'number behind display can be used
	pcontrol2$ = MID$(PCONTROL$, 2, 3)
	ipcontrol = VAL(pcontrol2$)

SELECT CASE pcleft$

CASE "D", "A"
 idxr = 1: idxl = 640: idyu = 479: idyo = 15
  IF ipcontrol > 0 THEN
    PCONTROL$ = pcleft$
    GOTO leave
  END IF
CASE "L"
    displaywastmp$ = "": PCONTROL$ = "D"
    GOTO leave
CASE "M"
  PCONTROL$ = pcleft$
  GOTO leave

CASE "R", "S", "J"
  SELECT CASE PCONTROL$
    CASE "RKILL": KILL filenametmp$: GOTO parametermenu
    CASE "RR"': GOTO readfilenow
    CASE "R"      'F2: list of parameter
          resp$="q"
          CALL zeingabe(igt, 0, 3, i, dummy, resp$, "Chapter Nr. [" + ptype$ + "] or RETURN for all; q or ESC => skip")
          SELECT CASE resp$
	  CASE "q", "Q" : GOTO parameinput
	  CASE " ": progrtmp$ = programname$
	  CASE "": progrtmp$ = programname$ + ptype$
	  CASE ELSE: progrtmp$ = programname$ + resp$
	  END SELECT
whatprmfileslabel:
       CALL whatprmfiles(s$(), programname$, progrtmp$, "prm", apardir$, irfilenr$, ilineparam)
	  IF irfilenr$ = "q" THEN iparam = 1: GOTO parametermenu
	     filenametmp$ = apardir$ + irfilenr$ + ".prm"


CASE "RN", "SN"
	ilineparam = ilineparam + 1
	IF s$(ilineparam) = "" GOTO whatprmfileslabel
	ipu = INSTR(s$(ilineparam), " ")
	   IF ipu > 0 THEN
	     irfilenr$ = MID$(s$(ilineparam), 1, ipu - 1)
	     filenametmp$ = apardir$ + irfilenr$ + ".prm"
'             GOTO readfilenow
	   ELSE
	     GOTO whatprmfileslabel
	   END IF
    CASE ELSE        'FOR CASES LIKE r1
     irfilenr$ = LCASE$(MID$(PCONTROL$, 2, 6))
     progrtmp$ = programname$
      IF irfilenr$ > "" THEN
       IF LEFT$(irfilenr$, 1) >= "0" AND LEFT$(irfilenr$, 1) <= "9" THEN irfilenr$ = ptype$ + irfilenr$
       irfilenr$ = progrtmp$ + irfilenr$': GOTO readfilenow
      END IF
   filenametmp$ = apardir$ + irfilenr$ + ".prm"
 END SELECT
readfilenow: 'xxrr
'ptype$ = MID$(irfilenr$, 3, 2)
  icheckerror = 0
  ON ERROR GOTO checkerror 'xxn

OPEN lcase$(filenametmp$) FOR INPUT AS #9
  IF icheckerror = 1 THEN
  mess$ = "The file " + irfilenr$ + " does not exist. You like a list? "
  resp$ = "y/n "
  CALL cmessage(4, -1, 1, 4, 15, ibackcol, mess$, resp$)
    IF resp$ = "y" THEN GOTO whatprmfileslabel
    filenametmp$ = filename$
    iparam = 1
   GOTO parametermenu
  END IF
  filename$ = filenametmp$
  IF LEFT$(ptype$, 1) < "A" THEN ptype$ = ""
CALL rwparameterdisk("R", 9)
  CLOSE (9)
  comment$ = "read from file "
  modified$ = "":
  ON ERROR GOTO 0
  IF igt <> 1 THEN iparam = iparam - 1
  GOTO setparameter

CASE "W" '
  SELECT CASE PCONTROL$
    CASE "WW": GOTO writefilenow
    CASE ELSE        'FOR CASES LIKE W1
      irfilenr$ = MID$(PCONTROL$, 2, 6)
      progrtmp$ = programname$
      irfilenr$ = progrtmp$ + irfilenr$
      irfilenr$ = LCASE$(irfilenr$)
      filenametmp$ = Lcase$(apardir$ + irfilenr$ + ".prm")
      IF PCONTROL$ = "W0" THEN GOTO writefilenow
    icheckerror = 0
    ON ERROR GOTO checkerror
    OPEN filenametmp$ FOR INPUT AS #9
    IF icheckerror = 0 THEN
    dummy$ = "": LINE INPUT #9, dummy$
    CLOSE (9)
    mess$ = "The file " + irfilenr$ + " exists already [ " + LEFT$(dummy$, 20) + "...]     "
    resp$ = "Overwrite y/n ?"
    CALL cmessage(4, -1, 1, 4, 15, ibackcol, mess$, resp$)
    IF resp$ <> "y" THEN
      mess$ = "You will get a list of existing files, later input a new file name"
      resp$ = "OK or ESC": CALL cmessage(4, -1, 1, 4, 15, ibackcol, mess$, resp$)
      IF resp$ = CHR$(27) GOTO parametermenu
       CALL whatprmfiles(s$(), programname$, progrtmp$, "prm", apardir$, irfilenr$, ilineparam)
	iparam = 1
	      mess$ = "Use another file name by using W...."
	      CALL cmessage(5, -1, 1, 4, 15, ibackcol, mess$, resp$)
	GOTO parametermenu
      ELSE
      CLOSE (9)'overwrite
      GOTO writefilenow
     END IF
     END IF
  END SELECT
writefilenow:
mess$ = "new title, auf " + pdir$
IF LEN(filemessage$) > 40 THEN
  mess$ = mess$ + " [" + MID$(filemessage$, 1, 40) + "...." + "]"
  ELSE
  mess$ = mess$ + " [" + filemessage$ + "]"
  END IF
  CALL cmessage(3, -2, 1, 15, 2, ibackcol, mess$, "")
  CALL zeingabe(igt, 0, 4, i, dummy, resp$, "")
  CALL cmessage(3, -2, 1, 0, 0, ibackcol, SPACE$(79), "")
  IF resp$ = CHR$(27) OR resp$ = "q" GOTO parametermenu
  IF resp$ > "" THEN filemessage$ = resp$
  filenametmp$=lCASE$(filenametmp$)
  filename$ = filenametmp$
  OPEN filenametmp$ FOR OUTPUT AS #9

CALL rwparameterdisk("W", 9)
  ON ERROR GOTO 0
  CLOSE (9)
  comment$ = "written to file "
  modified$ = ""
  iparam = 0
  GOTO parametermenu

CASE "G" ' a guided tour is to be opened, e.g., command "GTgm1" cause
  IF igt < 0 THEN     ' a suspended GUIDED TOUR will be continued
    igt = 1
    mess$ = " The interrupted tour will be continued, "
    resp$ = "OK": CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, resp$)
    GOTO parametermenu
   END IF

gtfilename$ = MID$(PCONTROL$, 3, 8)     ' opening of gm1.gt
IF gtfilename$ > "" THEN
  gtfilename$ = pdir$ + programname$ + gtfilename$ + ".GT"
  GOTO readgtnow
  END IF
END SELECT

checkpcontrol:
CALL gmanage(PCONTROL$)
IF PCONTROL$ = "" GOTO parametermenu
mess$ = PCONTROL$ + " is a nonsense input, try again or use F1"
CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
END SELECT
GOTO parametermenu


setparameter:
' transferring the array contents into single parameters used by the equations
kt = inv(1): kp = inv(2): kx = inv(3): ky = inv(4): kd = inv(5)
ki = inv(6): ke = inv(7): kr = inv(8): kn = inv(9): kg = inv(10)
k1 = inv(11): k2 = inv(12): k3 = inv(13): k4 = inv(14)
k5 = inv(15): k6 = inv(16): k7 = inv(17): k8 = inv(18)
k9 = inv(19): k10 = inv(20)

i = 1: da = flv(i, 1): ra = flv(i, 2): ba = flv(i, 3): sa = flv(i, 4): ca = flv(i, 5)
aa = flv(i, 6): ga = flv(i, 7): qa = flv(i, 8):  ta = flv(i, 10): ua = flv(i, 10):

i = 2: db = flv(i, 1): rb = flv(i, 2): bb = flv(i, 3): sb = flv(i, 4): cb = flv(i, 5)
ab = flv(i, 6): gb = flv(i, 7): qb = flv(i, 8):  tb = flv(i, 10): ub = flv(i, 10):

i = 3: dc = flv(i, 1): rc = flv(i, 2): bc = flv(i, 3): sc = flv(i, 4): cc = flv(i, 5)
ac = flv(i, 6): gc = flv(i, 7): qc = flv(i, 8):  tc = flv(i, 10): uc = flv(i, 10):

i = 4: dd = flv(i, 1): rd = flv(i, 2): bd = flv(i, 3): sd = flv(i, 4): cd = flv(i, 5)
ad = flv(i, 6): gd = flv(i, 7): qd = flv(i, 8):  td = flv(i, 10): ud = flv(i, 10):

i = 5: de = flv(i, 1): re = flv(i, 2): be = flv(i, 3): se = flv(i, 4): ce = flv(i, 5)
ae = flv(i, 6): ge = flv(i, 7): qe = flv(i, 8):  te = flv(i, 10): ue = flv(i, 10):

i = 6: df = flv(i, 1): rf = flv(i, 2): bf = flv(i, 3): sf = flv(i, 4): cf = flv(i, 5)
af = flv(i, 6): gf = flv(i, 7): qf = flv(i, 8):  tf = flv(i, 10): uf = flv(i, 10):

i = 7: dg = flv(i, 1): rg = flv(i, 2): bg = flv(i, 3): sg = flv(i, 4): cg = flv(i, 5)
ag = flv(i, 6): gg = flv(i, 7): qg = flv(i, 8):  tg = flv(i, 10): ug = flv(i, 10):

i = 8: dh = flv(i, 1): rh = flv(i, 2): bh = flv(i, 3): sh = flv(i, 4): ch = flv(i, 5)
ah = flv(i, 6): gh = flv(i, 7): qh = flv(i, 8):  th = flv(i, 10): uh = flv(i, 10):

i = 9: di = flv(i, 1): ri = flv(i, 2): bi = flv(i, 3): SI = flv(i, 4): ci = flv(i, 5)
ai = flv(i, 6): gi = flv(i, 7): qi = flv(i, 8):  ti = flv(i, 10): ui = flv(i, 10):

i = 10: dj = flv(i, 1): rj = flv(i, 2): bj = flv(i, 3): sj = flv(i, 4): cj = flv(i, 5)
aj = flv(i, 6): gj = flv(i, 7): qj = flv(i, 8):  tj = flv(i, 10): uj = flv(i, 10):


dx = flv(0, 1): dy = flv(0, 2): dz = flv(0, 3)
  IF ky > imyl - 1 THEN
    mess$ = "KY (Number of cells) too large (<=" + STR$(imyl - 1) + "), will be corrected"
    resp$ = "ok": CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, resp$)
    ky = imyl - 1: inv(4) = ky
  END IF
  IF kx > imxl - 1 THEN
    mess$ = "Kx (Number of cells) too large (<=" + STR$(imxl - 1) + "), will be corrected"
    resp$ = "ok": CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, resp$)
    kx = imxl - 1
    inv(3) = kx
  END IF

IF kp = 0 OR kt = 0 OR kx = 0 OR ky = 0 OR ke = 0 OR dx = 0 THEN
 mess$ = "List of parameters incomplete! check KT, KP, KX, KY, KE or DX"
 resp$ = "ok": CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, resp$)
 GOTO parameinput
END IF
IF PCONTROL$ = "D" THEN EXIT SUB
  IF pcleft$ = "J" THEN pcleft$ = "I"
   IF pcleft$ = "S" OR pcleft$ = "I" THEN
    PCONTROL$ = pcleft$
    EXIT SUB   'direct start after reading a parameter file
  END IF
GOTO parametermenu

leave: '-------------to be executed before going back to the main program----
EXIT SUB

''xxps
screenprintparameter:
FOR i = 19 - inv(9) + ivgashift TO 25 + ivgashift
LOCATE i, 1: PRINT SPACE$(80); : NEXT i
LOCATE 21 - inv(9) + ivgashift, 1
printparameter:
PRINT "Progr.: "; basfilename$; "; "; comment$; filename$; modified$; ""
ishift = (74 - (LEN(filemessage$))) / 2: IF ishift < 1 THEN ishift = 1
PRINT SPACE$(ishift) + filemessage$
FOR i = 1 TO 2: PRINT USING "#####&& "; inv(i); : PRINT "-"; iparnam$(i); : NEXT i
FOR i = 3 TO 10: PRINT USING "####&& "; inv(i); : PRINT "-"; iparnam$(i); : NEXT i
PRINT
FOR i = 11 TO 14
  PRINT USING "#####&& "; inv(i); : PRINT "-"; iparnam$(i);
NEXT i: PRINT "     ";
FOR i = 1 TO 3
 PRINT USING "###.##"; flv(0, i); : PRINT "-" + fparnam$(0, i); : NEXT i
PRINT "  " + LEFT$(displaywas$ + "-DW        ", 10)

FOR il = 1 TO inv(9)
 FOR i = 1 TO 7
 PRINT USING "###.####"; flv(il, i); : PRINT "-" + fparnam$(il, i); : NEXT i
'  PRINT formata$(3, flv(il, i); "-"; fparnam$(il, i);
IF il < inv(9) THEN PRINT
NEXT il
CLOSE (8)
GOTO parameinput

FindEquation:
 icheckerror = 0
IF ifree = 1 THEN COLOR 1, 15:
LOCATE 1, 1
OPEN aprodir$ + basfilename$ FOR INPUT AS #2
IF icheckerror = 1 THEN
    mess$ = " the file " + aprodir$ + basfilename$ + " does not exists"
    CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, resp$)
    GOTO parametermenu
END IF

DO UNTIL EOF(2)
  LINE INPUT #2, slies$
  slies$ = UCASE$(slies$)
  ipu = 1: ipu = INSTR(ipu, slies$, such$)
  IF ipu > 0 THEN
  EXIT DO
  END IF
  LOOP
such$ = "CASE " + LTRIM$(STR$(inv(7))) + " '" + UCASE$(ptype$)
 DO UNTIL EOF(2)
  LINE INPUT #2, slies$
  slies$ = UCASE$(slies$)
  ipu = 1: ipu = INSTR(ipu, slies$, such$)
  IF ipu > 0 THEN
  EXIT DO
  END IF
LOOP
IF ipu = 0 THEN
CLOSE (2)
mess$ = "sorry, no equation of this type in " + programloc$ + basfilename$
CALL cmessage(4, -1, 1, 15, 1, ibackcol, mess$, "RETURN")
GOTO parametermenu
ELSE
i = 1: printeq$(i) = slies$
	DO UNTIL EOF(2)
	  LINE INPUT #2, slies$
	  ipu = 1: ipu = INSTR(ipu, slies$, "CASE")
	  IF ipu = 0 THEN i = i + 1: printeq$(i) = slies$
	  IF ipu > 0 THEN EXIT DO
	LOOP
END IF
CLOSE (2)
GOTO equationfound


checkerror:
icheckerror = 1
resume next
END SUB'===========end of sub paramenu()  ========================


SUB PCXOUT (ipcx, ipcs) ' this is used in the DOS version to record the output as PcX
' on the way to make animated gif's
'ipcs file number, >0 to be on
'ipcx for continous->animated
IF ipcx < 0 AND ipcs < -1 THEN EXIT SUB ' normally no border
  ipcxcol = 2
  idxra = idxr / 2: idxra = idxra * 2 + 1:

IF ipcs > 0 THEN ' Single shots
  ipcx$ = MID$(STR$(ipcs), 2, 2)
  ipcxcol = 13
  IF ipcs < 10 THEN ipcx$ = "0" + ipcx$
  ipcs = ipcs + 1
ELSEIF ipcx > 0 THEN
  ipcxcol = 12: ipcx = ipcx + 1
  ipcx$ = MID$(STR$(ipcx), 2, 3)
     IF ipcx < 10 THEN
	 ipcx$ = "00" + ipcx$
       ELSE
	IF ipcx < 100 THEN ipcx$ = "0" + ipcx$
     END IF
END IF
IF ipcs > 0 OR ipcx > 0 THEN
  resp$ = dirtemp$ + pcx$ + ipcx$ + ".pcx"
  CALL cmessage(5, -1, 1, 1, 15, ibackcol, resp$, resp$)
  IF idyo < idyu THEN 'if not modified->full screen
  idyo = 479: idyu = 15: idxl = 5: idxra = 635
  END IF
  idyotemp = 479 - idyo - 3: IF idyotemp < 0 THEN idyotemp = 0
  idxltemp = idxl - 6: IF idxltemp < 0 THEN idxltemp = 0
  LINE (idxl - 8, idyo + 6)-(idxra + 9, idyu - 6), 15, B ' to remove green
  CALL packpcx(resp$, idxltemp, idyotemp, idxra + 8, 479 - idyu + 3)
END IF
LINE (idxl - 8, idyo + 6)-(idxra + 9, idyu - 6), ipcxcol, B
END SUB

'DEFBL A-G, O-Z
SUB perpendicular (x0, y0, ybase0, s0l, s0r, icol,  iwil)

IF y0 > s0r THEN 'line starts
  yba = ybase0
  IF s0l > yba THEN yba = s0l
  IF s0r > yba THEN yba = s0r
  IF yba < y0 THEN CALL linep(ihp, x0, y0, x0, yba, 0, iwil, icol,  0)
END IF
END SUB

'DEFBL A-G, O-Z
SUB prtscreen (scrfile$)
icheckerror = 0
on error goto screrror
OPEN scrfile$ FOR INPUT AS #2
IF icheckerror = 1 THEN
    mess$ = " the file " + pardir$ + scrfile$ + " does not exists"
    CALL cmessage(6, -1, 12, 15, 4, ibackcol, mess$, resp$)
ELSE
COLOR 2, 15
locate 1,1
izeile = 1
DO
LINE INPUT #2, mess$
IF EOF(2) THEN EXIT DO
'CALL cmessage(3, izeile, 1, 2, 15, ibackcol, mess$, resp$)
print left$(mess$, lastrow-1)
izeile = izeile + 1
LOOP
END IF
CLOSE (2)
EXIT SUB

screrror:
icheckerror = 1

resume next
on error goto 0
END SUB

SUB polygon (ihp, ix1, iy1, ipat, iwil, icol, ityplxx) STATIC
ON ERROR GOTO 0 '####
IF ipcs > -2 THEN
	IF ix1 < idxl THEN idxl = ix1
	IF ix1 > idxr THEN idxr = ix1
	IF iy1 < idyu THEN
	idyu = iy1
	END IF
	IF iy1 > idyo THEN idyo = iy1
END IF

SELECT CASE ipat
CASE 1' initiation
ixmem = ix1: iymem =iy1
pset (ixmem,iymem),icol
CASE 2' draw false color line
line -(ix1,iy1),icol
CASE 3' obsolete
CASE 4' fill and draw
line -(ixmem,iymem),icol
paint (ix1, iy1),icol, icol
case 5
icorand = icomem' Removal of the temporary  lines
PSET (msiluet(1), msiluet(1 + ms2)), icorand
FOR iplot = 2 TO i
LINE -(msiluet(iplot), msiluet(iplot + ms2)), icorand
NEXT iplot
LINE -(msiluet(1), msiluet(1 + ms2)), icorand
END SELECT

IF ihp = 0 OR ihp = 1 THEN EXIT SUB
SELECT CASE ipat

CASE 1' initation
 IF ihp = 6 OR ihp = 7 THEN
 IF icol <> icomem THEN
    SELECT CASE icol
    CASE 0, 8: icohp = 1
    CASE 2, 10: icohp = 3
    CASE 1, 9: icohp = 5
    CASE 12, 4, 6: icohp = 2
    CASE 3, 11: icohp = 7
    CASE 14: icohp = 4
    CASE ELSE
    icohp = 1
    END SELECT
    END IF
    PRINT #77, "SP"; icohp
    icomem = icol
    END IF

PRINT #77, "TR 0; PU PA"; ix1, iy1; "PD; PM 0";  'Transp aus, Polyg anfang

CASE 2: PRINT #77, "PA"; ix1, iy1;

CASE 3: PRINT #77, "PA"; msiluet(1); msiluet(1 + ms2); "PU";
PRINT #77, "PM 2; EP "; 'Polyg Ende"

CASE 4' fill
itype = 80: IF ihp < 6 THEN itype = 100 - 15 * icol
 IF itype < 10 THEN itype = 10
IF icol = 15 THEN itype = 0
PRINT #77, "ft 10"; itype 'Fill type
PRINT #77, "fp"        'fill
END SELECT
END SUB



'DEFBL A-G, O-Z
SUB rl (inz, x2, y2, s2, x1, yto, s1, y3, isi, iy, jyto, icol,  iwil, px)
'line from left to right
dym = (yto - y2) / krot
IF y2 >= s2 THEN xstart = x2: ystart = y2
FOR i = 1 TO krot'------Interpolation-------
  isi = isi - 1'new x-coordinate of the horizon
  s1 = msiluet(isi)'new y-coordinate of the horizon at point 1
  x1 = x2 - px
  y1 = y2 + dym
  IF y2 >= s2 THEN ' line is not hidden
    msiluet(isi + 1) = y2
    IF i = 1 AND y3 > y1 AND inz > 2 THEN
      ivisible = 0
      'xstart = x1: ystart = y1
      GOTO nextimodus
    END IF
    ivisible = 1
    IF y1 < s1 THEN
      xs = px * (s1 - y1) / (y2 + s1 - y1 - s2 + .000001)' line becomes invisible
      ys = y1 + xs * (y2 - y1) / px'point where kp becomes hidden
      xs = x1 + xs:
      ivisible = 0
      CALL linep(ihp, xstart, ystart, xs, ys, 0, iwil, icol,  0)
    ELSE
      msiluet(isi) = y1
    END IF
  ELSE 'point 2 was invisible
    IF y1 > s1 THEN 'point 1 becomes visible
      xs = px * (s1 - y1) / (y2 + s1 - y1 - s2 + .000001)' line becomes invisible
      ystart = y1 + xs * (y2 - y1) / px'point where kp becomes hidden
      xstart = x1 + xs:
      msiluet(isi) = y1
      ivisible = 1

    ELSE 'point 1 and 2was invisible,last kurve marked by ihintr>2
      IF iy = jyto AND ihintr > 2 THEN
	CALL linep(ihp, x2, y2, x2, y2, 0, iwil, icol,  0)' line after krot-loop
      END IF
    END IF
  END IF
nextimodus:
  y2 = y1'old left point becomes the new right point
  x2 = x1
  s2 = s1
NEXT i
IF ivisible = 1 THEN CALL linep(ihp, xstart, ystart, x1, y1, 0, iwil, icol,  0)
EXIT SUB
END SUB

DEFDBL A-G
DEFDBL O-Z
DEFINT H-N
SUB rwparameterdisk (rw$, ifilenumber)  STATIC
' to writeand read parameters
SELECT CASE RW$ 'xxrw
'xfilenametmp$
CASE "R", "RA" 'read
  LINE INPUT #ifilenumber, filemessage$
FOR i = 1 TO 10: INPUT #ifilenumber, inv(i): NEXT i
IF RW$ = "RA" THEN
INPUT #ifilenumber, dummy
END IF
FOR i = 11 TO 14: INPUT #ifilenumber, inv(i): NEXT i
FOR i = 1 TO 3: INPUT #ifilenumber, flv(0, i)  '': PRINT flv(0, i); :
NEXT i''dx, dy, dz
IF RW$ = "RA" THEN INPUT #ifilenumber, dummy: ''PRINT
kn = inv(9)
  FOR il = 1 TO kn
   FOR i = 1 TO 7
    INPUT #ifilenumber, flv(il, i)  '': PRINT flv(il, i); :
  NEXT i
  IF RW$ = "RA" THEN INPUT #ifilenumber, dummy: PRINT
  NEXT il
CLS : LOCATE 1, 1
FOR i = 0 TO kn
  INPUT #ifilenumber, gxa(i), gya(i), gfa(i), itypus(i), ico(i), iwi(i):
NEXT
  INPUT #ifilenumber, displaywas$, dxDUMMY, dxp, dyp, per, idosgraph'  = fkasten
  INPUT #ifilenumber, krot, ivorcol, ibackcol, istelz, ihintr
    IF EOF(ifilenumber) THEN
	igraph = 12
	ELSE
      INPUT #ifilenumber, igraph
    END IF
CALL screencheck

CASE "W" ' write
  PRINT #ifilenumber, filemessage$
  FOR i = 1 TO 9: PRINT #ifilenumber, USING "####"; inv(i); : PRINT #ifilenumber, ","; : NEXT i
  PRINT #ifilenumber, USING "####"; inv(10)
  FOR i = 11 TO 14: PRINT #ifilenumber, USING "####"; inv(i); : PRINT #ifilenumber, ","; : NEXT i
    PRINT #ifilenumber, USING "###.####"; flv(0, 1); : PRINT #ifilenumber, ",";
    PRINT #ifilenumber, USING "###.####"; flv(0, 2); : PRINT #ifilenumber, ",";
    PRINT #ifilenumber, USING "###.####"; flv(0, 3)

      FOR il = 1 TO inv(9)
  FOR i = 1 TO 6:
  PRINT #ifilenumber, USING "###.####"; flv(il, i); : PRINT #ifilenumber, ","; :
  NEXT i
   PRINT #ifilenumber, USING "###.####"; flv(il, 7)
  NEXT il

   FOR i = 0 TO inv(9)
  PRINT #ifilenumber, USING "####.###"; gxa(i); : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####.###"; gya(i); : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####.###"; gfa(i); : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####"; itypus(i); : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####"; ico(i); : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####"; iwi(i)
  NEXT i
  PRINT #ifilenumber, displaywas$; : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####.##"; dx; : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####.##"; dxp; : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####.##"; dyp; : PRINT #ifilenumber, ",";
  PRINT #ifilenumber, USING "####.##"; per; : PRINT #ifilenumber, ",";
  WRITE #ifilenumber, idosgraph
  WRITE #ifilenumber, krot, ivorcol, ibackcol, istelz, ihintr
  WRITE #ifilenumber, igraph


 CASE "PS" ' for display on screen
  color 1,15
  FOR i = 19 - inv(9) + ivgashift TO 25 + ivgashift
  LOCATE i, 1: PRINT SPACE$(80); : NEXT i
  LOCATE 21 - inv(9) + ivgashift, 1
PRINT "Progr.: "; basfilename$; "; "; comment$; filename$; modified$; ""
ishift = (74 - (LEN(filemessage$))) / 2: IF ishift < 1 THEN ishift = 1
PRINT SPACE$(ishift) + filemessage$
FOR i = 1 TO 2: PRINT USING "#####&& "; inv(i); : PRINT "-"; iparnam$(i); : NEXT i
FOR i = 3 TO 10: PRINT USING "####&& "; inv(i); : PRINT "-"; iparnam$(i); : NEXT i
PRINT
FOR i = 11 TO 14
  PRINT USING "#####&& "; inv(i); : PRINT "-"; iparnam$(i);
NEXT i: PRINT "     ";
FOR i = 1 TO 3
 PRINT USING "###.##"; flv(0, i); : PRINT "-" + fparnam$(0, i); : NEXT i
PRINT "  " + LEFT$(displaywas$ + "-DW        ", 10)

FOR il = 1 TO inv(9)
 FOR i = 1 TO 7

  PRINT USING "###.####"; flv(il, i); : PRINT "-"; fparnam$(il, i); :
NEXT i
IF il < inv(9) THEN PRINT
NEXT il

CASE "PE"
FOR i = 1 TO 2: PRINT #ifilenumber, iparnam$(i); "="; : PRINT #ifilenumber, USING "####&& "; inv(i); : PRINT #ifilenumber, ":"; : NEXT i
  FOR i = 3 TO 10: PRINT #ifilenumber, iparnam$(i); "="; : PRINT #ifilenumber, USING "###&& "; inv(i); : PRINT #ifilenumber, ":"; : NEXT i
  PRINT #ifilenumber,
  FOR i = 11 TO 14: PRINT #ifilenumber, iparnam$(i); "="; : PRINT #ifilenumber, USING "###&& "; inv(i); : PRINT #ifilenumber, ":"; : NEXT i
  PRINT #ifilenumber,
   FOR il = 1 TO inv(9)
   FOR i = 1 TO 7
    PRINT #ifilenumber, fparnam$(il, i); "="; : PRINT #ifilenumber, USING "###.####"; flv(il, i); : PRINT #ifilenumber, ":";
   NEXT i
   PRINT #ifilenumber,
   NEXT il

 CASE "PF"
'printparameter on file as stored
PRINT #ifilenumber, "Progr.: "; basfilename$; "; "; comment$; curdrive$ + programloc$ + pdir$ + filename$; modified$; ""
ishift = (74 - (LEN(filemessage$))) / 2: IF ishift < 1 THEN ishift = 1
PRINT #ifilenumber, SPACE$(ishift) + filemessage$
FOR i = 1 TO 2: PRINT #ifilenumber, USING "#####&& "; inv(i); : PRINT #ifilenumber, "-"; iparnam$(i); : NEXT i
FOR i = 3 TO 10: PRINT #ifilenumber, USING "####&& "; inv(i); : PRINT #ifilenumber, "-"; iparnam$(i); : NEXT i
PRINT #ifilenumber,
FOR i = 11 TO 14
  PRINT #ifilenumber, USING "#####&& "; inv(i); : PRINT #ifilenumber, "-"; iparnam$(i);
NEXT i: PRINT #ifilenumber, "     ";
FOR i = 1 TO 3
 PRINT #ifilenumber, USING "###.##"; flv(0, i); : PRINT #ifilenumber, "-" + fparnam$(0, i); : NEXT i
PRINT #ifilenumber, "  " + LEFT$(displaywas$ + "-DW        ", 10)

FOR il = 1 TO inv(9)
 FOR i = 1 TO 7
  PRINT #ifilenumber, USING "###.####"; flv(il, i); "-"; fparnam$(il, i); : NEXT i
IF il < inv(9) THEN PRINT #ifilenumber,
NEXT il

END SELECT
'modified$ = ""
END SUB



SUB smenu (s$(), irfilename$, iline, headline$)
'Produces a menu for selection, s$() contains the items to choose
'iline is returned
CLS
izlenmax = lastrow-5
'length of menu lines
'izlenmax = LEN(headline$) + 4
FOR ial = 1 TO 250
izlen = LEN(s$(ial))
IF izlen = 0 THEN EXIT FOR
IF izlen > izlenmax THEN izlen = izlenmax: s$(ial) = LEFT$(s$(ial), izlenmax-3) + "..."
'IF izlen > izlenmax THEN izlenmax = izlen
NEXT ial
iarmax = ial - 1: iline = 1: ivon = 1:
smenunew:
line (0,0) - (1500, 1050),1,BF
COLOR 15, 1
LOCATE 1, 1
PRINT "ÚÄ  " + headline$ + " " + STRING$(izlenmax - LEN(headline$) - 2, 196) + "¿"
ibis = ivon + lastline-3: IF ibis > iarmax THEN ibis = iarmax
FOR ial = ivon TO ibis
PRINT "³ " + s$(ial) + SPACE$(izlenmax - LEN(s$(ial))) + " ³"
NEXT ial
IF ibis = iarmax THEN
PRINT "ÀÄ" + STRING$(izlenmax, 196) + "ÄÙ";
ELSE
PRINT "ÀÄ" + STRING$(izlenmax - 9, 196) + " more ...ÄÙ";
END IF
COLOR 15, 2
DO
LOCATE lastline, 1: PRINT "Use arrows, Pg-UP or PG-DOWN to select, RETURN to take, ESC to quit, p=>on file";
'selectline
COLOR 1, 15'2, 0
LOCATE 2 + iline - ivon, 3
ioldline = iline
PRINT s$(iline) + SPACE$(izlenmax - LEN(s$(iline)))
i$=inkey$
i$ = ""
WHILE i$ = ""
i$ = INKEY$
WEND
i = ASC(RIGHT$(i$, 1))
IF ASC(LEFT$(a$, 1)) = 255 OR ASC(LEFT$(a$, 1)) = 0 THEN

SELECT CASE i    ' from the keybord
CASE 27, 113: irfilename$ = "q"
EXIT SUB
CASE 80, 32: IF iline < iarmax THEN iline = iline + 1
IF iline > ibis THEN
ivon = ivon + lastline-3
IF ivon > iarmax - lastline-4 THEN ivon = iarmax - lastline-4
IF ivon < 1 THEN ivon = 1
iline = ivon
END IF
GOTO smenunew
CASE 72: IF iline > 1 THEN iline = iline - 1
IF iline < ivon THEN ivon = ivon - lastline+4
IF ivon < 1 THEN ivon = 1
GOTO smenunew
CASE 81'pg-down
ivon = ivon + lastline-3 ' 22
iline = ivon
GOTO smenunew

CASE 73'pg-up
ivon = ivon - (lastline-3)' 22
IF ivon < 1 THEN ivon = 1
iline = ivon
GOTO smenunew

CASE 13
irfilename$ = s$(iline)
ipu = INSTR(s$(iline), " ")
IF ipu > 0 THEN irfilename$ = LEFT$(s$(iline), ipu - 1)
cls:  EXIT SUB

CASE 112
icheckerror = 0
'ON ERROR GOTO checkerrorsm   ' does not work
   dummy$ = dirtemp$ + "parafile.txt"
   OPEN dummy$ FOR OUTPUT AS #8
   IF icheckerror = 1 THEN
   irfilename$ = "q":
   GOTO smenunew
   EXIT SUB
   END IF
'PRINT #8, "ÚÄ  " + headline$ + " " + STRING$(izlenmax - LEN(headline$) - 2, 196) + "¿"
 PRINT #8, " " + headline$ + " " + STRING$(izlenmax - LEN(headline$) - 2, "-") + "-"
FOR ial = 1 TO iarmax
'PRINT #8, "³ " + s$(ial) + SPACE$(izlenmax - LEN(s$(ial))) + " ³"
PRINT #8, " " + s$(ial) + SPACE$(izlenmax - LEN(s$(ial))) + " "
NEXT ial
'PRINT #8, "ÀÄ" + STRING$(izlenmax, 196) + "ÄÙ"
PRINT #8, " " + STRING$(izlenmax, "-") + "--"
'CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, "OK")
COLOR 15, 2
LOCATE lastline, 1: PRINT "the parameterlist is written: "; dummy$; : INPUT ; "                      [OK]", resp$
   CLOSE #8
GOTO smenunew
CASE ELSE
BEEP
END SELECT
end if
LOCATE 2 + ioldline - ivon, 3
PRINT s$(ioldline) + SPACE$(izlenmax - LEN(s$(ioldline)))
LOOP
END SUB

SUB sorts (s$(), icount)
' to sort e.g. files for the menu, icount = number of elements
ICA = 0
FOR icc = 0 TO icount
VAM$ = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
FOR ICD = ICA TO icount
IF s$(ICD) > VAM$ GOTO SNEXTICD
IVAM = ICD
VAM$ = s$(ICD)
SNEXTICD:
NEXT ICD
s$(IVAM) = s$(ICA)
s$(ICA) = VAM$
ICA = ICA + 1
NEXT icc
EXIT SUB
 END SUB

SUB titles (inz, title$, mess$)
  ishift = (74 - (LEN(mess$))) / 2: IF ishift < 1 THEN ishift = 1
  title$ = SPACE$(ishift) + mess$
END SUB

SUB whatprmfiles (s$(), programnameloc$, progrtmp$, exten$, drive$, irfilename$, ilineparam)
''$INCLUDE: 'SHARED.bi'
' scans the disk for a particular type of files
' assures the correct order in the menu
' s$() contains afterwards the file names and the first line of that files
' i.e. the head lines

IF progrtmp$ = "" THEN
  mess$ = "Program name to be scanned (ESC to skip)  [" + programnameloc$ + "]"
  CALL zeingabe(igt, 0, 4, i, dummy, progrtmp$, mess$)
  IF progrtmp$ = "q" OR progrtmp$ = CHR$(27) THEN irfilename$ = "q": EXIT SUB
  IF progrtmp$ = "" THEN progrtmp$ = programnameloc$
END IF
exten$ = LCASE$(exten$)
ilr = LEN(progrtmp$)
IF progrtmp$ = "*" THEN progrtmp$ = ""  'for help-files
dummy$ = LCASE$(drive$ + progrtmp$ + "*." + exten$)
''color 15,1: locate 1,1: print dummy$;: sleep
s1$ = __DIR(dummy$, fbArchive)  'for FreeBasic .2
' __Dir("*.bas", fbArchive)
IF s1$ = "" THEN
resp$ = "ok"
CALL cmessage(4, -1, 1, 15, 4, 0, "sorry, no such files found", resp$)
irfilename$ = "q": EXIT SUB
END IF
CALL cmessage(3, -1, 1, 15, 2, 0, "... reading headlines....", "")
icount = 0: ilinecount = 0
DO
IF s1$ > "" THEN
OPEN drive$ + s1$ FOR INPUT AS #2
LINE INPUT #2, SI$
keloc = 0:
if exten$ = "PRM" or exten$ = "prm" then INPUT #2, ktloc, kploc, kxloc, kyloc, kdloc, kiloc, keloc
CLOSE #2
icount = icount + 1
ipu = 1: ipu = INSTR(ipu, s1$, ".")
s1$ = MID$(s1$, LEN(progrtmp$) + 1, ipu - LEN(progrtmp$) - 1)
s1$ = s1$ + "          "
kestring$ = "": IF keloc > 0 THEN kestring$ = MID$(STR$(keloc)+"    ", 1, 7)
s$(icount) = MID$(s1$, 1, 9) + kestring$ + SI$
's$(icount) = MID$(s1$, 1, 7) + SI$
IF VAL(s$(icount)) < 10 THEN
  s$(icount) = ".." + s$(icount)
  ELSEIF VAL(s$(icount)) < 100 THEN
  s$(icount) = LEFT$(s$(icount), 1) + "." + MID$(s$(icount), 2, 250)
  END IF
END IF

s1$ = __DIR(  )
LOOP UNTIL s1$ = ""
s$(icount + 1) = ""
CALL sorts(s$(), icount)
mess$ = "FILES: " + drive$ + progrtmp$ + "*." + exten$ + "     TITLES"
IF icount > 0 THEN
FOR i = 1 TO icount
IF LEFT$(s$(i), 2) = ".." THEN s$(i) = MID$(s$(i), 3, 250)
IF MID$(s$(i), 2, 1) = "." THEN s$(i) = LEFT$(s$(i), 1) + MID$(s$(i), 3, 250)
s$(i) = progrtmp$ + s$(i)
NEXT i
smenuagain:
CALL smenu(s$(), irfilename$, ilineparam, mess$)
EXIT SUB
ELSE 'icount=0
   PRINT "sorry, no such files"
   irfilename$ = "q"
 END IF
END SUB

SUB zeingabe (igt, iquest, inz, i, f, labels$, text$)
'allows inputting single constants, if igt>O from file (guided tour)
'iquest =1: original value is prompted
'inz=1 integer i: inz=2 floating point f, inz=3: string labels$
'text$ is the message displayed with the request
'---------------------------------------------------------

textprint$ = text$ + " "
IF iquest = 1 THEN
  textprint$ = textprint$ + " OLD = "
  IF inz = 1 THEN textprint$ = textprint$ + STR$(i)
  IF inz = 2 THEN textprint$ = textprint$ + formata$(2,f) ' (STR$(f)
  IF inz = 3 OR inz = 4 THEN textprint$ = textprint$ + labels$
textprint$ = textprint$ + "  NEW = "
END IF
CALL cmessage(1, -1, 1, 2, 15, ibackcol, textprint$, resp$)
ilength = LEN(textprint$) + 1
fac$ = ""
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_ ", resp$)
SELECT CASE igt
CASE -1000 TO 0
fac$ = ""
DO
DO: dummy$ = INKEY$: LOOP UNTIL dummy$ > ""
IF dummy$ = CHR$(13) THEN EXIT DO
IF dummy$ = CHR$(8) THEN
  lentext = LEN(fac$)
  IF LEN(fac$) > 0 THEN fac$ = LEFT$(fac$, LEN(fac$) - 1)
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
ELSE
SELECT CASE inz
CASE 1, 2
SELECT CASE dummy$
CASE "-", "+", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
fac$ = fac$ + dummy$
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
CASE CHR$(27): GOTO zende
CASE ELSE
BEEP
END SELECT
CASE 3, 4
IF dummy$ = CHR$(27) AND inz = 3 GOTO zende
IF dummy$ = CHR$(27) AND inz = 4 THEN labels$ = CHR$(27): GOTO zende
IF ASC(LEFT$(dummy$, 1)) = 255 OR ASC(LEFT$(dummy$, 1)) = 0 THEN
SELECT CASE ASC(RIGHT$(dummy$, 1))' a key was presssed
CASE 59: fac$ = "MENU"' F1"
CASE 60: fac$ = "R"
CASE 61: fac$ = "RN"
CASE 62: fac$ = "M4"
CASE 63: fac$ = "M5"
CASE 64: fac$ = "PE"
CASE 65: fac$ = "PEF"
CASE 66: fac$ = "W0"
CASE 67: fac$ = "R0"
CASE 68: fac$ = "SN"   'F10
CASE 133: fac$ = "SN"
CASE 134: fac$ = "FDELAY"

END SELECT
labels$ = fac$
EXIT SUB
END IF
fac$ = fac$ + dummy$
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", RESP$)
END SELECT
END IF
LOOP

CASE 1 TO 1000: LINE INPUT #11, fac$ 'only for GUIDED TOURS
IF igtdelay > 10 THEN CALL gtreturn(8, igt)
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
IF igtdelay > 1 THEN CALL gtreturn(8, igt)
IF fac$ = "x" GOTO zende
END SELECT
IF inz = 1 THEN i = VAL(fac$)
IF inz = 2 THEN f = VAL(fac$)
IF inz = 3 OR inz = 4 THEN labels$ = fac$
zende:
CALL cmessage(1, -1, 1, 15, 15, ibackcol, SPACE$(79), resp$)
color 1,15
END SUB
FUNCTION formata$ (inz, aa)

IF aa = 0 AND inz < 3 THEN x$ = "0": GOTO formadone
a = aa: ifound = 0: x$ = "": IF aa < 0 THEN x$ = "-": a = -a
IF a > 9999 THEN x$ = x$ + "####": GOTO formadone
ix = INT(a)
afrac = a - ix: ifrac = 10000 * afrac
IF ix = 0 THEN
x$ = x$ + "  0"
ELSE
FOR i = 2 TO 0 STEP -1
idiv = 10 ^ i
jx = INT(ix / idiv)
ixx = ix - 10 ^ i * jx
IF jx > 0 OR ifound = 1 THEN
ifound = 1
IF ix >= idiv OR ifound = 1 THEN
ifound = 1
x$ = x$ + RIGHT$(STR$(jx), 1)
ELSE
x$ = x$ + " "
END IF
ELSE
x$ = x$ + " "
END IF
ix = ixx
NEXT i
END IF


IF ifrac = 0 AND inz < 3 GOTO formadone
IF ifrac = 0 THEN
x$ = x$ + ".0000"
ELSE
x$ = x$ + "."
ix = ifrac

FOR i = 3 TO 0 STEP -1
idiv = 10 ^ i
jx = INT(ix / idiv)
ixx = ix - 10 ^ i * jx
IF ix = 0 AND inz < 3 GOTO formadone
x$ = x$ + RIGHT$(STR$(jx), 1)
ix = ixx
NEXT i
END IF
formadone:
IF LEFT$(x$, 3) = "-  " THEN x$ = "  -" + MID$(x$, 4, 6)
IF LEFT$(x$, 3) = "- " THEN x$ = " -" + MID$(x$, 3, 7)
formata$ = x$
END FUNCTION
