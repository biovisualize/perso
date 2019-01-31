DECLARE SUB switchscreen (inz%, iactive%, igraph%, kd%, programname$, fdelay!)
'DECLARE SUB rwdata (wasnun$, displaywas$)
DEFINT I-N
DECLARE SUB ColorPrint (text$, row%, col%, fc%, bc%)
DECLARE SUB packpcx (resp$, idummy%, jdummy%, idxr%, idyu%)
DECLARE SUB screencheck (igraph%, lastline%, lastrow%, ivgashift%, ifree%)
DECLARE SUB smenu (s$(), irfilename$, iline%, headline$)
DECLARE SUB sorts (s$(), icount%)
DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DECLARE SUB eingabe (igt%, inz%, iaf%(), f!(), label$(), text$, ianf%, ianz%)
DECLARE SUB gmanage (wasnun$)
DECLARE SUB gtourcontrol (pcontrol$, igt%, label$())
DECLARE SUB gtreturn (inz%, igt%)
DECLARE SUB init (inewscreen%, igraph%, ivorcol%, ibackcol%, ivgashift%)
DECLARE SUB linep (ihp%, x1!, y1!, x2!, y2!, ipat%, iwidth%, ico%, ityp%)
DECLARE SUB PCXOUT (ipcx%, ipcs%)
DECLARE SUB prtscreen (scrfile$)
DECLARE SUB rwparameterdisk (RW$, ifilenumber%)
DECLARE SUB titles (inz%, title$, mess$)
DECLARE SUB whatprmfiles (s$(), programnameloc$, progrtmp$, exten$, drive$, irfilename$, ilineparam)
DECLARE SUB zeingabe (igt%, iquest%, inz%, i%, f!, labels$, text$)
DEFINT I-N
' $INCLUDE: 'scommon.bi'
' $INCLUDE: '..\subs\errlist.bas'

SUB apnp (ihpp, xal, yal, lp, iwidth, ico, ityploc)
IF ityploc > 0 THEN
  PSET (xal, yal), ico
ELSE
  PSET (xal, yal)
END IF
IF ihpp = 0 OR ihpp = 1 THEN EXIT SUB
'
'' IF ihp = 6 OR ihp = 7 THEN
' IF ico <> icomem THEN
'    SELECT CASE ico
'    CASE 0, 8: icohp = 1
'    CASE 2, 10: icohp = 3
'    CASE 1, 9: icohp = 5
'    CASE 12, 4, 6: icohp = 2
'    CASE 3, 11: icohp = 7
'    CASE 14: icohp = 4
'    CASE ELSE
'    icohp = 1
'    END SELECT
'    PRINT #77, "SP"; icohp
'    icomem = ico
'    END IF
'IF ityp = 0 THEN
'  PRINT #77, " PAPU"; xa; ","; ya
'ELSE
'  PRINT #77, "PU"; xa; ","; ya; "RA"; xa + .7; ","; ya + .7
'END IF
EXIT SUB
END SUB

'DEFBL A-G, O-Z
SUB backfront (inz, x0, x1, y4, y1, s0r, s1r, s0l, s1l, ico, iwil, px, p4, pyy)
SHARED ihp

IF y4 > s0l THEN 'line can start
  IF y1 > s1r THEN 'full
    CALL linep(ihp, x0, y4, x1, y1, 0, iwil, ico, 0)
  ELSE 'becomes invisible
    IF inz < 5 OR inz > 6 THEN
      p4 = px / (y4 - y1 + s1r - s0l)
      xs = (s1r - y1) * p4
      ys = y1 + xs * (y4 - y1) / px
    CALL linep(ihp, x0, y4, x1 + xs, ys, 0, iwil, ico, 0)
    ELSE
      xs = (s1r - y1) * p4
      ys = y1 + xs * pyy / px
      CALL linep(ihp, x0, y4, x1 + xs, ys, 0, iwil, ico, 0)
    END IF
  END IF
END IF
EXIT SUB
END SUB

SUB clearscreen
LINE (0, 0)-(1200, 1050), 15, BF 'erease frame
END SUB

SUB clsscr (ipcs, ipcx, ibackcol)
SHARED idxr, idxl, idyu, idyo
LINE (0, 0)-(1640, 1480), ibackcol, BF 'erease frame
IF ipcx >= 0 THEN EXIT SUB ' nicht ver„ndern wenn movie l„uft
IF ipcs > -2 THEN idxr = 1: idxl = 640: idyu = 479: idyo = 15
END SUB

'DEFBL A-G, O-Z
SUB cmessage (itype, iline, irow, ilettercol, iletterback, ibackc, text$, resp$)
SHARED ivgashift

'''$INCLUDE: 'fmshared.bi'
' Prints a message on the screen, >3 centered
' itype = 2 or 4: a RETURN is required
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
      IF deltime > 0 THEN
      DO UNTIL TIMER - t1 > deltime
      resp$ = INKEY$
      IF resp$ > "" THEN EXIT DO
      LOOP
      END IF
      CALL ColorPrint(SPACE$(79), ilinea, 1, ibackc, ibackc)
      END IF
END SUB

'DEFBL A-G, O-Z
SUB eingabe (igt, inz, iaf(), f(), label$(), text$, ianf, ianz)
SHARED ivgashift, displaywas$, igtdelay, ibackcol, ifree
'Input routine for mupltiple changes of the plot parameters
CALL cmessage(1, -1, 1, 15, 15, ibackcol, SPACE$(79), resp$)
twas$ = "sabcdefgh"
FOR i = ianf TO ianz
  ipu = 1: ipu = INSTR(ipu, displaywas$, MID$(twas$, i + 1, 1))
  IF ipu > 0 THEN
    textprint$ = "Substance  : " + text$ + "  OLD = "
  IF inz = 1 THEN textprint$ = textprint$ + STR$(iaf(i))
  IF inz = 2 THEN textprint$ = textprint$ + STR$(f(i))
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
IF ifree = 1 THEN COLOR 1, 15
END SUB

'DEFBL A-G, O-Z
SUB gmanage (wasnun$) STATIC
SHARED per, krot, ibackcol, ivorcol, kn, ihintr, istelz, igt
SHARED gxa(), gya(), gfa(), iwi(), itypus(), ico()
SHARED lax(), lx(), lay(), ly(), pcx$
SHARED label$(), ipcx, ipcs, dirtemp$, ihp
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
factor = 1.6: IF wasnun$ = ">" THEN factor = .625' scaling for different screens

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
 mess$ = "ihintr: 1=hidden frame, 2=last doddet, shift KD=28,tick in 3"
 CALL zeingabe(igt, 1, 1, ihintr, dyp, resp$, mess$)

CASE "PCS"
 ipcx = -1
 mess$ = "Filename of the PCX-file"
 CALL zeingabe(igt, 0, 3, istelz, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 ipcs = -2
 ELSE
 pcx$ = resp$ + "-": ipcs = 1
 igraph = 12: CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)
 END IF


CASE "PCOFF", "P0": ipcx = -1: ipcs = -2'frame out"
 CALL cmessage(5, -1, 1, 4, 15, ibackcol, " PCX off", dummy$)

CASE "PCXDRIVE", "DRIVE"
 mess$ = "drive for pcx and parameter [ " + dirtemp$ + " ];   c,d,e...."
 resp$ = dirtemp$
 CALL zeingabe(igt, 0, 3, ipcx, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 ELSE
  resp$ = LEFT$(resp$, 1)
  IF resp$ > "b" AND resp$ < "z" THEN
  dirtemp$ = resp$ + ":\atemp\"
  ELSE
  BEEP
 END IF
END IF

CASE "PCB" 'just a blanck screen, e.g. end of animated gif
   LINE (0, 0)-(640, 480), 15, BF
   CALL PCXOUT(ipcx, ipcs)

CASE "PCFULL" 'take the whole screen
  idyo = 479: idyu = 10: idxl = 3: idxra = 635

CASE "HP"
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
    CALL cmessage(3, -1, 1, 4, 15, ibackcol, "         wird auf Platte kommen, File = " + resp$, dummy$)
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
    xa(il) = xa(il) + dxp
  NEXT
CASE "24", "VL"
  FOR il = 0 TO kn
    xa(il) = xa(il) - dxp
  NEXT
CASE "25", "VU"
  FOR il = 0 TO kn
    ya(il) = ya(il) - dyp
  NEXT
CASE "26", "VO"
  FOR il = 0 TO kn
    ya(il) = ya(il) + dyp
  NEXT

CASE "VH"
FOR il = 1 TO kn
FOR ix = lax(il) TO lx(il)
FOR iy = lay(il) TO ky(il) / 2
jyz = ly(il) + 1 - iy
zwischenwert = axy(il, ix, jyz)
ax(il, ix, jyz) = axy(il, ix, iy)
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

SUB growthxy (iinsert, js, imx, kn, lax(), lx(), lay(), ly(), axy())
IF js < imx - 1 THEN
FOR il = 1 TO kn:
FOR ix = lx(il) TO iinsert STEP -1          'still the old js is being used
     FOR iy = 1 TO ly(il)
      axy(il, ix + 1, iy) = axy(il, ix, iy)
   NEXT iy:  NEXT ix
   lx(il) = lx(il) + 1
   NEXT il
js = js + 1
ELSE
mess$ = "array becomes too large,  js>imx [" + STR$(js) + ", " + STR$(imx) + " ]"
CALL cmessage(3, -1, 1, 15, 4, ibackcol, mess$, "OK")
END IF

END SUB

'DEFBL A-G, O-Z
SUB gtourcontrol (pcontrol$, igt, label$()) STATIC
SHARED igraph, ivgashift, igtdelay, inewscreen, ibackcol, title$, fdelay
'control of GUIDED TOURS

DO WHILE igt > 0
IF istop = 1 GOTO istopagain' used if calculation is continued
readnextgt:
ON EOF(11) GOTO ENDGT
LINE INPUT #11, pcontrol$
SELECT CASE LEFT$(pcontrol$, 1)
  CASE " "
    label$(0) = pcontrol$
    CALL titles(1, title$, pcontrol$)
    GOTO readnextgt
  CASE "%":  GOTO readnextgt
  CASE "<"
    idelay = VAL(MID$(pcontrol$, 2, 7))
    CALL gtreturn(idelay * 10, igt)
    IF igt > 0 GOTO readnextgt
  CASE "+":
istopagain: ' used if calculation has been continued
    istop = 0
	IF igtdelay < 0 THEN
	CALL gtreturn(-igtdelay, igt)' delay if igtdelay is negative
	ELSE
	CALL gtreturn(100, igt)
	IF pcontrol$ = "C" OR pcontrol$ = "S" OR pcontrol$ = "N" THEN
	istop = 1: EXIT SUB
	END IF
	END IF
    IF igt > 0 GOTO readnextgt
END SELECT
SELECT CASE pcontrol$
    CASE ""
    pcontrol$ = "": EXIT SUB
    CASE "-", "_": label$(0) = "": title$ = ""

    CASE "#":
	  LOCATE 25 + ivgashift - 5, 1
	  FOR i = 1 TO 4: LINE INPUT #11, label$(i)
	  ON EOF(11) GOTO ENDGT
	  IF label$(i) = "" THEN EXIT FOR
	  IF label$(i) = "." THEN
	  label$(i) = ""
'''          LOCATE 1, 1: INPUT "xx", resp$

	  END IF
	  NEXT i
	  IF igtdelay > 0 THEN
	  FOR ii = 1 TO i - 1
	  CALL cmessage(3, ii - i - 1, 1, 4, 15, ibackcol, " " + label$(ii), "")
	  'CALL cmessage(3, ii - i, 1, 4, 15, ibackcol, " " + label$(ii), "")
	  NEXT
	  CALL cmessage(3, ii - i - 1, 1, 4, 15, ibackcol, SPACE$(78), "")
	  END IF
	  CALL gtreturn(100, igt)
	  label$(0) = ""
	IF pcontrol$ = "C" OR pcontrol$ = "S" OR pcontrol$ = "N" THEN
	istop = 1: EXIT SUB
	END IF


    CASE "&":
	  INPUT #11, ierease, izeile, idelay, inewscreen
	  ON EOF(11) GOTO ENDGT
	  IF ierease = 1 THEN
	  CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)
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
	 igt = 0: CLOSE (11): label$(0) = "": pcontrol$ = "": fdelay = 0
	  CLOSE (27)
	  mess$ = "-------   End of the GUIDED TOUR  ------"
	  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
    CASE ELSE:
	IF igtdelay > 5 AND igtdelay < 80 THEN
	CALL cmessage(1, -1, 1, 15, 1, ibackcol, pcontrol$ + "      ", "")
	CALL gtreturn(5, igt) 'short delay
	END IF
    EXIT SUB
END SELECT
LOOP
END SUB

'DEFBL A-G, O-Z
SUB gtreturn (inz, igt)
'makes delays or asks for <RETURN> in GUIDED TOURS
SHARED ivgashift, pcontrol$, ibackcol, fdelay, readdatafile$

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
  pcontrol$ = MID$(pcontrol$, 3, 80)
  IF pcontrol$ > "" THEN
  pcontrol$ = MID$(pcontrol$ + SPACE$(77), 1, 80)
  CALL ColorPrint(pcontrol$, 25 + ivgashift, 1, ibackcol, ibackcol)
  END IF
   CALL ColorPrint("[ok]", 25 + ivgashift, 76, 15, 13)
   resp$ = "":   DO UNTIL resp$ > "": resp$ = INKEY$: LOOP
   CALL ColorPrint(SPACE$(79), 25 + ivgashift, 1, 15, 15)

IF igt > 0 THEN
SELECT CASE resp$
CASE "c": pcontrol$ = "C"
CASE "s": pcontrol$ = "S"
CASE "n": pcontrol$ = "N"
CASE "d": pcontrol$ = "DP"
CASE "q"
GOTO terminateTour
CASE "-", "_"
  mess$ = " GUIDED TOUR suspended, back with command GT"
  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
  igt = -1: pcontrol$ = ""
CASE ELSE: pcontrol$ = ""
END SELECT
END IF
END SELECT
EXIT SUB

terminateTour:
  mess$ = "GUIDED TOUR terminated"
  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
  CLOSE (11)
  igt = 0: fdelay = 0!
'RETURN

END SUB

'DEFBL A-G, O-Z
SUB init (inewscreen, igraph, ivorcol, ibackcol, ivgashift) STATIC
CLS
END SUB

'DEFBL A-G, O-Z
SUB leftright (x1, x2, x3, y1, y2, y3, s1r, s2r, s3r, s1l, s2l, s3l, ico, iwil, px, p4)
SHARED ihp
IF y1 > s1l THEN 'line starts, point 1 (or 4) is visible
  IF y3 > s3r THEN 'point 3 (or 6) is visible
    CALL linep(ihp, x1, y1, x3, y3, 0, iwil, ico, 0)
  ELSE '3 not visible
    IF y2 > s2r THEN 'point 2 (or 5) is visible
      CALL linep(ihp, x1, y1, x2, y2, 0, iwil, ico, 0)
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
      CALL linep(ihp, x1 - xs, ys, x3, y3, 0, iwil, ico, 0)
    ELSE
      CALL linep(ihp, x1 - xs, ys, x2, y2, 0, iwil, ico, 0)
    END IF
  ELSE 'point 2 was not visible

    'CALL linep(ihp, x2 + 1, y2, x2 + 1, s2r, 2, iwil, 13, 0)
    IF y3 > s3r THEN 'point 3 (or 6) is visible
      IF y2 > s2l THEN
	CALL linep(ihp, x2, y2, x3, y3, 0, iwil, ico, 0)
      ELSE
	xs = (s2l - y2) * p4
	ys = y2 + xs * py / px
	CALL linep(ihp, x2 - xs, ys, x3, y3, 0, iwil, ico, 0)
	' PSET (x2, s2l), 11
	'PSET (x2, y2), 13
      END IF
      'PRINT y2, s2l, xs, px, y3
      'INPUT resp$

    END IF
  END IF
END IF
EXIT SUB
END SUB

'DEFBL A-G, O-Z
SUB linep (ihp, x1, y1, x2, y2, ipat, iwidth, ico, ityp) STATIC
' if ihP > 1 then plot in HPGL on #77 (from  sub gmanage())
'ipat: 0=linie; 1=Rechteck, 2=filled
'ityp = linetype 0=line ,1-4 dots etc., 5 every second half

'DIM linetype(4)
'linetype(1) = &HFFFF
'linetype(2) = &H4040
'linetype(3) = &HF0F0
'linetype(4) = &H8888
SHARED iwidthm, icomem, idxl, idxr, idyu, idyo, ipcs, ipcx
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
'LOCATE 1, 1: PRINT "a "; ihp; : INPUT resp$

SELECT CASE ipat
CASE 0' line
  SELECT CASE ityp'linetype
  CASE 2: LINE (x1, y1)-(x2, y2), ico, , &H4040
  CASE 3: LINE (x1, y1)-(x2, y2), ico, , &HF0F0
  CASE 4, 5:  'LINE (x1, y1)-(x2, y2), ico, , &H8888
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
      PSET (xl, yl), ico
      xl = xl + xr: yl = yl + yr
    NEXT i
    PSET (x2, y2), ico
  CASE ELSE   'normal line
SELECT CASE iwidth
CASE 0, 1
    LINE (x1, y1)-(x2, y2), ico
CASE 2, 3
xiwi = iwidth - 1
IF ABS(y2 - y1) < ABS(x2 - x1) THEN  '< 45ø
IF ABS(x2 - x1) < 1 THEN EXIT SUB 'line has no extension, may be should singe clircle
yl = y1
istep = 1
IF x2 < x1 THEN istep = -1
delta = (y2 - y1) / ABS(x2 - x1)
FOR ixl = x1 TO x2 + xiwi / 2 STEP istep
LINE (ixl, yl)-(ixl, yl + xiwi), ico
yl = yl + delta
NEXT
ELSE   '> 45ø
istep = 1
xl = x1 - iwidth / 2
IF y2 < y1 THEN istep = -1
IF ABS(y2 - y1) < 1 THEN EXIT SUB
delta = (x2 - x1) / ABS(y2 - y1)
FOR iyl = y1 TO y2 STEP istep
LINE (xl, iyl)-(xl + xiwi, iyl), ico
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
    LINE (xa1, ya1)-(xa2, ya2), ico
    LINE -(xb2, yb2), ico
    LINE -(xb1, yb1), ico
    LINE -(xa1, ya1), ico
    PAINT (x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2), ico
 END SELECT 'iwidth
 END SELECT 'ityp


CASE 1  'open rectangle

SELECT CASE iwidth
CASE 0, 1
  SELECT CASE ityp
  CASE 1: LINE (x1, y1)-(x2, y2), ico, B
  CASE 2: LINE (x1, y1)-(x2, y2), ico, B, &H4040
  CASE 3: LINE (x1, y1)-(x2, y2), ico, B, &HF0F0
  CASE 4: LINE (x1, y1)-(x2, y2), ico, B, &H8888
  END SELECT

CASE ELSE
iwl = iwidth - 1
LINE (x1 - iwl, y1)-(x2 + iwl, y1 - iwl), ico, BF
LINE (x2, y1 - iwl)-(x2 + iwl, y2 + iwl), ico, BF
LINE (x2 + iwl, y2)-(x1 - iwl, y2 + iwl), ico, BF
LINE (x1 - iwl, y2 + iwl)-(x1, y1 - iwl), ico, BF
END SELECT

CASE 2'  Rechteck voll
  LINE (x1, y1)-(x2, y2), ico, BF
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
	LINE (x1, y1)-(x21, y2), ico, BF
      CASE 7 TO 9
	FOR iyd = y1 TO y1 + idx1 STEP 1
	  LINE (x1, iyd)-(x21, iyd), ico, , &H5555
	  iyd = iyd + 1
	  LINE (x1, iyd)-(x21, iyd), ico, , &HAAAA
	NEXT iyd
      CASE 4 TO 6
	FOR iyd = y1 TO y1 + idx2 STEP 2
	  LINE (x1, iyd)-(x21, iyd), ico, , &H5555
	  iyd = iyd + 2
	  LINE (x1, iyd)-(x21, iyd), ico, , &HAAAA
	NEXT iyd
      CASE 1 TO 3
	FOR iyd = y1 TO y1 + idx2 STEP 2
	  LINE (x1, iyd)-(x21, iyd), ico, , &H1111
	  iyd = iyd + 2
	  LINE (x1, iyd)-(x21, iyd), ico, , &H4444
	NEXT iyd
      END SELECT 'shading

END SELECT'ipat

IF ihp = 0 OR ihp = 1 THEN EXIT SUB
  IF iwidth = 0 THEN iwidth = 1
  IF iwidth <> iwidthm THEN
  penwidth = .1 * iwidth
  PRINT #77, "PW"; penwidth
  iwidthm = iwidth
  END IF

 IF ico <> icomem THEN
' icohp = ico: GOTO weiter
    SELECT CASE ico
    CASE 0, 8: icohp = 1 'scharz
    CASE 1, 9: icohp = 5'blau
    CASE 2, 10: icohp = 3'grn
    CASE 12, 4, 6, 13: icohp = 2'rot
    CASE 3, 11: icohp = 7
    CASE 14: icohp = 4'gelb
    CASE ELSE
    icohp = 1
    END SELECT
'weiter:
'' versuchshalber
'icohp = ico
    PRINT #77, "SP"; icohp
    icomem = ico
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
'      PRINT #77, "PAPU"; xl; ","; yl; "PD"; xl + xr5; ","; yl + yr5
      xl = xl + xr: yl = yl + yr
    NEXT i
'      PRINT #77, "PAPU"; xl; ","; yl; "PD"; x2; ","; y2
'  PRINT #77, "PAPU "; PRINT #77;  USING "####.#";x1); ", "; PRINT #77;  USING "####.#";y1); "PD "; PRINT #77;  USING "####.#";x2); ","; PRINT #77;  USING "####.#";y2)
CASE 1 TO 3
  ilaa = ityp
    iilp = 1: PRINT #77, "LT", ilaa, filp
  PRINT #77, "PAPU "; : PRINT #77, USING "####.#"; x1; : PRINT #77, ", "; : PRINT #77, USING "####.#"; y1; : PRINT #77, "PD "; :
  PRINT #77, USING "####.#"; x2; : PRINT #77, ","; : PRINT #77, USING "####.#"; y2
 PRINT #77, "LT " 'sets line type back
CASE ELSE
'  PRINT #77, "PAPU"; x1; ","; y1; "PD"; x2; ","; y2
  PRINT #77, "PAPU "; : PRINT #77, USING "####.#"; x1; : PRINT #77, ", "; : PRINT #77, USING "####.#"; y1; :
  PRINT #77, "PD "; : PRINT #77, USING "####.#"; x2; : PRINT #77, ",";
  PRINT #77, USING "####.#"; y2
END SELECT 'itype
CASE 1  'offenes Rechteck
  PRINT #77, "PAPU"; x1; ","; y1; "PD";
  WRITE #77, x2, y1, x2, y2, x1, y2, x1, y1
CASE 2 'geschlossenes Rechteck
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

'DEFBL A-G, O-Z
SUB paramenu STATIC'=======================================================-
'Allows to change floating point parameters (for calculation)
'               or of integer parameters (for program flow)
'  or inputting a command. Control variable:   PCONTROL$
'  e.g. name of the parameter to be changed or command
'--------------------------------------------------------------------------
'   inv(): temporary storage of integer values KT, KP .. KG, K1..
'   flv():        ..    floating point values DA, RA ..
'   parnam$(): contains the names of the variables for identification
'''$INCLUDE: 'fmshared.bi'


SHARED programloc$, curdrive$, aprodir$, apardir$, pdir$, pdri1$, pardir$, pcx$
SHARED pcontrol$, pcontrol2$, comment$, icheckerror, openfilename$, irfilenr$, ilineparam
SHARED programname$, ipcontrol, filemessage$, iparam, readdatafile$, writedatafile$
SHARED basfilename$, modified$, filename$, lpt$, ptype$, title$, iwohin, dirtemp$
'for graphic
SHARED ipcx, ipcs, idxr, idxl, idyu, idyo
SHARED igt, iactive
SHARED itot&, tvv$, label$(), displaywas$, displaywastmp$
SHARED ihp, dxp, dyp, fkasten, d, per, krot, ivgashift, ianz, ireaddata
SHARED ivorcol, ibackcol, igraph, istelz, iletztk, ihintr, igtdelay
SHARED fdelay, iwidthm, exposure, ifree
'for parameter
SHARED fparnam$(), iparnam$(), flv(), inv(), s$(), sl$() 'for parameter
SHARED iplotsubst, icmdlin, ixmax, iymax, persp, imx, imy, ilm
SHARED diffmax, imxl, imyl
SHARED kt, kp, kx, ky, kd, ki, ke, kr, kn, kg, ks
SHARED k1, k2, k3, k4, k5, k6, k7, k8, k9, k0, dx, dy, dz
SHARED da, ra, ba, sa, ca, aa, ga, qa, ta, ua, db, rb, bb, sb, cb, ab, gb, qb, tb, ub
SHARED dc, rc, bc, sc, cc, ac, gc, qc, tc, uc, dd, rd, bd, sd, cd, ad, gd, qd, td, ud
SHARED de, re, be, se, ce, ae, ge, qe, te, ue, df, rf, bf, sf, cf, af, gf, qf, tf, uf
SHARED dg, rg, bg, sg, cg, ag, gg, qg, tg, ug, dh, rh, bh, sh, ch, ah, gh, qh, th, uh
SHARED di, ri, bi, si, ci, ai, gi, qi, ti, ui, dj, rj, bj, sj, cj, aj, gj, qj, tj, uj



DIM printeq$(50)
  a$ = "KT,KP,KX,KY,KD,KI,KE,KR,KN,KG,K1,K2,K3,K4,K5,K6,K8,K8,K9,K0,"
  FOR i = 1 TO 20: iparnam$(i) = MID$(a$, (i - 1) * 3 + 1, 2): NEXT i

  a$ = "DRBSCAGQTU"
  FOR il = 1 TO 10
  FOR i = 1 TO 10 '
  fparnam$(il, i) = MID$(a$, i, 1) + CHR$(64 + il)
  NEXT i: NEXT il
  fparnam$(0, 1) = "DX": fparnam$(0, 2) = "DY": fparnam$(0, 3) = "DZ"
resp$ = ""
ifound = 0

IF kn = 0 THEN 'initialization after program start
   irfilenr$ = programname$ + ptype$ + "1" + ".prm"
   filenametmp$ = apardir$ + programname$ + ptype$ + "1" + ".prm"
   progrtmp$ = programname$
IF ifree = 1 THEN COLOR 1, 15
iparam = 1
     GOTO readfilenow
END IF

parametermenu:
totaltime = TIMER - t1
IF iparam < 1 THEN CALL prtscreen("..\iniscr.txt")
showparameterlist: '--------- formatted print of parameters on screen
IF iparam <= 1 THEN
    CALL rwparameterdisk("PS", 8)
END IF

IF iparam = 2 GOTO parameinput'only bottom line is shown
IF inv(9) < 1 THEN inv(9) = 2
IF inv(9) > 10 THEN
mess$ = "please not more then 10 substances (KN <= 10)"
CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, "OK")
END IF
'GOTO screenprintparameter
parameinput:
pdirectinput = -1 'to find and input of the type "KT = 1"
IF igt > 0 THEN
CALL gtourcontrol(pcontrol$, igt, label$())  'if GUIDED TOUR is on
'IF igt = 0 THEN GOTO parameinput
ELSE
    IF PCOMMAND$ > "" THEN
     pcontrol$ = PCOMMAND$
     PCOMMAND$ = ""
    ELSE
     pcontrol$ = ""
     CALL zeingabe(igt, 0, 3, i, dummy, pcontrol$, "->")
  END IF
END IF
usepcontrol:
pcontrol$ = UCASE$(pcontrol$)'------------------ all in uppercase letters
pcleft$ = "": t1 = TIMER
INS = 0: ipcontrol = 0: pcontrol2$ = ""
ipu = INSTR(pcontrol$, "=")
IF ipu > 0 THEN  'e.g., KT=12 is also a legal input, used for menu
pdirectinput = VAL(MID$(pcontrol$, ipu + 1, 12))
pcontrol$ = MID$(pcontrol$, 1, ipu - 1)
END IF

IF pcontrol$ = "" THEN
iparam = iparam - 1: GOTO parametermenu

END IF
FOR i = 1 TO 20'----- Find the parameter name in parnam$(INS)
  IF pcontrol$ = iparnam$(i) THEN
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
  IF pcontrol$ = fparnam$(il, i) THEN
   IF pdirectinput = -1 THEN
    CALL zeingabe(igt, 1, 2, idummy, flv(il, i), dummy$, fparnam$(il, i))

    ELSE
    flv(il, i) = pdirectinput
    END IF
    IF LEFT$(pcontrol$, 1) = "D" AND flv(il, i) > diffmax AND il > 0 THEN
       mess$ = "to avoid numerical instability, diffusion must be < " + STR$(diffmax) + ", corrected"
       CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "")
       flv(il, i) = diffmax
      END IF
    modified$ = ", modified             ": comment$ = ""
  GOTO setparameter
  END IF
  NEXT i: NEXT il


SELECT CASE pcontrol$
CASE ""'--- input "RETURN" for full menu
  iparam = iparam - 1: GOTO parametermenu



CASE "NT": ipcx = 0: ipcs = -1: igraph = 12
CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)
idxr = 1: idxl = 640: idyu = 479: idyo = 15
pcontrol$ = "N": GOTO leave

CASE "NC"
 igraph = 12: mess$ = "Filename of the PCX-file"
 CALL zeingabe(igt, 0, 3, istelz, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 ELSE
 pcx$ = resp$ + "-": ipcx = 1: ipcs = -2
 END IF
pcontrol$ = "N": GOTO leave

CASE "P"
      CALL PCXOUT(ipcx, ipcs)
'      ELSE
'      IF ipcs> -2 THEN CALL PCXOUT(-2, 0)
CASE "DPweg##"
 igraph = 12
' idxr = 1: idxl = 640: idyu = 479: idyo = 15
 IF ipcs < 1 THEN
 mess$ = "Filename of the PCX-file"
 CALL zeingabe(igt, 0, 3, istelz, dxp, resp$, mess$)
 IF resp$ = "" OR resp$ = CHR$(27) THEN
 ELSE
 pcx$ = resp$ + "-": ipcs = 1: ipcx = -1
 END IF
 END IF
GOTO leave


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
  CALL zeingabe(igt, 1, 3, i, d, ptype$, "ptype")
  ptype$ = UCASE$(ptype$)
CASE "DW"
  CALL zeingabe(igt, 1, 3, i, d, displaywas$, "display what (sab...) ")
   displaywas$ = LCASE$(displaywas$)
  GOTO setparameter

CASE "FDELAY"
	 mess$ = "Delay in the between the displays, 0.1 is reasonable"
	 CALL zeingabe(igt, 1, 2, i, fdelay, resp$, mess$)
CASE "IGTDELAY"
    CALL zeingabe(igt, 1, 1, igtdelay, d, dummy$, "Delay in the GUIDED TOUR; in .1 seconds")

CASE "M4", "M5"
	SELECT CASE programname$
	CASE "SP", "XT"
	    IF pcontrol$ = "M4" THEN ipcontrol = 104
	    IF pcontrol$ = "M5" THEN ipcontrol = 105
	  pcontrol$ = "D"
	  EXIT SUB

	CASE ELSE
	    displaywastmp$ = ""
	    IF pcontrol$ = "M4" THEN ipcontrol = 24
	    IF pcontrol$ = "M5" THEN ipcontrol = 25
	    pcontrol$ = "D"
	    GOTO leave
	END SELECT


CASE "T": CALL cmessage(6, -1, 1, 15, 4, ibackcol, "Execution time was " + STR$(totaltime), "")

CASE "VGA"
  igraph = 12: CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)
CASE "EGA"
  igraph = 9: CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)

CASE "SCREEN", "SCR"

IF pdirectinput > 0 THEN
igraph = pdirectinput:
ELSE
mess$ = "Screen, 1:VGA, 2:XGA, 3:SXGA; 5-7:XGA-Pixel"
IF ifree = 3 THEN mess$ = "Screen: [return]: = VGA; 9: = EGA"
  CALL zeingabe(igt, 1, 1, igraph, d, dummy$, mess$)
END IF
  pdirectinput = 0: IF igraph = 0 THEN igraph = 12
  CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)

CASE "PE", "PEP", "PEF"
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
IF pcontrol$ = "PEF" OR pcontrol$ = "PEP" THEN
  CALL zeingabe(igt, 0, 3, i, d, resp$, "filename for saving equation and parameter " + dirtemp$)
  IF resp$ = "" OR resp$ = CHR$(27) GOTO parametermenu
  icheckerror = 0
'  ON ERROR GOTO checkerror
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
  icheckerror = 0
  ON ERROR GOTO checkerror
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
  CALL zeingabe(igt, 1, 3, i, d, pdir$, "Directory of parameter files ")
  IF RIGHT$(pdir$, 1) <> "\" THEN pdir$ = pdir$ + "\"

CASE "CLS":  iparam = 2:
  LINE (1, 1)-(1640, 1480), ibackcol, BF


CASE "MENU"
  pcontrol$ = ""
  CALL whatprmfiles(sl$(), programname$, "*", "HLM", aprodir$ + "..\help\", irfilenr$, ilineparamL)
  IF irfilenr$ = "q" THEN GOTO parametermenu
  OPEN aprodir$ + "..\help\" + irfilenr$ + ".HLM" FOR INPUT AS #2
  LINE INPUT #2, mess$: i = 1
  DO UNTIL EOF(2)
    LINE INPUT #2, sl$(i)
    IF sl$(i) = "" THEN EXIT DO
    i = i + 1
  LOOP
  CLOSE #2
  sl$(i) = ""
  CALL smenu(sl$(), pcontrol$, iline, mess$)
  IF pcontrol$ = "q" THEN pcontrol$ = ""
  IF RIGHT$(pcontrol$, 1) = "." THEN
	CALL zeingabe(igt, 0, 3, i, dummy, resp$, " substance to which the change should apply, a,b,...    ")


'    mess$ = SPACE$(10) + "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
'    Call ColorPrint (mess$, 15, 4, 15, 1)
'    mess$ = SPACE$(10) + "³  substance to which the change should apply, a,b,...     ³"
'    Call ColorPrint (mess$, 16, 4, 15, 1)
'    mess$ = SPACE$(10) + "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
'    Call ColorPrint (mess$, 17, 4, 15, 1)
'    resp$ = "": DO UNTIL resp$ > "": resp$ = INKEY$: LOOP

'    CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)
    CLS
    iparam = 0
    pcontrol$ = LEFT$(pcontrol$, 1) + resp$
  END IF
  iparam = 1
  GOTO usepcontrol

CASE ELSE
	pcleft$ = LEFT$(pcontrol$, 1)'number behind display can be used
	pcontrol2$ = MID$(pcontrol$, 2, 3)
	ipcontrol = VAL(pcontrol2$)

SELECT CASE pcleft$

CASE "D", "A"
 idxr = 1: idxl = 640: idyu = 479: idyo = 15
  IF ipcontrol > 0 THEN
    pcontrol$ = pcleft$
    GOTO leave
  END IF
CASE "L"
    displaywastmp$ = "": pcontrol$ = "D"
    GOTO leave
CASE "M"
  pcontrol$ = pcleft$
  GOTO leave

CASE "R", "S", "J"
  SELECT CASE pcontrol$
    CASE "RKILL": KILL filenametmp$: GOTO parametermenu
    CASE "RR"': GOTO readfilenow
    CASE "R"      'F2: list of parameter

      resp$ = "q"
      CALL zeingabe(igt, 0, 3, i, dummy, resp$, "Chapter [" + ptype$ + "] or RETURN; q or ESC => skip")

	  SELECT CASE resp$
	  CASE "q", "Q": GOTO parameinput
	  CASE " ": progrtmp$ = programname$
	  CASE "": progrtmp$ = programname$ + ptype$
	  CASE ELSE: progrtmp$ = programname$ + resp$
	  END SELECT
whatprmfileslabel:
       progrtmp$ = LCASE$(progrtmp$)
       CALL whatprmfiles(s$(), programname$, progrtmp$, "prm", apardir$, irfilenr$, ilineparam)
	  IF irfilenr$ = "q" THEN iparam = 1: GOTO parametermenu
	     filenametmp$ = apardir$ + irfilenr$ + ".prm"

CASE "RZ"     ' xxxx
    OPEN filenametmp$ FOR INPUT AS #9
    CALL rwparameterdisk("RA", 9)
    CLOSE (9)
    GOTO setparameter

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
     irfilenr$ = MID$(pcontrol$, 2, 6)
     progrtmp$ = programname$
      IF irfilenr$ > "" THEN
       IF LEFT$(irfilenr$, 1) >= "0" AND LEFT$(irfilenr$, 1) <= "9" THEN irfilenr$ = ptype$ + irfilenr$
       irfilenr$ = progrtmp$ + irfilenr$': GOTO readfilenow
      END IF
   irfilenr$ = LCASE$(irfilenr$)
   filenametmp$ = apardir$ + irfilenr$ + ".prm"
 END SELECT
readfilenow: 'xxrr
''ptype$ = MID$(irfilenr$, 3, 2)
  icheckerror = 0
  ON ERROR GOTO checkerror 'xxn
OPEN filenametmp$ FOR INPUT AS #9
  IF icheckerror = 1 THEN
  mess$ = " the file " + filenametmp$ + " does not exist. You like a list? "
  resp$ = "y/n "
  CALL cmessage(4, -1, 1, 4, 15, ibackcol, mess$, resp$)
    IF resp$ = "y" THEN GOTO whatprmfileslabel
'    IF resp$ = "y" THEN progrtmp$ = "": GOTO whatprmfileslabel
   GOTO parametermenu
  END IF
  filename$ = filenametmp$
  IF LEFT$(ptype$, 1) < "A" THEN ptype$ = ""
'  xfilenametmp$ = filename$
'
'   openfilename$ = filename$
'  filemessage$ = filename$
CALL rwparameterdisk("R", 9)
  CLOSE (9)'ivgashift
  comment$ = "read from file "
  modified$ = "":
  ON ERROR GOTO 0
  IF igt <> 1 THEN iparam = iparam - 1
  GOTO setparameter

CASE "W" 'xxww

  SELECT CASE pcontrol$
    CASE "WW": GOTO writefilenow
'    CASE "W0": filenametmp$ = apardir$ + ptype$ + "0":  GOTO writefilenow
    CASE ELSE        'FOR CASES LIKE W1
      irfilenr$ = MID$(pcontrol$, 2, 6)
      progrtmp$ = programname$
      IF LEFT$(irfilenr$, 1) >= "0" AND LEFT$(irfilenr$, 1) <= "9" THEN irfilenr$ = ptype$ + irfilenr$
      irfilenr$ = progrtmp$ + irfilenr$': GOTO readfilenow

   irfilenr$ = LCASE$(irfilenr$)
   filenametmp$ = apardir$ + irfilenr$ + ".prm"
''locate 25,1: print "for write", filenametmp$;: input;"[ R ]", yyy$

    IF pcontrol$ = "W0" THEN GOTO writefilenow
    icheckerror = 0
    ON ERROR GOTO checkerror 'xxn
    OPEN filenametmp$ FOR INPUT AS #9
    IF icheckerror = 0 THEN
    dummy$ = "": LINE INPUT #9, dummy$
    CLOSE (9)
    mess$ = filenametmp$ + " exists [ " + LEFT$(dummy$, 20) + "...]; Overwrite? "
    resp$ = "y/n "
    CALL cmessage(4, -1, 1, 4, 15, ibackcol, mess$, resp$)
    IF resp$ <> "y" THEN
      mess$ = "You will get a list of existing files, later input a new file name"
      resp$ = "OK": CALL cmessage(4, -1, 1, 4, 15, ibackcol, mess$, resp$)
      IF resp$ = CHR$(27) GOTO parametermenu
       CALL whatprmfiles(s$(), programname$, progrtmp$, "prm", apardir$, irfilenr$, ilineparam)
	iparam = 1
	      mess$ = "Type another file name by using w...."
	      CALL cmessage(5, -1, 1, 4, 15, ibackcol, mess$, resp$)
	GOTO parametermenu
      ELSE
      'overwrite
      GOTO writefilenow
     END IF
     END IF
  END SELECT
writefilenow:
'Editing noch rein CALL zeingabe(igt, 0, 3, i, dummy, pcontrol$, "->")
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
  filenametmp$ = LCASE$(filenametmp$)
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

gtfilename$ = MID$(pcontrol$, 3, 8)     ' opening of gm1.gt
IF gtfilename$ > "" THEN
  gtfilename$ = pdir$ + programname$ + gtfilename$ + ".GT"
  GOTO readgtnow
  END IF
END SELECT

checkpcontrol:
CALL gmanage(pcontrol$)
'' CALL rwdata(PCONTROL$, displaywas$) not in use
IF pcontrol$ = "" GOTO parametermenu
mess$ = pcontrol$ + " is a nonsense input, try again or use F1"
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

i = 9: di = flv(i, 1): ri = flv(i, 2): bi = flv(i, 3): si = flv(i, 4): ci = flv(i, 5)
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
IF pcontrol$ = "D" THEN EXIT SUB
  IF pcleft$ = "J" THEN pcleft$ = "I"
   IF pcleft$ = "S" OR pcleft$ = "I" THEN
    pcontrol$ = pcleft$
    EXIT SUB   'direct start after reading a parameter file
  '### correct?
  END IF
GOTO parametermenu

leave: '-------------to be executed before  going back to the main program----
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
'locate 1,1: print "ok now? ": input yyy$
CLOSE (8)
GOTO parameinput

FindEquation:
 icheckerror = 0
'  ON ERROR GOTO checkerror 'xxn
IF ifree = 1 THEN COLOR 1, 15:
LOCATE 1, 1
OPEN aprodir$ + basfilename$ FOR INPUT AS #2
IF icheckerror = 1 THEN
    mess$ = " the file " + aprodir$ + basfilename$ + " does not exists"
    CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, resp$)
    GOTO parametermenu
END IF

DO UNTIL EOF(2)
  LINE INPUT #2, sl$
  sl$ = UCASE$(sl$)
  ipu = 1: ipu = INSTR(ipu, sl$, such$)
  IF ipu > 0 THEN
  EXIT DO
  END IF
  LOOP
such$ = "CASE " + LTRIM$(STR$(inv(7))) + " '" + UCASE$(ptype$)
 DO UNTIL EOF(2)
  LINE INPUT #2, sl$
  sl$ = UCASE$(sl$)
  ipu = 1: ipu = INSTR(ipu, sl$, such$)
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
i = 1: printeq$(i) = sl$
	DO UNTIL EOF(2)
	  LINE INPUT #2, sl$
	  ipu = 1: ipu = INSTR(ipu, sl$, "CASE")
	  IF ipu = 0 THEN i = i + 1: printeq$(i) = sl$
	  IF ipu > 0 THEN EXIT DO
	LOOP
END IF
CLOSE (2)
GOTO equationfound

END SUB'===========end of sub paramenu()  ========================

SUB PCXOUT (ipcx, ipcs) ' meine private Variante
SHARED idxr, idyu, idxl, idyo, dxp, dyp, pcx$, dirtemp$
'fuer pcx:
'ipcs file number, >0 to be on
'ipcx for continous->animated
IF ipcx < 0 AND ipcs < -1 THEN EXIT SUB ' im Normalfall keine Umrandung
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
SUB perpendicular (x0, y0, ybase0, s0l, s0r, ico, iwil)
SHARED ihp

IF y0 > s0r THEN 'line starts
  yba = ybase0
  IF s0l > yba THEN yba = s0l
  IF s0r > yba THEN yba = s0r
  IF yba < y0 THEN CALL linep(ihp, x0, y0, x0, yba, 0, iwil, ico, 0)
END IF
END SUB

SUB polygon (ihp, ix1, iy1, ipat, iwidth, icol, itypl) STATIC
SHARED msiluet(), msiluetmax
SHARED ipcs, ipcx, idxr, idxl, idyo, idyu

ON ERROR GOTO 0 '####
IF ipcs > -2 THEN
	IF ix1 < idxl THEN idxl = ix1
	IF ix1 > idxr THEN idxr = ix1
	IF iy1 < idyu THEN
	idyu = iy1
	END IF
	IF iy1 > idyo THEN idyo = iy1
END IF
icomem = icol: icorand = 0: IF icol = 16 THEN icomem = 2: icorand = 12
SELECT CASE ipat
CASE 1' initiation
ixmem = ix1: iymem = iy1
PSET (ixmem, iymem), icol
'msiluet(i) = ix1: msiluet(i + ms2) = iy1'
CASE 2' draw false color line
'i = i + 1: msiluet(i) = ix1: msiluet(i + ms2) = iy1
LINE -(ix1, iy1), icol
CASE 3' obsolete
CASE 4' fill and draw
'PSET (msiluet(1), msiluet(1 + ms2)), icorand
'FOR iplot = 2 TO i
'LINE -(msiluet(iplot), msiluet(iplot + ms2)), icorand
'NEXT iplot
'LINE -(msiluet(1), msiluet(1 + ms2)), icorand
LINE -(ixmem, iymem), icol
'pset (ix1,iy1),icol

'sleep 2000
PAINT (ix1, iy1), icol, icol
'sleep 2000
CASE 5 'Obsolete
icorand = icomem' Removal of the tetative lines
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

CASE 4' ausmalen
itype = 80: IF ihp < 6 THEN itype = 100 - 15 * icol
 IF itype < 10 THEN itype = 10
IF icol = 15 THEN itype = 0
PRINT #77, "ft 10"; itype 'Fill type
PRINT #77, "fp"        'fill
END SELECT
END SUB

'DEFBL A-G, O-Z
SUB prtscreen (scrfile$)
OPEN scrfile$ FOR INPUT AS #2
IF icheckerror = 1 THEN
    mess$ = " the file " + pardir$ + scrfile$ + " does not exists"
    CALL cmessage(6, -1, 12, 15, 4, ibackcol, mess$, resp$)
ELSE
' ## COLOR 2, 15
izeile = 1
DO
LINE INPUT #2, mess$
IF EOF(2) THEN EXIT DO
CALL cmessage(3, izeile, 1, 2, 15, ibackcol, mess$, resp$)
IF izeile = 18 + ivgashift THEN EXIT DO
izeile = izeile + 1
LOOP
END IF
CLOSE (2)
END SUB

'DEFBL A-G, O-Z
SUB rl (inz, x2, y2, s2, x1, yto, s1, y3, isi, iy, jyto, ico, iwil, px)
SHARED msiluet(), krot, ihp

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
      CALL linep(ihp, xstart, ystart, xs, ys, 0, iwil, ico, 0)
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
	CALL linep(ihp, x2, y2, x2, y2, 0, iwil, ico, 0)' line after krot-loop
      END IF
    END IF
  END IF
nextimodus:
  y2 = y1'old left point becomes the new right point
  x2 = x1
  s2 = s1
NEXT i
IF ivisible = 1 THEN CALL linep(ihp, xstart, ystart, x1, y1, 0, iwil, ico, 0)
EXIT SUB
END SUB

'DEFBL A-G, O-Z
SUB rwparameterdisk (RW$, ifilenumber)
SHARED fparnam$(), iparnam$(), flv(), inv()
SHARED gxa(), gya(), gfa(), itypus(), ico(), iwi()
SHARED displaywas$, dxDUMMY, dxp, dyp, per, fkasten
SHARED krot, ivorcol, ibackcol, istelz, ihintr
SHARED filemessage$, igraph, ivgashift, ifree
SHARED basfilename$, comment$, filename$, modified$
SHARED curdrive$, programloc$, pdir$

'PRINT basfilename$; openfilename$: INPUT resp$

SELECT CASE RW$ 'xxrw
'xfilenametmp$
CASE "R", "RA"
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
  'PRINT i; gxa(i); gya(i); gfa(i); itypus(i), ico(i); iwi(i); : INPUT resp$
NEXT
  INPUT #ifilenumber, displaywas$, dxDUMMY, dxp, dyp, per, idosgraph
  ''PRINT displaywas$, dxDUMMY, dxp, dyp, per, fkasten; : INPUT resp$
  INPUT #ifilenumber, krot, ivorcol, ibackcol, istelz, ihintr
    IF EOF(ifilenumber) THEN
	igraph = 12
	ELSE
      INPUT #ifilenumber, igraph
    END IF
    IF idosgraph > 2 THEN igraph = idosgraph

CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)

CASE "W"
  idosgraph = igraph
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
  PRINT #ifilenumber, igraph
  WRITE #ifilenumber, krot, ivorcol, ibackcol, istelz, ihintr
  WRITE #ifilenumber, igraph


 CASE "PS"
  FOR i = 19 - inv(9) + ivgashift TO 25 + ivgashift
  LOCATE i, 1: PRINT SPACE$(80); : NEXT i
  LOCATE 21 - inv(9) + ivgashift, 1

'printparameter on sreen:
PRINT "Progr.: "; basfilename$; "; "; comment$; curdrive$ + programloc$ + filename$; modified$; ""
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

DEFINT H
'DEFBL A-G, O-Z
SUB smenu (s$(), irfilename$, iline, headline$)
SHARED igraph, dirtemp$
''$INCLUDE: 'fmshared.bi'

SHARED ivgashift, lpt$, icheckerror
SCREEN 0
ivgashift = 0
CLS
'lenght of menu lines
izlenmax = LEN(headline$) + 4
FOR ial = 1 TO 150
izlen = LEN(s$(ial))
IF izlen = 0 THEN EXIT FOR
IF izlen > 75 THEN izlen = 75: s$(ial) = LEFT$(s$(ial), 71) + " ..."
IF izlen > izlenmax THEN izlenmax = izlen
NEXT ial
iarmax = ial - 1: iline = 1: ivon = 1:
smenunew:
COLOR 15, 1
LOCATE 1, 1
PRINT "ÚÄ  " + headline$ + " " + STRING$(izlenmax - LEN(headline$) - 2, 196) + "¿"
ibis = ivon + 21: IF ibis > iarmax THEN ibis = iarmax
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
LOCATE 25, 1: PRINT "Use arrows, Pg-UP and PG-DOWN to select, RETURN to take, ESC to quit, p= file";
'selectline
COLOR 1, 15'2, 0
LOCATE 2 + iline - ivon, 3
ioldline = iline
PRINT s$(iline) + SPACE$(izlenmax - LEN(s$(iline)))
i$ = ""
WHILE i$ = ""
i$ = INKEY$
WEND
i = ASC(RIGHT$(i$, 1))
SELECT CASE i
CASE 27, 113: irfilename$ = "q": GOTO endsmenu
CASE 80, 32: IF iline < iarmax THEN iline = iline + 1
IF iline > ibis THEN
ivon = ivon + 22
IF ivon > iarmax - 21 THEN ivon = iarmax - 21
IF ivon < 1 THEN ivon = 1
iline = ivon
END IF
GOTO smenunew
CASE 72: IF iline > 1 THEN iline = iline - 1
IF iline < ivon THEN ivon = ivon - 22
IF ivon < 1 THEN ivon = 1
GOTO smenunew
CASE 81'pg-down
ivon = ivon + 22
iline = ivon
IF ivon > iarmax - 21 THEN
ivon = iarmax - 21
IF ivon < 1 THEN ivon = 1
iline = iarmax
END IF
GOTO smenunew
CASE 73'pg-up
ivon = ivon - 22
IF ivon < 1 THEN ivon = 1
iline = ivon
GOTO smenunew
CASE 13
ipu = INSTR(s$(iline), " ")
irfilename$ = LEFT$(s$(iline), ipu - 1)
REM
GOTO endsmenu
CASE 112
icheckerror = 0
ON ERROR GOTO checkerror' sm
   dummy$ = dirtemp$ + "parafile.txt"
   OPEN dummy$ FOR OUTPUT AS #8
   IF icheckerror = 1 THEN
   irfilename$ = "q":
   GOTO smenunew
   EXIT SUB
   END IF
'PRINT #8, "ÚÄ  " + headline$ + " " + STRING$(izlenmax - LEN(headline$) - 2, 196) + "¿"
 PRINT #8, "--  " + headline$ + " " + STRING$(izlenmax - LEN(headline$) - 2, "-") + "-"
FOR ial = 1 TO iarmax
'PRINT #8, "³ " + s$(ial) + SPACE$(izlenmax - LEN(s$(ial))) + " ³"
PRINT #8, "- " + s$(ial) + SPACE$(izlenmax - LEN(s$(ial))) + " "
NEXT ial
'PRINT #8, "ÀÄ" + STRING$(izlenmax, 196) + "ÄÙ"
PRINT #8, "--" + STRING$(izlenmax, "-") + "--"
'CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, "OK")
COLOR 15, 2
LOCATE 25, 1: PRINT "the parameterlist is written: "; dummy$; : INPUT ; "                      [OK]", resp$

   CLOSE #8
GOTO smenunew
CASE ELSE
BEEP
END SELECT
LOCATE 2 + ioldline - ivon, 3
PRINT s$(ioldline) + SPACE$(izlenmax - LEN(s$(ioldline)))
LOOP
endsmenu:
CALL screencheck(igraph, lastline, lastrow, ivgashift, ifree)
EXIT SUB

checkerrsm:
icheckerror = 1
RESUME NEXT
END SUB

'DEFBL A-G, O-Z
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

SUB switchscreen (inz, iactive, igraph, kd, programname$, fdelay) STATIC
'switch between two screens for screen 9
IF igraph <> 9 THEN EXIT SUB
IF programname$ = "XT" OR programname$ = "SP" THEN
SELECT CASE kd
CASE 3, 16, 17, 18, 19, 191, 21, 22, 23, 24, 25, 26, 28, 29, 30
END SELECT
END IF
REM
SELECT CASE inz
CASE 1'initiation
time1 = TIMER
	SELECT CASE iactive
	CASE 0: SCREEN 9, , 1, 0: iactive = 1'activ,visible
	CASE ELSE: SCREEN 9, , 0, 1: iactive = 0
	END SELECT
CASE 2'switch
IF fdelay > 0 THEN DO UNTIL TIMER - time1 > fdelay: LOOP: time1 = TIMER
	SELECT CASE iactive
	CASE 0: SCREEN 9, , 1, 0: iactive = 1
	CASE 1: SCREEN 9, , 0, 1: iactive = 0
	END SELECT
IF fdelay > 0 THEN DO UNTIL TIMER - t1 > fdelay * fdelnotebook: LOOP
	t1 = TIMER
	REM
CASE 3 'end
    SELECT CASE iactive
	CASE 1: SCREEN 9, , 0, 0: iactive = 0
	CASE 0: SCREEN 9, , 1, 1: iactive = 1
      END SELECT
CASE 4'initiation, visible
	SELECT CASE iactive
	CASE 1: SCREEN 9, , 0, 0: iactive = 0
	CASE ELSE: SCREEN 9, , 1, 1: iactive = 1
	END SELECT
END SELECT
END SUB

'DEFBL A-G, O-Z
SUB titles (inz, title$, mess$)
  ishift = (74 - (LEN(mess$))) / 2: IF ishift < 1 THEN ishift = 1
  title$ = SPACE$(ishift) + mess$
END SUB

'DEFBL A-G, O-Z
SUB whatprmfiles (s$(), programnameloc$, progrtmp$, exten$, drive$, irfilename$, ilineparam)

''$INCLUDE: 'fmshared.bi'
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
dummy$ = drive$ + progrtmp$ + "*." + exten$
'locate 1,1: print "dummy$", dummy$;: sleep
s1$ = DIR$(drive$ + progrtmp$ + "*." + exten$)   '###xxxyy
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
LINE INPUT #2, si$
ke = 0:
IF exten$ = "PRM" OR exten$ = "prm" THEN INPUT #2, kt, kp, kx, ky, kd, ki, ke
CLOSE #2
icount = icount + 1
ipu = 1: ipu = INSTR(ipu, s1$, ".")
s1$ = MID$(s1$, LEN(progrtmp$) + 1, ipu - LEN(progrtmp$) - 1)
s1$ = s1$ + "        "
kestring$ = "": IF ke > 0 THEN kestring$ = STR$(ke)
IF LEFT$(kestring$, 1) = " " THEN kestring$ = MID$(kestring$, 2, 5)
kestring$ = MID$(kestring$ + "     ", 1, 5)
s$(icount) = MID$(s1$, 1, 9) + kestring$ + si$
IF VAL(MID$(s$(icount), 1, 8)) < 10 THEN
  s$(icount) = ".." + s$(icount)
 ELSEIF VAL(s$(icount)) < 100 THEN
  s$(icount) = LEFT$(s$(icount), 1) + "." + MID$(s$(icount), 2, 250)
 END IF
END IF

s1$ = DIR$
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

'DEFBL A-G, O-Z
SUB zeingabe (igt, iquest, inz, i, f, labels$, text$)
'allows inputting single constants, if igt>O from file (guided tour)
'iquest =1: original value is prompted
'inz=1 integer i: inz=2 floating point f, inz=3: string labels$
'text$ is the message displayed with the request
'---------------------------------------------------------
SHARED ivgasift, igtdelay, pcontrol$
textprint$ = text$ + " "
IF iquest = 1 THEN
  textprint$ = textprint$ + " OLD = "
  IF inz = 1 THEN textprint$ = textprint$ + STR$(i)
  IF inz = 2 THEN
  textprint$ = textprint$ + STR$(f)
  END IF
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
'IF dummy$ = CHR$(27) THEN labels$ = CHR$(27): GOTO zende
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
CASE 68: fac$ = "SN"
CASE 68: fac$ = "SN"
CASE 133: fac$ = "SN"
CASE 134: fac$ = "FDELAY"
END SELECT
labels$ = fac$
EXIT SUB
END IF
fac$ = fac$ + dummy$
CALL cmessage(1, -1, ilength, 12, 15, ibackcol, fac$ + "_  ", resp$)
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
'##COLOR 1, 15
END SUB

