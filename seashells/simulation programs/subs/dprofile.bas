DECLARE SUB type3frame (ihp%, dx!, xo!, y2!, y3!, y4!, jja%, jjs%, ihintr%, iwil%)
DECLARE SUB showprof (inzloc%, iendmloc%, dwloc$, axt!(), jaloc%, jsloc%, iccloc%)
DECLARE SUB init (inewscreen%, igraph%, ivorcol%, ibackcol%, ivgashift%)
DECLARE SUB zeingabe (igt%, iquest%, inz%, i%, f!, labels$, text$)
DECLARE SUB clearscreen ()
DECLARE SUB polygon (ihpl%, ix1%, iy1%, ipat%, iwidth%, ico%, itypl%)
DECLARE SUB prof (axt!(), illoc%, dwloc$, ipatternloc%, inzloc%, iendmloc%, dxloc!, ffloc!, jaloc%, jsloc%, iyloc%, xoloc!, yoloc!)
DECLARE SUB apnp (ihpp%, xal!, yal!, lp%, iwidth%, ico%, ityploc%)
DECLARE SUB clsscr (ipcs%, ipcx%, ibackcol%)
DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DECLARE SUB linep (ihp%, x1!, y1!, x2!, y2!, ipat%, iwidth%, ico%, ityp%)
DEFINT H-N

' $INCLUDE: 'scommon.bi'
' $INCLUDE: '..\subs\errlist.bas'

SUB manipulationxt (pcontroloc$, kinsert) STATIC
SHARED lax(), lx(), lay(), ly()
SHARED ivgashift, displaywas$, iparam, itot&, k1, k2, igt, iactive, ptype$
SHARED axy(), axt(), ja, js, kx, kay, ky, kd, kn, dz, imx, imy, ilm, imxl, icc
SHARED mamessage$
imxl2 = imxl / 2
mamessage$ = ""
genedw$ = "": ipu = INSTR(displaywas$, "/")
IF ipu > 0 THEN genedw$ = MID$(displaywas$, ipu + 1, 20)

manipulationstartxt:
SELECT CASE pcontroloc$
CASE "Q": EXIT SUB
CASE "S"'to store present concentrations in memory arrays
ksmem = js
kamem = ja
iccmem = icc
IF js + imxl2 > imxl THEN
 CALL cmessage(6, -1, 1, 15, 4, ibackcol, "sorry, XT-array too large", "OK")
 EXIT SUB
END IF

FOR il = 0 TO kn
  FOR i = ja TO js
  axt(il, i + imxl2) = axt(il, i)
  NEXT i: NEXT il

IF genedw$ > "" THEN
IF 2 * ly(1) > imy THEN
 CALL cmessage(6, -1, 1, 15, 4, ibackcol, "sorry, XY-array too large", "OK")
ELSE
ksmem = js: kamem = ja
kyadd = imy / 2
      FOR il = 1 TO kn: FOR ix = ja TO js: FOR iy = kay TO ly(il)
      axy(il, ix, iy + kyadd) = axy(il, ix, iy)
      NEXT iy: NEXT ix: NEXT il
END IF
END IF
  mamessage$ = " Distributions are stored"

CASE "R"'to restore former state of cells from memory
 IF ksmem = 0 THEN
   CALL cmessage(6, -1, 1, 15, 4, ibackcol, "sorry, nothing was stored in the memory", "OK")
   EXIT SUB
END IF
itot& = 0
icc = iccmem: js = ksmem: ja = kamem

FOR il = 0 TO kn
  FOR i = ja TO js
   axt(il, i) = axt(il, i + imxl2)
  NEXT i: NEXT il

IF genedw$ > "" THEN
      FOR il = 1 TO kn:
      lax(il) = kamem: lx(il) = js
      FOR ix = ja TO js: FOR iy = 1 TO ly(il)
      axy(il, ix, iy) = axy(il, ix, iy + kyadd)
      NEXT iy: NEXT ix: NEXT il
END IF
icc = 1
mamessage$ = " Distributions are restored"


CASE "D"'general doubling
IF 2 * js > imxl THEN
 CALL cmessage(6, -1, 1, 15, 4, ibackcol, "sorry, XT-array too large", "OK")
 EXIT SUB
END IF
FOR i = js TO ja STEP -1
ii = 2 * i
FOR il = 0 TO kn
   axt(il, ii) = axt(il, i)
   axt(il, ii - 1) = axt(il, i)
  NEXT il: NEXT i

IF genedw$ > "" THEN
IF 2 * js > imy THEN
 CALL cmessage(6, -1, 1, 15, 4, ibackcol, "sorry, XY-array too large", "OK")
ELSE
      FOR ix = js TO ja STEP -1: FOR il = 1 TO kn: FOR iy = kay TO ly(il)
      ii = 2 * ix
      axy(il, ii, iy) = axy(il, ix, iy)
      axy(il, ii - 1, iy) = axy(il, ix, iy)
      NEXT iy: NEXT il: NEXT ix
END IF
FOR il = 1 TO kn: lx(il) = lx(il) * 2: NEXT
END IF
js = 2 * js: icc = 1


CASE "C"'to change margins of the field
ksnewinput:
  kanew = ja: ksnew = js
  mess$ = "Input new (smaller) field size: Present field size is:" + STR$(ja) + " - " + STR$(js)
  CALL cmessage(3, -2, 1, 4, ibackcol, ibackcol, mess$, "")
CALL zeingabe(igt, 1, 1, kanew, dummy, dummy$, "New left-most cell")
IF kanew = 0 THEN CALL zeingabe(igt, 1, 1, kanew, dummy, dummy$, "New left-most cell")

CALL zeingabe(igt, 1, 1, ksnew, dummy, dummy$, "New right-most cell")
LINE (1, 1)-(640, 40), 15, BF
  IF ksnew > js OR ksnew = 0 THEN ksnew = js
  IF kanew < ja THEN kanew = ja
  js = ksnew
  ja = kanew
  IF kd <> 25 THEN icc = icc + 3
IF genedw$ > "" THEN
   FOR il = 1 TO kn: lx(il) = js: lax(il) = ja: NEXT
END IF
  GOTO arraystore


CASE "ARRAYSTORE"
  GOTO arraystore


CASE "I"
CALL zeingabe(igt, 1, 1, kinsert, dummy, dummy$, " Position of the new cell, RETURN=right margin")
  IF kinsert = 0 THEN kinsert = js
  GOTO growth

CASE "G"
  GOTO growthpreparation
  'icc = icc + 1

CASE "T"
    IF kamem = 0 THEN
      CALL cmessage(4, -1, 1, 15, 4, ibackcol, "Nothing was stored, will be stored, use MT then again ", "OK")
      pcontroloc$ = "S"
      GOTO manipulationstartxt
      END IF
  itot& = 0
  ja = 1: i = 0
  mess$ = "Input new (smaller) field size: Present field size is:" + STR$(ja) + " - " + STR$(js)
  CALL cmessage(3, -2, 1, 4, ibackcol, ibackcol, mess$, "")
fromto:
ifrom = kamem: ito = ksmem
CALL zeingabe(igt, 1, 1, ifrom, dummy, dummy$, "Left-most cell of the piece to be added, 0=finish")
IF ifrom = 0 THEN
mamessage$ = "Transplantation complete"
icc = 1': LINE (0, 0)-(640, 40), 15, BF
EXIT SUB
END IF

CALL zeingabe(igt, 1, 1, ito, dummy, dummy$, "Right-most cell of the piece to be added")
IF ito = 0 THEN ito = ksmem
  IF ito > ksmem OR ifrom < kamem THEN
CALL zeingabe(igt, 0, 3, i, dummy, resp$, "Sorry, this piece is larger than the field, type RETURN")
    GOTO fromto
END IF
  idirection = 1
  IF ifrom > ito THEN idirection = -1
  FOR im = ifrom TO ito STEP idirection
    i = i + 1
IF i >= imxl THEN
CALL cmessage(6, -1, 1, 15, 4, ibackcol, " Sorry, XT-array too large!", "OK")
    END IF
    FOR il = 0 TO kn: axt(il, i) = axt(il, im + imxl2): NEXT
  IF genedw$ > "" THEN
IF i >= imxl THEN
CALL cmessage(6, -1, 1, 15, 4, ibackcol, " Sorry, XY-array too large!", "OK")
    END IF
    FOR il = 0 TO kn: FOR iy = 1 TO ly(il)
    axy(il, i, iy) = axy(il, im, iy + kyadd)
    NEXT iy:  NEXT il
  END IF
  NEXT im
  js = i: FOR il = 0 TO kn: lx(il) = i: NEXT
  GOTO fromto

CASE "A"
  itot& = 0
CALL zeingabe(igt, 0, 1, il, dummy, dummy$, "Array to be changed, 0=source, 1=a,2=b....")
  mess$ = "Present field size is:" + STR$(ja) + " - " + STR$(js)
  CALL cmessage(3, -2, 1, 4, ibackcol, ibackcol, mess$, "")
  i1 = ja: i2 = js
CALL zeingabe(igt, 1, 1, i1, dummy, dummy$, "First cell to be changed")
CALL zeingabe(igt, 1, 1, i2, dummy, dummy$, "Last cell to be changed")
CALL zeingabe(igt, 0, 2, i2, cfactor, dummy$, " factor [1 for no change] ")
LINE (0, 0)-(640, 40), 15, BF
'CALL cmessage(3, -2, 1, 4, ibackcol, ibackcol, Space$(79), "")
  IF i1 < ja THEN i1 = ja
  IF i2 = 0 OR i2 > js THEN i2 = js
  FOR i = i1 TO i2: axt(il, i) = axt(il, i) * cfactor: NEXT
  IF kd < 5 THEN icc = icc + 3
  GOTO arraystore
CASE "L", "RESTORESTART"
  FOR il = 1 TO kn
    FOR i = ja TO js
      IF il <= ilm THEN axt(il, i) = axy(il, i, 1)
    NEXT i
   NEXT il

CASE ELSE
  CLS
  LOCATE 10, 1
  PRINT "S = Save current results in a save buffer"
  PRINT "R = Restore previous results from the save buffer"
  PRINT "C = Change field size (for simulation of regeneration)"
  PRINT "I = Insert a new cell, also growth at marinal positions"
  PRINT "T = Transplant several tissues"
  PRINT "A = Change activator etc.  [axt()] in some cells"
  PRINT "D = doubling of each cell"
  PRINT " RETURN =  quitt without action"
  INPUT ; "Type of manipulation you want"; resp$
  IF resp$ = "" OR resp$ = CHR$(27) THEN EXIT SUB
pcontroloc$ = UCASE$(resp$)
  GOTO manipulationstartxt
END SELECT'  'select of manipulation
EXIT SUB



growthpreparation:
SELECT CASE k1'Where to insert additional cell
      CASE 0'terminal growth
	IF ja > 1 THEN
	  ja = ja - 1
	  FOR il = 1 TO kn: axt(il, ja) = axt(il, ja + 1): NEXT il
	END IF
	kinsert = js
      CASE 1'two insertion, one at each half
	km = ja + (js - ja) / 2
	kinsert = ja + RND * (km - ja)'insertion in the first half
	IF ja > 1 THEN
	  ja = ja - 1
	  FOR il = 1 TO kn
	    FOR ix = ja TO kinsert: axt(il, ix) = axt(il, ix + 1): NEXT ix
	  NEXT il
	END IF
	kinsert = km + RND * (js - km)'insertion in the second half
      CASE 2: kinsert = ja + (js - ja) / 2'central
      CASE 3: kinsert = ja + (js - ja) / 3 'one-third
      CASE 4: kinsert = ja + (js - ja) * RND'random position
      CASE 5: kinsert = js - 1 'posterior proliferation zone
      CASE 6: kinsert = js - k2 'posterior proliferation zone
      CASE 7' Random, but only IF axt(1,kinsert) < dz 'for trichom
      FOR i = 1 TO 20
      kinsert = ja + (js - ja) * RND'random position
      IF axt(3, kinsert) < dz THEN EXIT FOR
      IF i = 20 THEN
      CALL cmessage(1, -1, 1, 15, 4, ibackcol, "No place for proliferation found", "OK")
      EXIT SUB
      END IF
      NEXT i
CASE 8'two insertion, one at 1/4 and one at 3/4
	kinsert = ja + (js - ja) / 4 'insertion in the first half
	IF ja > 1 THEN
	  ja = ja - 1
	  FOR il = 1 TO kn
	    FOR ix = ja TO kinsert: axt(il, ix) = axt(il, ix + 1): NEXT ix
	  NEXT il
	END IF
	km = ja + (js - ja) / 4
	kinsert = ja + 3 * (js - ja) / 4
CASE 9'two insertion, one at 3/4 and one everywhere
	kinsert = 3 * ja / 4 + RND * (js - ja) / 4'insertion in the first half
		  FOR il = 1 TO kn
	    FOR ix = kinsert TO js: axt(il, ix + 1) = axt(il, ix): NEXT ix
	  NEXT il
	js = js + 1
	kinsert = ja + RND(js - ja)
CASE 10:   kinsert = ja + (js - ja) * 2 / 3 'second -third
CASE 12:
kinsert = 1
CASE ELSE
	kinsert = js
END SELECT
	IF kinsert > js THEN kinsert = js
	IF kinsert < ja THEN kinsert = ja
growth: 'subroutine
IF js = imxl THEN
CALL cmessage(1, -1, 1, 15, 4, ibackcol, " No further growth is possible, too many cells!", "OK")
ELSE
  FOR il = 1 TO kn
    FOR i = js TO kinsert STEP -1'still the old js is being used!
    axt(il, i + 1) = axt(il, i)
    NEXT i: NEXT il


IF genedw$ > "" THEN
IF js < imx THEN
FOR il = 1 TO kn:
FOR ix = lx(il) TO kinsert STEP -1          'still the old js is being used!
     FOR iy = 1 TO ly(il)
      axy(il, ix + 1, iy) = axy(il, ix, iy)
   NEXT iy:  NEXT ix
   ' wenn k1= 12 werden vorne zellen eingesetzt und hinten verschwinden sie
   'kx bleibt
   IF k1 <> 12 THEN lx(il) = lx(il) + 1

   NEXT il
ELSE
mess$ = "xy-array becomes too large,  js>imx"
CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
END IF
END IF

  IF k1 <> 12 THEN js = js + 1' js is adjusted to the new field size
END IF
EXIT SUB

arraystore:
IF genedw$ = "" THEN
IF icc <= imy AND js <= imx THEN
  FOR il = 1 TO kn
    FOR i = ja TO js
      IF il <= ilm THEN axy(il, i, icc) = axt(il, i)
    NEXT i
    IF ja < lax(il) THEN lax(il) = ja
    IF js > lx(il) THEN lx(il) = js
  ly(il) = icc: NEXT il
il = 0  's ist nicht zeitabhÑngig
    FOR i = ja TO js
    axy(il, i, 1) = axt(il, i)
    NEXT i
    IF ja < lax(il) THEN lax(il) = ja
    IF js > lx(il) THEN lx(il) = js
  ly(il) = 1
END IF
END IF
EXIT SUB

END SUB

SUB prof (axt(), il, dw$, ipattern, inz, iendm, dx, ff, ja, js, iy, xo, yo) STATIC
'this routine makes the drawings----------------------------------------
'a(ix,il): array to be plotted    il: substance
'dw$: display what                ipattern: controlling some patterns
'inz: type of the plot            iendm: = 99 if plot is to be finished
'ff: normalization factor         ja: left cell js: right cell
'iy: counting index, =1-> initialization, xo, yo: origin
'------------------------------------------------------------------------

SHARED igraph, dy, ivorcol, ibackcol, krot, per
SHARED ico(), iwi(), itypus(), ibm()
SHARED ivgashift, msiluet(), msiluetmax, ihp, fdelay, radius, yhzyl, DeltaWi
SHARED dxp, dyp, icc, k1, k2, k3, k4, ihintr, istelz
SHARED ipcs, ipcx, idxr, idxl, idyo, idyu

DIM sp$(11), dp$(11), ipat(1 TO 4, 1 TO 5), icolf(10), col(3, 10), ym(10)
icolor = ico(il)
iwil = iwi(il)
f10 = 2.5 * ff
f1 = 40 * ff
'colours for false colour plot:
icolf(0) = 15: icolf(1) = 14: icolf(2) = 11: icolf(3) = 10: icolf(4) = 7: icolf(5) = 13
icolf(6) = 3: icolf(7) = 2: icolf(8) = 6: icolf(9) = 5: icolf(10) = 0
twas$ = "sabcdefghij"
tv$(il) = MID$(twas$, il + 1, 1)
iwhatarray = 1: iwhatarray = INSTR(iwhatarray, dw$, tv$(il)) - 1
IF iwhatarray = -1 THEN EXIT SUB
iplottotal = LEN(dw$)

profilestart:
IF iendm = 99 GOTO profileframe
IF ja = 0 THEN
CALL cmessage(6, -1, 1, 15, 4, ibackcol, "left cell is zero, something is wrong", "")
END IF
IF iy = 1 AND ahidden = 0 THEN '----------- Initialization ---------------------
  SELECT CASE inz
  CASE 1, 2  '3D-plots
    msiluetm5 = msiluetmax / iplottotal - 1
    ahidden = -1: IF inz = 2 THEN ahidden = 1000
    FOR i = 1 TO msiluetmax: msiluet(i) = ahidden: NEXT i
    IF krot = 0 THEN krot = 2
    angle = ATN(1! / (krot))
    pdx = dx * COS(angle)
    px = pdx / krot
    pyy = dx * per * COS(angle)
    pdyx = dx * SIN(angle) * per
    pyx = pdyx / krot
    f1 = 40 * ff: fmodus = f1 / krot
  CASE 3, 18, 19, 24'------new curves are drawn
  CASE 4, 5'----- schematic plot with letters --------
    FOR i% = 1 TO 10: dp$(i%) = CHR$(96 + i%): NEXT i%
    dp$(11) = CHR$(96 + 10): dp$(0) = CHR$(96)
    spp$ = "N .,:;∞±≤€€"
    FOR i% = 0 TO 9: sp$(i%) = MID$(spp$, i% + 1, 1): NEXT i%
  CASE 6 TO 14 'pixelpattern for pixel plots
    ipat(1, 1) = &H4444
    ipat(1, 2) = &H1111
    ipat(1, 3) = &H8888
    ipat(1, 4) = &H2222
    ipat(2, 1) = &HAAAA
    ipat(2, 2) = &H5555
    ipat(2, 3) = &H3333
    ipat(2, 4) = &HCCCC
    ipat(3, 1) = &HEEEE
    ipat(3, 2) = &HBBBB
    ipat(3, 3) = &HDDDD
    ipat(3, 4) = &H7777
    ipat(4, 1) = &HFFFF
    ipat(4, 2) = &HFFFF
    ipat(4, 3) = &HFFFF
    ipat(4, 4) = &HFFFF
    'for false colour,inz=6-8
    icolf(0) = 15: icolf(1) = 14: icolf(2) = 11: icolf(3) = 10: icolf(4) = 7: icolf(5) = 13
    icolf(6) = 3: icolf(7) = 2: icolf(8) = 6: icolf(9) = 5: icolf(10) = 0

CASE 15: alpha = 0: iradius = 25 'for circular plots
CASE 16'(used for plots around a cell surface)
f1 = 40 * ff
CASE 10'for plots in different brown colours, works only with MS BASIC
isc = 5
col(1, 1) = .3: col(1, isc) = 1
col(2, 1) = 0: col(2, isc) = .8
col(3, 1) = 0: col(3, isc) = .4
FOR ic = 1 TO 3
FOR isci = 1 TO isc
col(ic, isci) = col(ic, 1) + (isci - 1) * (col(ic, isc) - col(ic, 1)) / (isc - 1)
ipat(ic, isci) = 63 * col(ic, isci)
NEXT isci: NEXT ic
FOR i = 1 TO isc
PALETTE isc + 1 - i, 65536 * ipat(3, i) + 256 * ipat(2, i) + ipat(1, i)
NEXT i
REM

  CASE ELSE
  END SELECT
END IF
' ------ Drawing the curves--------

SELECT CASE inz
CASE 1 '3D plot: time = background -> foreground, with hidden lines
  f1 = 40 * ff: fmodus = f1 / krot
  x2 = px * (ja * krot + iy) + xo
  ya = -pyx * ja * krot + pyy * iy + yo
  y2 = ya + axt(il, ja) * f1
  j = msiluetm5 * iwhatarray + iy + (ja - 1) * krot
  IF j > msiluetm5 * (iwhatarray + 1) THEN
  CALL cmessage(1, -1, 1, 15, 4, ibackcol, "max array size for hidden lines reached, use a smaller KROT", "")
  EXIT SUB
  END IF

  s2 = msiluet(j)'y-Coordinate of the still visible line
  IF y2 > s2 THEN 'left point is visible
    yb = ya: IF s2 > yb THEN yb = s2
    'PRESET (x2, yb)'Initialtion at the left side
'    LINE (x2, yb)-(x2, y2), icolor
     CALL linep(ihp, x2, yb, x2, y2, 0, iwil, icolor, 0)
    msiluet(j) = y2'new horizon for hidden lines
    ivisible = 1
  END IF
   xanf = x2
   yanf = y2
  FOR ix = ja + 1 TO js
    dysilu = (axt(il, ix) - axt(il, ix - 1)) * fmodus - pyx
    xr = 0
    yr = 0
    FOR i = 1 TO krot'------Interpolation-------
      j = j + 1'new x-coordinate of the horizon
      IF j > msiluetm5 * (iwhatarray + 1) THEN
      CALL cmessage(1, -1, 1, 15, 4, ibackcol, "max array size for hidden lines reached, use a smaller KROT", "")
	EXIT SUB
      END IF
      s1 = s2'new y-coordinate of the horizon at point 1
      y1 = y2'old left point becomes the new right point
      x1 = x2
      x2 = x2 + px
      y2 = y1 + dysilu
      s2 = msiluet(j)'new horizon
      IF y1 > s1 THEN 'IF ivisible > 0 THEN     ' line is not hidden
	IF y2 >= s2 THEN ' point 1 and 2 have been visible
	  xr = xr + px
	  yr = yr + dysilu
	  msiluet(j) = y2
	ELSE
	  xs = px * (s1 - y1) / (y2 + s1 - y1 - s2 + .000001)' line becomes invisible
	  ys = xs * (y2 - y1) / px'point where kt becomes hidden
	  xr = xr + xs
	  yr = yr + ys
	  'LINE -STEP(xr, yr), icolor
     xend = xanf + xr
     YEND = yanf + yr
     CALL linep(ihp, xanf, yanf, xend, YEND, 0, iwil, icolor, 0)'  line after krot-loop
     xanf = xend
     yanf = YEND
	  xr = 0!
	  yr = 0!
	END IF
      ELSE
	IF y2 > s2 THEN ' point 2 besomes visible
	  xs = px * (s1 - y1) / (y2 + s1 - y1 - s2 + .001)
	  ys = y1 + xs * (y2 - y1) / px
	  xs = x1 + xs
	  xanf = xs
	  yanf = ys
	  'PRESET (xs, ys)
	  xr = x2 - xs
	  yr = y2 - ys
	  msiluet(j) = y2
	END IF
      END IF
    NEXT i'end of krot loop
     IF xr > 0 THEN
     xend = xanf + xr
     YEND = yanf + yr
     CALL linep(ihp, xanf, yanf, xend, YEND, 0, iwil, icolor, 0)'  line after krot-loop
     xanf = xend
     yanf = YEND
     END IF
  NEXT ix' end IX- loop
  ya = -pdyx * (js + ipattern) + pyy * iy + yo
  CALL linep(ihp, xend, YEND, xend, ya, 0, iwil, icolor, 0)

CASE 2'3D plot: time = background -> forground
  f1 = 40 * ff: fmodus = f1 / krot
  IF iy = ibaseline + 1 THEN 'remove last baseline during continuation
    x2 = px * (ja * krot + iy - 1) + xo
    y2 = pdyx * ja - pyy * (iy - 1) + yo
    LINE (x2, y2 - 1)-(x2 + dxtot, y2 + dytot - 1), ibackcol
  END IF
  x2 = px * (ja * krot + iy) + xo
  ya = pdyx * ja - pyy * iy + yo
  y2 = ya + axt(il, ja) * f1
  yma = ya: ym2 = y2
  xm2 = x2
  j = msiluetm5 * iwhatarray + iy + (ja - 1) * krot
  'j = (il - 1) *  * iwhatarray + iy + (ja - 1) * krot
  IF j > msiluetm5 * (iwhatarray + 1) THEN
    CALL cmessage(1, -1, 1, 15, 4, ibackcol, "max array size for hidden lines reached, use a smaller KROT", "")
    EXIT SUB
  END IF
  IF ya < msiluet(j) THEN msiluet(j) = ya
  s2 = msiluet(j)

  FOR ix = ja + 1 TO js
    dy = (axt(il, ix) - axt(il, ix - 1)) * fmodus + pyx
    'Change by difference per step
    ddy = dy / px'+ shift in y-direction
    xmmodus = x2
    ymmodus = y2
    xr = 0
    yr = 0
    FOR i = 1 TO krot'------Interpolation-------
      j = j + 1'new x-coordinate of the horizon
      IF j > msiluetm5 * (iwhatarray + 1) THEN
	CALL cmessage(1, -1, 1, 15, 4, ibackcol, "max array size for hidden lines reached, use a smaller KROT", "")
	EXIT SUB
      END IF
      s1 = s2'new y-coordinate of the horizon at point 1
      y1 = y2'old left point becomes the new right point
      x1 = x2
      x2 = x1 + px
      y2 = y1 + dy
      s2 = msiluet(j)'new horizon
      IF y1 >= s1 THEN ' point 1 has to be removed
	IF y2 > s2 THEN ' point 2 has to be removed too
	  xl = x1
	  yl = y1
	  DO WHILE xl <= x2
	    LINE (xl, yl)-(xl, yma), ibackcol
	    xl = xl + 1
	    yl = yl + ddy
	  LOOP

	ELSE 'remove from point 1 to intersection
	  xs = px * (s1 - y1) / (y2 + s1 - y1 - s2 + .000001)' intersection
	  ys = xs * (y2 - y1) / px'point where kt becomes visible
	  xr = x1 + xs
	  xl = x1
	  yl = y1
	  DO WHILE xl <= xr
	    LINE (xl, yl)-(xl, yma), ibackcol
	    xl = xl + 1
	    yl = yl + ddy
	  LOOP
	END IF
      ELSE ' point 1 was visible
	IF y2 > s2 THEN '     line at point 2 must be removed
	  xs = px * (s1 - y1) / (y2 + s1 - y1 - s2 + .001)'point 2 becomes visible in
	  yr = y1 + xs * (y2 - y1) / px
	  xr = x1 + xs
	  xl = xr
	  yl = yr
	  DO WHILE xl <= x2
	    LINE (xl, yl)-(xl, yma), ibackcol
	    xl = xl + 1
	    yl = yl + ddy
	  LOOP
	END IF
      END IF
      msiluet(j) = y2
    NEXT i'End of krot-loop
    LINE (xmmodus, ymmodus)-(x2, y2), icolor
  NEXT ix
  msiluet(j) = yma
  xmb = x2
  ymb = yma + pdyx * (js - ja)
  LINE -(x2, ymb), icolor'left border
  LINE (xm2, ym2)-(xm2, yma), icolor'left border

CASE 3, 24 'plot of single curves, 24 for hydra up and down
type3:
  f1 = 40! * ff: y150 = yo - 150
  ahidden = 0
  ya = axt(il, ja) * f1 + yo - iycor
  xa = xo
IF icolor > 19 GOTO stepcurves
  IF il = 0 THEN   'special for source density = dots
    FOR i = ja TO js
      ya = axt(il, i) * f1 + yo - iycor
      CALL linep(ihp, xa, ya, xa + iwi(il), ya + iwi(il), 2, iwil, icolor, 0)
      xa = xa + dx
    NEXT i
  ELSE
    FOR i = ja + 1 TO js
      xb = xa + dx
      yb = axt(il, i) * f1 + yo - iycor
 '     IF yb < y150 THEN yb = y150
      CALL linep(ihp, xa, ya, xb, yb, 0, iwi(il), icolor, itypus(il))
      xa = xb: ya = yb
    NEXT i
  END IF
EXIT SUB
stepcurves:
icol20 = icolor - 20
iwi2 = iwi(il) / 2
  ya = axt(il, ja) * f1 + yo - iycor
  xa = xo - .5 * dx
'  xb = xa + dx
    '  LINE (xa + iwi2, ya)-(xb - iwi2, ya + iwi(il)), icol20, BF
      CALL linep(ihp, xa - iwi2, ya, xa + dx + iwi2, ya + iwi(il), 2, iwi(il), icol20, itypus(il))
    FOR i = ja + 1 TO js
      xa = xa + dx
      yb = axt(il, i) * f1 + yo
      CALL linep(ihp, xa - iwi2, ya, xa + iwi2, yb + iwi(il), 2, iwi(il), icol20, itypus(il))
      CALL linep(ihp, xa - iwi2, yb, xa + dx + iwi2, yb + iwi(il), 2, iwi(il), icol20, itypus(il))
      ya = yb
    NEXT i


CASE 4, 5'plot with symbols
  amx = .00000001#
  lastrow = 80: IF ivgashift > 5 THEN lastrow = 128
  FOR ix = ja TO js
    IF axt(il, ix) > amx THEN amx = axt(il, ix)
  NEXT ix
  PRINT tv$(il); : PRINT USING "#####.###"; amx;
  am10 = 8 / amx: dummy$ = " ∫"
  FOR ix = ja TO js
    nx = axt(il, ix) * am10 + 1
    IF axt(il, ix) < 0 THEN nx = 0
    dummy$ = dummy$ + sp$(nx)
  NEXT ix
    dummy$ = dummy$ + "∫  "
  PRINT MID$(dummy$, 1, lastrow - 11);
   inotused = lastrow - LEN(dummy$) - 13
   IF inotused > 30 THEN
 ' printing in percent if array is smaller 16' neads still field adaptation
 jsmax = js: IF jsmax - ja + 1 > 16 THEN jsmax = ja - 1 + inotused / 3
  FOR ix = ja TO jsmax
    n = axt(il, ix) * 100 / amx + .4
    IF axt(il, ix) >= 0 THEN
    PRINT USING "###"; n;
    ELSE
    PRINT " --";
    END IF
  NEXT ix
  END IF
  PRINT


'----------vertical pixel pattern in course of time (especially for shell patterns)----
CASE 6 TO 14
  x1 = xo: y1 = yo
  CALL linep(ihp, x1 - iwi(1), y1, x1 - 1, y1 - 1, 2, 1, ivorcol, 1)'right border
SELECT CASE inz
 CASE 6, 10' = false colour
    FOR ix = ja TO js
      jlocal = axt(il, ix) * f10
      IF jlocal > 10 THEN jlocal = 10
      IF jlocal >= krot THEN LINE (x1, y1)-(x1 + dx, y1 - 1.5), icolf(jlocal), BF
      x1 = x1 + dx
    NEXT ix

 CASE 7, 11'   solid line
    FOR ix = ja TO js
      IF axt(il, ix) * ff > 1.5 THEN LINE (x1, y1)-(x1 + dx, y1 - 1.5), icolor, BF
      x1 = x1 + dx
    NEXT ix

CASE 8, 9, 12, 13, 14  '8 and 10 with stable pattern, 6-9 = for vga, 10 - 14 for pixel in higher resolution
   FOR ix = ja TO js
    krnd = RND * 3 + 1
      jlocal = axt(il, ix) * ff
      IF jlocal >= 1 THEN
      IF jlocal > 4 THEN jlocal = 4
      LINE (x1, y1)-(x1 + dx, y1), icolor, , ipat(jlocal, krnd)
	  IF ivgashift > 5 THEN
      IF inz = 8 OR inz = 9 THEN
    krnd = RND * 3 + 1
      LINE (x1, y1 - .7)-(x1 + dx, y1 - .7), icolor, , ipat(jlocal, krnd)'draw second line underneath
    krnd = RND * 3 + 1
'         LINE (x1, y1 - 1.4)-(x1 + dx, y1 - 1.4), icolor, , ipat(jlocal, krnd)'draw second line underneath
	  END IF
	  END IF
	  END IF
      x1 = x1 + dx
    NEXT ix

CASE 10 'brownish colour   - not in use in Power Basic
    FOR ix = ja TO js
      jlocal& = axt(il, ix) * ff
      IF jlocal& > 5 THEN jlocal& = 5
      IF jlocal& >= 1 THEN LINE (x1, y1)-(x1 + dx, y1), jlocal&
      x1 = x1 + dx
    NEXT ix
  REM
  END SELECT
    CALL linep(ihp, x1 + 1, y1, x1 + iwi(1), y1 - 1, 2, 1, ivorcol, 1)''left border



  CASE 15'-drawing on circles, Nautilus
  'PER controls the angular displacement
  krnd = RND * 3 + 1
  FOR ix = ja TO js
    jlocal& = axt(il, ix) * ff
    IF jlocal& >= 1 THEN
      IF jlocal& > 4 THEN jlocal& = 4
      FOR ddx = 0 TO dx STEP .3
      x1 = 320 + dx * (iradius + ix + ddx) * COS(alpha)
      y1 = 240 + dx * (iradius + ix + ddx) * SIN(alpha)
      x2 = 320 + dx * (iradius + ix + ddx) * COS(alpha + per)
      y2 = 240 + dx * (iradius + ix + ddx) * SIN(alpha + per)
      LINE (x1, y1)-(x2, y2), icolor
      NEXT ddx
    END IF
  NEXT ix
  alpha = alpha + .5 * per
  x1 = xo: y1 = yo
  CALL linep(ihp, x1 - iwi(1), y1, x1 - 1, y1, 0, iwi(1), ivorcol, 1) 'right border

CASE 16, 161
wi = 6.28 / (js - ja + 1)
wi1 = -(ja - 1) * wi + 1.57: ityploc = 1
yos = yo: IF inz = 161 THEN yos = yo + (icc - 1) * istelz
ixa = xo + (radius + axt(il, ja) * f1) * COS(wi1)
iya = yos + (radius + axt(il, ja) * f1) * per * SIN(wi1)'
CALL polygon(ihp, ixa, iya, 1, iwil, icolor, ityploc)
FOR ix = ja + 1 TO js
wi1 = -(ix - 1) * wi + 1.57
ixa = xo + (radius + axt(il, ix) * f1) * COS(wi1)
iya = yos + (radius + axt(il, ix) * f1) * per * SIN(wi1)'per *
CALL polygon(ihp, ixa, iya, 2, iwil, icolor, ityploc)
NEXT
CALL polygon(ihp, ixa, iya, 3, iwil, icolor, ityploc)
ixa = xo: iya = yos
CALL polygon(ihp, ixa, iya, 4, iwil, icolor, ityploc)'fill

CASE 17 'Ring of cells, for organizer
    ibm(1) = icolor
    IF icolor = 0 THEN ibm(1) = 7: IF icolor = 20 THEN ibm(1) = 27
    IF icolor = 2 THEN ibm(1) = 10: IF icolor = 22 THEN ibm(1) = 30
    IF icolor = 1 THEN ibm(1) = 11: IF icolor = 21 THEN ibm(1) = 31
    IF icolor = 12 THEN ibm(1) = 6: IF icolor = 32 THEN ibm(1) = 26
   ianzahl2 = (js - ja + 1) / 2
   SELECT CASE ipattern
   CASE 0
   wi1 = 0
   jaa = ja: jss = ja + ianzahl2
   CASE 1
   jaa = ja + ianzahl2
   jss = js + 1
   wi1 = DeltaWi * ianzahl2
   END SELECT
   irot = ihintr
   ix1 = xo + radius * COS(wi1)
   iy1 = yo + radius * SIN(wi1) * per
   ixx = jaa + irot:
   IF ixx > js THEN
   ixx = ixx - js + ja - 1
   END IF
   iy1f = axt(il, ixx) * f1
   iy1h = iy1 + iy1f + 2
   FOR ixx = jaa + 1 + irot TO jss + irot
   ix = ixx: IF ixx > js THEN ix = ixx - js + ja - 1
   wi2 = wi1 + DeltaWi
   icow = icolor: IF SIN(wi2) > 0 THEN icow = ibm(1)
   ix2 = xo + radius * COS(wi2)
   iy2 = yo + radius * SIN(wi2) * per
   iy2f = axt(il, ix) * f1
   iy2h = iy2 + iy2f + 2

   IF icow < 20 THEN
  IF idyo < iy2h THEN idyo = iy2h
  IF idyu > iy2 THEN idyu = iy2
  IF idxl > ix2 THEN idxl = ix2
  IF idxr < ix2 THEN idxr = ix2
    LINE (ix1, iy1)-(ix1, iy1h), icow
  istep = 1: IF ix2 < ix1 THEN istep = -1
  iyu = iy1: iyo = iy1h
  LINE (ix1, iyu)-(ix1, iyo), icow
  IF ix2 - ix1 <> 0 THEN
  dyu = (iy2 - iy1) / (ix2 - ix1) * istep
  dyo = (iy2h - iy1h) / (ix2 - ix1) * istep
  stepzahl = 1
	  FOR ixs = ix1 + istep TO ix2 STEP istep
	  iyu = iy1 + dyu * stepzahl: iyo = iy1h + dyo * stepzahl
	  LINE (ixs, iyu)-(ixs, iyo), icow
	  stepzahl = stepzahl + 1
	  NEXT ixs
   END IF
  LINE (ix2, iy2)-(ix2, iy2h), icow
ELSE 'only line
    x1 = ix1: y1 = iy1h: x2 = ix2: y2 = iy2h:
    CALL linep(ihp, x1, y1, x2, y2, 0, iwi(il), icow - 20, 0)
END IF

   ix1 = ix2: iy1 = iy2: iy1h = iy2h: iy1f = iy2f: wi1 = wi2
   NEXT ixx




CASE 18, 19, 191
      iycor = 0: IF inz = 18 THEN iycor = (icc - 1) * dyp'automatisch verschoben
      IF icolor > 39 THEN ' Curves
      icolor = icolor - 40
      IF ihintr > 0 THEN 'basis-linie mit ityp
      CALL linep(ihp, xo - dx / 2, yo, xo + (js - ja) * dx + dx / 2, yo - ihintr, 2, 1, icolor, 1)
      END IF
      GOTO type3
      END IF
      IF icolor < 20 THEN
      ixo = xo
      iyo = yo + 4 - iycor: IF f1 < 0 THEN iyo = yo - 4
      iyoo = yo - iycor
      CALL polygon(ihp, ixo, iyoo, 1, iwil, icolor, ityp)
      ixa = ixo: iya = iyo + axt(il, ja) * f1: 'IF iya > idyo THEN idyo = iya
      CALL polygon(ihp, ixa, iya, 2, iwil, icolor, ityp)
      FOR ix = ja + 1 TO js
      iya = iyo + axt(il, ix) * f1: 'IF iya > idyo THEN idyo = iya
      ixa = ixa + dx
      CALL polygon(ihp, ixa, iya, 2, iwil, icolor, ityp)
      NEXT ix
      CALL polygon(ihp, ixa, iyoo, 2, iwil, icolor, ityp)
      CALL polygon(ihp, ixo, iyoo, 3, iwil, icolor, ityp)
      CALL polygon(ihp, ixo + 3, (iyoo + iyo) / 2, 4, iwil, icolor, ityp)
 '     LINE (xo, yo)-(xo + (js - ja) * dx, yo + 4 - iwi(1)), 15, BF

      ELSE
      icolor = icolor - 20'Rectangles
      xa = xo - dx / 2:
      FOR ix = ja TO js
      CALL linep(ihp, xa, yo, xa + dx - istelz, yo + axt(il, ix) * f1, 2, 0, icolor, ityp)
      xa = xa + dx
    NEXT ix
    END IF
EXIT SUB


CASE 20 'for Oszillation of leftmost cell
  f1 = 40! * ff
  ya = axt(il, ja) * f1 + yo
  IF iy = 1 THEN
    PRESET (xo, ya), icolor
    ym(il) = ya
  ELSE
  '  LINE (xo, ya + iwi(il))-(xo + 1, ym(il)), icolor, BF
   IF ya - ym(il) > 0 THEN
   LINE (xo, ym(il) - iwi(il))-(xo + 1, ya + iwi(il)), icolor, BF
   ELSE
   LINE (xo, ym(il) + iwi(il))-(xo + 1, ya - iwi(il)), icolor, BF
  END IF
  END IF
  ym(il) = ya
  'LINE (xo, yo - 1)-(xo, yo - 2), ivorcol'Basis-Line


CASE 21'rectangles
  f1 = 40! * ff
  xa = xo + .5 * dx
    FOR i = ja TO js
      xb = xa + dx * .8
      ya = axt(il, i) * f1 + yo
      CALL linep(ihp, xa, yo, xb, ya, 2, 1, i, 0)
      xa = xa + dx
    NEXT i
CASE 26 'Ring of cells, for organizer
   DeltaWi = 6.28 / js: js21 = js / 2 + 1: wi1 = 0
   FOR ix = ja TO js   'Pigment wird gezeichnet wenn a Åber einer Schwelle
   wi2 = wi1 + DeltaWi
   icoltemp = icolor: wi21 = wi2
   IF ix < js21 AND il = 1 THEN icoltemp = 7
   IF ix < js21 AND il = 2 THEN icoltemp = 10
   ih = axt(il, ix) * f1
   IF il = 1 THEN
   FOR i = 0 TO ih
   CIRCLE (xo, yhzyl + i), radius, icotemp, wi1, wi2, per
   NEXT
   ELSE
   CIRCLE (xo, yhzyl + ih), radius, icolor, wi1, wi2, per
   CIRCLE (xo, yhzyl + ih + 1), radius, icolor, wi1, wi2, per
   END IF
   'END IF
   wi1 = wi2
   NEXT ix
IF ipcs > -2 THEN
	idxl = xo - radius - 4
	idxr = xo + radius + 4
	IF yhzyl - radius * per < idyu THEN idyu = yhzyl - radius * per
	IF yhzyl + radius * per > idyo THEN idyo = yhzyl + radius * per
END IF

CASE 27
   DeltaWi = 6.28 / js: wi1 = 0
   wi1 = 0:
   FOR ix = ja TO js
   wi2 = wi1 + DeltaWi
   IF axt(il, ix) * ff > 1 THEN
   CIRCLE (320, 245), radius, icolor, wi1, wi2
   END IF
   wi1 = wi2
   NEXT ix
IF ipcs > -2 THEN
	IF 320 - radius < idxl THEN idxl = 320 - radius
	IF 320 + radius > idxr THEN idxr = 320 + radius
	IF 245 - radius < idyu THEN idyu = 245 - radius
	IF 245 + radius > idyo THEN idyo = 245 + radius
END IF

CASE 28 'Zylinder fÅr Phyllotaxis
   DeltaWi = 6.28 / (js - ja + 1): js21 = js / 2 + 1: wi1 = 0
   IF ihintr > js THEN ihintr = 0
    IF icolor = 0 THEN ico2 = 7
    IF icolor = 2 THEN ico2 = 10
    IF icolor = 1 THEN ico2 = 11
    IF icolor = 12 THEN ico2 = 6

   FOR ix = ja TO js   'Pigment wird gezeichnet wenn a Åber einer Schwelle
   ixx = ix + ihintr
   IF ixx > js THEN ixx = ixx - js + ja - 1
   wi2 = wi1 + DeltaWi
   icotemp = icolor: wi21 = wi2
   IF SIN(wi2) > 0 THEN icotemp = ico2
   IF axt(il, ixx) * ff > 1 THEN
   CIRCLE (xo, yhzyl), radius, icotemp, wi1, wi2, per
   IF il = 1 THEN
   CIRCLE (xo, yhzyl + 1), radius, icotemp, wi1, wi2, per
   END IF
   END IF
   wi1 = wi2
   NEXT ix
IF ipcs > -2 THEN
	IF xo - radius - 4 < idxl THEN idxl = xo - radius - 4
	IF xo + radius + 4 > idxr THEN idxr = xo + radius + 4
	IF yhzyl - radius * per < idyu THEN idyu = yhzyl - radius * per
	IF yhzyl + radius * per > idyo THEN idyo = yhzyl + radius * per
END IF

CASE 29' Dastellung eine Zelle ein Rechteck
'LINE (20, 45)-(620, 50), 1, BF
x1 = xo
FOR ix = ja TO js
x2 = x1 + dx
afl = yo + axt(il, ix) * f1
IF il = 1 THEN
CALL linep(ihp, x1, yo, x2, afl, 1, 1, icolor, 1)', BF'GrÅner Block=Aktivator
LINE (x1, afl)-(x2, 480), 15, BF 'Block weiss malen, alten Wert lîschen
ELSE
CALL linep(ihp, x1, afl, x2, afl + iwi(il), 1, 1, icolor, 0)
END IF
x1 = x2
NEXT

END SELECT
EXIT SUB

profileframe: '-------------finishing frame after simulation
ahidden = 0
SELECT CASE inz
CASE 1
IF istelz = 4 THEN  ' add last line of the inhibitor
  x1 = px * (ja * krot + iy) + xo
  ya = -pyx * ja * krot + pyy * iy + yo
  y1 = ya + axt(2, ja) * f1
  CALL linep(ihp, x1, y1, x1, ya, 0, iwil, ico(2), 2)
  pdx3 = pdx / 3
  FOR ix = ja + 1 TO js
  x2 = x1 + pdx
  ya = -pyx * ix * krot + pyy * iy + yo
  y2 = ya + axt(2, ix) * f1
  CALL linep(ihp, x1, y1, x1 + pdx3, y1 + (y2 - y1) / 3, 0, iwil, ico(2), 1)
  x1 = x2
  y1 = y2
  NEXT ix
  CALL linep(ihp, x1, y1, x1, ya, 0, iwil, ico(2), 2)
 END IF

  x1 = px * ((ja + ipattern) * krot + 1) + xo
  y1 = -pdyx * (ja + ipattern) + pyy + yo
  x2 = x1 + pdx * (js - ja)
  y2 = y1 - pdyx * (js - ja)
  CALL linep(ihp, x1, y1, x2, y2, 0, iwil, icolor, 0)
  x3 = x2 + (iy - 1) * px
  y3 = y2 + pyy * (iy - 1)
  CALL linep(ihp, x2, y2, x3, y3, 0, iwil, icolor, 0)
  x4 = x3 - pdx * (js - ja)
  y4 = y3 + pdyx * (js - ja)
  CALL linep(ihp, x3, y3, x4, y4, 0, iwil, icolor, 2)
  CALL linep(ihp, x4, y4, x1, y1, 0, iwil, icolor, 2)
  'LINE -(x4, y4), icolor, , &H1010
  'LINE -(x1, y1), icolor, , &H1010
  yver = y4 + axt(il, ja) * f1
  CALL linep(ihp, x4, y4, x4, yver, 0, iwil, icolor, 2)
  LINE (x4, y4)-(x4, yver), icolor, , &H1010

CASE 2
  ja = ja + ipattern
  x1 = px * (ja * krot + 1) + xo
  y1 = pdyx * ja - pyy + yo
  x2 = px * (ja * krot + iy) + xo
  y2 = pdyx * ja - pyy * iy + yo
  x3 = x2 + (js - ja) * px * krot
  y3 = y2 + pdyx * (js - ja)
  LINE (x3, y3)-(x3 - x2 + x1, y3 - y2 + y1), icolor, , &H1010
  LINE -(x1, y1), icolor, , &H1010
  LINE -(x2, y2), icolor
  LINE (x2, y2 - 1)-(x3, y3 - 1), icolor
  dxtot = x3 - x2
  dytot = y3 - y2
  ibaseline = iy


CASE 6, 8, 9, 11, 12, 14, 17
CALL linep(ihp, xo - 2, y1 - 1, xo + dx * (js - ja + 1) + 2, y1 - 1, 0, 1, ivorcol, 0)
CALL linep(ihp, xo - 2, y1 - 2, xo + dx * (js - ja + 1) + 2, y1 - 2, 0, 1, ivorcol, 0)
CASE 7, 10
CALL linep(ihp, x1 + 1, yo - 2, x1 + 1, yo + (js - ja + 1) * dx + 1, 0, 1, ivorcol, 0)
CALL linep(ihp, x1, yo - 2, x1, yo + (js - ja + 1) * dx + 1, 0, 1, ivorcol, 0)
CASE 22, 13
CALL linep(ihp, xo - 2, y1 + 1, xo + dx * (js - ja + 1) + 2, y1 + 1, 0, 1, ivorcol, 0)
CALL linep(ihp, xo - 2, y1 + 2, xo + dx * (js - ja + 1) + 2, y1 + 2, 0, 1, ivorcol, 0)

END SELECT
EXIT SUB
END SUB

SUB showprof (inzloc, iendm, dwloc$, axt(), jaloc, jsloc, iccloc) STATIC
SHARED kx, ky, kd, kn, kg, icol, k1, k2, k3, k4, ifree
SHARED dx, dy, ip$, krot, per, iparam, iactive, ianima, radius
SHARED gxa(), gfa(), ibm(), yhzyl, DeltaWi
SHARED gya(), ico(), itypus(), iwi(), msiluet(), msiluetmax
SHARED itot&, label$(), title$, displaywas$, icheckerror, lpt$, dirtemp$
SHARED ihp, dxp, dyp, fkasten, ivgashift, ianz, ireaddata
SHARED ivorcol, ibackcol, igraph, istelz, iletztk, ihintr
SHARED fdelay, ipcx, ipcs, idxl, idxr, idyu, idyo

'calls the prof(ile)- subroutine for each substance
inz = inzloc: dw$ = dwloc$: icc = iccloc
ja = jaloc: js = jsloc: iy = iyloc
DIM am(10)
ipattern = 0

''IF INSTR(dw$, "w") > 0 THEN CALL rwdata("WXT", dw$)

SELECT CASE inz
CASE 0
  IF iendm = 99 THEN EXIT SUB
  CALL clearscreen
  FOR ilv = LEN(dw$) TO 1 STEP -1
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    IF il >= 0 AND il < 9 THEN
    IF itypus(il) = 25 THEN
    dwtemp$ = MID$(dw$, ilv, 1)
    GOTO case25
END IF
    CALL prof(axt(), il, dw$, 0, itypus(il), iendm, dx, gfa(il), ja, js, iccloc, gxa(il), gya(il))
'    CALL prof(axt(), il, dw$, ipat(il), itypus(il), iendm, dx, gfa(il), ja, js, icc, gxa(il), gya(il))
    END IF
  NEXT ilv

CASE 1, 2
  CALL prof(axt(), 4, dw$, 0, inz, iendm, dx, gfa(4), ja, js, iccloc, gxa(4), gya(4))
  CALL prof(axt(), 2, dw$, 0, inz, iendm, dx, gfa(2), ja, js, iccloc, gxa(2), gya(2))
  CALL prof(axt(), 3, dw$, 0, inz, iendm, dx, gfa(3), ja, js, iccloc, gxa(3), gya(3))
  CALL prof(axt(), 1, dw$, 0, inz, iendm, dx, gfa(1), ja, js, iccloc, gxa(1), gya(1))
  IF icc = 1 THEN
  CALL prof(axt(), 0, dw$, 0, inz, iendm, dx, gfa(0), ja, js, icc, gxa(0), gya(0))
  END IF
CASE 3, 24, 29
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 OR kg > 0 THEN
  xa = gxa(1): yo = gya(1): y2 = 470:
  IF istelz > yo THEN y2 = istelz
  IF xa = 0 THEN xa = 320 - (js - ja) / 2 * dx: IF xa < 5 THEN xa = 5' centering
'  xa = 320 - ((js - ja + 1) / 2) * dx
  IF inz = 24 THEN yo = gya(1)
  jam = ja:
  END IF
  y3 = yo - iwi(1): y4 = yo - iwi(1) - iwi2
  xo = xa + dx * (ja - jam) 'to have correct position after change of field
  CALL clearscreen
  jja = ja: jjs = js:
  IF kd = 29 THEN
  LINE (xo, yo - 1)-(xo + (jjs - jja + 1) * dx, yo - 3), 1, BF
  ELSE
  CALL type3frame(ihp, dx, xo, y2, y3, y4, jja, jjs, ihintr, iwi(1))
  END IF

IF inz <> 24 THEN
GOTO drawnow
ELSE
    yshift = 150
    CALL prof(axt(), 1, dw$, 0, inz, iendm, dx, gfa(1), ja, js, icc, xo, yo + yshift)
    CALL prof(axt(), 2, dw$, 0, inz, iendm, dx, gfa(2), ja, js, icc, xo, yo + yshift)
    CALL prof(axt(), 3, dw$, 0, inz, iendm, dx, gfa(3), ja, js, icc, xo, yo)
    CALL prof(axt(), 4, dw$, 0, inz, iendm, dx, -gfa(4), ja, js, icc, xo, yo + yshift)
    CALL prof(axt(), 5, dw$, 0, inz, iendm, dx, -gfa(5), ja, js, icc, xo, yo + yshift)
    CALL prof(axt(), 6, dw$, 0, inz, iendm, dx, -gfa(6), ja, js, icc, xo, yo + yshift)
    CALL prof(axt(), 7, dw$, 0, inz, iendm, dx, -gfa(7), ja, js, icc, xo, yo + yshift)
END IF

CASE 4, 5'----- schematic plot with letters --------
  iparm = 1
'''''only for FreeBasic   color 1,15
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 THEN CLS
  LOCATE 1, 1
 LOCATE 1, 1
 lastrow = 80
 PRINT "Size"; : PRINT USING "##"; ja; : PRINT "-"; : PRINT USING "###"; js;
  dummy$ = " …" + STRING$(js - ja + 1, "Õ") + "ª"
  PRINT MID$(dummy$, 1, lastrow - 11)
  FOR ilv = 1 TO LEN("s" + dw$ + "h")
    il = ASC(MID$("s" + dw$ + "h", ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    CALL prof(axt(), il, "s" + dw$ + "h", 0, inz, iendm, dx, gb, ja, js, 1, xo, yo)
  NEXT ilv
'''''''''''''''''  COLOR 1, 15
  PRINT "IT"; : PRINT USING "########"; itot&;
  dummy$ = " »" + STRING$(js - ja + 1, "Õ") + "º"
  IF (js - ja) > 65 THEN dummy$ = " »" + STRING$(lastrow - 46, "Õ") + " That is only the left portion..."
  PRINT MID$(dummy$, 1, lastrow - 11): PRINT : PRINT



CASE 6 TO 14'----- pixel-plot, 14 bottom up for phyllotaxis
  IF icc = 1 THEN
    SELECT CASE ivgashift 'units=screen-pixel
    CASE 12: xmax = 800: ymax = 600
    CASE 23: xmax = 1024: ymax = 786
    CASE 37: xmax = 1280: ymax = 1024
    CASE ELSE: xmax = 648: ymax = 480
    END SELECT
    IF inz > 5 AND inz < 10 THEN xmax = 648: ymax = 480' even at large screen normal (DOS) scaling
    WINDOW (0, 0)-(xmax, ymax)
    x1 = 1 + dx * ja + gxa(1)
    x2 = 1 + dx * js + gxa(1)
    y1 = ymax - 4: IF inz = 14 THEN y1 = 23

    IF inz = 8 OR inz = 12 THEN
      y1 = y1 - 50    'Frame for plot of stable pattern,  axt(8, i)
      CALL linep(ihp, x1 - iwi(1), y1 - 1, x2 + iwi(1), y1 - iwi(1), 2, 1, ivorcol, 1)
      CALL linep(ihp, x2 + 1, y1, x2 + iwi(1), y1 + 51, 2, 1, ivorcol, 1)
      CALL linep(ihp, x2 + iwi(1), y1 + 50 + iwi(1), x1 - iwi(1), y1 + 51, 2, 1, ivorcol, 1)
      CALL linep(ihp, x1 - 1, y1 + 51, x1 - iwi(1), y1, 2, 1, ivorcol, 1)
      amz = 45: IF gfa(8) > 0 THEN amz = 45 * gfa(8)
      xm = x1 + .5 * dx: ym = y1 + axt(8, ja) * amz
      FOR i = ja + 1 TO js  '
	  xz = xm + dx: yz = y1 + axt(8, i) * amz
	  CALL linep(ihp, xz, yz, xm, ym, 0, iwi(1), ivorcol, 1)
      xm = xz: ym = yz
      NEXT i
      y1 = y1 - 10 ' new starting point of the plots
   END IF
   END IF
   ydiff = 1: IF inz = 14 THEN ydiff = -1

  FOR ilv = LEN(displaywas$) TO 1 STEP -1
    il = ASC(MID$(displaywas$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0 's=source
      x1 = 1 + dx * ja + gxa(il)
      x2 = 1 + dx * (js + 1) + gxa(il)
       IF icc = 1 THEN ' first line of the frame
	 CALL linep(ihp, x1 - iwi(1), y1 + ydiff, x2 + iwi(1), y1 + ydiff * iwi(1), 2, 1, ivorcol, 1)
   END IF
    IF iendm = 99 OR y1 = 22 THEN 'last line
    iendm = 999
    CALL linep(ihp, x1 - iwi(1), y1, x2 + iwi(1), y1 - iwi(1) * ydiff, 2, 1, ivorcol, 1)
   EXIT SUB
   ELSE
    IF y1 < 22 THEN EXIT SUB 'reaching the lower end of the screen
	CALL prof(axt(), il, dw$, kv, inz, iendm, dx, gfa(il), ja, js, icc, x1, y1)

  END IF
  NEXT ilv
    y1 = y1 - ydiff



CASE 15  ' on circle, Nautilus
    il = 1
    CALL prof(axt(), il, dw$, 0, inz, iendm, dx, gfa(il), ja, js, icc, xo, yo)


CASE 16 ', 161 ' Circle, as for chemotactic orientation,
  IF iendm = 99 THEN EXIT SUB
IF inz = 16 OR icc = 1 THEN CALL clearscreen
  xo = gxa(1): yo = gya(1)
  IF gxa(1) = 0 THEN xo = 320: yo = 240
  radius = dx / 4 * (js - ja)
 axt(0, js + 1) = axt(0, ja)' Quelle ist sonst nicht definiert
'IF inz = 161 THEN yo = gya(1) + (icc - 1) * istelz
  FOR ilv = LEN(dw$) TO 1 STEP -1
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    IF il >= 0 AND il <= kn THEN
     CALL prof(axt(), il, dw$, ipattern, inz, iendm, dx, gfa(il), ja, js, icc, xo, yo)
    END IF
  NEXT ilv

  CIRCLE (xo, yo), radius, 0
  PAINT (xo, yo), 15, 0
  wi = -6.28 / (js - ja + 1)
  wi1 = wi * (ja - 1) + 1.57
  x1 = xo + radius * COS(wi1)
  y1 = yo + radius * SIN(wi1) ' * per
  FOR ix = ja TO js
  wi1 = wi * ix + 1.57
  xa = xo + radius * COS(wi1)
  ya = yo + radius * SIN(wi1)' * per
  CALL linep(ihp, x1, y1, xa, ya, 0, iwi(1), 0, 1)
  xm = xo + .8 * radius * COS(wi1)
  ym = yo + .8 * radius * SIN(wi1) '* per
  CALL linep(ihp, xm, ym, xa, ya, 0, iwi(1), 0, 1)
  x1 = xa: y1 = ya
  NEXT ix


  IF dy > 0 AND k2 > 0 THEN 'arrow
      'locate 1,1: print "vor arrow "; dy; k2;: input resp$
  wff = -6.28 * (k2 - 1) / (js - ja + 1) + 1.57
  aw = 3! * radius
  x = xo + aw * COS(wff)
  Y = yo + aw * SIN(wff) '* per
  xa = xo + aw * COS(wff) * .7
  ya = yo + aw * SIN(wff) * .7 '* per
  CALL linep(ihp, x, Y, xa, ya, 0, iwi(1), ivorcol, 1)
  x = xo + aw * COS(wff + .03) * .79
  Y = yo + aw * SIN(wff + .03) * .79' * per
  CALL linep(ihp, x, Y, xa, ya, 0, iwi(1), ivorcol, 1)
  x = xo + aw * COS(wff - .03) * .79
  Y = yo + aw * SIN(wff - .03) * .79 * per
  CALL linep(ihp, x, Y, xa, ya, 0, iwi(1), ivorcol, 1)
  END IF


CASE 17' ring of cells, plot in Y; for organizer
  IF iendm = 99 THEN EXIT SUB
  DeltaWi = 6.28 / (js - ja + 1)
  CALL clearscreen
  xo = gxa(1)
  IF gxa(1) = 0 THEN xo = 319: yhzyl = 240
  radius = dx / 2 * (js - ja)
  axt(0, js + 1) = axt(0, ja)' Quelle ist sonst nicht definiert
  FOR ilv = LEN(dw$) TO 1 STEP -1'erst hinten
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    CALL prof(axt(), il, dw$, 0, inz, iendm, dx, gfa(il), ja, js, icc, xo, gya(il))
  NEXT ilv

  FOR ilv = 1 TO LEN(dw$)    ' dann vorn
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    CALL prof(axt(), il, dw$, 1, inz, iendm, dx, gfa(il), ja, js, icc, xo, gya(il))
  NEXT ilv



CASE 18, 19, 191'Individual plots, area filled, 19 with new screen
'191 curves and pixel-plots, as for Ecoli
'18 untereinander, nur bei icc=1 gelîscht
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 OR kg > 0 THEN
  jam = ja
  xa = gxa(1)
  IF gxa(1) = 0 THEN xa = 320 - ((js - ja + 1) / 2) * dx
  IF gxa(1) > 550 THEN xa = gxa(1) - (js - ja) * dx' shift to the right     320 - ((js - ja + 1) / 2) * dx
  END IF
  xo = xa + dx * (ja - jam) 'to have correct position after change of field
  IF inz > 18 THEN CALL clearscreen
  IF inz = 18 AND icc = 1 THEN CALL clearscreen
GOTO drawnow

CASE 20 'for display of oscillation within a single cell
  IF icc = 1 THEN
    y1 = gya(1): y2 = y1 + dy: x1 = 10
    LINE (x1 - 2, y2)-(x1 + 1, y1 - 4), ivorcol, BF
  ELSE
    x1 = x1 + 1: LINE (x1, y1 - 2)-(x1 + 3, y1 - 4), ivorcol, BF
     END IF
  FOR ilv = LEN(dw$) TO 1 STEP -1
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    'IF il > 0 AND il <= kn THEN
    IF il <= kn THEN
    CALL prof(axt(), il, dw$, kv, inz, iendm, dx, gfa(il), ja, js, icc, x1, y1)
    END IF
  NEXT ilv
  
CASE 21'for gene activation, different colors for each element (genes, not cells)
  IF iendm = 99 THEN EXIT SUB
  xo3 = 320 - (js + 1) / 2 * dx
  xo = xo3 + (ja - 1) * dx
  x2 = xo + (js - ja + 2) * dx
  CALL clearscreen
    yo = gya(3): y2 = yo + axt(3, 1) * gfa(3) * 40
    LINE (xo, yo)-(x2, y2), ico(3), BF
    CALL prof(axt(), 1, dw$, 0, inz, iendm, dx, gfa(1), ja, js, icc, xo, gya(1))
    CALL prof(axt(), 2, dw$, 0, inz, iendm, dx, gfa(2), ja, js, icc, xo, gya(2))

CASE 22'x-distribution as pixel density, y-axis arbitrary and constant
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 OR kg > 0 THEN xo = 320 - (js / 2 + ja - 1) * dx
  CALL clearscreen
  FOR ilv = LEN(dw$) TO 1 STEP -1
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il > 0 AND il < 9 THEN
    yo = gya(il)
    'CALL prof(axt(), il, dw$, ipat(il), inz, iendm, dx, gfa(il), ja, js, icc, xo, yo)
    CALL prof(axt(), il, dw$, 0, inz, iendm, dx, gfa(il), ja, js, icc, xo, yo)
    END IF
  NEXT ilv


CASE 23  'two plots side by side (for regeneration)
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 OR kg > 0 THEN
  y2 = 440: yo = 60:
  xa = gxa(1): yo = gya(1): y2 = 450: IF dyp > 0 THEN y2 = yo + dyp - 50
  IF xa = 0 THEN xa = 320 - (js - ja + 1) / 2 * dx
  jam = ja:
  END IF
  iwi2 = iwi(1) / 2
  y3 = yo - iwi(1): y4 = yo - iwi(1) - iwi2
  xo = xa + dx * (ja - jam) 'to have correct position after change of field
  CALL clearscreen
  xo = xa: jja = ja: jjs = k4 - 1:
  CALL type3frame(ihp, dx, xo, y2, y3, y4, jja, jjs, ihintr, iwi(1))
  FOR il = kn TO 0 STEP -1
    CALL prof(axt(), il, dw$, 0, 3, iendm, dx, gfa(il), ja, k4 - 1, icc, xo, yo)
  NEXT il
  jja = k4 + 1: jjs = js: xo = xa + (jja - ja) * dx
  CALL type3frame(ihp, dx, xo, y2, y3, y4, jja, jjs, ihintr, iwi(1))
  FOR il = kn TO 0 STEP -1
    CALL prof(axt(), il, dw$, 0, 3, iendm, dx, gfa(il), k4 + 1, js, icc, xo, yo)
  NEXT il

CASE 25 'colored random dots for substances according to dw$
case25:   'level il, density proportional to level
yo = gya(1) - ihintr * (icc - 1)
 xo = gxa(1) + (ja - 1) * dx
from19to25:
  dwtemp$ = dw$
  istelzm = istelz: IF istelzm = 0 THEN istelzm = 30
  dxt = (js - ja + 1) * dx
  yoh = yo + istelzm
  CALL linep(ihp, xo, yo - 1, xo + dxt + 1, yoh, 2, iwil, ibackcol, 1)
  CALL linep(ihp, xo, yo - 1, xo + dxt + 1, yoh, 1, iwil, ivorcol, 1)
  CALL linep(ihp, xo - 1, yo - 2, xo + dxt + 2, yoh + 1, 1, iwil, ivorcol, 1)
  idx = dx
  FOR ix = ja TO js
    xb = xo + (ix - ja) * dx + 1
    yb = yo
    su = 0
    ill = 0
    FOR ilv = 1 TO LEN(dwtemp$)
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    IF il >= 0 AND il < 9 THEN
    ill = ill + 1
      ibm(ill) = ico(il)
      am(ill) = axt(il, ix) * .3 * gfa(il)
      su = su + am(ill)
    END IF
    NEXT ilv

      anc = 0
  IF su > .01 THEN
      FOR ibc = 1 TO ill'normieren
	IF su < 1! THEN
	  anc = anc + am(ibc)
	ELSE
	  anc = anc + am(ibc) / su
	END IF
   agfa(ibc) = anc'anteil summativ
      NEXT ibc
      FOR ikx = 1 TO idx'Pixel horizontal
	ybb = yb
	  FOR iKY = 1 TO istelz
	    rra = RND
	    IF rra < agfa(ill) THEN
	      i = 1
	      DO UNTIL rra < agfa(i)
		i = i + 1
'                IF i > ill THEN EXIT DO
	      LOOP
	      icolor = ibm(i)
	      CALL apnp(ihp, xb, ybb, 0, 2, icolor, 1)
	    END IF
	    ybb = ybb + 1!
	  NEXT iKY
	xb = xb + 1!
      NEXT ikx
    END IF
  NEXT ix
EXIT SUB

CASE 26' ring of cells for organizer
  IF iendm = 99 THEN EXIT SUB
  CALL clearscreen
  radius = dx * (js - ja): xo = gxa(1): yhzyl = gya(1):
  'radius = 10 * dx: xo = gxa(1): yhzyl = gya(1):
GOTO drawnow

CASE 27'plot as disk, for phyllotaxis
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 THEN radius = 222
GOTO drawnow
   radius = .995 * radius'NÑchster Plot etwas kleinerer Radius bei kd=27

CASE 28'plot as cylinder, for phyllotaxis
  IF iendm = 99 THEN EXIT SUB
  IF icc = 1 THEN   'initial settings
  yhzyl = gya(1)
  xo = gxa(1): icountl = 0
  radius = dx / 2 * (js - ja)
  IF gxa(1) = 0 THEN xo = 319: yhzyl = 40
  END IF
  icountl = icountl + 1
  IF icountl > 8 THEN 'yellow circle to illustrate the cylinder
   CIRCLE (xo, yhzyl), radius, 14, , , per
   yhzyl = yhzyl + 1: icountl = 0
  END IF
   yhzyl = yhzyl + 1
GOTO drawnow

CASE ELSE
mess$ = "for this kd no graphic display is yet installed; routine showprof"
CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
  iendm = 999

END SELECT
EXIT SUB

drawnow:
  FOR ilv = LEN(dw$) TO 1 STEP -1
    il = ASC(MID$(dw$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    'IF il > 0 AND il <= kn THEN
    IF il >= 0 AND il <= kn THEN
    CALL prof(axt(), il, dw$, ipattern, inz, iendm, dx, gfa(il), ja, js, icc, xo, gya(il))
    END IF
  NEXT ilv

SELECT CASE inz
CASE 191
   yo = gya(1) - istelz - 20
   GOTO from19to25
CASE 27: radius = .995 * radius 'NÑchster Plot etwas kleinerer Radius bei kd=27
END SELECT


END SUB

SUB type3frame (ihp, dx, xo, y2, y3, y4, jja, jjs, ihintr, iwil)
'ihintr macht marks including thickness if > 0
  x2 = xo - dx / 2 + (jjs - jja + 1) * dx
    CALL linep(ihp, xo - dx / 2, y4, xo - dx / 2, y2 + iwil / 2, 0, iwil, ivorcol, 1)
    CALL linep(ihp, xo - dx / 2 - iwil / 2, y3 - 1, x2 + iwil / 2, y3 - 1, 0, iwil, ivorcol, 1)
    CALL linep(ihp, x2, y4, x2, y2 + 2, 0, iwil, ivorcol, 1)
    x3 = xo - dx / 2
    IF ihintr > 0 THEN 'marks for the cells
    FOR i = jja TO jjs + 1
      CALL linep(ihp, x3, y4, x3, y4 - 5, 0, ihintr, ivorcol, 1)
      x3 = x3 + dx
    NEXT i
   END IF
END SUB

