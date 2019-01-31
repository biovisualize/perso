DECLARE SUB lightgray (icoloc%, ir%, ig%, ib%)
DECLARE SUB switchscreen (inz%, iactive%, igraph%, kd%, programname$, fdelay)
DECLARE SUB rwparameterdisk (RW$, ifilenumber%)
DECLARE SUB whatprmfiles (s$(), programnameloc$, progrtmp$, exten$, drive$, irfilename$, ilineparam%)
DECLARE SUB screencheck (igraph%, lastline%, lastrow%, ivgashift%, ifree%)
DECLARE SUB disp (kdisplay%, il%, ax!(), ico%, iwi%, ff!, xo!, yo!, kax%, kx%, kay%, ky%, tvv$)
DECLARE SUB backfront (inz%, x0!, x1!, y4!, y1!, s0r!, s1r!, s0l!, s1l!, ico%, iwil%, px!, p4!, pyy!)
DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DECLARE SUB growthxy (iinsert%, js%, imx%, kn%, lax%(), lx%(), lay%(), ly%(), ax!())
DECLARE SUB leftright (x1!, x2!, x3!, y1!, y2!, y3!, s1r!, s2r!, s3r!, s1l!, s2l!, s3l!, ico%, iwil%, px!, p4!)
DECLARE SUB linep (ihp%, x1!, y1!, x2!, y2!, ipat%, iwidth%, ico%, ityp%)
DECLARE SUB perpendicular (x0!, y0!, ybase0!, s0l!, s0r!, ico%, iwil%)
DECLARE SUB rl (inz%, x2!, y2!, s2!, x1!, yto!, s1!, y3!, isi%, iy%, jyto%, ico%, iwil%, px!)
DECLARE SUB zeingabe (igt%, iquest%, inz%, i%, f!, labels$, text$)

DEFINT I-N
' $INCLUDE: 'scommon.bi'
' $INCLUDE: '..\subs\errlist.bas'

SUB disp (kdisplay, il, axy(), icol, iwil, ff, xo, yo, kax, kx, kay, ky, tvv$) STATIC
SHARED icolm(), ibm(), a2(), ansp(), icc, kg
SHARED ihp, dx, dy, dz, per, label$(), displaywas$
SHARED istelz, iletztk, ihintr, krot, ivorcol, ibackcol
SHARED msiluet(), msiluetmax, icheckerror
SHARED idxr, idxl, idyo, idyu
DIM ty$(14)


SELECT CASE kdisplay
CASE 0, 1, 2, 3, 4, 5, 6, 110 TO 115
  FOR i = 1 TO msiluetmax: msiluet(i) = 0: NEXT
  IF krot = 0 OR kdisplay = 5 OR kdisplay = 6 THEN krot = 2
  IF krot * kx + ky > msiluetmax THEN
    krot = (msiluetmax - ky) / kx
    mess$ = "KROT too large, korrected to " + STR$(krot)
    CALL cmessage(6, -1, 1, 15, 6, ibackcol, mess$, resp$)
  END IF
  winkel = ATN(1 / krot)
  pdx = dx * COS(winkel)
  px = pdx / krot
  pdyx = per * SIN(winkel) * dx
  pyx = pdyx / krot
  pyy = per * SQR(dx * dx - px * px)
  f1 = 40 * ff
  fmodus = f1 / krot
END SELECT

SELECT CASE kdisplay
CASE 0 TO 4' ================ Hidden line plot, 2=reverse
  IF krot = 0 THEN krot = 2
  jyfrom = kay: jyto = ky: iystep = 1
  IF kdisplay = 2 THEN
    pdyx = -pdyx: pyx = -pyx: pyy = -pyy: jyfrom = ky: jyto = kay: iystep = -1
  END IF
  kxfirstmem = krot * (kx - 1) + kay
  kxlastmem = kx
  FOR iy = jyfrom TO jyto STEP iystep
    kxlast = kx
    y2 = axy(il, kx, iy) * f1
    IF kdisplay <> 3 THEN 'skip zero-concentration in all other modes
      DO WHILE y2 = 0 AND kxlast > kax
	kxlast = kxlast - 1
	y2 = axy(il, kxlast, iy) * f1
      LOOP
    END IF
    IF kxlastmem <= kxlast AND kxlastmem > kax AND kdisplay = 1 THEN '-------basic lines after zero-skip,
    'isi = krot * (kxlast - 1) + iy
    's2 = msiluet(isi)
    'hier mu0 noch die Bremse hinein wenn es keliner geworden war und wieder gr”áer wurde
      ybaseto = yo - pdyx * (kxlast) + (iy - 1) * pyy
      x2 = xo + px * iy + pdx * (kxlast - 1)
      y1 = ybaseto + (kxlast - kxlastmem) * pdyx
      x1 = x2 - (kxlast - kxlastmem) * pdx
      IF kxlastmem < kxlast THEN
	CALL linep(ihp, x2, ybaseto, x1, y1, 0, iwil, icol, 1)'inner corner
	CALL linep(ihp, x1, y1, x1, y1 + axy(il, kxlastmem, iy) * f1, 0, iwil, icol, 1)
	CALL backfront(kdisplay, x1, x1 - px, y1, y1 - pyy, 0, 0, 0, 0, icol, iwil, px, p4, pyy)
      ELSE '= no change of size
       IF iy > jyfrom THEN CALL backfront(kdisplay, x1, x1 - px, y1, y1 - pyy, 0, 0, 0, 0, icol, iwil, px, p4, pyy)
      END IF
    END IF
    kxlastmem = kxlast
    'IF kxlast > kxlastmem THEN kxlastmem = kxlast
    ybaseto = yo - pdyx * (kxlast) + (iy - 1) * pyy
    y2 = y2 + ybaseto
    isi = krot * (kxlast - 1) + iy
    x2 = xo + px * iy + pdx * (kxlast - 1)
    s2 = msiluet(isi)
    CALL perpendicular(x2, y2, ybaseto, s2, s2, icol, iwil)
    ivisible = 1'may change if kdisplay=3
    IF y2 > msiluet(isi) THEN msiluet(isi) = y2
    FOR ix = kxlast - 1 TO kax STEP -1
'   IF ihintr < 1 THEN
'   IF axy(1, ix, iy) > .5 THEN icol = 2
'   IF axy(2, ix, iy) > .5 THEN icol = 1
'   IF axy(3, ix, iy) > .5 OR axy(3, ix + 1, iy) > .5 THEN icol = 12
'   END IF
    IF kdisplay > 2 THEN
	IF iy = jyfrom THEN ' front
	  CALL perpendicular(x2, y2, ybaseto, s2, s2, icol, iwil)
	ELSE
	  yto = axy(il, ix + 1, iy - 1) * f1
	  IF kdisplay = 4 AND yto = 0 THEN
	    CALL perpendicular(x2, y2, ybaseto, s2, s2, icol, iwil)
	  ELSE
	    y3 = ybaseto - pyy + yto
	    s1 = msiluet(isi - 1)
	    CALL backfront(kdisplay, x2, x2 - px, y2, y3, s2, s1, s2, s1, icol, iwil, px, p4, pyy)
	  END IF
	END IF
      END IF
      'ybaseto = ybaseto + pdyx
      yto = axy(il, ix, iy) * f1
      IF yto = 0 THEN
       IF kdisplay <> 3 THEN
	IF kdisplay < 3 THEN CALL perpendicular(x2, y2, ybaseto, s2, s2, icol, iwil)
	kxfirst = isi
	GOTO nextiycase2
      END IF
      END IF
      ybaseto = ybaseto + pdyx
      yto = yto + ybaseto
      CALL rl(kdisplay, x2, y2, s2, x1, yto, s1, y3, isi, iy, jyto, icol, iwil, px)
    NEXT ix
    IF kdisplay > 2 AND iy > jyfrom THEN
      yto = axy(il, kax, iy - 1) * f1
      IF kdisplay = 4 AND yto = 0 THEN
	CALL perpendicular(x2, y2, ybaseto, s2, s2, icol, iwil)
      ELSE
	y3 = ybaseto - pyy + yto
	s1 = msiluet(isi - 1)
	CALL backfront(kdisplay, x2, x2 - px, y2, y3, s2, s1, s2, s1, icol, iwil, px, p4, pyy)
      END IF
    ELSE
      CALL perpendicular(x2, y2, ybaseto, s2, s2, icol, iwil)
    END IF
    kxfirst = isi
nextiycase2:
    IF kdisplay = 4 AND kxfirst < kxfirstmem AND iy > kay THEN ' zero konzentration
	  idiff = (kxfirstmem - kxfirst) ' are skipped with kddisplay = 4
      x1 = x2 + px * idiff
      y1 = ybaseto - pyx * idiff
      CALL linep(ihp, x2, ybaseto, x1, y1, 0, iwil, icol, 1)
    END IF
    IF kxfirst < kxfirstmem THEN kxfirstmem = kxfirst
  NEXT iy
  irx = 0
  iry = 0
  GOTO frame
 CASE 110 TO 115 'for dotted line at the end
   iy = ky
    y2 = axy(il, kx, iy) * f1
      ybaseto = yo - pdyx * kx + (iy - 1) * pyy
      x2 = xo + px * iy + pdx * (kx - 1)
      y2 = y2 + ybaseto
      CALL linep(ihp, x2, y2, x2, ybaseto, 0, iwil, icol, kdisplay - 110)

    FOR ix = kx - 1 TO kax STEP -1
      ybaseto = ybaseto + pdyx
      y1 = axy(il, ix, iy) * f1 + ybaseto
      x1 = x2 - pdx
      CALL linep(ihp, x2, y2, x1, y1, 0, iwil, icol, kdisplay - 110)
      x2 = x1: y2 = y1
    NEXT ix
      CALL linep(ihp, x1, y1, x1, ybaseto, 0, iwil, icol, kdisplay - 110)

CASE 5, 6 'column-graphic
  ddx = .01 * dx
  w9 = ATN(.5)
  px = dx * COS(w9) * .5
  pyy = per * SQR(dx * dx - px * px)
  py = per * SIN(w9) * dx * .5
  pdx = 2 * px
  pdy = 2 * py
  sch = 2
  p4 = px / (pyy + py)
  FOR iy = kay TO ky
    isi = 2 * kx + iy
    x1 = xo - 1.5 * px + px * isi
    yos = yo - (pyy + pdyx) / 2
    ybase1 = yos - pdy * (kx + 1) + (iy - 1) * pyy
    FOR ix = kx TO kax STEP -1
      x0 = x1 + px
      x2 = x1 - px
      x3 = x1 - pdx
      ybase3 = ybase1 + pdy
      ybase0 = ybase1 + pyy

      yhight = axy(il, ix, iy) * f1
      y1 = ybase1 + yhight
      y2 = y1 + py
      y3 = y1 + pdy
      y4 = y1 + pyy
      y5 = y4 + py
      y6 = y4 + pdy
      s0r = msiluet(isi + 1)
      s1r = msiluet(isi)
      s2r = msiluet(isi - 1)
      s3r = msiluet(isi - 2)
      isil = isi + 200
      s0l = msiluet(isil + 1)
      s1l = msiluet(isil)
      s2l = msiluet(isil - 1)
      s3l = msiluet(isil - 2)
      '   IF kdisplay = 6 AND yhight > sch THEN
      '   ybase2 = ybase1 + dy

      '   CALL xbackfront(x0, x1, ybase0, ybase1, s0r, s1r, s0l, s1l,iwil,icol,ihp, px, p4)
      '   END IF

      IF kdisplay = 5 OR yhight > sch THEN
	CALL leftright(x1, x2, x3, y1, y2, y3, s1r, s2r, s3r, s1l, s2l, s3l, icol, iwil, px, p4)
	CALL leftright(x0, x1, x2, y4, y5, y6, s0r, s1r, s2r, s0l, s1l, s2l, icol, iwil, px, p4)
	CALL backfront(kdisplay, x0, x1, y4, y1, s0r, s1r, s0l, s1l, icol, iwil, px, p4, pyy)
	CALL backfront(kdisplay, x2, x3, y6, y3, s2r, s3r, s2l, s3l, icol, iwil, px, p4, pyy)
	CALL perpendicular(x0, y4, ybase0, s0l, s0r, icol, iwil)
	CALL perpendicular(x1, y1, ybase1, s1l, s1r, icol, iwil)
	CALL perpendicular(x3, y3, ybase3, s3l, s3r, icol, iwil)
      END IF
      'new silhouette
      IF y4 > msiluet(isil + 1) THEN msiluet(isil + 1) = y4
      IF y5 > msiluet(isi) THEN msiluet(isi) = y5
      IF y5 > msiluet(isil) THEN msiluet(isil) = y5
      IF y6 > msiluet(isi - 1) THEN msiluet(isi - 1) = y6
      IF y6 > msiluet(isil - 1) THEN msiluet(isil - 1) = y6
      IF y3 > msiluet(isi - 2) THEN msiluet(isi - 2) = y3
      x1 = x3
      ybase1 = ybase3
      isi = isi - 2
    NEXT ix
  NEXT iy
  irx = 1
  iry = 1
  GOTO frame

CASE 7
zylinder:  '-----------------------------------------------------
  iwil = iwil
  radius = dx * (ky - kay + 1) / 6.28
  fpy = 4 * dx * ff
  sq = dx * (1 - per * per)
  xpl = xo
  ypl = yo - sq * kx / 2
  IF krot = 5 THEN ypl = yo
  IF krot = 6 THEN ypl = yo - sq * kx
  aaa = 0: k1t = kax: kxt = kx
  IF krot >= 3 THEN
    aaa = axy(il, kx, 1)'Spitze
    kxt = kx - 1
  END IF
  aab = 0
  IF krot >= 4 THEN
    aab = axy(il, kax, 1)'Spitze
    k1t = kax + 1
  END IF
  wq = 6.28 / (ky - kay + 1)
  ' drawing of the circles
  FOR ix = k1t TO kxt
    '       nx = kxt + kax - ix
    iwile = iwil' thickness of lines, higher in the foreground
    ym = ypl + sq * (ix)
    angle = dy * wq
    pr = axy(il, ix, kay) * fpy + radius
    y1 = ym + per * pr * COS(angle)
    x1 = xpl + pr * SIN(angle)
    FOR iy = kay + 1 TO ky
      angle = ((iy - kay) + dy) * wq
      pr = axy(il, ix, iy) * fpy + radius
      y2 = ym + per * pr * COS(angle)
      x2 = xpl + pr * SIN(angle)
      IF COS(angle) < 0 OR ix = k1t THEN iwile = iwil + 1
      CALL linep(ihp, x1, y1, x2, y2, 0, iwile, icol, 1)
      'IF iwile > iwil THEN
      'END IF
      x1 = x2
      y1 = y2
    NEXT iy

    '       nx = kxt + k1t - ix
    '       nx = kxt + kax - ix
    angle = dy * wq
    pr = axy(il, ix, kay) * fpy + radius
    y2 = ym + per * pr * COS(angle)
    x2 = xpl + pr * SIN(angle)
    CALL linep(ihp, x1, y1, x2, y2, 0, iwile, icol, 1)
  NEXT ix

  ' drawing the lines
  'nx = kxt + kax - k1t
  yma = ypl + sq * (kxt) + aaa * fpy * (1 - per ^ 2)
  ymb = ypl + sq * k1t - aab * fpy * (1 - per ^ 2)
  FOR iy = kay TO ky
    angle = (iy - kay + dy) * wq
    iwile = iwil
    IF COS(angle) < 0 THEN iwile = iwil + 1
    x1 = xpl'lines start at the centre
    y1 = yma

    FOR ix = kxt TO k1t STEP -1
      '   nx = kxt + kax - ix
      ym = ypl + sq * (ix)
      angle = (iy - kay + dy) * wq
      pr = axy(il, ix, iy) * fpy + radius
      y2 = ym + per * pr * COS(angle)
      x2 = xpl + pr * SIN(angle)
      iwiler = iwile
      IF krot < 2 AND ix = kx THEN
	x1 = x2: y1 = y2
      ELSE
	CALL linep(ihp, x1, y1, x2, y2, 0, iwile, icol, 1)
      END IF
      x1 = x2
      y1 = y2
    NEXT ix
    IF krot >= 4 THEN CALL linep(ihp, x1, y1, xpl, ymb, 0, iwile, icol, 1)'second tip
  NEXT iy'


CASE 8, 9  '------------------------------------pixel-------------------------------------
  dxt = (kx - kax + 1) * dx
  ddy = dx
'''  ico = ico(il)
IF dy > 0 AND kdisplay = 9 THEN ddy = dy
  dyt = (ky - kay + 1) * ddy
  IF xo = 0 THEN
  IF icc = 1 OR kg > 0 THEN
  xoa = 320 - (kx / 2 + kax - 1) * dx
  jam = kax
  END IF
  xa = xoa + dx * (kax - jam)
  ELSE
  xa = xo + dx * (kax - 1)
  END IF
  CALL linep(ihp, xa - 1, yo - 1, xa + dxt, yo + dyt + 1, 1, iwil, ivorcol, 1)
  CALL linep(ihp, xa - 2, yo - 2, xa + dxt + 1, yo + dyt + 2, 1, iwil, ivorcol, 1)
  FOR ix = kax TO kx
    FOR iy = kay TO ky
      icoly = icol
      IF kdisplay = 9 OR kdisplay = 19 THEN
      icoly = iy + icol - 1: IF icoly > 14 THEN icoly = icoly - 14
      icoly = ibm(icoly)
      END IF
      x1 = xa + (ix - kax) * dx
      y1 = yo + (iy - kay) * ddy
      x2 = x1 + dx
      y2 = y1 + ddy
      n = 8 * axy(il, ix, iy) * ff
      IF n < krot THEN n = 0
  IF n > 0 THEN CALL linep(ihp, x1, y1, x2, y2, 3, iwil, icoly, n)
    NEXT iy
  NEXT ix

CASE 11' colored random dots, density proportional to level
  IF xo = 0 THEN
  IF icc = 1 OR kg > 0 THEN
  xa = 320 - ((js - ja + 1) / 2) * dx
  jam = ja
  END IF
  xo = xa + dx * (ja - jam) 'to have correct position after change of field
  END IF
  inkd = dx
  dxx = (kx - kax + 1) * inkd
  dyy = (ky - kay + 1) * inkd

  IF il = ASC(MID$(displaywas$, LEN(displaywas$), 1)) - 96 THEN
    'clears region at lowerst priority
  LINE (xo - 1, yo - 1)-(xo + dxx + 1, yo + dyy + 1), ibackcol, BF
  CALL linep(ihp, xo - 1, yo - 1, xo + dxx + 2, yo + dyy + 2, 1, 1, ivorcol, 1)
  CALL linep(ihp, xo, yo, xo + dxx + 1, yo + dyy + 1, 1, 1, ivorcol, 1)
  END IF
  FOR ix = kax TO kx
    FOR iy = kay TO ky
      xb = xo + (ix - kax) * dx + 1
      yb = yo + (iy - kay) * dx + 1
      anc = axy(il, ix, iy) * ff * .5
	FOR ikx = 1 TO inkd
	  ybb = yb
	  FOR iky = 1 TO inkd
	    rra = RND
	    IF rra < anc THEN
     IF iwil > 1 THEN
	CALL linep(ihp, xb, ybb, xb + iwil, ybb + iwiloc, 2, 1, icol, 1)
      ELSE
	CALL linep(ihp, xb, ybb, xb, ybb, 2, 1, icol, 1)
 '       CALL apnp(ihp, xb, ybb, 0, iwiloc, icol, 1)
      END IF
	    END IF
	    ybb = ybb + 1!
	  NEXT iky
	  xb = xb + 1!
	NEXT ikx
    NEXT iy: NEXT ix


CASE 12, 23 TO 25
    am = -1
    FOR iy = kay TO ky
      FOR ix = kax TO kx
	IF axy(il, ix, iy) > am THEN am = axy(il, ix, iy)
      NEXT ix
    NEXT iy


PRINT "  "; tvv$; : PRINT "-Maximum = ";
IF am >= 1000 THEN
  PRINT USING "#######.##"; am;
ELSE
  PRINT USING "###.####"; am;
END IF
PRINT itot; " iterations   ";
PRINT " Field size: ";
PRINT USING "###"; kax; kx; kay; ky

ploxprint: 'subroutine which can be reached with gosub
'Symbols for plot-labelling
ul$ = "É": um$ = "Í": ur$ = "»": ml$ = "º": mr$ = "º": ll$ = "È"
lm$ = "Í": lr$ = "¼"
ty$(0) = "-": ty$(1) = " ": ty$(2) = ".": ty$(3) = ":": ty$(4) = "°"
ty$(5) = "±": ty$(6) = "²": ty$(7) = "Û": ty$(8) = "Û": ty$(9) = "X"
ty$(10) = "X": ty$(11) = "X"
amx = 7 / (am + .00001): ram100 = 100 / (am + .0000001)

IF am < .0001 THEN EXIT SUB
'------upper frame-----
'PRINT SPACE$(LEN(label$(1)));
PRINT ul$;
FOR ix = kax TO kx: PRINT um$;
NEXT ix
PRINT ur$
IF kx - kax < 16 THEN zwraum$ = SPACE$(22 - kx + kax)
FOR iy = ky TO kay STEP -1
'  IF iy < 10 THEN PRINT label$(iy);no longer used
  PRINT mr$;
  FOR ix = kax TO kx
    n = axy(il, ix, iy) * amx + 1.0001
    IF n <= 0 THEN n = 0: IF n > 11 THEN n = 11
    PRINT ty$(n);
    msiluet(ix) = axy(il, ix, iy) * ram100 + .4
  NEXT ix
  PRINT mr$;
  IF kx - kax > 16 THEN
    PRINT
  ELSE
    PRINT zwraum$;
    FOR ix = kax TO kx: PRINT USING "###"; msiluet(ix);
    NEXT ix
    PRINT
  END IF
NEXT iy
'PRINT SPACE$(LEN(label$(1)));
PRINT ll$;
FOR ix = kax TO kx: PRINT lm$;
NEXT ix
PRINT lr$
IF kdisplay = 25 THEN INPUT "[OK]"; resp$
EXIT SUB

frame:
  '---front and left
 xos = xo: IF kdisplay = 5 OR kdisplay = 6 THEN xos = xo - 1.5 * px
 yos = yo: IF kdisplay = 5 OR kdisplay = 6 THEN yos = yo - (pyy + pdyx) / 2
  x1 = xos + px * (krot * (kax - 1) + kay)
  y1 = yos - pdyx * kax + pyy * (kay - 1)
  x2 = x1 + pdx * (kx - kax + irx)
  y2 = y1 - pdyx * (kx - kax + irx)
  'behind and right--------------
  x3 = x2 + px * (ky - kay + irx)
  IF kdisplay = 7 THEN x3 = x2 + px * (ky - kay)
  y3 = y2 + pyy * (ky - kay + iry)
  x4 = x1 + px * (ky - kay + irx)
  y4 = y1 + pyy * (ky - kay + iry)
  SELECT CASE kdisplay
  CASE 0, 1, 3, 4, 5, 6, 10
  CALL linep(ihp, x1, y1, x2, y2, 0, iwil, icol, 0)
  CALL linep(ihp, x2, y2, x3, y3, 0, iwil, icol, 0)
   IF ihintr < 1 THEN EXIT SUB
    IF ihintr > 0 THEN CALL linep(ihp, x3, y3, x4, y4, 0, 1, icol, 4)
    ver = axy(il, kax, ky) * f1
    IF ihintr > 1 THEN CALL linep(ihp, x4, y4, x4, y4 + ver, 0, 1, icol, 4)
    IF ihintr > 0 THEN CALL linep(ihp, x4, y4, x1, y1, 0, 1, icol, 4)

    IF kdisplay = 6 THEN
      CALL linep(ihp, x3, y3, x4, y4, 0, iwil, icol, 2)'
      CALL linep(ihp, x4, y4, x1, y1, 0, iwil, icol, 2)'
    END IF
    IF kdisplay = 10 THEN
      CALL linep(ihp, x3, y3, x4, y4, 0, iwil, icol, 1)'
      CALL linep(ihp, x4, y4, x1, y1, 0, iwil, icol, 1)'
    END IF
  CASE 2
'iwil>1 geht nicht
    IF ihintr = 1 THEN CALL linep(ihp, x2, y2, x1, y1, 0, 1, icol, 5)'back
    IF ihintr = 2 THEN CALL linep(ihp, x2, y2, x1, y1, 0, 1, icol, 1)
    CALL linep(ihp, x4, y4, x3, y3, 0, 1, icol, 1)'front
    CALL linep(ihp, x4, y4, x1, y1, 0, 1, icol, 1) 'left
    CALL linep(ihp, x2, y2, x3, y3, 0, 1, icol, 5) 'right
  END SELECT
  IF istelz > 0 AND iletztk > 10 THEN
    stelzm = iletztk - y1
    CALL linep(ihp, x1, y1, x1, y1 + stelzm, 0, 1, icol, 2)'Stelzen
    CALL linep(ihp, x2, y2, x2, y2 + stelzm, 0, 1, icol, 2)'Stelzen
    CALL linep(ihp, x3, y3, x3, y3 + stelzm, 0, 1, icol, 2)'Stelzen
    CALL linep(ihp, x4, y4, x4, y4 + stelzm, 0, 1, icol, 2)'Stelzen
  END IF
  iletztk = y1
  EXIT SUB
CASE ELSE

END SELECT' -----------------------------------------------------------------------
EXIT SUB
END SUB

'DEFBL A-G, O-Z
SUB manipulateXY (mwasnun$, iinsert) STATIC
''$INCLUDE: 'SHARED.bi'
SHARED axy(), iwas, kdisplay, displaywas$, imx, imy
SHARED lax(), lx(), lay(), ly(), gxa(), gfa(), gya(), ico()
SHARED icolm(), ibm(), a2(), ansp(), kax, ja, js, kay, jy, icc
SHARED itypus(), iwi(), label$(), title$, msiluet()
SHARED ihp, dxp, dyp, dx, dy, per, krot, ivgashift, igraph, ianz, ireaddata
SHARED istelz, iletztk, ihintr, iparam, iactive, iwidthm, ianima
SHARED ivorcol, ibackcol, dataname$, readdatafile$, writedatafile$
SHARED itot, kn, igt, ilm
SHARED idxr, idxm, idyu, idym, ipcx, ipcs

startmanixy:

SELECT CASE mwasnun$

CASE "GM"     'growth at the right margin
      iinsert = js
      CALL growthxy(iinsert, js, imx, kn, lax(), lx(), lay(), ly(), axy())

CASE "CA", "A"
il = -1: ix = 0: iy = 0
CALL zeingabe(igt, 0, 1, il, dummy, resp$, "Substance to be changed, 1= a, 2=b, ect.")
IF il = -1 THEN EXIT SUB
CALL zeingabe(igt, 0, 1, ix, dummy, resp$, "ix of the cell to be changed")
IF ix = 0 THEN EXIT SUB
CALL zeingabe(igt, 0, 1, iy, dummy, resp$, "iy of the cell to be changed")
IF iy = 0 THEN EXIT SUB
CALL zeingabe(igt, 1, 2, ix, axy(il, ix, iy), resp$, "new value")

CASE "S"        'to store present state of cells in memory arrays
 IF 2 * jy > imy THEN
  mess$ = "Array too large to be stored, (jy too large or imy too small)"
  resp$ = "OK"
  CALL cmessage(4, -1, 1, 15, 12, ibackcol, mess$, resp$)
  EXIT SUB
 END IF

kaxmem = lax(1): kxmem = lx(1): kaymem = lay(1): kymem = ly(1)
kyadd = imy / 2
      FOR il = 1 TO kn: FOR ix = kax TO js: FOR iy = kay TO jy
       axy(il, ix, iy + kyadd) = axy(il, ix, iy)
      NEXT iy: NEXT ix: NEXT il
CALL cmessage(3, -1, 1, 15, 10, ibackcol, "State has been stored", "OK")

CASE "R"        'to restore former state of cells from memory
 IF kxmem = 0 THEN
   mess$ = "sorry, nothing was stored"
   CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
   EXIT SUB
 END IF
kax = kaxmem: js = kxmem: kay = kaymem: jy = kymem: icc = 1
      FOR il = 1 TO kn:
      lax(il) = kaxmem: lx(il) = kxmem: lay(il) = kaymem: ly(il) = kymem
      FOR ix = ja TO js: FOR iy = kay TO jy
       axy(il, ix, iy) = axy(il, ix, iy + kyadd)
	 NEXT iy: NEXT ix: NEXT il
mess$ = "      State has been restored"
CALL cmessage(3, -1, 1, 15, 2, ibackcol, mess$, "OK")


CASE "C"'to change x-extention of the field
  kanew = kax: ksnew = js
mess$ = " Input new (smaller) field size: Present field size is: " + STR$(kax) + " - " + STR$(js)
CALL cmessage(3, -2, 1, 15, 4, ibackcol, mess$, "OK")
CALL zeingabe(igt, 1, 1, kanew, dummy, dummy$, "New left-most cell")
CALL zeingabe(igt, 1, 1, ksnew, dummy, dummy$, "New right-most cell")
CALL cmessage(3, -2, 1, 15, 4, ibackcol, "", "OK")
  IF ksnew = 0 THEN ksnew = js
  IF kanew < kax THEN kanew = kax
  js = ksnew
  kax = kanew
      FOR il = 1 TO kn: lx(il) = js: lax(il) = kax: NEXT


CASE "DE"      'to remove a piece (delete)
  irem1 = kax: rem2 = js
  mess$ = " present field size is now " + STR$(kax) + "; " + STR$(js)
  CALL cmessage(3, -1, 1, 15, 4, ibackcol, mess$, "OK")
  mess$ = " Position of leftmost cell of piece to be removed:  "
  CALL zeingabe(igt, 0, 1, irem1, dummy, dummy$, mess$)
  mess$ = " Position rightmost cell of piece to be removed:  "
  CALL zeingabe(igt, 0, 1, irem2, dummy, dummy$, mess$)
      iremnumber = irem2 - irem1 + 1 'number of cells to be removed
      FOR il = 1 TO kn
      lx(il) = js - iremnumber
      FOR ix = (irem2 + 1) TO js
	 FOR iy = 1 TO ly(il)
	     axy(il, ix - iremnumber, iy) = axy(il, ix, iy)
      NEXT iy: NEXT ix: NEXT il
      js = js - iremnumber

CASE "0", "0C" 'to set a and d (activator and differentiation to zero)
mess$ = " X-region to be wiped out; Present field size is: " + STR$(kax) + " - " + STR$(js)
kanew = 1: ksnew = js: kaynew = 1: kynew = jy
CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
CALL zeingabe(igt, 1, 1, kanew, dummy, dummy$, "X-left-most cell")
CALL zeingabe(igt, 1, 1, ksnew, dummy, dummy$, "X-right-most cell")
  IF ksnew = 0 OR ksnew > js THEN ksnew = js
  IF kanew < 1 THEN kanew = 1

mess$ = "Y-region to be wiped out; Present field size is: " + STR$(kay) + " - " + STR$(jy)
CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
CALL zeingabe(igt, 1, 1, kaynew, dummy, dummy$, "Y-left-most cell")
CALL zeingabe(igt, 1, 1, kynew, dummy, dummy$, "Y-right-most cell")
  IF kynew < 1 OR kynew > jy THEN kynew = jy
  IF kaynew < 1 THEN kaynew = kay

FOR ix = kanew TO ksnew
FOR iy = kaynew TO kynew
 axy(1, ix, iy) = 0
IF mwasnun$ = "0" THEN
  axy(4, ix, iy) = 0:
ELSE
  axy(3, ix, iy) = 0
END IF
NEXT iy: NEXT ix

CASE "D"       'double in X
jinsert = iinsert
FOR jinsert = js TO 1 STEP -1
CALL growthxy(iinsert, js, imx, kn, lax(), lx(), lay(), ly(), axy())
NEXT

CASE "K" 'for removal of the central meristem, k1 = off-centre
'bell kurve in D: whatever larger - > set to zero
  jxx = (js + 1) / 2: jyy = (jy + 1) / 2
  FOR ix = 1 TO js
      FOR iy = 0 TO jy
       axy(8, ix, iy) = iex(ix, iy)
      centerkill = 10 * EXP(-cc * (jxx - k1 - ix) ^ 2) * EXP(-cc * (jyy - iy) ^ 2)
      IF centerkill > 1 THEN
      FOR il = 1 TO 3:  axy(il, ix, iy) = 0: NEXT
      iex(ix, iy) = 0
      END IF
       axy(4, ix, iy) = centerkill:
      NEXT iy: NEXT ix


CASE "GX"
IF iinsert = 0 THEN
 mess$ = " A row of cell will be inserted; position?, 0=centre"
CALL zeingabe(igt, 1, 1, iinsert, dummy, dummy$, mess$)
IF iinsert = 0 THEN iinsert = (lax(1) + lx(1)) / 2
END IF
CALL growthxy(iinsert, js, imx, kn, lax(), lx(), lay(), ly(), axy())


CASE "GY"
IF iinsert = 0 THEN
mess$ = " A column of cells will be inserted; position?, 0=centre"
CALL zeingabe(igt, 1, 1, iinsert, dummy, dummy$, mess$)
IF iinsert = 0 THEN iinsert = (lay(1) + ly(1)) / 2
END IF
IF jy < imy - 1 THEN
FOR il = 1 TO kn
FOR iy = jy TO iinsert STEP -1          'still the old js is being used
FOR ix = 1 TO lx(il)
       axy(il, ix, iy + 1) = axy(il, ix, iy)
   NEXT ix:  NEXT iy
   ly(il) = ly(il) + 1
   NEXT il

jy = jy + 1                             'finally js is adjusted to the new state
ELSE
mess$ = "array becomes too large,  js>imx [" + STR$(js) + ", " + STR$(imx) + " ]"
CALL cmessage(3, -1, 1, 15, 4, ibackcol, mess$, "OK")
END IF

CASE "GR", "T"'graft pieces together
      IF kxmem = 0 THEN
      CALL cmessage(3, -1, 1, 15, 4, ibackcol, "must be first stored", "OK")
      mwasnun$ = "S"
      GOTO startmanixy
      END IF
      FOR ix = 1 TO imx
	 mempos(ix) = 0 'first mop up anything left from previous manipulations
      NEXT ix
      jxright = 0
      IF igt = 0 THEN
      FOR i = 18 + ivgashift TO 24 + ivgashift:
      LOCATE i, 1: PRINT SPACE$(80); : NEXT
      LOCATE 18 + ivgashift, 1
      PRINT " You can combine cells from the stored state to create a new field."
      PRINT " Each cell can be used in the new field several times."
      PRINT " The stored field has"; kxmem; "cells."
      PRINT " If you want to take just one cell, return two identical positions. ";
      PRINT "Stored field size is"; kaxmem; " - "; kxmem; ""
      END IF
graftarray:
  mess$ = " starting cell of the piece      (or RETURN = end of grafting process)"
  CALL zeingabe(igt, 0, 1, ifrom, dummy, dummy$, mess$)
  IF ifrom = 0 THEN GOTO graftassembly
  mess$ = "  end cell of the piece"
  CALL zeingabe(igt, 0, 1, ito, dummy, dummy$, mess$)
      itofromdiff = ABS(ito - ifrom)
	 IF (ileftx + 1 + itofromdiff) > imx THEN
CALL cmessage(3, -1, 1, 15, 4, ibackcol, " Piece too long, has been cuttet", "OK")
	    itofromdiff = imx - ileftx - 1: ito = ifrom + ifromdiff
	 END IF
	 istep = 1: IF ifrom > ito THEN istep = -1
	 FOR ix = ifrom TO ito STEP istep
	 jxright = jxright + 1
	    mempos(jxright) = ix
	 NEXT ix
      IF igt = 0 THEN
      FOR i = 18 + ivgashift TO 24 + ivgashift:
      LOCATE i, 1: PRINT SPACE$(80); : NEXT
      LOCATE 18 + ivgashift, 1
      PRINT "Stored field size is"; kaxmem; " - "; kxmem; ""
	 PRINT "present field size is 1 - "; jxright;
	 PRINT " The field contains for far the following cells:"; jxright; "in total"
	 FOR jxx = 1 TO jxright
	    PRINT mempos(jxx);
	 NEXT jxx
	 END IF
	 js = jxright
      GOTO graftarray
CASE "SG"
      GOTO graftassembly


CASE ELSE
CLS
iinsert = 0
LOCATE 1, 1
PRINT " What shall be done?"
PRINT
PRINT "  To ...                                            type"
PRINT
PRINT "     store the current state                         s"
PRINT "     restore the stored state                        r"
PRINT "     change field size                               c"
PRINT "     set local alterations of array s, a, b...       a"
PRINT "     double all cells in X-dir.                      d"
PRINT "     growth at the right margin                      gm"
PRINT "     wipe a and d in a part of the field             0"
PRINT "     insert a column in the field (ix)               gx"
PRINT "     insert a column in the field (ix)               gx"
PRINT "     kill centre for meristem exp., cc = radius      k"
PRINT "     remove a piece from the field                   de"
PRINT "     transplant or rearrange pieces in X-dir.        t"
PRINT "     restore a graft combination                     sg"
PRINT
PRINT "     go back to the parameter menu                   RETURN"
PRINT
INPUT resp$: resp$ = UCASE$(resp$)
IF resp$ = "" OR resp$ = CHR$(27) THEN EXIT SUB
mwasnun$ = resp$
GOTO startmanixy
END SELECT
EXIT SUB

'''''''''''''########## SUB's ##########



graftassembly:
       'Now all pieces are being assembled
      js = jxright
      lx(1) = js
      jy = kymem
      FOR ix = 1 TO js
      FOR iy = 1 TO jy
      FOR il = 1 TO kn
	     axy(il, ix, iy) = axy(il, mempos(ix), iy + kyadd)
	 NEXT il
      NEXT iy
      NEXT ix
      FOR il = 1 TO kn: lx(il) = js: NEXT
      EXIT SUB
       'Now you can see what you have done

END SUB

SUB showdisplay (kdispl, displaywastmp$) STATIC
SHARED axy(), iwas, kdisplay
SHARED lax(), lx(), lay(), ly(), gxa(), gfa(), gya(), ico()
SHARED icolm(), ibm(), a2(), ansp(), js, jy, icc
SHARED itypus(), iwi(), label$(), title$, msiluet()
SHARED ihp, dxp, dyp, dx, dy, per, krot, ivgashift, igraph, ianz, ireaddata
SHARED istelz, iletztk, ihintr, iparam, iactive, iwidthm, ianima
SHARED ivorcol, ibackcol, dataname$, readdatafile$, writedatafile$
SHARED itot, kn, igt, ilm, iendm
SHARED idxr, idxm, idyu, idym, ipcx, ipcs

ibm(1) = 1: ibm(2) = 2: ibm(3) = 12: ibm(4) = 4: ibm(5) = 5: ibm(6) = 12: ibm(7) = 13
ibm(8) = 8: ibm(9) = 9: ibm(10) = 10
ibm(11) = 11: ibm(12) = 12: ibm(13) = 13: ibm(14) = 14: ibm(15) = 15
DIM ity(14)



displaywas$ = LCASE$(displaywastmp$)
twas$ = "sabcdefghij"
    FOR i = 0 TO kn: ity(i) = 0: NEXT


xmax = 640:  ymax = 480
SELECT CASE kdispl ' for actual pixels if larger screen resolution is used
  CASE 8, 9, 11, 13, 33
    SELECT CASE ivgashift 'units=screen-pixel
    CASE 12: xmax = 800: ymax = 600
    CASE 23: xmax = 1024: ymax = 786
    CASE 37: xmax = 1280: ymax = 1024
   END SELECT
 END SELECT
    WINDOW (0, 0)-(xmax, ymax)


' what is to be displayed
FOR ilv = LEN(displaywas$) TO 1 STEP -1
    il = ASC(MID$(displaywas$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
	IF il > ilm THEN
	mess$ = "Too many substances, il or kn  <=" + STR$(ilm)
	CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, "OK")
	EXIT SUB
	END IF
    tvv$ = MID$(twas$, il + 1, 1)
    kdisplay = kdispl: IF kdisplay = 0 THEN kdisplay = itypus(il)
    ity(il) = kdisplay
  NEXT ilv

SELECT CASE kdispl

CASE 0 TO 12, 24, 25
 IF kdispl = 12 OR kdispl > 23 THEN CLS : LOCATE 1, 1
FOR ilv = LEN(displaywas$) TO 1 STEP -1
    il = ASC(MID$(displaywas$, ilv, 1)) - 96'a=asc(97)
    IF il = 19 THEN il = 0
    tvv$ = MID$(twas$, il + 1, 1)
   CALL disp(ity(il), il, axy(), ico(il), iwi(il), gfa(il), gxa(il), gya(il), lax(il), lx(il), lay(il), ly(il), tvv$)
  NEXT ilv


CASE 13, 131, 33, 331 'colored random dots, density proportional to level
  '111, 331 = gray , not in qbx
  js = lx(1): ja = lax(1): kay = lay(1): ky = ly(1):
  xo = gxa(1): yo = gya(1) '- dyp * (icc - 1)
  IF xo = 0 THEN
    IF icc = 1 OR kg > 0 THEN
    xa = 320 - ((js - ja + 1) / 2) * dx
    jam = ja
  END IF
  xo = xa + dx * (ja - jam) 'to have correct position after change of field
  END IF
  ''IF iwas < 2 AND icc = 1 THEN CALL clsscr(ipcs, ipcx,ibackcol)
  inkd = dx
  dxx = (js - ja + 1) * dx
  dyy = (ky - kay + 1) * dx
  IF kdispl = 33 OR kdispl = 13 THEN 'bei 331 keinen Rand
  LINE (xo - 1, yo - 1)-(xo + dxx + 2, yo + dyy + 2), ibackcol, BF
  CALL linep(ihp, xo - 1, yo - 1, xo + dxx + 2, yo + dyy + 2, 2, 1, ibackcol, 1)
  CALL linep(ihp, xo, yo, xo + dxx + 1, yo + dyy + 1, 1, 1, ivorcol, 1)
  CALL linep(ihp, xo - 1, yo - 1, xo + dxx + 2, yo + dyy + 2, 1, 1, ivorcol, 1)
  ELSE
  CALL linep(ihp, xo - 1, yo - 1, xo + dxx + 2, yo + dyy + 2, 1, 1, 15, 1)
  CALL lightgray(7, 62, 62, 62)
  END IF
    yb = yo + (iy - kay) * dx
  inc = 0
  FOR ix = ja TO js
    FOR iy = kay TO ky
      xb = xo + (ix - ja) * dx + 1
      yb = yo + (iy - kay) * dx + 1
      'light gray  background
      IF kdisplay = 131 AND axy(2, ix, iy) > 0 THEN LINE (xb, yb)-(xb + dx, yb + dx), 7, BF
      su = 0
      inuse = 0
      FOR il = 0 TO kn
	IF ity(il) = 11 OR ity(il) = 13 OR ity(il) = 33 OR ity(il) = 131 THEN
	  inuse = inuse + 1
	  icolm(inuse) = ico(il)
	  a2(inuse) = axy(il, ix, iy) * gfa(il) * .3
	  su = su + a2(inuse)
	 END IF
      NEXT il

      IF su > .01 THEN
	anc = 0
	FOR ibc = 1 TO inuse'normieren
	  IF su < 1! THEN
	    anc = anc + a2(ibc)
	  ELSE
	    anc = anc + a2(ibc) / su
	  END IF
	  ansp(ibc) = anc'anteil summativ
	NEXT ibc
	FOR ikx = 1 TO inkd
	  ybb = yb
	  FOR iky = 1 TO inkd
	    rra = RND
	    IF rra < ansp(inuse) THEN
	      i = 1
	      DO UNTIL rra < ansp(i)
		i = i + 1
	      LOOP
	iwiloc = iwi(i): icoloc = icolm(i)
 IF iwiloc > 1 THEN
'          makes a little line to appear more black
    CALL linep(ihp, xb, ybb, xb + 1, ybb + 1, 0, iwiloc, icoloc, 1)'    , 11         '------- ganz
       ELSE
    CALL linep(ihp, xb, ybb, xb, ybb, 0, iwiloc, icoloc, 1)'    , 11         '------- ganz
	    END IF
	    END IF
	    ybb = ybb + 1!
	  NEXT iky
	  xb = xb + 1!
	NEXT ikx
      END IF
    NEXT iy: NEXT ix

CASE 14, 34 'leaves
  IF gxa(1) = 0 THEN
  dyp = 400: d2 = 400! / jy
  y2 = 50!: x2 = 300! - js / 2 * d2
  ELSE
  y2 = gya(1): x2 = gxa(1): d2 = dx
  END IF
  d21 = d2 - 1!: d22 = d2 / 2!

  FOR ix = 1 TO js
    FOR iy = 1 TO jy
      dxt = x2 + d2 * (ix - 1)
      dyt = y2 + d2 * (iy - 1)
      IF gfa(1) * axy(1, ix, iy) > 1 THEN
	CALL linep(ihp, dxt, dyt, dxt + d2, dyt + d2, 2, iwi(1), ico(1), 1)
       END IF
      IF gfa(3) * axy(3, ix, iy) > 1 THEN
	 CALL linep(ihp, dxt, dyt, dxt + d22, dyt + d2, 0, iwi(3), ico(3), 1)
	 CALL linep(ihp, dxt + d22, dyt + d2, dxt + d2, dyt, 0, iwi(3), ico(3), 1)
	END IF
      IF gfa(4) * axy(4, ix, iy) > 1 THEN
	 CALL linep(ihp, dxt, dyt, dxt + d2, dyt + d2, 1, 1, ico(4), 1)
	 IF iwi(4) > 1 THEN CALL linep(ihp, dxt + 1, dyt + 1, dxt + d2 - 1, dyt + d2 - 1, 1, 1, ico(4), 1)
	 IF iwi(4) > 1 THEN CALL linep(ihp, dxt + .5, dyt + .5, dxt + d2 - .5, dyt + d2 - .5, 1, iwi(4), ico(4), 1)
	END IF
     NEXT: NEXT
  CALL linep(ihp, x2 - 2, y2 - 2, x2 + d2 * js + 2, y2 + d2 * jy + 2, 1, iwi(1), ivorcol, 1)
  'LINE (x2 - 3, y2 - 3)-(x2 + d2 * js + 3, y2 + d2 * jy + 3), ivorcol, B

IF kn > 4 THEN
  y2 = gya(5): x2 = gxa(5): d2 = dx

  FOR ix = 1 TO js
    FOR iy = 1 TO jy
      dxt = x2 + d2 * (ix - 1)
      dyt = y2 + d2 * (iy - 1)
      IF gfa(5) * axy(5, ix, iy) > 1 THEN
	CALL linep(ihp, dxt, dyt, dxt + d2, dyt + d2, 2, iwi(5), ico(5), 1)
       END IF
      IF gfa(7) * axy(7, ix, iy) > 1 THEN
	CALL linep(ihp, dxt, dyt + d2, dxt + d22, dyt, 0, iwi(3), ico(7), 1)
	CALL linep(ihp, dxt + d22, dyt, dxt + d2, dyt + d2, 0, iwi(3), ico(7), 1)
	END IF
      IF gfa(8) * axy(8, ix, iy) > 1 THEN
	 CALL linep(ihp, dxt, dyt, dxt + d2, dyt + d2, 1, iwi(4), ico(8), 1)
	 LINE (dxt + 1, dyt + 1)-(dxt + d2 - 1, dyt + d2 - 1), ico(8), B
	END IF
    NEXT: NEXT
  CALL linep(ihp, x2 - 2, y2 - 2, x2 + d2 * js + 2, y2 + d2 * jy + 2, 1, iwi(1), ivorcol, 1)
  CALL linep(ihp, x2 - 3, y2 - 3, x2 + d2 * js + 3, y2 + d2 * jy + 3, 1, 1, ivorcol, 1)
END IF
CASE ELSE
  mess$ = "for this kd no graphic display is yet installed; routine showprof"
  CALL switchscreen(3, iactive, igraph, kd, programname$, fdelay)
  CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
  iendm = 999
END SELECT


END SUB

