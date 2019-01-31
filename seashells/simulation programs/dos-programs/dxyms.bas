'Program for the simulation of pattern formation in two-dimensional fields
' (c) by Hans Meinhardt, 1995-2002

'-----------------------------------------------------------------------
' A word of precaution...                                                                 ³
'  The software was written with care but the customer knows that
'  software cannot be produced without errors. Springer-Verlag and the
'  author disclaim any liability for software errors or their
'  consequences.
'
'  All user rights to the software are held exclusively by the author.
'  Springer-Verlag does not author data and programs but makes them
'  available. Any commercial use or distribution independently of the
'  book is not permitted
'-----------------------------------------------------------------------

' This version is appropriate for the compilation with POWER-BASIC;
' use XYMS.BAS for MS - BASIC

 DEFINT H-N
DECLARE SUB backfront (inz%, x0!, x1!, y4!, y1!, s0r!, s1r!, s0l!, s1l!, ico%, iwil%, px!, p4!, pyy)
DECLARE SUB clsscr (ipcs, ipcx, ibackcol)
DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DECLARE SUB disp (kdisplay%, il%, ax!(), ico%, iwi%, ff!, xo!, yo!, kax%, kx%, kay%, ky%, tvv$)
DECLARE SUB growthxy (iinsert%, js%, imx%, kn%, lax%(), lx%(), lay%(), ly%(), ax!())
DECLARE SUB eingabe (igt%, inz%, iaf%(), f!(), label$(), text$, ianf%, ianz%)
DECLARE FUNCTION formata$ (inz%, aa!)
DECLARE SUB gmanage (wasnun$)
DECLARE SUB gtourcontrol (pcontrol$, igt%, label$())
DECLARE SUB gtreturn (inz%, igt%)
DECLARE SUB init (inewscreen%, igraph%, ivorcol%, ibackcol%, ivgashift%)
DECLARE SUB leftright (x1!, x2!, x3!, y1!, y2!, y3!, s1r!, s2r!, s3r!, s1l!, s2l!, s3l!, ico%, iwil%, px!, p4!)
DECLARE SUB linep (ihp%, x1!, y1!, x2!, y2!, ipat%, iwidth%, ico%, ityp%)
DECLARE SUB manipulateXY (mwasnun$, iinsert%)
DECLARE SUB memdifu (il, ix, iy, afx, DAX, rax, diffx, axl(), ieml())
DECLARE SUB paramenu ()
DECLARE SUB PCXOUT (ipcx%, ipcs%)
DECLARE SUB perpendicular (x0!, y0!, ybase0!, s0l!, s0r!, ico%, iwil%)
DECLARE SUB prtscreen (scrfile$)
DECLARE SUB rl (inz%, x2!, y2!, s2!, x1!, yto!, s1!, y3!, isi%, iy%, jyto%, ico%, iwil%, px!)
DECLARE SUB rwparameterdisk (rw$, ifilenumber%)
DECLARE SUB setiexiem (kx, ky, kn, k2, iex(), iem(), axy())
DECLARE SUB showdisplay (kdispl%, displaywastmp$)
DECLARE SUB smenu (s$(), irfilename$, iline%, headline$)
DECLARE SUB sorts (s$(), icount%)
DECLARE SUB titles (inz%, title$, mess$)
DECLARE SUB whatprmfiles (s$(), programnameloc$, progrtmp$, exten$, drive$, irfilename$, ilineparam)
DECLARE SUB zeingabe (igt%, iquest%, inz%, i%, f!, labels$, text$)
DECLARE SUB screencheck (igraph%, lastline%, lastrow%, ivgashift%, ifree%)
DECLARE SUB switchscreen (inz%, iactive%, igraph%, kd%, programname$, fdelay)
DECLARE SUB ColorPrint (text$, row%, col%, fc%, bc%)
DECLARE SUB packpcx (resp$, idummy%, jdummy%, idxr%, idyu%)
'$INCLUDE: 'scommon.bi'

ifree = 3 'qbx
''$INCLUDE "dlibxy-p.inc"'------for Power-Basic only
programname$ = "XY"
basfilename$ = "DXYMS.BAS"
ptype$ = ""
dirtemp$ = "..\tmp\"
curdrive$ = ""
programloc$ = ""
pdir$ = "..\PARAM-XY\"
aprodir$ = curdrive$ + programloc$
apardir$ = curdrive$ + programloc$ + pdir$

imxl = 36: ilm = 8: imx = 21: imy = 21
pbilm = 9: pbimx = 55: pbimy = 55

imxl = imx: imyl = imy
DIM axy(ilm, imx, imy) 'for very large arrays, this statement must be de-activated...
DIM iparnam$(20), fparnam$(10, 10)
DIM arandom(imx, imy), iex(imx, imy), iem(imx, imy)
DIM parnam$(75), flv(10, 10), inv(21)' to store parameters
DIM label$(10), agene(6)
DIM asu(ilm), u(ilm, imx), aleft(ilm), digfa(ilm), afc(ilm)
DIM lax(10), lx(10), lay(10), ly(10), gxa(10), gfa(10)
DIM gya(10), ico(10), itypus(10), iwi(10)
DIM s$(150), sl$(60)
DIM aam(1) ' kann weg
'DIM agrad(1, 1)
DIM mempos(imx), icolm(10), ibm(16), a2(10), ansp(10)
msiluetmax = 500
DIM msiluet(msiluetmax)
ipcs = -2: ipcx = -2
ihp = 1
ibackcol = 15
diffmax = .2'Maximalwert fr die Diffusion
CLS : RANDOMIZE TIMER

'--- set parameter and check screen
parameter:
CALL paramenu


Mainstart:
mess$ = "": time1 = 0
SELECT CASE pcontrol$
CASE "D", "A", "DP"
iinit = 1:
    IF pcontrol$ = "A" THEN
'    CALL gmanage("VU")
'    CALL gmanage("VR")
    iinit = 2
 ELSE
CALL clsscr(ipcs, ipcx, ibackcol)
 END IF
IF ipcontrol = 0 THEN
CALL clsscr(ipcs, ipcx, ibackcol)
CALL showdisplay(kd, displaywas$)
ELSE
CALL showdisplay(ipcontrol, displaywas$)
END IF
    IF pcontrol$ = "DP" THEN
      CALL PCXOUT(ipcx, ipcs)
      ELSE
      IF ipcs > -2 THEN CALL PCXOUT(-2, 0)
    END IF
ihp = 1: iparam = 2: ipcontrol = 0
jy = ly(1): js = lx(1): kax = lax(1): kay = lay(1)

GOTO parameter

CASE "M"
       iinsert = 0
	CALL manipulateXY(pcontrol2$, iinsert)
	GOTO makedraw
CASE "S", "FS", "FN", "N", "C", "FC", "I", "II", "FI", "D"
      icount = 0
      igrowth = 1
      IF pcontrol$ = "N" OR pcontrol$ = "FN" GOTO screeninit
      IF pcontrol$ = "C" OR pcontrol$ = "FC" GOTO continueCalc
      itot = 0      ' will start right after end select
   CASE ELSE
     CALL cmessage(6, -1, 1, 15, 4, ibackcol, pcontrol$ + "- No such command ", "OK"): GOTO parameter
  END SELECT

'--- This is the start where you enter with pcontrol$="S"

      js = kx: jy = ky: kax = 1: kay = 1
FOR il = 0 TO ilm: lax(il) = 1: lx(il) = js: lay(il) = 1: ly(il) = jy: NEXT

sra = ra: IF ca > 0 THEN sra = ca
fran = kr: fran = fran / 100: fran5 = .5 * fran
IF pcontrol$ <> "II" THEN
	FOR iy = 1 TO imy
	FOR ix = 1 TO imx      'source density =random fluctuation
	axy(0, ix, iy) = sra * (1 + RND * fran - fran5)
	arandom(ix, iy) = 1 + RND * fran - fran5
	NEXT ix: NEXT iy
END IF



FOR iy = 1 TO jy
FOR ix = 1 TO js     'set all cell to initial condtions
	axy(1, ix, iy) = ga
	axy(2, ix, iy) = gb
	IF ilm >= 3 THEN axy(3, ix, iy) = gc
	IF ilm >= 4 THEN axy(4, ix, iy) = gd
	IF ilm >= 5 THEN axy(5, ix, iy) = ge
	IF ilm >= 6 THEN axy(6, ix, iy) = gf
	IF ilm >= 7 THEN axy(7, ix, iy) = gg
	IF ilm >= 8 THEN axy(8, ix, iy) = gh
	IF ilm >= 9 THEN axy(9, ix, iy) = gi
	IF ilm >= 10 THEN
	axy(10, ix, iy) = gj
	END IF
	NEXT ix: NEXT iy
jxx = (js + 1) / 2
jyy = (jy + 1) / 2





SELECT CASE ki
   CASE 1
      axy(1, jxx, jyy) = aa
      axy(3, jxx, jyy) = ac
   CASE 2
      axy(0, jxx, jyy) = aa * axy(0, jxx, jyy)
   CASE 3
      axy(1, jxx - 2, jyy - 2) = aa
      axy(4, jxx - 2, jyy - 2) = ad
   CASE 12  'for Hydra hypostome and tentakles
      FOR ix = 1 TO js
      FOR iy = 1 TO jy
      axy(3, ix, iy) = gc * EXP(-ac * (jxx - ix) ^ 2) * EXP(-ac * (jyy - iy) ^ 2)
      NEXT iy: NEXT ix
   CASE 13 ' for Notochord
     FOR iy = k2 TO ky: axy(1, jxx, iy) = aa: NEXT
     axy(3, jxx, ky) = ac
     axy(3, jxx, 1) = ad

   CASE 14:  'for insertion of new maxima during growth
     axy(1, 3, 3) = aa
     axy(1, js - 2, 3) = aa
     axy(1, js - 2, ky - 2) = aa
     axy(1, 3, ky - 2) = aa

    CASE 21  'Initiation for line formation
      axy(4, 3, 3) = ad
    IF ab > 0 THEN ' for local production for target finding
      FOR ix = 1 TO kx
      FOR iy = 1 TO ky
      ixtarget = kx / 3: iytarget = ky - 3
      distance = SQR((ix - ixtarget) ^ 2 + (iy - iytarget) ^ 2)
      axy(5, ix, iy) = bc * EXP(-ab * distance)
      NEXT iy: NEXT ix
    END IF

      FOR ix = 1 TO kx  ' for Folkman-simu
      FOR iy = 1 TO ky
      IF ab = 0 THEN
      axy(5, ix, iy) = bc:
      IF iy > ky / 2 THEN axy(5, ix, iy) = bc + sc
      END IF
      axy(6, ix, iy) = 0
      NEXT: NEXT
    IF sb > 0 THEN ' for Folkman - simu
      FOR iy = 15 TO 20: FOR ix = 15 TO 20
      axy(6, ix, iy) = sb 'additional inhibitor
      NEXT ix: NEXT iy
    END IF

   CASE 36 'initial condition for Hydra zylinder and meristem/leaves
      FOR iy = 1 TO imy: axy(1, kx, iy) = aa
      FOR ix = 1 TO imx
	 IF ilm >= 3 AND ac <> 0 THEN axy(3, ix, iy) = gc * EXP(-ac * (ix - 1))
       NEXT ix: NEXT iy
   CASE 36
      FOR iy = 1 TO imy: axy(1, kx, iy) = aa
      FOR ix = 1 TO imx
       IF ilm >= 3 AND ac <> 0 THEN axy(3, ix, iy) = gc * EXP(-ac * (ix - 1))
     NEXT ix: NEXT iy

CASE ELSE
CALL cmessage(6, -1, 1, 15, 4, ibackcol, ptype$ + "No initial condition defined for this KI ", "OK")

END SELECT

CALL setiexiem(kx, ky, kn, k2, iex(), iem(), axy())
'    setiexiem (kx, ky, kn, K2, iexl(), ieml(), axl())
IF ke > 900 THEN 'using rounded fields, unusued cells set to zero
FOR il = 1 TO kn ' set all to zero if outside
FOR ix = 1 TO kx
FOR iy = 0 TO ky
axy(il, ix, iy) = axy(il, ix, iy) * iex(ix, iy)
NEXT: NEXT: NEXT
END IF


screeninit:
CALL switchscreen(1, iactive, igraph, kd, programname$, fdelay)
refresh:
CALL clsscr(ipcs, ipcx, ibackcol)
CALL showdisplay(kd, displaywas$)
IF iendm = 999 THEN GOTO parameter
CALL switchscreen(2, iactive, igraph, kd, programname$, fdelay)
IF icount >= kt OR pcontrol$ = "D" OR pcontrol$ = "I" OR pcontrol$ = "II" GOTO EndSimulation
 IF ipcx > -1 THEN CALL PCXOUT(ipcx, ipcs)

' --- Here you will enter with pcontrol$= "C"
continueCalc:
dra = 1 - ra - 4 * da: drb = 1 - rb - 4 * db
drc = 1 - rc - 4 * dc: drd = 1 - rd - 4 * dd
dre = 1 - re - 4 * de: drf = 1 - rf - 4 * df
drg = 1 - rg - 4 * dg: drh = 1 - rh - 4 * dh
dri = 1 - ri - 4 * di: drj = 1 - rj - 4 * dj

rad = rd / ra: taf = rf / ra
IF kg > 0 THEN 'growth by insertion of additional cells
 igrowth = igrowth + 1
 IF igrowth > kg THEN
     kinsert = 0: iynsert = 0: igrowth = 1:
     SELECT CASE k1
     CASE 0:
     kinsert = js' FOR MERISTEM: iyinsert = jy
     CASE 1: kinsert = kax ' foot in hydra
     CASE 2: kinsert = (js + 1) / 2
     CASE 3
       kinsert = js - 2   'sub-hypostome
       IF js < 3 THEN kinsert = 1
     CASE 4: kinsert = js: iyinsert = jy
     CASE 5:
	kinsert = (js + 1) / 2
	iyinsert = (lay(1) + ly(1)) / 2
     CASE 6:
	kinsert = (js + 1) / 2 - 1
	IF js > 11 THEN kinsert = (js + 1) / 2 - 2
	iyinsert = (lay(1) + ly(1)) / 2 - 1
	IF jy > 11 THEN iyinsert = (jy + 1) / 2 - 2
     CASE 7: kinsert = (js + 1) / 2
     CASE 8: iyinsert = 1

CASE ELSE:      CALL cmessage(6, -1, 1, 15, 4, ibackcol, ptype$ + "- mode of growth ? K1=1-6 ", "OK")
     END SELECT
     IF kinsert > 0 THEN CALL manipulateXY("GX", kinsert)
     IF iyinsert > 0 THEN CALL manipulateXY("GY", iyinsert)
  END IF
END IF
  FOR iprint = 1 TO kp '-- SECOND LOOP: kp iterations until next plot ------
  ' Boundary conditions: impermeable; this is achieved by
  ' giving the cells kax - 1 and js+1 the same values as cells kax resp.js
  ' virtual left border cells:
   nx = js + 1
   ny = jy + 1
ixleft = kax
ixright = js
IF k3 = 5 THEN
ixleft = js
ixright = kax
END IF

   'border conditions
SELECT CASE k3
CASE 0, 1, 5'normal impermeable
   FOR il = 1 TO kn
	   FOR ix = kax TO js
	     axy(il, ix, ny) = axy(il, ix, jy)
	     u(il, ix) = axy(il, ix, 1): NEXT ix: NEXT il
CASE 2, 3, 4'Cylinder
   FOR il = 1 TO kn
	   FOR ix = kax TO js
	     axy(il, ix, ny) = axy(il, ix, 1)
	     u(il, ix) = axy(il, ix, jy): NEXT ix: NEXT il
END SELECT
   FOR il = 1 TO kn
	   FOR iy = 1 TO jy
	   axy(il, nx, iy) = axy(il, ixright, iy)
	   NEXT iy: NEXT il
  '------ THIRD LOOP: cells kax...js get new values --------------


SELECT CASE ke'EQUATIONS 'selection of the equation according to ke

CASE 21 ' Activator-inhibitor system, sa=saturation
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = 1 TO js: GOSUB olddecay
	aq = s * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / B
	axy(2, ix, iy) = olddecaydiffB + aq
NEXT: NEXT

CASE 23 ' Activator-inhibitor system, sa=saturation
'bb is Michaelis-Menten constant
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = 1 TO js: GOSUB olddecay
	axy(1, ix, iy) = olddecaydiffA + s * (a * a / ((sb + B) * (1 + sa * a * a)) + ba)
	axy(2, ix, iy) = olddecaydiffB + s * a * a + bb
	NEXT: NEXT

CASE 24 ' activator-depleted substance
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = 1 TO js: GOSUB olddecay
	aq = s * B * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / (1 + sa * a * a)
	axy(2, ix, iy) = olddecaydiffB + bb - aq
	IF axy(2, ix, iy) < 0 THEN axy(2, ix, iy) = 0
	NEXT: NEXT

CASE 28 ' Inhibition if an inhibition
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = 1 TO js: GOSUB olddecay
	axy(1, ix, iy) = olddecaydiffA + s / (sa + c * c) + ba
	axy(2, ix, iy) = olddecaydiffB + rb * a
	axy(3, ix, iy) = olddecaydiffC + rc / (sc + a * a / (sb + B * B))
	NEXT: NEXT

CASE 122 'LI Net-like structure    (a,b) Activator-inhibitor
FOR iy = kay TO jy: GOSUB StoreLeftCell   '(c) substrate (d) differentiation
FOR ix = 1 TO js: GOSUB olddecay
	aq = s * c * a * a
	dq = d * d
	axy(1, ix, iy) = olddecaydiffA + aq / B + ba * d
	axy(2, ix, iy) = olddecaydiffB + aq + bb * d
	axy(3, ix, iy) = olddecaydiffC + bc - cc * c * d
	axy(4, ix, iy) = olddecaydiffD + rd * dq / (1 + sd * dq) + bd * a
	NEXT: NEXT

CASE 123 'LI Net-like structure - special cases,
'in e prodction rate of substrate, in f additional inhibitor
FOR iy = kay TO jy: GOSUB leftB
FOR ix = 1 TO js: GOSUB afcstore
	aq = s * c * aqf
	dq = d * d
	ax(1, ix, iy) = olddecaydiffA + aq / B + ba * d + .00001
	ax(2, ix, iy) = olddecaydiffB + aq + bb * d + .0001 + axy(6, ix, iy)
	ax(3, ix, iy) = olddecaydiffC + axy(5, ix, iy) - cc * c * d
	ax(4, ix, iy) = olddecaydiffD + dq / (1! + 10! * dq) + bd * a
	NEXT: NEXT


CASE 125 'HY- , used for meristem - leaves
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = kax TO js: GOSUB olddecay
	aq = c * s * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / B
	axy(2, ix, iy) = olddecaydiffB + aq
	'Source
	axy(3, ix, iy) = olddecaydiffC + rc * a + bc - sc * c * f
	'tentakles
	dfq = d * d + bd
	dq = rd * c * arandom(ix, iy) * dfq / (1 + sd * dfq) / (1 + ce * a)
	axy(4, ix, iy) = olddecaydiffD + dq / e
	axy(5, ix, iy) = olddecaydiffE + dq + be
	'Foot
	fq = rf * (f * f + bf) / c
	axy(6, ix, iy) = olddecaydiffF + fq / g
	axy(7, ix, iy) = olddecaydiffG + fq + bg
	NEXT: NEXT
CASE 126 ' derived from Hydrasimulation, for meristem (ab) and leaves (de)
' c = source density makes compentece for leaf formation
	  ' Foot (f,g)
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = 1 TO js: GOSUB olddecay
	aq = c * s * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / B
	axy(2, ix, iy) = olddecaydiffB + aq
	'Quelldichte
	axy(3, ix, iy) = olddecaydiffC + rc * a + bc - sc * c * f
	'Tentakeln
	dfq = d * d + bd
	dq = rd * c * arandom(ix, iy) * dfq / (1 + sd * dfq) / (1 + ce * a)
	axy(4, ix, iy) = olddecaydiffD + dq / e
	axy(5, ix, iy) = olddecaydiffE + dq + be
	NEXT: NEXT

CASE 127 'HY- -------------- Kopf, Tentakel -- towards phyllo--
FOR iy = kay TO jy: GOSUB leftB
FOR ix = kax TO js: GOSUB afcstore
	aq = c * s * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / B
	axy(2, ix, iy) = olddecaydiffB + aq
	'Source
	axy(3, ix, iy) = olddecaydiffC + rc * a + bc
	'tentakles
	dfq = d * d + bd
	dq = rd * arandom(ix, iy) * (d * d + c * bd) / (1 + sd * dfq) / (1 + ce * a)
	axy(4, ix, iy) = olddecaydiffD + dq / e
	axy(5, ix, iy) = olddecaydiffE + dq + be
	NEXT: NEXT



CASE 131 'LI Net-like structure    (a,b) Activator-inhibitor
FOR iy = kay TO jy: GOSUB StoreLeftCell   '(c) substrate (d) differentiation
FOR ix = 1 TO js: GOSUB olddecay
	aq = s * C * a * a
	dq = D * D
	axy(1, ix, iy) = olddecaydiffA + aq / B + ba * D
	axy(2, ix, iy) = olddecaydiffB + aq + bb * D + .00001 + axy(6, ix, iy)
	axy(3, ix, iy) = olddecaydiffC + bc - cC * C * D + axy(5, ix, iy)
	axy(4, ix, iy) = olddecaydiffD + rd * dq / (1 + sd * dq) + bd * a
	NEXT: NEXT




CASE 901 'GM-  ---  activator and inhibitor, for rounded fields
FOR iy = kay TO jy: GOSUB roundleftb
FOR ix = 1 TO js: GOSUB roundafcstore
IF iex(ix, iy) > 0 THEN
	aq = s * (a * a + ba) 'a
	axy(1, ix, iy) = olddecaydiffA + aq / B / (1 + sa * a * a)
	axy(2, ix, iy) = olddecaydiffB + aq
END IF
NEXT: NEXT

CASE 902 'MB AS in Membran
   FOR iy = kay TO jy: GOSUB roundleftb
   FOR ix = 1 TO js: GOSUB roundafcstore
    IF iex(ix, iy) > 0 THEN
      axy(3, ix, iy) = 2
    IF iem(ix, iy) > 0 THEN
      axy(4, ix, iy) = 2
      CALL memdifu(1, ix, iy, a, da, ra, olddecaydiffA, axy(), iem())
	aq = B * s * (aqf + ba)
	axy(1, ix, iy) = olddecaydiffA + aq
	axy(2, ix, iy) = olddecaydiffB - aq
	  ELSE
	   axy(2, ix, iy) = olddecaydiffB + bb' - aq
	  END IF
    END IF
   NEXT: NEXT

'-----------------------------------------


CASE 926 'SU two Systems for WNT/beta-cat activator and inhibitor
'for rounded fields
	 ' high a is basic production for c, peak on hill
   FOR iy = kay TO jy: GOSUB roundleftb
   FOR ix = 1 TO js: GOSUB roundafcstore
    IF iex(ix, iy) > 0 THEN
	aq = s * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / B
	axy(2, ix, iy) = olddecaydiffB + aq
       cq = rc * (c * c + bc * a * a)
	axy(3, ix, iy) = olddecaydiffC + cq / d
	axy(4, ix, iy) = olddecaydiffD + cq + bd
     END IF
   NEXT: NEXT



CASE 132 ' elongation of a stripe by a moving spot
FOR iy = kay TO jy: GOSUB StoreLeftCell
FOR ix = 1 TO js: GOSUB olddecay
	aq = s * (a * a + ba * c)
	axy(1, ix, iy) = olddecaydiffA + aq / B / (1 + sa * a * a)
	axy(2, ix, iy) = olddecaydiffB + s * a * a + bb
	cq = e * arandom(ix, iy) * (c * c + bc * a)
	axy(3, ix, iy) = olddecaydiffC + rc * e * c * c / d
	axy(4, ix, iy) = olddecaydiffD + rd * cq + bd
	axy(5, ix, iy) = olddecaydiffE + be - se * e * a
	NEXT: NEXT

CASE 202 'closed loops by patch (ab) and stripe system (c,d)
  FOR iy = kay TO jy: GOSUB StoreLeftCell
  FOR ix = 1 TO js: GOSUB olddecay
	aq = s * B * (a * a + ba)
	axy(1, ix, iy) = olddecaydiffA + aq / (1 + sa * a * a)
	axy(2, ix, iy) = olddecaydiffB + bb - aq
	cq = rc * B ^ 2 * arandom(ix, iy) * (c * c + bc) / (d * (1 + sc * c * c))
	axy(3, ix, iy) = olddecaydiffC + cq
	axy(4, ix, iy) = olddecaydiffD + rc * c * c
	NEXT: NEXT


CASE ELSE
IF igraph = 9 THEN CALL switchscreen(3, iactive, igraph, kd, programname$, fdelay)
CALL cmessage(6, -1, 1, 15, 4, ibackcol, "No equation of this type, illegal KE ", "OK")
GOTO parameter
END SELECT 'ke
'-------------- End of equations ---------------------------------------------


IF k3 > 2 AND k3 < 5 THEN 'Cylinder with a tip at one (k3=3) or tips at both (k3=4) ends
FOR il = 1 TO kn
SELECT CASE il
CASE 1: diff = da
CASE 2: diff = db
CASE 3: diff = dc
CASE 4: diff = dd
CASE 5: diff = de
CASE 6: diff = df
CASE 7: diff = dg
END SELECT
sum1 = 0: sum2 = 0   'tip at js - averaging
	FOR iy = 1 TO jy
	sum1 = sum1 + axy(il, js, iy)
	sum2 = sum2 + axy(il, js - 1, iy): NEXT iy
	sum1 = sum1 / jy
	sum2 = sum2 / jy
	FOR iy = 1 TO jy: axy(il, js, iy) = sum1 - diff * (sum1 - sum2): NEXT
IF k3 = 4 THEN
	sum1 = 0: sum2 = 0
	FOR iy = 1 TO jy
	sum1 = sum1 + axy(il, kax, iy)
	sum2 = sum2 + axy(il, kax + 1, iy): NEXT iy
	sum1 = sum1 / jy
	sum2 = sum2 / jy
	FOR iy = 1 TO jy: axy(il, kax, iy) = sum1 - diff * (sum1 - sum2): NEXT
  END IF
NEXT il
END IF

   IF INKEY$ = CHR$(27) GOTO makedraw
itot = itot + 1
NEXT iprint
icount = icount + 1
GOTO refresh    ' next iteration

erroverflow:
CALL switchscreen(3, iactive, igraph, kd, programname$, fdelay)
mess$ = "Error, presumable an overflow, num. instability, zero-division"
CALL cmessage(6, -1, 1, 15, 4, ibackcol, mess$, "OK")
GOTO parameter

makedraw:
CALL clsscr(ipcs, ipcx, ibackcol)
   CALL showdisplay(kd, displaywas$)
   CALL switchscreen(2, iactive, igraph, kd, programname$, fdelay)

EndSimulation:
iparam = 2: ipcontrol = 0
CALL switchscreen(3, iactive, igraph, kd, programname$, fdelay)
GOTO parameter

END  '==================  some routines  ==========

leftB:
StoreLeftCell:  ' concentration in the left cell is stored in al...
    al = axy(1, ixleft, iy): bl = axy(2, ixleft, iy)
    cl = axy(3, ixleft, iy): dl = axy(4, ixleft, iy)
    IF kn > 4 THEN el = axy(5, ixleft, iy): fl = axy(6, ixleft, iy):
    gl = axy(7, ixleft, iy):   zhl = axy(8, ixleft, iy)
    RETURN

afcstore:
olddecay:
   a = axy(1, ix, iy): B = axy(2, ix, iy): s = axy(0, ix, iy)
  olddecaydiffA = dra * a + da * (u(1, ix) + axy(1, ix, iy + 1) + al + axy(1, ix + 1, iy))
  olddecaydiffB = drb * B + db * (u(2, ix) + axy(2, ix, iy + 1) + bl + axy(2, ix + 1, iy))
  al = a: bl = B: u(1, ix) = a: u(2, ix) = B
   IF kn > 2 THEN
     c = axy(3, ix, iy): d = axy(4, ix, iy)
     olddecaydiffC = drc * c + dc * (u(3, ix) + axy(3, ix, iy + 1) + cl + axy(3, ix + 1, iy))
     olddecaydiffD = drd * d + dd * (u(4, ix) + axy(4, ix, iy + 1) + dl + axy(4, ix + 1, iy))
     cl = c: dl = d: u(3, ix) = c: u(4, ix) = d
   IF kn > 4 THEN
     e = axy(5, ix, iy): f = axy(6, ix, iy): g = axy(7, ix, iy): zh = axy(8, ix, iy)
     olddecaydiffE = dre * e + de * (u(5, ix) + axy(5, ix, iy + 1) + el + axy(5, ix + 1, iy))
     olddecaydiffF = drf * f + df * (u(6, ix) + axy(6, ix, iy + 1) + fl + axy(6, ix + 1, iy))
     olddecaydiffG = drg * g + dg * (u(7, ix) + axy(7, ix, iy + 1) + gl + axy(7, ix + 1, iy))
     olddecaydiffH = drh * zh + dh * (u(8, ix) + axy(8, ix, iy + 1) + zhl + axy(8, ix + 1, iy))
     el = e: fl = f: gl = g: zhl = h
     u(5, ix) = e: u(6, ix) = f: u(7, ix) = g: u(8, ix) = g
   END IF
   END IF
 RETURN




roundleftb:

FOR il = 1 TO kn: aleft(il) = 0: NEXT
IF iy = 1 THEN
FOR il = 1 TO kn: FOR ixx = 1 TO kx: u(il, ixx) = 0: NEXT: NEXT
END IF
RETURN

roundafcstore:

      jr = iex(ix + 1, iy)
      jl = iex(ix - 1, iy)
      ju = iex(ix, iy - 1)
      jd = iex(ix, iy + 1)

      s = axy(0, ix, iy)
      a = axy(1, ix, iy)
	 aright = (axy(1, ix + 1, iy) - a) * jr
	 down = (axy(1, ix, iy + 1) - a) * jd
	 olddecaydiffA = a * (1 - ra) + da * (u(1, ix) * ju + aright + down + aleft(1) * jl)
	 u(1, ix) = -down: aleft(1) = -aright
      B = axy(2, ix, iy)
	 aright = (axy(2, ix + 1, iy) - B) * jr
	 down = (axy(2, ix, iy + 1) - B) * jd
	 olddecaydiffB = B * (1 - rb) + db * (u(2, ix) * ju + aright + down + aleft(2) * jl)
	 u(2, ix) = -down: aleft(2) = -aright  'rb
	 IF kn > 2 THEN
      c = axy(3, ix, iy)
	 aright = (axy(3, ix + 1, iy) - c) * jr
	 down = (axy(3, ix, iy + 1) - c) * jd
	 olddecaydiffC = c * (1 - rc) + dc * (u(3, ix) * ju + aright + down + aleft(3) * jl)
	 u(3, ix) = -down: aleft(3) = -aright

      d = axy(4, ix, iy)
	 aright = (axy(4, ix + 1, iy) - d) * jr
	 down = (axy(4, ix, iy + 1) - d) * jd
	 olddecaydiffD = d * (1 - rd) + dd * (u(4, ix) * ju + aright + down + aleft(4) * jl)
	 u(4, ix) = -down: aleft(4) = -aright
      IF kn > 4 THEN
      e = axy(5, ix, iy)
	 aright = (axy(5, ix + 1, iy) - e) * jr
	 down = (axy(5, ix, iy + 1) - e) * jd
	 olddecaydiffE = e * (1 - re) + de * (u(5, ix) * ju + aright + down + aleft(5) * jl)
	 u(5, ix) = -down: aleft(5) = -aright

      f = axy(6, ix, iy)
	 aright = (axy(6, ix + 1, iy) - f) * jr
	 down = (axy(6, ix, iy + 1) - f) * jd
	 olddecaydiffF = f * (1 - rf) + df * (u(6, ix) * ju + aright + down + aleft(6) * jl)
	 u(6, ix) = -down: aleft(6) = -aright
      IF kn > 6 THEN
      g = axy(7, ix, iy)
	 aright = (axy(7, ix + 1, iy) - g) * jr
	 down = (axy(7, ix, iy + 1) - g) * jd
	 olddecaydiffG = g * (1 - rg) + dg * (u(7, ix) * ju + aright + down + aleft(7) * jl)
	 u(7, ix) = -down: aleft(7) = -aright

      xxh = axy(8, ix, iy)
	 aright = (axy(8, ix + 1, iy) - xxh) * jr
	 down = (axy(8, ix, iy + 1) - xxh) * jd
	 olddecaydiffH = xxh * (1 - rh) + dh * (u(8, ix) * ju + aright + down + aleft(8) * jl)
	 u(8, ix) = -down: aleft(8) = -aright
      END IF
      END IF
      END IF

RETURN



END  '==================  End of Main-program  ==========
END  '==================  End of Main-program  ==========

SUB memdifu (il, ix, iy, afx, DAX, rax, diffx, axl(), ieml())
'   memdifu(1, ix, iy, a, DA, ra, olddecaydiffA)
diffx = afx * (1 - rax)
IF ieml(ix + 1, iy) = 1 THEN diffx = diffx + DAX * (axl(il, ix + 1, iy) - afx)
IF ieml(ix - 1, iy) = 1 THEN diffx = diffx + DAX * (axl(il, ix - 1, iy) - afx)
IF ieml(ix, iy + 1) = 1 THEN diffx = diffx + DAX * (axl(il, ix, iy + 1) - afx)
IF ieml(ix, iy - 1) = 1 THEN diffx = diffx + DAX * (axl(il, ix, iy - 1) - afx)
'diagonal:
IF ieml(ix + 1, iy + 1) = 1 THEN diffx = diffx + DAX * (axl(il, ix + 1, iy + 1) - afx)
IF ieml(ix + 1, iy - 1) = 1 THEN diffx = diffx + DAX * (axl(il, ix + 1, iy - 1) - afx)
IF ieml(ix - 1, iy + 1) = 1 THEN diffx = diffx + DAX * (axl(il, ix - 1, iy + 1) - afx)
IF ieml(ix - 1, iy - 1) = 1 THEN diffx = diffx + DAX * (axl(il, ix - 1, iy - 1) - afx)
END SUB

SUB setiexiem (kx, ky, kn, k2, iexl(), ieml(), axl())
FOR ix = 1 TO kx: FOR iy = 1 TO ky
   iexl(ix, iy) = 1: NEXT iy: NEXT ix 'active area
FOR iy = 0 TO ky + 1: iexl(0, iy) = 0: iexl(kx + 1, iy) = 0: NEXT' border zero
FOR ix = 0 TO kx + 1: iexl(ix, 0) = 0: iexl(ix, ky + 1) = 0: NEXT

IF k2 > 0 THEN
	ilx = k2 ' cutting the edges as first step towards rounding
	IF k2 = 99 THEN ilx = ky / 3
	ILXX = ilx
	FOR iy = 1 TO ilx
	FOR ix = 1 TO ILXX
	iexl(ix, iy) = 0
	iexl(kx + 1 - ix, iy) = 0
	iexl(ix, ky + 1 - iy) = 0
	iexl(kx + 1 - ix, ky + 1 - iy) = 0
	NEXT ix
	ILXX = ILXX - 1
	NEXT iy

	ilx = ilx + 1
	IF kx > 13 THEN
	iexl(ilx, 1) = 0'   improvement of the rounding
	iexl(kx + 1 - ilx, 1) = 0
	iexl(ilx, ky) = 0
	iexl(kx + 1 - ilx, ky) = 0
	END IF

	IF ky > 13 THEN
	iexl(1, ilx) = 0
	iexl(kx, ilx) = 0
	iexl(1, ky + 1 - ilx) = 0
	iexl(kx, ky + 1 - ilx) = 0
	END IF
  END IF

'for membrane-bound activation: what are the membrane cells
FOR iy = 0 TO ky + 1
FOR ix = 0 TO kx + 1
ieml(ix, iy) = 0
NEXT: NEXT
FOR iy = 1 TO ky
FOR ix = 1 TO kx
IF iexl(ix, iy) > 0 THEN
IF iexl(ix - 1, iy) = 0 OR iexl(ix + 1, iy) = 0 THEN ieml(ix, iy) = 1
IF iexl(ix, iy + 1) = 0 OR iexl(ix, iy - 1) = 0 THEN ieml(ix, iy) = 1
IF iexl(ix - 1, iy + 1) = 0 OR iexl(ix + 1, iy + 1) = 0 THEN ieml(ix, iy) = 1
IF iexl(ix - 1, iy - 1) = 0 OR iexl(ix + 1, iy - 1) = 0 THEN ieml(ix, iy) = 1
END IF
NEXT: NEXT
END SUB

