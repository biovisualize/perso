'Program for the simulation of of sea shell patterns
' onedimensional space-time simulations
' (c) by Hans Meinhardt, 1995-2008

'-----------------------------------------------------------------------
' A word of precaution...
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

' This version is appropriate for the compilation with PowerBasic
' use DXYMS.BAS for MS - BASIC

DEFINT H-N
DECLARE SUB showdisplay (kdispl%, displaywastmp$)
DECLARE SUB oscillation (ila%, C!, D!, olddecaydiffC!, olddecaydiffD!, ja%, js%)
DECLARE SUB screencheck (igraph%, lastline%, lastrow%, ivgashift%, ifree%)
DECLARE SUB clsscr (ipcs%, ipcx%, ibackcol%)
DECLARE SUB manipulationxt (pcontroloc$, kinsert%)
DECLARE SUB showprof (inzloc%, iendmloc%, dwloc$, axt!(), jaloc%, jsloc%, iccloc%)
DECLARE SUB hormone (ila%, ahorm!, jaloc%, jsloc%)
DECLARE SUB switchscreen (inz%, iactive%, igraph%, KD%, programname$, fdelay)
DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DECLARE SUB gmanage (wasnun$)
DECLARE SUB gtreturn (inz%, igt%)
DECLARE SUB init (inewscreen%, igraph%, ivorcol%, ibackcol%, ivgashift%)
DECLARE SUB paramenu ()
DECLARE SUB PCXOUT (ipcx%, ipcs%)
DECLARE SUB zeingabe (igt%, iquest%, inz%, i%, f!, labels$, text$)

'$INCLUDE: 'scommon.bi'
ifree = 3 'qbx or PB

$INCLUDE "..\subs\pblib-p"
$include "..\subs\errlist.bas"
$INCLUDE "..\subs\dlib.bas"
$segment
$INCLUDE "..\subs\ddisplay.bas"
$segment

$INCLUDE "..\subs\dprofile.bas"
$include "c:\bas\pb\pbgraph.inc"
screen 12
Call Inigraphics
$link "c:\bas\pb\pbgv2.pbl"
$segment



imxl = 631: ilm = 9: imx = 20: imy = 20
pbimxl = 801: pbilm = 3: pbimx = 51: pbimy = 181
imyl = imxl
programname$ = "SP":
pdir$ = "..\PARAM-SP\"
basfilename$ = "DSP.BAS"
DATA 10,22,55,75,115,180,195,240,310,352,375,425,455,555,585,595,630
imxl2 = imxl / 2:
ianzmax = 9: msiluetmax = 700

dirtemp$ = "..\tmp\"
curdrive$ = ""
programloc$ = ""
aprodir$ = curdrive$ + programloc$
apardir$ = curdrive$ + programloc$ + pdir$

DIM axy(ilm, imx, imy), axt(ilm, imxl)'for very large arrays, this statement must be de-activated...
DIM arandom(imx, imy), arandomxt(imxl)
DIM iparnam$(20), fparnam$(10, 10)
DIM flv(10, 10), inv(21)' to store parameters
DIM label$(10)
DIM asu(ilm), u(ilm, imx), digfa(ilm), afc(ilm)
DIM lax(10), lx(10), lay(10), ly(10), gxa(10), gfa(10), aam(10)
DIM gya(10), ico(10), itypus(10), iwi(10)
DIM tv$(10), agfa(10), itota(10), s$(280), sl$(40)
'DIM agrad(1, 1)
DIM mempos(imx), icolm(10), ibm(16), a2(10), ansp(10)
msiluetmax = 1000
DIM msiluet(msiluetmax)
ihp = 1
diffmax = .4'max. value for diffusion
esc$ = CHR$(27)
ipcx = -2: ipcs = -2
fdelay = 0!: igtdelay = 20: ibackcol = 15
CLS : RANDOMIZE 123

parameter:
CALL paramenu

 '-----------------  start / continuation of program ------------------
mess$ = ""
SELECT CASE PCONTROL$
CASE "A"
      CALL gmanage("VU")
      CALL gmanage("VR")
'showdisplay
CASE "D"
IF ipcontrol = 0 THEN
    SELECT CASE KD
    CASE 3, 4, 5, 15 TO 25, 29, 30
     CALL clsscr(ipcs, ipcx, 15)
    CALL showprof(KD, 1, displaywas$, axt(), ja, js, icc)
    END SELECT
    ELSE 'kd=1,2..
    SELECT CASE ipcontrol
    CASE 104, 105
    CALL showprof(ipcontrol - 100, 1, displaywas$, axt(), ja, js, icc)
    CASE ELSE
    CALL clsscr(ipcs, ipcx, 15)
    CALL showdisplay(ipcontrol, displaywas$)
    END SELECT
    END IF
    IF ipcs > -2 THEN CALL PCXOUT(-2, 0)
    iparam = 2: ipcontrol = 0: ihp = 1
CASE "DP"
      CALL PCXOUT(ipcx, ipcs)


CASE "S", "Sm", "N", "C", "I", "II", "D", "CS"
  dra = 1! - ra - 2! * DA '- sparing simulation time: kt is not necessary
  drb = 1! - rB - 2! * db '-   to repeat this calculation of constant
  drc = 1! - rc - 2! * DC '-             ..
  drd = 1! - rd - 2! * DD '-             ..
  dre = 1! - re - 2! * DE '-             ..
  drf = 1! - rf - 2! * DF '-             ..
  drg = 1! - rg - 2! * dg '-             ..
  drh = 1! - rh - 2! * dh
  DBCC = drb: ahorm = 0
   icountot = 0: imanipulate = 0: iendm = 0
IF PCONTROL$ = "N" OR PCONTROL$ = "NR" GOTO refresh
IF PCONTROL$ = "C" AND iparam < 2 GOTO refresh
IF PCONTROL$ = "C" GOTO continuext

   '---------------------    initial conditions  --------------
  ja = kx: js = ky: FOR il = 1 TO KN: lax(il) = 1: lay(il) = 1: NEXT il
  afa = 1: bfb = 1
  ' IF ke = 2 OR ke = 5 THEN
  '     afa = bb / ta  'Steady state for the depletion mechanims
  '     bfb = bb / (ta * afa * afa)
  ' END IF
 imxl2 = imxl / 2: IF ky > imxl2 THEN imxl2 = imxl
  FOR i = 1 TO imxl2
    IF PCONTROL$ <> "II" THEN
      rho = ra: IF ca > 0 THEN rho = ca
      axt(0, i) = rho * (1! + 2 * KR / 100 * (RND - .5))'      * EXP(ad * i)'source density
      arandomxt(i) = 1! + 2 * KR / 100 * (RND - .5)
    END IF
    'RND: randomnumbers between 0.0 and 1.0 result in +-IR% fluctuation
    axt(1, i) = afa * gA '-- initial condition: A is set to AA in all
    axt(2, i) = bfb * gB '-- cells, B, C, and D to AB, AC, and AD
    axt(3, i) = gC '-- respectively. In the following, certain
    axt(4, i) = gD '-- cells might be given other values for A
    axt(5, i) = ge
    axt(6, i) = gf
    axt(7, i) = gg
    axt(8, i) = 1: IF KN > 7 THEN axt(8, i) = gh
  NEXT i
    jx2 = (ky + kx) / 2


  SELECT CASE ki '---- KI determines particular initial conditions -------
  CASE 1: axt(1, ja) = aa: axt(3, ja) = aC: axt(5, ja) = ae  'left cell
  CASE 2: axt(1, ja) = aa: axt(3, ja) = aC: axt(1, js) = aa: axt(3, js) = aC
       axt(5, ja) = ae: axt(5, js) = ae
  CASE 3:
  jx = js / 2: axt(1, jx) = aa: axt(3, jx) = aC
       axt(5, jx) = ae  'cell in the centre
  CASE 4
    DO WHILE i > 0 'particular cells are initiated during run-time
      mess$ = "# of cell to be initially active (axt(#) = aa), <return>=stop"
      CALL zeingabe(igt, 0, 1, i, dummy, duummy$, mess$)
      axt(1, i) = aa
    LOOP
  CASE 5, 15
    FOR i = 1 TO 17
    READ ix 'Special cells are red from data statement
    IF ix > js THEN EXIT FOR
    axt(1, ix) = aa: axt(3, ix) = aC: axt(5, ix) = ae
    NEXT i: RESTORE
    IF ki = 15 GOTO STEPPATTERN 'an additional stable pattern is assumed
  CASE 6 'Random cells are activated at maximum 50 cells distance
    ix = RND * 30
    FOR i = 1 TO 20
    axt(1, ix) = aa: axt(3, ix) = aC
    ix = ix + RND * 40 + 10
    IF ix > js THEN EXIT FOR
    NEXT i
  CASE 7 'sinusoidal prepattern, used for space-dependent
    amax7 = 0'                    substrate production
    select case KD
    case 14,16,17,27,28 'circular case
     FOR i = 1 TO ky             ' ab adds a linear gradient (for Nautilus Pompilius)
      idistance = ABS(k2 - i): 
      if abs(i+ky-k2) < idistance then idistance = abs(i+ky-k2)
      if abs(k2+ky-i) < idistance then idistance = abs(k2+ky-i)
      axt(0, i) = axt(0,i) *(1! + dy * COS(idistance * 3.14 / k1)) ^ 2
     NEXT i
    case else
       FOR i = 1 TO ky             ' ab adds a linear gradient (for Nautilus Pompilius)
       axt(8, i) = 1! + dy * (COS((i - k2) * 3.14 / k1)) ^ 2 + i * aB / ky
       IF axt(8, i) > amax7 THEN amax7 = axt(8, i)
    NEXT i
    FOR i = 1 TO ky: axt(8, i) = axt(8, i) / amax7 : NEXT i'Normalization
    end select
 CASE 8 'Exponential gradient in the source density
    '    arteficial spatial periodic, stable in time, dy determines
    '    the difference between maxima and minima, wavelength = field.
    '    K2 is the phase; ab adds a linear gradient (for Nautilus Pompilius)
    FOR ix = 1 TO ky: axt(0, ix) = axt(0, ix) * EXP(-aB * (ix - 1)): NEXT ix
  CASE 9 'a more step-like distribution
STEPPATTERN:
    FOR i = 1 TO js: axt(8, i) = 1: NEXT
    FOR i = 1 TO ky: axt(8, i) = 1: NEXT
    FOR i = k2 TO K4: axt(8, i) = 1 + dy: NEXT
    FOR i = k1 + 1 TO ky: axt(8, i) = axt(8, i - k1): NEXT'Repetition after K1 cells
    FOR i = 1 TO ky: axt(8, i) = axt(8, i) + i * aB / ky: NEXT
    FOR ji = 1 TO DZ
      al = axt(8, kx)'smoothing
      FOR i = kx TO ky - 1: axt(8, i) = (axt(8, i) + axt(8, i + 1)) / 2: NEXT i
      FOR i = ky TO kx + 1 STEP -1: axt(8, i) = (axt(8, i) + axt(8, i - 1)) / 2
      NEXT i: NEXT ji
    amax7 = 0 'normalization
    FOR i = 1 TO ky: IF axt(8, i) > amax7 THEN amax7 = axt(8, i)
    NEXT i
    FOR i = 1 TO ky: axt(8, i) = axt(8, i) / amax7: NEXT i'normalization

 CASE 10 'the left-most cell may function as pace-makers
    axt(8, ja) = aB 'left cell
 CASE 11 'left and right cells can be pacemaker
    axt(8, ja) = aB
    axt(8, js) = aB

  '------------Special cases of non-shell pattern  ------

 CASE 12
    '    for chemotaxis; artificial asymmetry,  stable in time, dy determines
    '    the difference between maxima and minima, K1 the spatial wavelength.
    '    K2 is the phase; AB adds a linear gradient; could be changed by k4 = 16
   FOR i = 1 TO js:
      rho = ra: IF ca > 0 THEN rho = ca
    IF PCONTROL$ <> "II" THEN
      axt(0, i) = rho * (1! + 2 * KR / 100 * (RND - .5)) 'neue Quelldichte  s
      axt(8, i) = 1! + dy * COS(((i - k2) / (js)) * 6.283) + ab * i / ky
     axt(0, i) = axt(0, i) * axt(8, i)
    END IF
  NEXT i


 CASE 121 'for position-dependent gen-activation
    'ca = Efficiency of the genes, cb=Slope of the gradient
    FOR i = 1 TO js: axt(8, i) = EXP(cB * (i - 1))
    axt(6, i) = ba * axt(8, i): NEXT 'Positional Information
    fkr = KR / 100
    FOR iy = 1 TO 4 ' Efficiency of the genes in the autocatalysis
    axt(0, iy) = ra * (1 + fkr * RND - .5 * fkr): NEXT


 CASE 123'for gene activation in a single cell
      axt(1, ja) = aa ' Gen 1 a bit more activated
      FOR i = ja TO js: axt(8, i) = 0: NEXT
      axt(8, js) = aB ' Signal for switching, acts only on one gene
 CASE 124: 'for gene activation in a single cell with threshold
      FOR i = 1 TO js: axt(8, i) = 1 + i * aB / ky: axt(2, i) = ba * axt(8, i)
     NEXT i 'Pos. Inf. in B

CASE 125 ' for border cells
 axt(3, ky / 2) = 1
 axt(3, ky / 2 + 1) = 1

CASE 126 'Small initial gradient for Hydra-Simulation
    FOR ix = 1 TO ky: axt(3, ix) = gC * EXP(-aC * (ix - 1)): NEXT ix

CASE 130  ' this is for feather; d (anti-rachis) and f (rachis) at opposite positions
   axt(4,k2) =  ad:   axt(6, k2 + ky/2) = af

  CASE ELSE
    mess$ = "KI, (initial conditions) must be between 1 and 11"
    resp$ = "ok": CALL cmessage(4, -1, 1, 15, 4, 0, mess$, resp$)
    GOTO parameter
  END SELECT
     itot& = 0

refresh:
  IF PCONTROL$ = "CS" THEN
    CALL zeingabe(igt, 1, 1, ico(1), dummy, dummy$, "new colour, 0-14")
  ELSE
    IF PCONTROL$ <> "SM" THEN CALL clsscr(ipcs%, ipcx%, ibackcol%)
  END IF

FOR i = 1 TO msiluetmax: msiluet(i) = 0: NEXT
  FOR il = 0 TO ilm'reset store array
    KAX(il) = ja: kx(il) = js: kay(il) = 1
    FOR jx = 1 TO imx
      FOR jy = 1 TO imy
	axy(il, jx, jy) = 0
      NEXT jy: NEXT jx: NEXT il
  icc = 0
  igrowth = 0
'  CALL cmessage(5, -1, 1, 1, 15, ibackcol, title$, resp$)
simulation:
' ON ERRORGOTO erroverflow
  icc = icc + 1
CALL showprof(KD, iendm, displaywas$, axt(), ja, js, icc)
if iendm = 999 goto FinishPlot' Lower border of the plot was reached in KD= 6-13
    IF ipcx > -1 THEN CALL PCXOUT(ipcx, ipcs)
'   IF title$>"" AND igraph = 9 THEN CALL cmessage(5, -1, 1, 1, 15, ibackcol, title$, resp$)
   CALL manipulationxt("ARRAYSTORE", kinsert)
    CALL switchscreen(2, iactive, igraph, KD, programname$, fdelay)
   IF icountot >= kt OR PCONTROL$ = "I" OR PCONTROL$ = "II" GOTO EndSimulation
   IF INKEY$ = esc$ THEN GOTO makedraw' interrupted by ESC key
continuext:
igrowth = igrowth + 1
IF KG > 0 THEN 'growth by insertion of additional cells if kg>0
IF igrowth >= KG AND js < imxl - 1 THEN
   igrowth = 0
CALL manipulationxt("G", kinsert)
END IF
END IF


' Begin of the iterations. After each KP iterations the concentration are
' plotted. This will happens KT times. Altogether kp * kt time steps will
' be calculated.
jrechts = js: jlinks = ja:
IF k1 = 99 THEN jrechts = ja: jlinks = js' cyclic boundary
SELECT CASE KD 'inherent cyclic
CASE 14,16, 161, 17, 27, 28
jrechts = ja: jlinks = js
END SELECT

FOR iprint = 1 TO KP  '-- SECOND LOOP: kp iterations until next plot ------

' Boundary conditions: impermeable; this is achieved by
' giving the cells ja-1 and js+1 the same values as cells ja resp.js
' or , if k1=11, then cyclic
  FOR il = 1 TO KN: axt(il, js + 1) = axt(il, jrechts): NEXT il 'right border cells
  al = axt(1, jlinks): bl = axt(2, jlinks) ' virtual left border cells:
  IF KN > 2 THEN cl = axt(3, jlinks): dl = axt(4, jlinks):
  IF KN > 4 THEN el = axt(5, jlinks): fl = axt(6, jlinks):  gl = axt(7, jlinks): zhl = axt(8, jlinks)


 ON ERROR GOTO erroverflow

SELECT CASE KE'EQUATIONS  '---- selection of the equation

'      ================ Main equations ==================
CASE 21 '- activator - inhibitor mechanism: B is inhibitor --------------
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s * (a * a / b + ba)
     axt(2, i) = olddecaydiffB + s * a * a + bb
   NEXT i

CASE 211 '- Aktivator - Inhibitor Mechanismus: B is Inhibitor --------------
   'Das ist eine spezielle Version von Gleichung 21 um mit Simulationen in
   'zwei getrennten Bereichen Regeneration zu zeigen
   IF K4 = 0 THEN K4 = (js - ja) / 2 + 2
   axt(1, K4) = axt(1, K4 - 1): axt(2, K4) = axt(2, K4 - 1):
   FOR i = ja TO K4 - 1: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s * (a * a / b + ba)
     axt(2, i) = olddecaydiffB + s * a * a + bb
   NEXT i
   al = axt(1, K4 + 1): bl = axt(2, K4 + 1):
   FOR i = K4 + 1 TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s * (a * a / b + ba)
     axt(2, i) = olddecaydiffB + s * a * a + bb
   NEXT i


CASE 23 '- activator - inhibitor mechanism: B is inhibitor --------------
   FOR i = ja TO js: GOSUB olddecay:
     'with saturation and Michaelis-Menten-cinetics
     axt(1, i) = olddecaydiffA + s * (a * a / (sb + b) / (1 + sA * a * a) + ba)
     axt(2, i) = olddecaydiffB + s * a * a + bb
   NEXT i

CASE 24 '- activator-depletion mechanism:  ----
     '   a activator,  b substrate,
     '   axt(8,i) may contain a stable pattern (normalized to 1)
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1 + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * axt(8, i)
	IF axt(2, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 51 '------- Crossings  a: activator, b: substrate c: inhibitor
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a + ba) / (1! + sA * a * a) / (sb + sc * C)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * axt(8, i)
     axt(3, i) = olddecaydiffC + rc * a
     IF axt(2, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 501 '------- Crossings  a: activator, b: substrate c: inhibitor
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a + ba) / (1! + sA * a * a)
     axt(1, i) = olddecaydiffA + aq / C
     axt(2, i) = olddecaydiffB - sb * a * b + bb
     axt(3, i) = olddecaydiffC + rc * a * a
     IF axt(2, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 502 '------- hopefully phyllotaxis with auxin depletion
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * C * (a * a + ba)
     axt(1, i) = olddecaydiffA + aq / b
     axt(2, i) = olddecaydiffB + aq
     axt(3, i) = olddecaydiffC + bc - sc * C * a
     IF axt(3, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 503 '------- Crossings  a: activator, b: substrate c: inhibitor
   FOR i = ja TO js: GOSUB olddecay:
' c full system, non-linear
     aq = s * b * (a * a + ba) / (sb + sc * C)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb
     axt(3, i) = olddecaydiffC + rc * a * a
     IF axt(2, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 504 '------- Crossings  a: activator, b: substrate c: inhibitor
' circa =  51 but depletion can be switches off by cc
' inhibitor production is non-linear
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a + ba) / (1! + sA * a * a) / (sb + sc * C)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - sb * aq + bb * axt(8, i)
     axt(3, i) = olddecaydiffC + rc * a * a
     IF axt(2, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 52 '--- one activator - two substrates
   FOR i = ja TO js: GOSUB olddecay:
       aqfs = a * a / (1 + sA * a * a)
       aqb = s * b * aqfs
       aqc = cB * C * aqfs
       axt(1, i) = olddecaydiffA + aqb + aqc
       axt(2, i) = olddecaydiffB - aqb + bb
       axt(3, i) = olddecaydiffC - aqc + bc
   NEXT i
CASE 511 '- activator - inhibitor mechanism: B is inhibitor --------------
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + C * s * (a * a / b + ba)
     axt(2, i) = olddecaydiffB + C * s * a * a + bb
     axt(3, i) = olddecaydiffC + bc - sc * a * C
   NEXT i

CASE 512 '------- Crossings  a: activator, b: substrate c: inhibitor
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a + ba) / (1! + sA * a * a) / (sb + sc * C)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - cc * aq + bb
     axt(3, i) = olddecaydiffC + rc * a * a
     IF axt(2, i) < 0 THEN axt(2, i) = 0
   NEXT i

CASE 61 '-- Branches controlled by a hormone  : Olivia Porphyria ----------
     '  Hormone (c) changes lifetime of the inhibitor
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * a * a / (1! + sA * a * a) + ba
     axt(1, i) = olddecaydiffA + aq / (sb + b)
     axt(2, i) = olddecaydiffB + aq + bb
     ahorm = ahorm + rc * a 'hormone production by a
     IF i = js THEN 'averaging
     CALL hormone(3, ahorm, ja, js)
      rbb = rB / C '---- effective inhibitor decay rate
      drb = 1! - 2! * db - rbb
     END IF
   NEXT i

CASE 71 '-- activation (a, b), enhancement (c, d)
   FOR i = ja TO js: GOSUB olddecay:
     'and  extinguishing  (e, f) reaction
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq - ce * a * e
     axt(2, i) = olddecaydiffB - aq + bb * (1 + sb * C + cB * D)
     cq = rc * a * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (sd + D)
     axt(4, i) = olddecaydiffD + cq + bd
     eqf = e * e
     eq = re * f * (eqf / (1! + se * eqf) + be)
     axt(5, i) = olddecaydiffE + eq + sf * a
     axt(6, i) = olddecaydiffF - eq + BF * a + cf * axt(8, i)
     IF axt(2, i) < 0 THEN axt(2, i) = 0'to avoid numerical instabilities
     IF axt(6, i) < 0 THEN axt(6, i) = 0
   NEXT i

CASE 711 '-- activation (ab) and extinguishing  (e,f) reaction
     'as 71 without enhancing reaction
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq - ce * a * e
     axt(2, i) = olddecaydiffB - aq + bb
     eqf = e * e
     eq = re * f * (eqf / (1! + se * eqf) + be)
     axt(5, i) = olddecaydiffE + eq + sf * a
     axt(6, i) = olddecaydiffF - eq + BF * a + cf * axt(8, i)
     IF axt(2, i) < 0 THEN axt(2, i) = 0
     IF axt(6, i) < 0 THEN axt(6, i) = 0
   NEXT i

CASE 712 '-- activation (ab) and enhancing (c,d) reaction
     'as 71 without extinguishing reaction
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * (1 + sb * C + cB * D) * axt(8, i)
     IF axt(2, i) < 0 THEN axt(2, i) = 0
     cq = rc * a * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (sd + D)
     axt(4, i) = olddecaydiffD + cq + bd
   NEXT i

CASE 713 '-- activation (ab) and enhancing (c,d) reaction
     'as 71 without extinguishing reaction
     'as 712 but for band modulated is live time of the activator
   FOR i = ja TO js:
   dra = 1 - ra / axt(8, i) - 2 * DA
   GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * (1 + sb * C + cB * D)
     IF axt(2, i) < 0 THEN axt(2, i) = 0
     cq = rc * a * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (sd + D)
     axt(4, i) = olddecaydiffD + cq + bd
   NEXT i


CASE 714 '-- sharp dots (d, e) along invisible lines (c)
     'and waves (a, b)
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * (1 + ce * e)
     eq = re * a * C * (e * e + be)
     axt(5, i) = olddecaydiffE + eq / (sf + f)
     axt(6, i) = olddecaydiffF + eq
   NEXT i


CASE 715 '-- activation (ab) and enhancing (c,d) reaction
     'as 71 without extinguishing reaction
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * (1 + sb * C + cB * D) * axt(8, i)
     IF axt(2, i) < 0 THEN axt(2, i) = 0
     cq = arandomxt(i) * rc * a * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (sd + D)
     axt(4, i) = olddecaydiffD + cq + bd
   NEXT i

CASE 716 '-- activation (ab) and enhancing (c,d) reaction
     'as 71 without extinguishing reaction
     ' as 712 but  c-activator removal is space-dependent
     ' low h = stable!
     ' towards Bednalli
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * (1 + sb * C + cB * D)
     IF axt(2, i) < 0 THEN axt(2, i) = 0
     cq = arandomxt(i) * rc * a * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (1 + sc * C * C) / (sd + D) - C * cc * axt(8, i)
     axt(4, i) = olddecaydiffD + cq + bd
   NEXT i


CASE 72 '---  a, b = pigmentation, c, d = enhancement, e = pool
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba * C)
     axt(1, i) = olddecaydiffA + aq
     frompool = bb * se * e / (1 + se * e + ce * b) * (1 + sc * C)
     IF frompool > e THEN frompool = e
     axt(2, i) = olddecaydiffB - aq + frompool
     cq = rc * arandomxt(i) * a * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (sd + D)
     axt(4, i) = olddecaydiffD + cq
     axt(5, i) = olddecaydiffE + be * axt(8, i) - frompool
     IF axt(2, i) < 0 THEN axt(2, i) = 0
     IF axt(5, i) < 0 THEN axt(5, i) = 0
   NEXT i

CASE 77 '---------two activator-depletion mechanisms, c kills a
       'plus a stagered dot system that enhances a
       'for Cymbiolacca wisemani
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq - cc * a * C
     axt(2, i) = olddecaydiffB - aq + bb * (1 + ce * e)
     IF axt(2, i) < 0 THEN axt(2, i) = 0
' killing reaction
     cqf = C * C
     cq = arandomxt(i) * D * (cqf / (1! + sc * cqf) + bc)
     axt(3, i) = olddecaydiffC + cq + sd * a
     axt(4, i) = olddecaydiffD - cq + bd * a + cd * e
     IF axt(4, i) < 0 THEN axt(4, i) = 0
'enhancing reaction
     axt(5, i) = olddecaydiffE + re * a * arandomxt(i) / g * (e * e / f + be)
     axt(6, i) = olddecaydiffF + rf * e * e / g
     axt(7, i) = olddecaydiffG + rg * e
   NEXT i

CASE 91 '---  Clithon
     ' a,b -> pigmentation (AS-system)
     ' c,d -> precondition/background (AI-system)
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * C * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb
     cqf = C ^ 2
     REM axt(9,i) = 1 � random fluctuations (given by KR in %)
     cq = rc * arandomxt(i) * (cqf / (1! + sc * cqf) + bc * a)
     axt(3, i) = olddecaydiffC + cq / (D + cd * b + se * e)
     axt(4, i) = olddecaydiffD + cq + bd * axt(8, i)
     axt(5, i) = olddecaydiffE + be * axt(8, i) * C'the long term poison
   NEXT i

'CASE ============ Special cases and modified equations ==================

CASE 242 '- Natica euzona
   FOR i = ja TO js:
     dra = 1 - ra * axt(8, i) - 2! * DA
     GOSUB olddecay:
     aq = axt(8, i) * s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * axt(8, i)
   NEXT i

CASE 27 'autocatalysis by an inhibition of an inhibition (a and c)
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s / (sA + C * C) + ba
     axt(2, i) = olddecaydiffB + rB * a
     axt(3, i) = olddecaydiffC + rc / (sc + a * a / (sb + b * b))
   NEXT i

CASE 41 ' activator - inhibitor mechanism and feedback on the source (c)
   FOR i = ja TO js: GOSUB olddecay:
     aq = C * s * (a * a + ba)    '------    simple rows of dots
     axt(1, i) = olddecaydiffA + aq / b
     axt(2, i) = olddecaydiffB + aq + bb
     'Source (C)
     axt(3, i) = olddecaydiffC + rc * a + bc
  NEXT i
CASE 42 '--Two activ.-inh. systems a, b and c, d. c inhibits also a
   FOR i = ja TO js: GOSUB olddecay:
     aq = a * a / (1! + sA * a * a)'    ----    for Junona
     axt(1, i) = olddecaydiffA + s * (aq / (b + sb * C) + ba)
     axt(2, i) = olddecaydiffB + rB * aq + bb
     cq = rc * arandomxt(i) * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / D
     axt(4, i) = olddecaydiffD + cq
   NEXT i

CASE 43 '- activator - inhibitor mechanism:
     'lifetime of the inhibitior is space-dependent
     ' shifted horizontal lines, one system
   FOR i = ja TO js:
   drb = 1 - rB * axt(8, i) - 2 * db
   GOSUB olddecay:
     aq = a * a / (1 + sA * a * a)
     axt(1, i) = olddecaydiffA + s * (aq / b + ba)
     axt(2, i) = olddecaydiffB + rB * aq
   NEXT i

CASE 44 '-- activation (a, b) and enhancing (c, d) reaction
     ' shifted horizontal lines, two systems
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba + cc * C)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * axt(8, i)
     cq = rc * D * (C * C / (1 + sc * C * C) + bc)
     axt(3, i) = olddecaydiffC + cq
     axt(4, i) = olddecaydiffD - cq + bd
   NEXT i

CASE 45 '--Activ- Depletion system for pigment, Ai system for stable patter c, d
   FOR i = ja TO js: GOSUB olddecay:
     '   a activator,  b substrate,
     aq = s * b * (a * a + ba)/ (1 + sA * a * a)/(1 + sb * c)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb + cc * c
      IF axt(2, i) < 0 THEN axt(2, i) = 0
       cq = rc * arandomxt(i) * (C * C + bc)
       axt(3, i) = olddecaydiffC + cq /(1 + sc * c * c) / D
       axt(4, i) = olddecaydiffD + cq
   NEXT i

CASE 53 'shifted dots = c has negative feedback on a and b
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s / C * (a * a / b + ba)
     axt(2, i) = olddecaydiffB + rB * a * a / C + bb
     axt(3, i) = olddecaydiffC + rc * a
   NEXT i

CASE 531 'shifted dots = c has negative feedback on a and b
' f�r die Bl�tter,
' d,e=zweites System, f�r Polarit�t
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s / (C + cc * D) * (a * a / b + ba)
     axt(2, i) = olddecaydiffB + rB * a * a / C + bb
     axt(3, i) = olddecaydiffC + rc * a
     dq = rd * arandomxt(i) * (D * D + bd)
     axt(4, i) = olddecaydiffD + dq / (e + cd * a) + bd * a + sd * b
     axt(5, i) = olddecaydiffE + dq + be
     axt(6, i) = a * D
   NEXT i

CASE 532 'shifted dots = c has negative feedback on a and b
   FOR i = ja TO js: GOSUB olddecay:  '=53 with saturation
     axt(1, i) = olddecaydiffA + s / C * (a * a / ((1 + sA * a * a) * b) + ba)
     axt(2, i) = olddecaydiffB + rB * a * a / C + bb
     axt(3, i) = olddecaydiffC + rc * a
   NEXT i

CASE 54 'shifted dots triangles =  sum of two inhibitors
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s * (a * a + ba) / (sb * b + sc * C)
     axt(2, i) = olddecaydiffB + rB * a * a + bb
     axt(3, i) = olddecaydiffC + rc * a
 NEXT i

CASE 541 'shifted dots triangles =  sum of two inhibitors
   FOR i = ja TO js: GOSUB olddecay: 'with saturation
     axt(1, i) = olddecaydiffA + s * (a * a + ba) / (1 + sA * a * a) / (sb * b + sc * C)
     axt(2, i) = olddecaydiffB + rB * a * a + bb
     axt(3, i) = olddecaydiffC + rc * a
 NEXT i

CASE 542 'shifted dots triangles =  sum of two inhibitors
'both ful pattern forming systems
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s * (a * a + ba) / (sb * b + sc * C) / (1 + sA * a * a)
     axt(2, i) = olddecaydiffB + rB * a * a + bb
     axt(3, i) = olddecaydiffC + rc * a * a
 NEXT i

CASE 62 '- Branches controlled by a hormone  : Olivia Porphyria ----------
     '  Hormone (c) changes lifetime of the activator
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * a * a / (1! + sA * a * a) + ba
     axt(1, i) = olddecaydiffA + aq / (sb + b)
     axt(2, i) = olddecaydiffB + aq + bb
     ahorm = ahorm + rc * a '------ hormone production by a
     IF i = js THEN
     CALL hormone(3, ahorm, ja, js)
       raa = ra * C '---- effective inhibitor decay rate
       dra = 1! - 2! * DA - raa
     END IF
   NEXT i

CASE 63 '- Branches controlled by a hormone  : Olivia Porphyria ----------
     '  Hormone (c) changes production rate of substrate b
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1 + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb / (cB + sb * C)
     ahorm = ahorm + rc * a '------ hormone production by a
     IF i = js THEN CALL hormone(3, ahorm, ja, js)
   NEXT i

CASE 64 ' Branches controlled by a hormone  : Olivia Porphyria ----------
     '  Hormone changes lifetime of the inhibitor
     '  two inhibitors are involved (b,d) a: activator, c: hormone
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq / (sb + sd * D + b)
     axt(2, i) = olddecaydiffB + aq + bb
     axt(4, i) = olddecaydiffD + rd * a
     ahorm = ahorm + rc * a ' hormone production by a
     IF i = js THEN
     CALL hormone(3, ahorm, ja, js)
       rbb = rB / C '---- effective inhibitor decay rate
       drb = 1! - 2! * db - rbb
     END IF
   NEXT i

CASE 73 '-- Survival by change of substrate production
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq - sb * a * C
     axt(2, i) = olddecaydiffB - aq + bb * C
     axt(3, i) = olddecaydiffC + rc * b
   NEXT i

CASE 731 ' Production rate of the substrate increases in non-activated
     ' periods via c
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * C
     axt(3, i) = olddecaydiffC + rc / (cc + bc * a)
   NEXT i

CASE 81 '- activator-depletion mechanisms, homogeneous oscillation
'oscillation blocks substrate production or enhances activator destruction
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq - cc * a * C
     axt(2, i) = olddecaydiffB - aq + bb / (1 + sb * C)
IF i = js THEN CALL oscillation(3, C, D, olddecaydiffC, olddecaydiffD, ja, js)
   NEXT i

CASE 83 '----- Crossing solution plus oscillations
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a + ba) / (1! + sA * a * a) / (sb + sc * C)
     axt(1, i) = olddecaydiffA + aq - cc * D * a
     axt(2, i) = olddecaydiffB - aq + bb
     axt(3, i) = olddecaydiffC + rc * a
IF i = js THEN CALL oscillation(4, D, e, olddecaydiffD, olddecaydiffE, ja, js)
   NEXT i

CASE 832 ' Crossing solution plus oscillations (de -> ai-system)
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a + ba) / (1! + sA * a * a) / (sb + sc * C)
     axt(1, i) = olddecaydiffA + aq - cc * D * a
     axt(2, i) = olddecaydiffB - aq + bb * (1 + cB * D)
     axt(3, i) = olddecaydiffC + rc * a
     dq = rd * arandomxt(i) * (D * D + bd)
     axt(4, i) = olddecaydiffD + dq / e
     axt(5, i) = olddecaydiffE + dq
   NEXT i

CASE 84 'triangles with global stop, two inhibitors, hormone and oscillation
     'a=activator, b,c inhibitors; cd = oscillation, e=hormone
   FOR i = ja TO js: GOSUB olddecay:
     axt(1, i) = olddecaydiffA + s * (a * a / ((1 + sA * a * a) * (b + sc * C + sf * f)) + ba) - cc * a * D
     axt(2, i) = olddecaydiffB + s * a * a + bb'rb in 54
     axt(3, i) = olddecaydiffC + rc * a
     ahorm = ahorm + rf * a '------ AD gets hormone input
     IF i = js THEN
     CALL oscillation(4, D, e, olddecaydiffD, olddecaydiffE, ja, js)
     CALL hormone(6, ahorm, ja, js)
     END IF
   NEXT i

CASE 85 '-Branches versus triangles: antagonistic action of the c-d oscillation
     ' d boosts the delay rate, c lowers it
     ' e isadditional inhibitor
   FOR i = ja TO js:
   drb = (1 - rbb - 2 * db)
   GOSUB olddecay:
      rbb = rB * (1 + cB * D) / (1 + cc * C)' effective inhibitor decay rate
      drb = 1! - 2! * db - rbb
     aq = s * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq / (sb + se * e + b)
     axt(2, i) = b * (1 - rbb - 2 * db) + db * (bl + axt(2, i + 1)) + aq
     cq = rc * arandomxt(i) * (C * C + bc)
     axt(3, i) = olddecaydiffC + cq / (sd + D)
     axt(4, i) = olddecaydiffD + cq
     axt(5, i) = olddecaydiffE + re * a
   NEXT i

CASE 86 'three inhibitors (b,e,f) and oscillation - L. hieroglyphica
   FOR i = ja TO js
      C = axt(3, i): D = axt(4, i)
      rbb = rB * (1 + cB * D + cd * C) / (1 + cc * C)'---- effective Inhibitor
      drb = 1! - 2! * db - rbb           '     Zerfallsrate
   GOSUB olddecay:
     aq = s * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq / (sb + sf * f + se * e + b)
     axt(2, i) = b * (1 - rbb - 2 * db) + db * (bl + axt(2, i + 1)) + aq + bb
     cq = rc * arandomxt(i) * (C * C + bc)         'c and d - > oscillation
     axt(3, i) = olddecaydiffC + cq / (sd + D)  '= A-I system
     axt(4, i) = olddecaydiffD + cq
     axt(5, i) = olddecaydiffE + re * a
     axt(6, i) = olddecaydiffF + rf * a
   NEXT i

CASE 87 'Activator-substrate plus two additional inhibitors
     '=> three inhibitory actions
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * ((a * a + ba) / (1! + sA * a * a)) / (D + sb * C)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb
     axt(3, i) = olddecaydiffC + rc * a
     axt(4, i) = olddecaydiffD + bd * a
   NEXT i

CASE 92 ' a,b -> pigmentation (AS-system)
     ' c,d -> precondition/background (AI-system)
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb * C
     cq = rc * arandomxt(i) * (C * C / (1! + sc * C * C) + bc)
     axt(3, i) = olddecaydiffC + cq / (bd + D)
     axt(4, i) = olddecaydiffD + cq
   NEXT i

CASE 93 ' a,b -> pigmentation (AS-system)
     ' c,d -> precondition/background (AI-system)
     ' e,f -> Extinguishing system (AS-system)
   FOR i = ja TO js: GOSUB olddecay:
     aq = s * C * b * (a * a / (1! + sA * a * a) + ba)
     axt(1, i) = olddecaydiffA + aq
     axt(2, i) = olddecaydiffB - aq + bb
     cqf = C * C
     cq = rc * arandomxt(i) * D * (cqf / (1! + sc * cqf) + bc * a)
     axt(3, i) = olddecaydiffC + cq - cc * C * e
     axt(4, i) = olddecaydiffD - cq + bd
     'extinguishing reaction
     eqf = e ^ 2
     eq = re * f * (eqf / (1! + se * eqf) + be + ce * C)
     axt(5, i) = olddecaydiffE + eq
     axt(6, i) = olddecaydiffF - eq + BF * C
  NEXT i

' CASE ==========  Andere Simulationen wie Gen-Aktivierung, Hydra....

CASE 121 'Activation of several genes by a graded morphogen distribution
    'e = Repressor, f = positional information
    'sa, sb, sc.. = Efficiency of the genes in the autokatalyse
    ' a(i,iy)= ra+(1 +- Fluctuations)
    ' cb slope of the gradient
    ' e = a measure for the antagonistic reaction, competition
    ' f = positionnal information
    ' ba absolute influence of the morphogen
   FOR i = ja TO js: GOSUB olddecay' :
    e = sA * a * a + sb * b * b + sc * C * C + sd * D * D
    dq = sd * axt(0, 4) * D * D: axt(4, i) = olddecaydiffD + (dq + f * axt(3, i)) / e
    cq = sc * axt(0, 3) * C * C: axt(3, i) = olddecaydiffC + (cq + f * axt(2, i)) / e
    bq = sb * axt(0, 2) * b * b: axt(2, i) = olddecaydiffB + (bq + f * axt(1, i)) / e
    aq = sA * axt(0, 1) * a * a: axt(1, i) = olddecaydiffA + aq / e
    axt(5, i) = olddecaydiffE + aq + bq + cq + dq
    axt(6, i) = ba * axt(8, i)
   NEXT i

CASE 123 'Activation of several genes in a single cell
'  i=Gene Nr., not, as usual, the position
   FOR i = ja TO js: GOSUB olddecay:
	aq = a * a / (1! + sA * a * a)
	axt(2, i) = axt(8, i) * ba
	axt(1, i) = olddecaydiffA + s * (aq / C + b)
	ahorm = ahorm + rc * aq
	IF i = js THEN
	axt(3, 1) = axt(3, 1) * (1! - rc) + ahorm
	FOR iic = ja TO js
	  axt(3, iic) = axt(3, 1)
	NEXT iic
	ahorm = 0
	END IF
   NEXT i
CASE 124  'Only one gene, with threshold
   FOR i = ja TO js: GOSUB olddecay:
	axt(1, i) = olddecaydiffA + ra * a * a / (1! + sA * a * a) + b
	axt(2, i) = ba * axt(8, i)
   NEXT i

  CASE 125 'GE- BISTABLE STATE, FOR POLAR-BORDER CELLS#
' SLBO does not hinder the prodction but the effect of APT on J/S
'a = STAT , activated by UDP (term ba * b), inhibited by APT; thin inhibition is undermined  by SLBO
' J/S is  excluded by pole cells (term cc * c)
'b = UDP, produced by pole cells ( = c)
'c = pole cells, defined by initial condition, unchanged
'd = APT is quenching the signal (a) that is produced J/S (=> a); is inhibited by SLBO (term cd* e * e
'  and by itself (term sd*d*d, both terms are non-linear; a base-line APT production exists (se)
'e = SLBO , produced by j/S (a), inhibits  and is inhibited by APT (d)
   FOR i = ja TO js: GOSUB olddecay
      axt(1, i) = olddecaydiffA +  ba * B / (1 + cc * c * c + sa * A * A + cb * d/(1 + cd * e * e ))
      axt(2, i) = olddecaydiffB + bb * C
      axt(4, i) = olddecaydiffD + bd * (a / (1 + sd * d * d + sc * e) + se)
      axt(5, i) = olddecaydiffE + be * a * a / (1 + ce * d* d)
  NEXT i


CASE 126 'HY- - Hydra hypostome, tentacles and foot
   FOR i = ja TO js: GOSUB olddecay:
	aq = C * s * (a * a + ba)
	axt(1, i) = olddecaydiffA + aq / b
	axt(2, i) = olddecaydiffB + aq + bb
	'Source
	IF rc > 0 THEN axt(3, i) = olddecaydiffC + rc * a + bc - rc * C * f
	dfq = D * D + bd
	dq = C * cd * dfq / (1 + sd * dfq) / (1 + ce * a)
	axt(4, i) = olddecaydiffD + dq / e
	axt(5, i) = olddecaydiffE + dq + be
	'Foot
	fq = rf * (f * f + BF) / C'foot-system,
	axt(6, i) = olddecaydiffF + fq / g
	axt(7, i) = olddecaydiffG + fq + bg * C
   NEXT i


CASE 127 ' Activator-Inhibitor system, with feedback on the source
' if sc>0: Activator makes the feedback, source has to be diffusible
' if cc>0: Inhibitor makes the feedback, source has not to be diffusible
   FOR i = ja TO js: GOSUB olddecay:
	aq = C * s * (a * a + ba)
	axt(1, i) = olddecaydiffA + aq / b
	axt(2, i) = olddecaydiffB + aq + bb
	'Source
	axt(3, i) = olddecaydiffC + sc * a + cc * b / C + bc
   NEXT i

CASE 128 'A set of each other on long range activating but locally exclusive
' activator-inhibitor-Systems: an alternative way to generate polarity
   FOR i = ja TO js: GOSUB olddecay:
       rep = a * a + C * C + e * e + g * g
       axt(1, i) = olddecaydiffA + s * (a + ba * C) ^ 2 / b / rep
       axt(2, i) = olddecaydiffB + rB * a * a
       axt(3, i) = olddecaydiffC + arandomxt(i) * rc * (C + ba * (a + e)) ^ 2 / D / rep
       axt(4, i) = olddecaydiffD + rd * C ^ 2
       axt(5, i) = olddecaydiffE + arandomxt( i + js) * re * (e + ba * (C + g)) ^ 2 / f / rep
       axt(6, i) = olddecaydiffF + rf * e ^ 2
       axt(7, i) = olddecaydiffG + arandomxt( 2 * i) * rg * (g + ba * e) ^ 2 / zh / rep
       axt(8, i) = olddecaydiffH + rh * (g + ba * e) ^ 2
NEXT i


  CASE 130 'feather: branching zone via lokal saturation
 ' a-c activator - two inhibitor system' d,e -> A-I system for anti-rachis
   FOR i = ja TO js: GOSUB olddecay:   ' f,g -> A-I system for rachis
    aq = s * (a * a + ba)
     axt(1, i) = olddecaydiffA + (1 + ce * d) * aq / (sb * b + sc * c) / (1 + sa * a * a)
     bbinh = (1 + sf * f) / (1 + sd * d)
     axt(2, i) = olddecaydiffB + rb * a * a + bb * bbinh
     axt(3, i) = olddecaydiffC + bc * a * a
     dq= arandomxt(i) * rd * (d * d + bd)' d,e = anti-rachis
       axt(4, i) = olddecaydiffD + dq / e ' (e + se* e)
       axt(5, i) = olddecaydiffE + dq + be
     fq = rf * (f * f + bf) ' f,g = rachis
       axt(6, i) = olddecaydiffF + fq / g '(g+ sg * e)
       axt(7, i) = olddecaydiffG + fq  + bg
        axt(8, i) = bbinh  ' to show pos inf
 NEXT i

CASE 132 'for chemotactic orientation, AI system
' B, the inhibitor is uniformly distributed within the cell
   FOR i = ja TO js: GOSUB olddecay:
   axt(1, i) = olddecaydiffA + s * (a * a / b + ba) / (1! + sa * a * a)
   ahorm = ahorm + s * a * a + bb
	IF i = js THEN CALL hormone(2, ahorm, ja, js)
   axt(3, i) = s
   NEXT i


CASE 135 'GC-- two antagonists
'  b=uniformely distributed inhibitor s'=s/(sc+c)
   FOR i = ja TO js: GOSUB olddecay
  axt(1, i) = olddecaydiffA + s / (sc + cc * c) * (A * A / B + ba) / (1 + sa * A * A)
  axt(3, i) = olddecaydiffC + bc * A
  axt(4, i) = s
   ahorm = ahorm + s / (sc + c) * A * A + bb
	IF i = js THEN CALL hormone(2, ahorm, ja, js)
   NEXT i

CASE 136 'EC = a: activator, b: substrate,  c: wave that burn down
'two as-systems minD= ab, not a pattern forming system, becomes destructed
' by minE = C. a has the function of local poisening = kills
' local minE (c) due to depletion
' So for PNAS: MinE at Membrane depends on MinD, in this case linearely
   FOR i = ja TO js: GOSUB olddecay
     aqf = a * a: aq = s * b * (aqf + ba) / (1 + sA * aqf)
     cq = rc * arandomxt(i) * a / (1 + sd * aqf) * D * (C * C + bc) / (1 + sc * C * C)
    axt(1, i) = olddecaydiffA + aq - cc * a * C
     'a(1, i) = olddecaydiffA + ba - cc * a * c
    axt(2, i) = olddecaydiffB - aq + bb
    IF axt(2, i) < 0 THEN axt(2, i) = 0
    axt(3, i) = olddecaydiffC + cq
    axt(4, i) = olddecaydiffD - cq + bd
    IF axt(4, i) < 0 THEN axt(4, i) = 0
' ftz is an further system
     eq = re * arandomxt(i) * f * (e * e + be + sf * C) / (1 + ce * a)
    axt(5, i) = olddecaydiffE + eq - se * e * a
    axt(6, i) = olddecaydiffF - eq + BF
   NEXT i

 '-------------- End of equations ---------------------------------------------
 CASE ELSE: GOTO nosuchequation
  END SELECT
  itot& = itot& + 1
  NEXT iprint '------------- End of loop 2 (kp iterations until next plot) -----
  icountot = icountot + 1: imanipulate = imanipulate +1
  IF imanipulate = K3 THEN
    SELECT CASE K4'  K3: Number of printouts after which the change occurs
    CASE 1'  K4 Type of change,  DZ new parameter
      mess$ = "Basic activator production ba changed from" + STR$(ba) + " to" + STR$(DZ)
      ba = DZ
    CASE 2
      mess$ = "decay rate of b (inhibitor of substrate) changed from" + STR$(tb) + " to" + STR$(DZ)
      rB = DZ: drb = 1! - rB - 2! * db
    CASE 3
      mess$ = "bb = basic inhibitor/substrate production changed from" + STR$(bb) + " to" + STR$(DZ)
      bb = DZ
    CASE 4
      mess$ = "decay rate of the activator a changed from" + STR$(ra) + " to" + STR$(DZ)
      ra = DZ: dra = 1! - ra - 2! * DA
    CASE 5
      mess$ = "Activator concentration will be changed in a fraction of the field"
      istart = ja: istop = js
      CALL zeingabe(igt, 1, 1, istart, dummy, dummy$, "Perturbation starts at cell")
      CALL zeingabe(igt, 1, 1, istop, dummy, dummy$, "Perturbation stops at cell")
      IF istart < ja THEN istart = ja
      IF istop = 0 OR istop > js THEN istop = js
      CALL zeingabe(igt, 0, 2, istop, factor, dummy$, "Factor by which the activator is to be changed, 1= unchanged")
      FOR i = istart TO istop
      axt(1, i) = factor * axt(1, i): NEXT i
      mess$ = "Activator concentration locally changed "
    CASE 6
      FOR ix = 1 TO js: axt(1, ix) = axt(1, ix) * DZ: NEXT
      mess$ = "the activator reduced via dz by a factor " + STR$(DZ)
    CASE 7
      mess$ = "production and decay rate of C changed from" + STR$(tc) + " to" + STR$(DZ)
      tc = DZ: drc = 1! - tc - 2! * DC
    CASE 8
      mess$ = "saturation (sa) of the activator a changed from" + STR$(sA) + " to" + STR$(DZ)
      sA = DZ
    CASE 9
      mess$ = "CB changed from" + STR$(cB) + " to" + STR$(DZ)
      cB = DZ
    CASE 10
      mess$ = "concentration of an array to be selected can be changed"
      CALL manipulationxt("A", kinsert)
    CASE 11
      mess$ = "Diffusion rate of activator and inhibitor is set to zero"
      DA = 0: db = 0: dra = 1 - ra: drb = 1 - rB
    CASE 12
      mess$ = "array C is set to DZ between ja+10 and js-10"
      FOR i = ja + 10 TO js - 10: axt(3, i) = DZ: NEXT i
    CASE 13
      mess$ = "DW=displaywhat is set to a"
      displaywas$ = "a"
    CASE 14
      mess$ = "be is set to DZ"
      be = DZ
    CASE 15
      mess$ = "array A is set to DZ between ja+10 and js-10"
      FOR i = ja + 10 TO js - 10: axt(1, i) = DZ: NEXT i
    CASE 16, 17
      mess$ = "s and external gradient will be changed, k2 position, DZ change pos."
    if k4 = 16 then imanipulate =0  ' the change can occur several times
    k2 = k2 + ky / 4 + RND * ky / 3
    IF k2 > js THEN k2 = k2 - js
    inv(12) = k2
      FOR i = 1 TO js:
      rho = ra: IF ca > 0 THEN rho = ca
     axt(0, i) = rho * (1 + 2 * kr / 100 * (RND - .5)) 'neue Quelldichte  s
     axt(8, i) = 1 + dy * COS(((i - k2) / (js)) * 6.283)
     axt(0, i) = axt(0, i) * axt(8, i)
     axt(4, i) = axt(0, i)
    NEXT i

    CASE ELSE
     mess$ = "No such modification is implemented, K4 must be 1... 15"
    CALL gtreturn(0, 0)
    END SELECT
    CALL cmessage(7, -1, 1, 15, 2, ibackcol, mess$, "OK")
  END IF
    GOTO simulation

   '------------------------- END OF LOOP 1 ----------------------------
nosuchequation:
    mess$ = "no such equation"
printmessage:
    CALL showprof(KD, 1, displaywas$, axt(), ja, js, icc)
    CALL switchscreen(3, iactive, igraph, KD, programname$, fdelay)
    CALL cmessage(4, -1, 1, 15, 4, ibackcol, mess$, "OK")
    GOTO parameter



makedraw:
   CALL showprof(KD, 1, displaywas$, axt(), ja, js, icc)
   IF ipcx > -1 THEN CALL PCXOUT(ipcx, ipcs)
EndSimulation:
  CALL showprof(KD, 99, displaywas$, axt(), ja, js, icc)
Finishplot:
    CALL switchscreen(3, iactive, igraph, KD, programname$, fdelay)
  iparam = 2: ipcontrol = 0
  GOTO parameter

CASE "M"
	CALL manipulationxt(pcontrol2$, kinsert)
   CALL showprof(KD, 1, displaywas$, axt(), ja, js, icc)
   IF ipcx > -1 THEN CALL PCXOUT(ipcx, ipcs)
    CALL switchscreen(2, iactive, igraph, KD, programname$, fdelay)
  GOTO Finishplot

CASE "A"
END SELECT
GOTO parameter
erroverflow:
mess$ = "an error occured, presumable an overflow, num. instability"
CALL cmessage(4, -1, 1, 15, 4, 0, mess$, resp$)
RESUME parameter




END  '==================  End of Main-program  ==========


olddecay:
      a = axt(1, i): b = axt(2, i): s = axt(0, i):
      olddecaydiffA = dra * a + DA * (al + axt(1, i + 1)): al = a
      olddecaydiffB = drb * b + db * (bl + axt(2, i + 1)): bl = b
       IF KN > 2 THEN
	C = axt(3, i): D = axt(4, i):
	olddecaydiffC = drc * C + DC * (cl + axt(3, i + 1)): cl = C
	olddecaydiffD = drd * D + DD * (dl + axt(4, i + 1)): dl = D
	IF KN > 4 THEN
	  e = axt(5, i): f = axt(6, i): g = axt(7, i):
	  olddecaydiffE = dre * e + DE * (el + axt(5, i + 1)): el = e
	  olddecaydiffF = drf * f + DF * (fl + axt(6, i + 1)): fl = f
	  olddecaydiffG = drg * g + dg * (gl + axt(7, i + 1)): gl = g
	  IF KN > 7 THEN
	  zh = axt(8, i):
	  olddecaydiffH = drh * zh + dh * (zhl + axt(8, i + 1)): zhl = zh
	END IF
	END IF
      END IF
RETURN

SUB hormone (ila, ahorm, ja, js)
SHARED flv(), axt()
rx = flv(ila, 2)
	  axt(ila, 1) = axt(ila, 1) * (1! - rx) + ahorm / (js - ja + 1)'C() represents hormone
	  FOR iic = ja TO js
	    axt(ila, iic) = axt(ila, 1)
	  NEXT iic
	  ahorm = 0
  EXIT SUB
END SUB

SUB oscillation (ila, C, D, olddecaydiffC, olddecaydiffD, ja, js)
'for homogenious oscillations
SHARED axt(), flv()
	cqf = C * C
	cq = flv(ila, 2) * D * (cqf / (1! + flv(ila, 4) * cqf) + flv(ila, 3))
	C = olddecaydiffC + cq
	D = olddecaydiffD - cq + flv(ila + 1, 3)
	FOR jic = 1 TO js
	axt(ila, jic) = C
	axt(ila + 1, jic) = D
	NEXT jic
  EXIT SUB
END SUB

