 '------------------------------------------------------------
' A simple program to simulate biological pattern formation
' (C) Hans Meinhardt, MPI Tuebingen
DEFDBL A-G
DEFDBL O-Z
DEFINT H-N
ON ERROR GOTO checkerror

' A simple program to simulate biological pattern formation
' (C) Hans Meinhardt, MPI Tuebingen

' The program is written in BASIC. Convenient compilers are
' Freebasic for Windows or Unix, PowerBasic 3.5 for DOS
' Microsoft QB 4.5, or QBX

' i = 1 ... n= Number of cell
imax = 640: DIM ax(imax), bx(imax), cx(imax), s(imax)
simtyp$ = "p"' First simulation: periodic pattern
start:
SELECT CASE simtyp$
CASE "p": text$ = "Periodic Pattern"
KT = 100 'Number of displays
KP = 20 ' 'number of iterations between the displays
'it*ip = number of iterations in total
n = 40' number of cells
DA = .01' Diffusion of the activator
RA = .02' Removal rate of the activator
BA = .001 ' Activator-independent activator production rate
DB = .4' Diffusion of the inhibitor
RB = .03' Removal rate of the inhibitor
RC = 0' only Activator-Inhibitor system, Substance c not in use
fa = 60: fb = 60: fc = 0' Scaling factors for display
igraph = 1: boundary$ = "tight"
CASE "g": text$ = "Graded Distribution"
KT = 50: n = 12: KP = 200
DA = .004: RA = .0015: BA = .0001: DB = .4: RB = .0025: RC = 0
igraph = 1: boundary$ = "tight": fa = 60: fb = 60: fc = 0
CASE "o": text$ = "Pattern destabilizes itself: polar oscillations"
KT = 200: KP = 500: n = 12: DA = .003: RA = .002: BA = .05
DB = .4: RB = .003: DC = .001: RC = .0001:
igraph = 1: fa = 40: fb = 50: fc = 50: boundary$ = "tight"
CASE "h": text$ = "Pattern destabilizes itself: helical phyllotaxis"
KT = 144: KP = 400: n = 40: DA = .005: RA = .004: BA = .05
DB = .4: RB = .006: DC = .003: RC = .0002:
igraph = 2: fa = 40: fb = 50: fc = 50' Graph-type and Scaling
boundary$ = "cyclic"
CASE ELSE: GOTO TheEND ' each other key: the end
END SELECT

REM ----------- Initial conditions --------------------------
FOR i = 1 TO n
s(i) = RA * (.99 + .02 * RND)'"Source density" = Production of the
'activator, proportional to the decay rate +- 2% fluctuation
ax(i) = 1'general initial activator concentration
bx(i) = 1'general initial inhibitor concentration
cx(i) = 0: NEXT
SCREEN 12: t = TIMER: CLS 'Initialization of graphic and timing
WINDOW (1, 1)-(640, 480) ' coordinate system
LINE (1, 1)-(640, 480), 15, BF 'background white
LOCATE 30, 1:
PRINT "green = Activator, red = Inhibitor, blue = Source density; any key: stop";
LOCATE 1, (80 - LEN(text$)) / 2: PRINT text$

continuo: ' if calculation is continued...
DeltaWi = 6.28 / n: yzyl = 30: radius = 100
perspective = .15' for cylinder of phyllotaxis
idx = 600 / n: fs = 350 / RA 'Pixel-size of a cell
FOR itot = 0 TO KT' total numberplots
FOR iprint% = 1 TO KP' Calculations between plots
SELECT CASE boundary$
CASE "cyclic"
al = ax(n): bl = bx(n): cl = cx(n)'al is the concentration in the cell
'left of the actual cell. Left-most cell = last cell = cyclic
ax(n + 1) = ax(1)' Concentration in a virtual cell on the right
bx(n + 1) = bx(1)' equal concentration of the left-most cell
cx(n + 1) = cx(1)' for all substances
CASE "tight"
al = ax(1): bl = bx(1): cl = cx(1)'al is the concentration in the cell
'left of the actual cell. Left-most cell = virtual cell with the same
'concentration
ax(n + 1) = ax(n)' concentration in a virtual cell to the right of the
bx(n + 1) = bx(n)' right-most cell is equal to the concentration in the
cx(n + 1) = cx(n)' right-most cell, this for all substances
END SELECT
REM ---------- Reactions ------

FOR i = 1 TO n' i = current cell, n = right cell
a = ax(i) 'local activator-concentration
b = bx(i) 'local inhibitor1-concentration
c = cx(i) 'local inhibitor2-concentration
'Calculation of a new activator and inhibitor concentration in cell i
Diffactivator = DA * ((al - a) + (ax(i + 1) - a)) 'Exchange with neighbor
ax(i) = a + s(i) * (a * a + BA) / (b + c) - RA * a + Diffactivator
bx(i) = b + s(i) * a * a - RB * b + DB * ((bl - b) + (bx(i + 1) - b))
IF RC > 0 THEN ' c only in oscillating patterns and phyllotaxis
cx(i) = c + RC * a - RC * c + DC * ((cl - c) + (cx(i + 1) - c))
END IF
al = a: bl = b: cl = c'The not yet changed concentrations
'are used as left cell concentrations in the subsequent cell
NEXT i
NEXT iprint%
REM ----------------Plot -------------

SELECT CASE igraph
CASE 1' Each cell is a rectangle
LINE (20, 45)-(620, 50), 1, BF
x1 = 20' Position of the first rectangle
FOR i = 1 TO n
x2 = x1 + idx
afl = 51 + ax(i) * fa: LINE (x1, 51)-(x2, afl), 2, BF'green=activator
LINE (x1, afl)-(x2, 450), 15, BF 'remaining part white: erase old plot
bfl = 51 + bx(i) * fb: LINE (x1, bfl)-(x2, bfl + 10), 12, BF
sfl = 51 + s(i) * fs: LINE (x1, sfl)-(x2, sfl + 3), 1, BF
IF RC > 0 THEN
cfl = 51 + cx(i) * fc: LINE (x1, cfl)-(x2, cfl + 3), 13, BF 'second inhibitor
END IF
x1 = x2: NEXT
DO UNTIL TIMER - t > .1: LOOP: t = TIMER'slow down if computer is too fast
CASE 2 ' transparent cylinder for phyllotaxis
LOCATE 30, 1: PRINT "black/grey = high activator concentration, y-coordinate=time, any key=stop";
wi1 = 0: n21 = n / 2 + 1 'front or back of the cylinder
yzyl = yzyl + 3'Next plot one pixel row higher
FOR ix% = 1 TO n'Black if above a threshold
wi2 = wi1 + DeltaWi
ico = 0: IF ix% < n21 THEN ico = 7 'Cells are gray if seen from inside
IF ax(ix%) > 1 THEN
CIRCLE (320, yzyl), radius, ico, wi1, wi2, perspective'Several lines
CIRCLE (320, yzyl + 1), radius, ico, wi1, wi2, perspective'on top of
CIRCLE (320, yzyl + 2), radius, ico, wi1, wi2, perspective'each other
END IF
wi1 = wi2: NEXT ix%
IF icount = 0 THEN CIRCLE (320, yzyl), radius, 10, , , perspective
icount = icount + 1 'green circle for clarity
IF icount = 3 THEN icount = 0
END SELECT
IF INKEY$ > "" THEN EXIT FOR 'any key -> exit
NEXT itot
LOCATE 30, 1: PRINT "c = continue; s = start again; n = new type of ";
PRINT "simulation, ... other keys = end";
a$ = "": DO UNTIL a$ > "": a$ = INKEY$: LOOP' wait for key
SELECT CASE a$
CASE "c", "w": GOTO continuo
CASE "s": GOTO start
CASE "n": LOCATE 30, 1:
PRINT "p = Periodic; g= gradient, o = oscillating polar, h = helical,other key = end";
simtyp$ = "": DO UNTIL simtyp$ > "": simtyp$ = INKEY$: LOOP' wait for key
GOTO start
END SELECT
TheEND:
LOCATE 30, 1: t = TIMER
PRINT ".... a program for simulation of biol. pattern formation;(c) Hans Meinhardt";
DO UNTIL TIMER - t > 3: LOOP
END

'-------------------End of the Program-------------------------------

checkerror:
IF ERR > 0 THEN PRINT "There was an error, code "; ERR, : SLEEP
RESUME NEXT

