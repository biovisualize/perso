DEFDBL A-G
DEFDBL M-Z
DEFINT H-K

ON ERROR GOTO checkerror

' Model for the simulations FtsZ, MinD and MinE interactions.
' see Meinhardt and de Boer (2001)PNAS 98,14202-14207.
' from: H.Meinhardt: "Algorithmic beauty of sea shells"
' (Springer Verlag) (c) H.Meinhardt, Tübingen, Germany

' Program is written in BASIC. Convenient compilers are
' Freebasic for Windows or Unix, PowerBasic 3.5 for DOS
' Microsoft QB 4.5, or QBX


DIM F(100), D(100), E(100) 'membrane bound F, D, E
DIM Fdif(100), Ddif(100), Edif(100) ' Diffusible f, d, e
DIM rhoF(100), rhoD(100), rhoE(100)'for production rates with fluctuations
RANDOMIZE TIMER ' Set random generator. By different fluctuations,
'initiation of simulation can be slightly different
KT = 350'Number of displays
KP = 400'number of iterations between the displays
'KT*KP = number of iterations in total
kx = 15'Total number of spatial elements
idx = 540 / kx' Pixelsper spatial element
'constants used (first letter(s) D=diffusion, mu=removal, kappa couplings
DD = .02: muD = .002: sigmaD = .05: muDE = .0004'MinD, bound
DDdif = .2: muDdif = 0: sigmaDdif = .004'MinD, diffusible
DE = .0004: muE = .0005: sigmaE = .1: kappaDE = .5'MinE, bound
DEdif = .2: muEdif = .0002: sigmaEdif = .002'MinE, diffusible
DF = .002: muF = .004: sigmaF = .1: muFD = .002'FtsZ, non-diffusible
DFdif = .2: muFdif = .002: sigmaFdif = .006'FtsZ, diffusible

start:
REM ----------- Initial condition--------------------------
FOR I = 1 TO kx'Kx= Number of spatial elements
F(I) = 1: D(I) = 1: E(I) = 1'all zero
Fdif(I) = 1: Ddif(I) = 1: Edif(I) = 1
rhoF(I) = muF * (.995 + .01 * RND)'Fluctuation of the autocatalysis
rhoD(I) = muD * (.995 + .01 * RND)
rhoE(I) = muE * (.995 + .01 * RND)
NEXT I
SCREEN 12: WINDOW (1, 1)-(640, 480)' Setting graphic display
LINE (1, 1)-(640, 480), 15, BF'Plott' first white background
GOSUB plot
LOCATE 30, 1: PRINT " Initiation: homogenous except 1% fluctuation, press RETURN or f to include FtsZ";
t1 = TIMER:
DO UNTIL TIMER - t1 > .2: LOOP
LOCATE 30, 1: PRINT SPACE$(80);
LOCATE 30, 1: PRINT " MinD = green, MinE = red, FtsZ = blue; --> any key = stop";
continuo:
FOR ITOT = 0 TO KT'outer loop for iteration
FOR iprint = 1 TO KP' after finishing this loop: display
t1 = TIMER

'Boundary impermeable, virtual left (Fleft etc.) and right [F(kx+1) etc.]
'cell with the same concentration-->no diffusion
Fleft = F(1): Dleft = D(1): Eleft = E(1)
Fdifleft = Fdif(1): Ddifleft = Ddif(1): Edifleft = Edif(1)
F(kx + 1) = F(kx): D(kx + 1) = D(kx): E(kx + 1) = E(kx)
Fdif(kx + 1) = Fdif(kx): Ddif(kx + 1) = Ddif(kx): Edif(kx + 1) = Edif(kx)

REM ---------- Reactions------
FOR I = 1 TO kx' i = actual cell, local concentration are saved
Flocal = F(I): Dlocal = D(I): Elocal = E(I)
Fdiflocal = Fdif(I): Ddiflocal = Ddif(I): Ediflocal = Edif(I)'
' 1. Calculating FtsZ (=F) and diffus. FtsZ (=Fdif)
prodF = rhoF(I) * Fdiflocal * (Flocal * Flocal + sigmaF)
F(I) = prodF + Flocal * (1 - muF - muFD * Dlocal) + DF * (Fleft + F(I + 1) - 2 * Flocal)
Fdif(I) = Fdiflocal * (1 - muFdif) + sigmaFdif - prodF + DFdif * (Fdifleft + Fdif(I + 1) - 2 * Fdiflocal)
Fleft = Flocal: Fdifleft = Fdiflocal'present concentration ->
' 2. Calculating MinD (=D) and diffus. MinD (=Ddif)
prodD = rhoD(I) * Ddiflocal * (Dlocal * Dlocal + sigmaD)
D(I) = prodD + Dlocal * (1 - muD - muDE * Elocal) + DD * (Dleft + D(I + 1) - 2 * Dlocal)
Ddif(I) = Ddiflocal * (1 - muDdif) + sigmaDdif - prodD + DDdif * (Ddifleft + Ddif(I + 1) - 2 * Ddiflocal)
Dleft = Dlocal: Ddifleft = Ddiflocal' present concentration->left-cell concentration
' 3. Calculating MinE (=>E) and diffus. MinE (=>Edif)
prodE = rhoE(I) * Ediflocal * Dlocal / (1 + kappaED * Dlocal * Dlocal) * (Elocal * Elocal + sigmaE) / (1 + kappaE * Elocal * Elocal)
E(I) = prodE + E(I) * (1 - muE) + DE * (Eleft + E(I + 1) - 2 * Elocal)
Edif(I) = Ediflocal * (1 - muEdif) + sigmaEdif - prodE + DEdif * (Edifleft + Edif(I + 1) - 2 * Ediflocal)
Eleft = Elocal: Edifleft = Ediflocal' present concentration->left-cell concentration
NEXT I
NEXT iprint
GOSUB plot
IF INKEY$ > "" THEN EXIT FOR
NEXT ITOT

LOCATE 30, 1: PRINT "c = continue; s = a new start, f start with FtsZ; all other keys = END";
DO: aa$ = LCASE$(INKEY$): IF aa$ > "" THEN EXIT DO
LOOP
IF aa$ = "c"  GOTO continuo
if aa$ = "f" then a$="f": goto start
IF aa$ = "s" then a$ = "": GOTO start
LOCATE 30, 1: PRINT space$(80);: t1 = TIMER:
LOCATE 30, 1: PRINT "A model for the MinD/MinE oscillation in E.Coli, (C) Hans Meinhardt    [RETURN]";
DO: a$ = INKEY$:
IF a$ > ""  THEN EXIT DO
if TIMER - t1 > 2 then exit do
loop
end


plot:
t1 = TIMER:
DO UNTIL TIMER - t1 > .05: LOOP
ix1 = 40
LINE (ix1, 45)-(ix1 + kx * idx, 50), 0, BF'Plott' first black baseline
FplotD = 50: FplotE = 20: FplotF = 40'scaling plott
FOR I = 1 TO kx
ix2 = ix1 + idx
IF a$ = "f" THEN LINE (ix1, 51)-(ix2, 51 + FplotF * F(I)), 1, BF'Blue bar for FtsZ
LINE (ix1, 51)-(ix2, 51 + FplotE * E(I)), 12, BF'MinE= red
LINE (ix1, 51)-(ix2, 51 + FplotD * D(I)), 2, BF'MinD= green
IF a$ = "f" THEN LINE (ix1, 51 + FplotF * F(I))-(ix2, 50 + FplotF * F(I)), 1, BF 'a bar for hidden F
LINE (ix1, 51 + FplotE * E(I))-(ix2, 49 + FplotE * E(I)), 12, BF'a bar for hidden E
Fmax = 51 + FplotE * E(I)

IF a$ = "f" AND 51 + FplotF * F(I) > Fmax THEN Fmax = 51 + FplotF * F(I)
IF 51 + FplotD * D(I) > Fmax THEN Fmax = 51 + FplotD * D(I)
LINE (ix1, 480)-(ix2, Fmax), 15, BF'top white = erase previous plot
ix1 = ix2
NEXT I
RETURN

''-------------------------------------------------------- END -----------------------------------


checkerror:
IF ERR > 0 THEN PRINT "There was an error, code "; ERR, : SLEEP
RESUME NEXT

