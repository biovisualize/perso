DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DEFINT H-N

SUB boxup (idummy)
LOCATE 1, 1
PRINT "boxup does not work in QBX";
END SUB

DEFSNG H-N
SUB ColorPrint (text$, row%, col%, fc%, bc%) STATIC
'SHARED igraph
IF fc% < 8 THEN
COLOR fc% + 8
ELSE
COLOR fc%
END IF
LOCATE row%, col%: PRINT text$;
COLOR 15
END SUB

DEFINT H-N
SUB defbox (x1, y1, x2, y2, idummy%, i)
'this is a dummy substitue for a routine not available in QBX
END SUB

SUB lightgray (iwhatcol, ilevelr, ilevelg, ilevelb)
PALETTE iwhatcol, 65536 * ilevelb + 256 * ilevelg + ilevelr
END SUB


SUB packpcx (resp$, idummy, jdummy, idxr, idyu)
'  LINE (0, 479)-(idxr, 479 - idyu), 14, B
  resp$ = " no pcx, this is a qbx file!"
  CALL cmessage(3, -1, 1, 1, 15, ibackcol, resp$, "OK")
END SUB

SUB screencheck (igraph, lastline, lastrow, ivgashift, ifree)
ifree = 3 '= QB
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
  WINDOW (0, 0)-(640, 480)
'  WINDOW (0, 0)-(1000, 750)
  LINE (0, 0)-(1500, 1250), 15, BF
  lastrow = 80: ivgashift = lastline - 25
END SUB

