COMMON SHARED  axy(), axt(), agrad(), programloc$, curdrive$, aprodir$, apardir$, pdir$, pdri1$, pardir$
COMMON SHARED  pcontrol$, pcontrol2$, comment$, icheckerror, openfilename$, irfilenr$, ilineparam
COMMON SHARED  readdatafile$, writedatafile$, programname$, ipcontrol, filemessage$, iendm
COMMON SHARED  basfilename$, lpt$, ptype$, title$, iwohin, dirtemp$, filename$, modified$
'for graphic
COMMON SHARED  mempos(), icolm(), ibm(), a2(), yml(), ansp(), msiluet()
COMMON SHARED  p4, px, pyx, pdx, pdyx, pyy, radius
COMMON SHARED  lax(), lx(), lay(), ly(), gxa(), gfa(), aam(), kax, ja, js, kay, jy, icc
COMMON SHARED  gya(), ico(), itypus(), iwi(), msiluetmax, igt, iparam, iactive
COMMON SHARED  itot, tvv$, tv$(), agfa(), label$(), displaywas$, displaywastmp$
COMMON SHARED  ihp, dxp, dyp, fkasten, d, per, krot, ivgashift, ianz, ireaddata
COMMON SHARED  ivorcol, ibackcol, igraph, istelz, iletztk, ihintr, igtdelay
COMMON SHARED  fdelay, iwidthm, exposure, ifree, lastline, lastrow, idosgraph
'for parameter
COMMON SHARED  fparnam$(), iparnam$(), flv(), inv() 'for parameter
COMMON SHARED  iplotsubst, icmdlin, ixmax, iymax, persp, imx, imy, ilm
COMMON SHARED  diffmax, imxl, imyl
COMMON SHARED  kt, kp, kx, ky, kd, ki, ke, kr, kn, kg, ks
COMMON SHARED  k1, k2, k3, k4, k5, k6, k7, k8, k9, k0, dx, dy, dz
COMMON SHARED  da, ra, ba, sa, ca, aa, ga, qa, ta, ua, db, rb, bb, sb, cb, ab, gb, qb, tb, ub
COMMON SHARED  dc, rc, bc, sc, cc, ac, gc, qc, tc, uc, dd, rd, bd, sd, cd, ad, gd, qd, td, ud
COMMON SHARED  de, re, be, se, ce, ae, ge, qe, te, ue, df, rf, bf, sf, cf, af, gf, qf, tf, uf
COMMON SHARED  dg, rg, bg, sg, cg, ag, gg, qg, tg, ug, dh, rh, bh, sh, ch, ah, gh, qh, th, uh
COMMON SHARED  di, ri, bi, si, ci, ai, gi, qi, ti, ui, dj, rj, bj, sj, cj, aj, gj, qj, tj, uj
ifree = 1

SUB lightgray (iwhatcol, ilevelr, ilevelg, ilevelb)
PALETTE iwhatcol, 65536 * ilevelb + 256 * ilevelg + ilevelr
END SUB

SUB ColorPrint (text$, irow, icolumn, iforegroundc, ibackgroundc) STATIC
COLOR iforegroundc, ibackgroundc
locate irow, icolumn
Print text$;
'xxcolpr
END SUB



SUB packpcx (resp$, idummy, jdummy, idxr, idyu)  ' this is only a dummy in FreeBasic
'  LINE (0, 479)-(idxr, 479 - idyu), 14, B
  resp$ = " no pcx, this is a qbx or FreeBASIC file!"
  CALL cmessage(3, -1, 1, 1, 15, ibackcol, resp$, "OK")
END SUB

SUB SCREENCHECK ''(igraph, lastline, lastrow, ivgashift, ifreebas)
fdelay = 0: if idosgraph = 9 then fdelay = .04
SELECT CASE igraph:
'CASE 9:igraph= 12: lastline = 30: lastrow = 80: iscreen= 12 'EGA no longer in use
'CASE 1,12: lastline = 30: lastrow = 80: iscreen= 12
CASE 1: lastline = 30: lastrow = 80: iscreen= 12
CASE 9, 12, 2,19: lastline = 37: lastrow = 100: iscreen= 19  'Standard
CASE 3, 20: lastline = 48: lastrow = 128: iscreen= 20
CASE 4, 21: lastline = 62: lastrow = 158: iscreen= 21

CASE 5: lastline = 37: lastrow = 100:iscreen= 19
CASE 9, 12, 6, 20: lastline = 48: lastrow = 128: iscreen= 20
CASE 7, 21: lastline = 62: lastrow = 158: iscreen= 21
CASE ELSE
LOCATE 1, 1: beep: PRINT igraph; " sorry, no such screen, will be corrected to 12"
lastline = 30: lastrow = 80: iscreen= 12
END SELECT

SCREEN iscreen, ,2
__SCREENSET 1, 1
color 1,15
SELECT CASE igraph
CASE 5: WINDOW (0, 0)-(800, 600)
CASE 6: WINDOW (0, 0)-(1024, 768)
CASE 7: WINDOW (0, 0)-(1280, 1024)
CASE ELSE:
WINDOW (0, 0)-(648, 480)
END SELECT
Line (0, 0)-(1640, 1480), 15,BF
ivgashift = lastline - 25
END SUB
