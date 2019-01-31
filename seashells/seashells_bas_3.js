// simulation programs/dos-programs/dsp

const kt = 45,
kp = 12,
kx = 1,
ky = 20,
kd = 1,
ki = 1,
ke = 21,
kr = 3,
kn = 2,
kg = 0
k1 = 2,
k2 = 2,
k3 = 0,
k4 = 0,
dx = 16.0000,
dy = 0.0000,
dz = 12.0000
da = 0.0100,
ra = 0.0500,
ba = 0.0100,
sa = 0.0000,
ca = 0.0000,
aa = 1.5000,
ga = 1.5000,
db = 0.4000,
rb = 0.0800,
bb = 0.0000,
sb = 0.0000,
cb = 0.0000,
ab = 1.5000,
gb = 1.5000

let   dra = 1 - ra - 2 * da
  drb = 1 - rb - 2 * db
  drc = 1 - rc - 2 * dc

  // dbcc = drb
  // ahorm = 0
  // icountot = 0: imanipulate = 0: iendm = 0

// kt = inv(1): kp = inv(2): kx = inv(3): ky = inv(4): kd = inv(5)
// ki = inv(6): ke = inv(7): kr = inv(8): kn = inv(9): kg = inv(10)
// k1 = inv(11): k2 = inv(12): k3 = inv(13): k4 = inv(14)
// k5 = inv(15): k6 = inv(16): k7 = inv(17): k8 = inv(18)
// k9 = inv(19): k10 = inv(20)

// i = 1: da = flv(i, 1): ra = flv(i, 2): ba = flv(i, 3): sa = flv(i, 4): ca = flv(i, 5)
// aa = flv(i, 6): ga = flv(i, 7): qa = flv(i, 8):  ta = flv(i, 10): ua = flv(i, 10):

// i = 2: db = flv(i, 1): rb = flv(i, 2): bb = flv(i, 3): sb = flv(i, 4): cb = flv(i, 5)
// ab = flv(i, 6): gb = flv(i, 7): qb = flv(i, 8):  tb = flv(i, 10): ub = flv(i, 10):

const allA = [],
allB = [],
allS = []

// todo: initialize
// '---------------------    initial conditions  --------------
 //  ja = kx: js = ky: FOR il = 1 TO KN: lax(il) = 1: lay(il) = 1: NEXT il
 //  afa = 1: bfb = 1
 //  ' IF ke = 2 OR ke = 5 THEN
 //  '     afa = bb / ta  'Steady state for the depletion mechanims
 //  '     bfb = bb / (ta * afa * afa)
 //  ' END IF
 // imxl2 = imxl / 2: IF ky > imxl2 THEN imxl2 = imxl
 //  FOR i = 1 TO imxl2
 //    IF PCONTROL$ <> "II" THEN
 //      rho = ra: IF ca > 0 THEN rho = ca
 //      axt(0, i) = rho * (1! + 2 * KR / 100 * (RND - .5))'      * EXP(ad * i)'source density
 //      arandomxt(i) = 1! + 2 * KR / 100 * (RND - .5)
 //    END IF
 //    'RND: randomnumbers between 0.0 and 1.0 result in +-IR% fluctuation
 //    axt(1, i) = afa * gA '-- initial condition: A is set to AA in all
 //    axt(2, i) = bfb * gB '-- cells, B, C, and D to AB, AC, and AD
 //    axt(3, i) = gC '-- respectively. In the following, certain
 //    axt(4, i) = gD '-- cells might be given other values for A
 //    axt(5, i) = ge
 //    axt(6, i) = gf
 //    axt(7, i) = gg
 //    axt(8, i) = 1: IF KN > 7 THEN axt(8, i) = gh
 //  NEXT i
 //    jx2 = (ky + kx) / 2

let al = 0, // todo: wrap around
bl = 0

for(let i = ja; i > js; i++) {
  let a = allA[i]
  let b = allB[i]
  let s = allS[i]

  const olddecaydiffa = dra * a + da * (al + allA[i + 1])
  al = a
  const olddecaydiffb = drb * b + db * (bl + allB[i + 1])
  bl = b

  allA[i] = olddecaydiffa + s * (a * a / b + ba)
  allB[i] = olddecaydiffb + s * a * a + bb
}


// case 8, 9  '------------------------------------pixel-------------------------------------
//   dxt = (kx - kax + 1) * dx
//   ddy = dx
// '''  ico = ico(il)
// if dy > 0 and kdisplay = 9 then ddy = dy
//   dyt = (ky - kay + 1) * ddy
//   if xo = 0 then
//   if icc = 1 or kg > 0 then
//   xoa = 320 - (kx / 2 + kax - 1) * dx
//   jam = kax
//   end if
//   xa = xoa + dx * (kax - jam)
//   else
//   xa = xo + dx * (kax - 1)
//   end if
//   call linep(ihp, xa - 1, yo - 1, xa + dxt, yo + dyt + 1, 1, iwil, ivorcol, 1)
//   call linep(ihp, xa - 2, yo - 2, xa + dxt + 1, yo + dyt + 2, 1, iwil, ivorcol, 1)
//   for ix = kax to kx
//     for iy = kay to ky
//       icoly = icol
//       if kdisplay = 9 or kdisplay = 19 then
//       icoly = iy + icol - 1: if icoly > 14 then icoly = icoly - 14
//       icoly = ibm(icoly)
//       end if
//       x1 = xa + (ix - kax) * dx
//       y1 = yo + (iy - kay) * ddy
//       x2 = x1 + dx
//       y2 = y1 + ddy
//       n = 8 * axy(il, ix, iy) * ff
//       if n < krot then n = 0
//   if n > 0 then call linep(ihp, x1, y1, x2, y2, 3, iwil, icoly, n)
//     next iy
//   next ix