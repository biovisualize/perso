const KT = 45,
KP = 12,
KX = 1,
KY = 20,
KD = 1,
KI = 1,
KE = 21,
KR = 3,
KN = 2,
KG = 0
K1 = 2,
K2 = 2,
K3 = 0,
K4 = 0,
DX = 16.0000,
DY = 0.0000,
DZ = 12.0000
DA = 0.0100,
RA = 0.0500,
BA = 0.0100,
SA = 0.0000,
CA = 0.0000,
AA = 1.5000,
GA = 1.5000,
DB = 0.4000,
RB = 0.0800,
BB = 0.0000,
SB = 0.0000,
CB = 0.0000,
AB = 1.5000,
GB = 1.5000


let   dra = 1! - ra - 2! * DA
  drb = 1! - rB - 2! * db
  drc = 1! - rc - 2! * DC
  drd = 1! - rd - 2! * DD
  dre = 1! - re - 2! * DE
  drf = 1! - rf - 2! * DF
  drg = 1! - rg - 2! * dg
  drh = 1! - rh - 2! * dh
  DBCC = drb: ahorm = 0
   icountot = 0: imanipulate = 0: iendm = 0

// kt = inv(1): kp = inv(2): kx = inv(3): ky = inv(4): kd = inv(5)
// ki = inv(6): ke = inv(7): kr = inv(8): kn = inv(9): kg = inv(10)
// k1 = inv(11): k2 = inv(12): k3 = inv(13): k4 = inv(14)
// k5 = inv(15): k6 = inv(16): k7 = inv(17): k8 = inv(18)
// k9 = inv(19): k10 = inv(20)

// i = 1: da = flv(i, 1): ra = flv(i, 2): ba = flv(i, 3): sa = flv(i, 4): ca = flv(i, 5)
// aa = flv(i, 6): ga = flv(i, 7): qa = flv(i, 8):  ta = flv(i, 10): ua = flv(i, 10):

// i = 2: db = flv(i, 1): rb = flv(i, 2): bb = flv(i, 3): sb = flv(i, 4): cb = flv(i, 5)
// ab = flv(i, 6): gb = flv(i, 7): qb = flv(i, 8):  tb = flv(i, 10): ub = flv(i, 10):

let al = 0, // todo: wrap around
bl = 0


for(let i = ja; i > js; i++) {
  let A = axt(1, i)
  let B = axt(2, i)
  let s = axt(0, i)

  const olddecaydiffA = dra * A + da * (al + axt(1, i + 1))
  al = A
  const olddecaydiffB = drb * B + db * (bl + axt(2, i + 1))
  bl = B

  axt(1, i) = olddecaydiffA + s * (a * a / b + ba)
  axt(2, i) = olddecaydiffB + s * a * a + bb
}


// CASE 8, 9  '------------------------------------pixel-------------------------------------
//   dxt = (kx - kax + 1) * dx
//   ddy = dx
// '''  ico = ico(il)
// IF dy > 0 AND kdisplay = 9 THEN ddy = dy
//   dyt = (ky - kay + 1) * ddy
//   IF xo = 0 THEN
//   IF icc = 1 OR kg > 0 THEN
//   xoa = 320 - (kx / 2 + kax - 1) * dx
//   jam = kax
//   END IF
//   xa = xoa + dx * (kax - jam)
//   ELSE
//   xa = xo + dx * (kax - 1)
//   END IF
//   CALL linep(ihp, xa - 1, yo - 1, xa + dxt, yo + dyt + 1, 1, iwil, ivorcol, 1)
//   CALL linep(ihp, xa - 2, yo - 2, xa + dxt + 1, yo + dyt + 2, 1, iwil, ivorcol, 1)
//   FOR ix = kax TO kx
//     FOR iy = kay TO ky
//       icoly = icol
//       IF kdisplay = 9 OR kdisplay = 19 THEN
//       icoly = iy + icol - 1: IF icoly > 14 THEN icoly = icoly - 14
//       icoly = ibm(icoly)
//       END IF
//       x1 = xa + (ix - kax) * dx
//       y1 = yo + (iy - kay) * ddy
//       x2 = x1 + dx
//       y2 = y1 + ddy
//       n = 8 * axy(il, ix, iy) * ff
//       IF n < krot THEN n = 0
//   IF n > 0 THEN CALL linep(ihp, x1, y1, x2, y2, 3, iwil, icoly, n)
//     NEXT iy
//   NEXT ix