/*
* wrap around

*/

// converted from Pattern.bas https://www.eb.tuebingen.mpg.de/emeriti/hans-meinhardt/biuprom/
const width = 640,
  height = 480,
  ax = [],
  bx = [],
  cx = [],
  s = [],
  // Periodic Pattern
  KT = 60, //Number of displays
  KP = 20, // number of iterations between the displays
  // it*ip = number of iterations in total
  n = 40, //number of cells
  DA = .01, //Diffusion of the activator
  RA = .02, //Removal rate of the activator
  BA = .001, //Activator-independent activator production rate
  DB = .4, //Diffusion of the inhibitor
  RB = .03, //Removal rate of the inhibitor
  RC = 0, //only Activator-Inhibitor system, Substance c not in use
  // Scaling factors for display
  fa = 60,
  fb = 60,
  fc = 0,
  fs = 350 / RA,

  // igraph = 1,
  boundary = "tight"

// CASE "g": text$ = "Graded Distribution"
// KT = 50: n = 12: KP = 200
// DA = .004: RA = .0015: BA = .0001: DB = .4: RB = .0025: RC = 0
// igraph = 1: boundary$ = "tight": fa = 60: fb = 60: fc = 0
// CASE "o": text$ = "Pattern destabilizes itself: polar oscillations"
// KT = 200: KP = 500: n = 12: DA = .003: RA = .002: BA = .05
// DB = .4: RB = .003: DC = .001: RC = .0001:
// igraph = 1: fa = 40: fb = 50: fc = 50: boundary$ = "tight"
// CASE "h": text$ = "Pattern destabilizes itself: helical phyllotaxis"
// KT = 144: KP = 400: n = 40: DA = .005: RA = .004: BA = .05
// DB = .4: RB = .006: DC = .003: RC = .0002:
// igraph = 2: fa = 40: fb = 50: fc = 50' Graph-type and Scaling
// boundary$ = "cyclic"

// ----------- Initial conditions --------------------------

for (let i=0; i<=n; i++) {
  s.push(RA * (.99 + .02 * Math.random())) // "Source density" = Production of the activator, proportional to the decay rate +- 2% fluctuation
  ax.push(1) // general initial activator concentration
  bx.push(1) //general initial inhibitor concentration
  cx.push(0)
}

const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');
canvas.width = width
canvas.height = height

// continuo: if calculation is continued...
const idx = width / n // Pixel-size of a cell

for(let itot = 0; itot<=KT; itot++) { // total number of plots
  // ctx.fillStyle = "rgba(255, 255, 255, 0.02)"
  // ctx.fillRect(0, 0, width, height)
  // ctx.lineWidth = 0.1 + itot / KT * 2
  ctx.lineWidth = 0.5

  for(let iprint = 0; iprint<=KP; iprint++) { // calculations between plots

    // boundary
    // CASE "cyclic"
    let al = ax[n],
      bl = bx[n],
      cl = cx[n] // al is the concentration in the cell left of the actual cell. Left-most cell = last cell = cyclic
    ax[n + 1] = ax[1] // Concentration in a virtual cell on the right
    bx[n + 1] = bx[1] // equal concentration of the left-most cell
    cx[n + 1] = cx[1] // for all substances
    // CASE "tight"
    // let al = ax[1]
    //   bl = bx[1]
    //   cl = cx[1] // al is the concentration in the cell left of the actual cell. Left-most cell = virtual cell with the same concentration
    // ax[n + 1] = ax[n] // concentration in a virtual cell to the right of the
    // bx[n + 1] = bx[n] // right-most cell is equal to the concentration in the
    // cx[n + 1] = cx[n] // right-most cell, this for all substances

    // ---------- Reactions ------

    for (let i=0; i<=n; i++) { // i = current cell, n = right cell
      let a = ax[i], // local activator-concentration
        b = bx[i], // local inhibitor1-concentration
        c = cx[i] // local inhibitor2-concentration
      // Calculation of a new activator and inhibitor concentration in cell i
      let Diffactivator = DA * ((al - a) + (ax[i + 1] - a)) // Exchange with neighbor
      ax[i] = a + s[i] * (a * a + BA) / (b + c) - RA * a + Diffactivator
      bx[i] = b + s[i] * a * a - RB * b + DB * ((bl - b) + (bx[i + 1] - b))
      if (RC > 0) { // c only in oscillating patterns and phyllotaxis
        cx[i] = c + RC * a - RC * c + DC * ((cl - c) + (cx[i + 1] - c))
      }
      // The not yet changed concentrations are used as left cell concentrations in the subsequent cell
      al = a
      bl = b
      cl = c
    }
  }

  // ----------------Plot -------------

  // LINE (20, 45)-(620, 50), 1, BF
  let x1 = 0 // Position of the first rectangle

  ctx.beginPath()
  ctx.strokeStyle = "deepskyblue"

  let afl = ax[0] * fa
  ctx.moveTo(x1, afl)
  for (let i=0; i<=n; i++) {
    let x2 = x1 + idx
    afl = ax[i] * fa
    // LINE (x1, 51)-(x2, afl), 2, BF // green=activator

    ctx.lineTo(x2, afl)

    if (RC > 0) {
      let cfl = cx[i] * fc
      // LINE (x1, cfl)-(x2, cfl + 3), 13, BF // second inhibitor

      // ctx.strokeStyle = "orange"
      // ctx.moveTo(x1, cfl)
      // ctx.lineTo(x2, cfl + 3)
      // ctx.stroke()
    }
    x1 = x2
  }
  ctx.stroke()


  ctx.beginPath()
  ctx.strokeStyle = "orange"
  x1 = 0
  let bfl = bx[0] * fb
  ctx.moveTo(x1, bfl)
  for (let i=0; i<=n; i++) {
    let x2 = x1 + idx
    bfl = 51 + bx[i] * fb
    ctx.lineTo(x2, bfl)

    x1 = x2
    prevBfl = bfl
  }
  ctx.stroke()

  ctx.beginPath()
  ctx.strokeStyle = "violet"

  let sfl = s[0] * fs
  x1 = 0
  ctx.moveTo(x1, sfl)
  for (let i=0; i<=n; i++) {
    let x2 = x1 + idx
    sfl = s[i] * fs
    ctx.lineTo(x2, sfl)

    x1 = x2
  }
  ctx.stroke()

}
// DO UNTIL TIMER - t > .1: LOOP: t = TIMER'slow down if computer is too fast

