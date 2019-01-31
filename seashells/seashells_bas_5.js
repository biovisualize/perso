// converted from Pattern.bas https://www.eb.tuebingen.mpg.de/emeriti/hans-meinhardt/biuprom/
let width = 640,
  height = 480,
  ax = [],
  bx = [],
  cx = [],
  s = [],

  // test
  KT = 60, //Number of displays
  KP = 20, // number of iterations between the displays
  n = 40, //number of cells
  // parameters
  DA = .04, //Diffusion of the activator
  RA = .02, //Removal rate of the activator
  BA = .001, //Activator-independent activator production rate
  DB = .4955, //Diffusion of the inhibitor
  RB = .03, //Removal rate of the inhibitor
  RC = 0, //only Activator-Inhibitor system, Substance c not in use
  // Scaling factors for display
  fa = 60,
  fb = 60,
  fc = 0,
  fs = 350 / RA,
  boundary = "tight"

  // // Periodic Pattern
  // KT = 60, //Number of displays
  // KP = 20, // number of iterations between the displays
  // n = 40, //number of cells
  // DA = .01, //Diffusion of the activator
  // RA = .02, //Removal rate of the activator
  // BA = .001, //Activator-independent activator production rate
  // DB = .4, //Diffusion of the inhibitor
  // RB = .03, //Removal rate of the inhibitor
  // RC = 0, //only Activator-Inhibitor system, Substance c not in use
  // // Scaling factors for display
  // fa = 60,
  // fb = 60,
  // fc = 0,
  // fs = 350 / RA,
  // boundary = "tight"

  // Graded Distribution
  // KT = 50
  // KP = 200
  // n = 12
  // DA = .004
  // RA = .0015
  // BA = .0001
  // DB = .4
  // RB = .0025
  // RC = 0
  // boundary = "tight"
  // fa = 60
  // fb = 60
  // fc = 0

  // Pattern destabilizes itself: polar oscillations
  // KT = 200
  // KP = 500
  // n = 12
  // DA = .003
  // RA = .002
  // BA = .05
  // DB = .4
  // RB = .003
  // DC = .001
  // RC = .0001
  // fa = 40
  // fb = 50
  // fc = 50
  // boundary = "tight"

  // Pattern destabilizes itself: helical phyllotaxis
  // KT = 144
  // KP = 400
  // n = 40
  // DA = .005
  // RA = .004
  // BA = .05
  // DB = .4
  // RB = .006
  // DC = .003
  // RC = .0002
  // fa = 40
  // fb = 50
  // fc = 50
  // boundary = "cyclic"

// ----------- Initial conditions --------------------------

for (let i=0; i<=n; i++) {
  s.push(RA * (.99 + .02 * Math.random())) // "Source density" = Production of the activator, proportional to the decay rate +- 2% fluctuation
  ax.push(1) // general initial activator concentration
  bx.push(1) //general initial inhibitor concentration
  cx.push(0)
}

const canvas = document.querySelector('canvas#graph');
const ctx = canvas.getContext('2d');
canvas.width = width
canvas.height = height

// continuo: if calculation is continued...
const idx = width / n // Pixel-size of a cell

for(let itot = 0; itot<=KT; itot++) { // total number of plots
  ctx.fillStyle = "rgba(255, 255, 255, 0.03)"
  ctx.fillRect(0, 0, width, height)
  ctx.lineWidth = 0.1 + itot / KT * 1.5
  // ctx.lineWidth = 1

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

  ctx.beginPath()
  ctx.strokeStyle = "deepskyblue"
  let x1 = 0 // Position of the first rectangle
  let afl = ax[0] * fa
  ctx.moveTo(x1, afl)
  for (let i=0; i<=n; i++) {
    let x2 = x1 + idx
    afl = ax[i] * fa
    ctx.lineTo(x2, afl) // activator
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
    bfl = bx[i] * fb
    ctx.lineTo(x2, bfl) // inhibitor
    x1 = x2
  }
  ctx.stroke()

  // ctx.beginPath()
  // ctx.strokeStyle = "violet"
  // x1 = 0
  // let sfl = s[0] * fs
  // ctx.moveTo(x1, sfl)
  // for (let i=0; i<=n; i++) {
  //   let x2 = x1 + idx
  //   sfl = s[i] * fs
  //   ctx.lineTo(x2, sfl) // source density

  //   x1 = x2
  // }
  // ctx.stroke()

  if (RC > 0) {
    ctx.beginPath()
    ctx.strokeStyle = "lime"
    x1 = 0
    let cfl = cx[0] * fc
    ctx.moveTo(x1, cfl)
    for (let i=0; i<=n; i++) {
      let x2 = x1 + idx
      cfl = cx[i] * fc
      ctx.lineTo(x2, cfl) // second inhibitor
      x1 = x2
    }
    ctx.stroke()
  }

}
