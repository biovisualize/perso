/*
TODO:
* make render using computeConcentrations as a generator
* use renderSlicer
* better resolution
* find how to reproduce oliva
* find a name for substanceC
* "boundary" parameter
*/

// converted from Pattern.bas https://www.eb.tuebingen.mpg.de/emeriti/hans-meinhardt/biuprom/

// ----------- Parameters --------------------------

const presets = [
  {
    numExecutions: 60,
    numIterations: 6,
    numCells: 40,
    // parameters
    activatorDiffusion: .02,
    activatorRemovalRate: .08,
    activatorProductionRate: .1,
    inhibitorDiffusion: .01,
    inhibitorRemovalRate: .014,
    substanceCRemovalRate: 0,
    boundary: "tight",
    // Scaling factors for display
    fa: 700,
    fb: 350,
    fc: 0,
    // fs: 350 / activatorRemovalRate
    fs: 350 / .02
  },
  {
    numExecutions: 60,
    numIterations: 20,
    numCells: 40,
    // parameters
    activatorDiffusion: .04,
    activatorRemovalRate: .02,
    activatorProductionRate: .001,
    inhibitorDiffusion: .4955,
    inhibitorRemovalRate: .03,
    substanceCRemovalRate: 0,
    boundary: "tight",
    // Scaling factors for display
    fa: 60,
    fb: 60,
    fc: 0,
    // fs: 350 / activatorRemovalRate
    fs: 350 / .02
  },
  { // Periodic Pattern
    numExecutions: 60, // Number of displays
    numIterations: 20, // number of iterations between the displays
    numCells: 40, // number of cells
    activatorDiffusion: .01, // Diffusion of the activator
    activatorRemovalRate: .02, // Removal rate of the activator
    activatorProductionRate: .001, // Activator-independent activator production rate
    inhibitorDiffusion: .4, // Diffusion of the inhibitor
    inhibitorRemovalRate: .03, // Removal rate of the inhibitor
    substanceCRemovalRate: 0, // only Activator-Inhibitor system, Substance c not in use
    boundary: "tight",
    // Scaling factors for display
    fa: 60,
    fb: 60,
    fc: 0,
    // fs: 350 / activatorRemovalRate
  },
  {// Graded Distribution
    numExecutions: 50,
    numIterations: 200,
    numCells: 12,
    activatorDiffusion: .004,
    activatorRemovalRate: .0015,
    activatorProductionRate: .0001,
    inhibitorDiffusion: .4,
    inhibitorRemovalRate: .0025,
    substanceCRemovalRate: 0,
    boundary: "tight",
    fa: 60,
    fb: 60,
    fc: 0
  },
  {  // Pattern destabilizes itself: polar oscillations
    numExecutions: 200,
    numIterations: 500,
    numCells: 12,
    activatorDiffusion: .003,
    activatorRemovalRate: .002,
    activatorProductionRate: .05,
    inhibitorDiffusion: .4,
    inhibitorRemovalRate: .003,
    substanceCDiffusion: .001,
    substanceCRemovalRate: .0001,
    boundary: "tight",
    fa: 40,
    fb: 50,
    fc: 50
  },
  {  // Pattern destabilizes itself: helical phyllotaxis
    numExecutions: 144,
    numIterations: 400,
    numCells: 40,
    activatorDiffusion: .005,
    activatorRemovalRate: .004,
    activatorProductionRate: .05,
    inhibitorDiffusion: .4,
    inhibitorRemovalRate: .006,
    substanceCDiffusion: .003,
    substanceCRemovalRate: .0002,
    boundary: "cyclic",
    fa: 40,
    fb: 50,
    fc: 50
  }
]

function* computeConcentrations ({
  numExecutions,
  numIterations,
  numCells,
  activatorDiffusion,
  activatorRemovalRate,
  activatorProductionRate,
  inhibitorDiffusion,
  inhibitorRemovalRate,
  substanceCDiffusion,
  substanceCRemovalRate,
  boundary
},{activatorX, ihibitorX, substanceCX, sourceDensity}) {
  for(let i = 0; i<=numExecutions; i++) { // total number of plots
    let activator = 0,
      inhibitor = 0,
      substanceC = 0


    for(let j = 0; j<=numIterations; j++) { // calculations between plots

      // boundary
      // CASE "cyclic"
      let activatorLeftCell = activatorX[numCells],
        inhibitorLeftCell = ihibitorX[numCells],
        substanceCLeftCell = substanceCX[numCells] // activatorLeftCell is the concentration in the cell left of the actual cell. Left-most cell = last cell = cyclic
      activatorX[numCells + 1] = activatorX[1] // Concentration in a virtual cell on the right
      ihibitorX[numCells + 1] = ihibitorX[1] // equal concentration of the left-most cell
      substanceCX[numCells + 1] = substanceCX[1] // for all substances
      // CASE "tight"
      // let activatorLeftCell = activatorX[1]
      //   inhibitorLeftCell = ihibitorX[1]
      //   substanceCLeftCell = substanceCX[1] // activatorLeftCell is the concentration in the cell left of the actual cell. Left-most cell = virtual cell with the same concentration
      // activatorX[numCells + 1] = activatorX[numCells] // concentration in a virtual cell to the right of the
      // ihibitorX[numCells + 1] = ihibitorX[numCells] // right-most cell is equal to the concentration in the
      // substanceCX[numCells + 1] = substanceCX[numCells] // right-most cell, this for all substances

      // ---------- Reactions ------

      for (let k=0; k<=numCells; k++) { // k = current cell
        let activatorConcentration = activatorX[k], // local activator-concentration
          inhibitorConcentration = ihibitorX[k], // local inhibitor1-concentration
          substanceCConcentration = substanceCX[k] // local inhibitor2-concentration
        // Calculation of a new activator and inhibitor concentration in cell i
        let Diffactivator = activatorDiffusion * ((activatorLeftCell - activatorConcentration) + (activatorX[k + 1] - activatorConcentration)) // Exchange with neighbor
        activator = activatorConcentration + sourceDensity[k] * (activatorConcentration * activatorConcentration + activatorProductionRate) / (inhibitorConcentration + substanceCConcentration) - activatorRemovalRate * activatorConcentration + Diffactivator
        inhibitor = inhibitorConcentration + sourceDensity[k] * activatorConcentration * activatorConcentration - inhibitorRemovalRate * inhibitorConcentration + inhibitorDiffusion * ((inhibitorLeftCell - inhibitorConcentration) + (ihibitorX[k + 1] - inhibitorConcentration))
        if (substanceCRemovalRate > 0) { // c only in oscillating patterns and phyllotactivatorXis
          substanceC = substanceCConcentration + substanceCRemovalRate * activatorConcentration - substanceCRemovalRate * substanceCConcentration + substanceCDiffusion * ((substanceCLeftCell - substanceCConcentration) + (substanceCX[k + 1] - substanceCConcentration))
          substanceCX[k] = substanceC
        }
        // The not yet changed concentrations are used as left cell concentrations in the subsequent cell
        activatorLeftCell = activatorConcentration
        inhibitorLeftCell = inhibitorConcentration
        substanceCLeftCell = substanceCConcentration
        activatorX[k] = activator
        ihibitorX[k] = inhibitor
      }
    }

    yield {
      i,
      activator,
      inhibitor,
      substanceC
    }
  }
  return true
}

function simulate (config) {
  const {numCells, numExecutions, activatorRemovalRate} = config

  const cellWidth = width / numCells // Pixel-size of a cell
  const cellHeight = height / numExecutions 

  const activatorX = [],
    ihibitorX = [],
    substanceCX = [],
    sourceDensity = []

  // ----------- Initial conditions --------------------------

  for (let k=0; k<=numCells; k++) {
    const isRandomlyActivated = ~~(Math.random()*1.5) > 0.5
    sourceDensity[k] = activatorRemovalRate * (.99 + .02 * Math.random()) // "Source density" = Production of the activator, proportional to the decay rate +- 2% fluctuation
    activatorX[k] = isRandomlyActivated ? 1 : 0.3 // general initial activator concentration
    ihibitorX[k] = 1 //general initial inhibitor concentration
    substanceCX[k] = 0
  }

  const concentrationGenerator = computeConcentrations(config, {activatorX, ihibitorX, substanceCX, sourceDensity})

  console.log(concentrationGenerator.next().value)
  console.log(concentrationGenerator.next().value)
  console.log(concentrationGenerator.next().value)
  console.log(concentrationGenerator.next().value)
  console.log(concentrationGenerator.next().value)
  console.log(concentrationGenerator.next().value)
}

function renderGraph ({
  i,
  numExecutions,
  numCells,
  cellWidth,
  activatorX,
  ihibitorX,
  sourceDensity,
  substanceCX,
  fa,
  fb,
  fc,
  fs
}) {
  // const normalizedExecutionNumber = i / numExecutions
  // ctx.strokeOpacity = normalizedExecutionNumber
  ctx.fillStyle = `rgba(255, 255, 255, ${1 / numExecutions})`
  ctx.fillRect(0, 0, width, height)
  // ctx.lineWidth = 0.1 + i / normalizedExecutionNumber

  ctx.beginPath()
  ctx.strokeStyle = "deepskyblue"
  let x1 = 0 // Position of the first rectangle
  let afl = activatorX[0] * fa
  ctx.moveTo(x1, afl)
  for (let k=0; k<=numCells; k++) {
    let x2 = x1 + cellWidth
    afl = activatorX[k] * fa
    ctx.lineTo(x2, afl) // activator
    x1 = x2
  }
  ctx.stroke()

  ctx.beginPath()
  ctx.strokeStyle = "orange"
  x1 = 0
  let bfl = ihibitorX[0] * fb
  ctx.moveTo(x1, bfl)
  for (let k=0; k<=numCells; k++) {
    let x2 = x1 + cellWidth
    bfl = ihibitorX[k] * fb
    ctx.lineTo(x2, bfl) // inhibitor
    x1 = x2
  }
  ctx.stroke()

  // ctx.beginPath()
  // ctx.strokeStyle = "violet"
  // x1 = 0
  // let sfl = sourceDensity[0] * fs
  // ctx.moveTo(x1, sfl)
  // for (let k=0; k<=n; k++) {
  //   let x2 = x1 + cellWidth
  //   sfl = sourceDensity[k] * fs
  //   ctx.lineTo(x2, sfl) // source density

  //   x1 = x2
  // }
  // ctx.stroke()

  // if (substanceCRemovalRate > 0) {
    ctx.beginPath()
    ctx.strokeStyle = "lime"
    x1 = 0
    let cfl = substanceCX[0] * fc
    ctx.moveTo(x1, cfl)
    for (let k=0; k<=numCells; k++) {
      let x2 = x1 + cellWidth
      cfl = substanceCX[k] * fc
      ctx.lineTo(x2, cfl) // second inhibitor
      x1 = x2
    }
    ctx.stroke()
  // }
}

function renderTexture ({
  i,
  numCells,
  cellWidth,
  cellHeight,
  activatorX,
  ihibitorX,
  sourceDensity,
  substanceCX,
  fa,
  fb
}) {
  let afl = activatorX[0] * fa
  let bfl = ihibitorX[0] * fb
  for (let k=0; k<=numCells; k++) {
    afl = activatorX[k] * fa
    bfl = ihibitorX[k] * fb
    const concentration = ((afl - bfl) + 75) / 120
    // console.log(afl, bfl, afl - bfl, afl > bfl, concentration)
    textureCtx.fillStyle = `rgba(0, 0, 0, ${concentration})`
    // textureCtx.fillStyle = `rgba(0, 0, 0, ${+(afl > bfl)})`
    textureCtx.fillRect(k * cellWidth, i * cellHeight, cellWidth, cellHeight) // concentration
  }
}

const width = 640,
  height = 480

const canvas = document.querySelector('canvas#graph');
const ctx = canvas.getContext('2d');
canvas.width = width
canvas.height = height

const textureCanvas = document.querySelector('canvas#texture');
const textureCtx = textureCanvas.getContext('2d');
textureCanvas.width = width
textureCanvas.height = height

simulate(presets[2])
    // renderGraph({
    //   i,
    //   numExecutions,
    //   numCells,
    //   cellWidth,
    //   activatorX,
    //   ihibitorX,
    //   sourceDensity,
    //   substanceCX,
    //   fa,
    //   fb,
    //   fc,
    //   fs
    // })

    // renderTexture({
    //   i,
    //   numCells,
    //   cellWidth,
    //   cellHeight,
    //   activatorX,
    //   ihibitorX,
    //   sourceDensity,
    //   substanceCX,
    //   fa,
    //   fb
    // })
