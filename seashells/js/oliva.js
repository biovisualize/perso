const size = 400
let dac
let dbc
let dc
const imax = size
//timev = 0, t1 , t2, t3;
const activator = []
const inhibitor = []
const sourceDensity = []
const numLines = size  //Number of lines to be calculated,
let x = 0
let y = 0
let i = 0
let previousActivatorConcentration
let previousInhibitorConcentration
let localActivatorConcentration
let localInhibitorConcentration
let bsa
let allA = []  
let lifeOfInhibitor = 0
var saturatingSelfReinforcement
const iterations = 6  //Number of iterations between the representations
let iteration = 0
var dx = 1   //Cell width in pixels
const fieldOffset = 10
var numCells = size + fieldOffset*2  //Number of cells
let hormoneConcentration = 0.5 //Hormone concentration
// Simulation in a large field

const activatorDiffusion = 0.02 //Diffusion of the activator
const activatorDecayRate = 0.08  //Decay rate of the activator
const activatorProduction = 0.1  //Basic production of the activator
const autocatalysisSaturation = 0.01 //Saturation of autocatalysis
const inhibitorDiffusion = 0.01 //Diffusion of the inhibitor
const inhibitorDecayRate = 0.014 //Decay rate of the inhibitor
const inhibitorConstant = 0.05  //Michaelis-Menten constant of inhibition
const hormoneLife = 0.11  //Life of the hormone


function run(ctx){

// ----------- initial conditions --------------------------
for(x=0; x<=numCells; x++) {
   activator[x] = 0.3   //Activator, general initial concentration
   inhibitor[x] = 1  //Inhibitor, general initial concentration
   sourceDensity[x] = activatorDecayRate * (0.96 + 0.08 * Math.random()) //Small fluctuation of autocatalysis
}
x = 10
for(i=1; i<20; i++) {  //initially activated cells, random
    activator[x] = 1
    x = x + 20 * ~~(Math.random()*10) + 1
    if (x > imax) i = 22
}
dac = 1 - activatorDecayRate - 2 * activatorDiffusion // These factors are always constant
dbc = 1 - inhibitorDecayRate - 2 * inhibitorDiffusion // needed, they are therefore calculated to start again


//for (y1=15; y1<500; y1++) {
  for (y = 0; y < numLines; y++) {
    allA[y] = []
    for (var iteration=1; iteration<iterations; iteration++) { // Beginning of the iteration
        //Rem -----  --- render opaque
        previousActivatorConcentration = activator[fieldOffset]     //previousActivatorConcentration is the concentration of activator cell to the left of the current
        previousInhibitorConcentration = activator[fieldOffset]     //Cell. Since the same concentration, no diffusion through the edge
        activator[numCells + 1] = activator[numCells]   //Cell to the right of the right margin = margin cell
        inhibitor[numCells + 1] = inhibitor[numCells]   //i.e. no exchange between cells with the same concentration
        bsa = 0  //BSA => missing later sum of the activations of all cells

        // Rem ---------- reactions ------
        for (x=fieldOffset; x<numCells; x++) { // i = current cell kx = right cell
          localActivatorConcentration = activator[x] //local activator concentration
          localInhibitorConcentration = inhibitor[x] //local inhibitory concentration

          saturatingSelfReinforcement = sourceDensity[x] * (localActivatorConcentration * localActivatorConcentration / (1 + autocatalysisSaturation * localActivatorConcentration * localActivatorConcentration) + 0.0000001)
          //Calculate the new activator and inhibitor concentrations in the cell i
          activator[x] = localActivatorConcentration * dac + activatorDiffusion * (previousActivatorConcentration + activator[x + 1]) + saturatingSelfReinforcement / (inhibitorConstant + localInhibitorConcentration)
          //1/localInhibitorConcentration = retardant effect of the inhibitor
          inhibitor[x] = localInhibitorConcentration * dbc + inhibitorDiffusion * (previousInhibitorConcentration + inhibitor[x + 1]) + saturatingSelfReinforcement //new inhibitor concentration
          bsa = bsa + hormoneLife * localActivatorConcentration //Sum of the activation, the hormone production
          previousActivatorConcentration = localActivatorConcentration //actual concentration of the cell i is the concentration
          previousInhibitorConcentration = localInhibitorConcentration // left cell in computational cell i +1
          allA[y].push(activator[x])
        } //Next i
        hormoneConcentration = hormoneConcentration * (1 - hormoneLife) + bsa / numCells //New hormone concentration, 1/kx = normalization
        lifeOfInhibitor = inhibitorDecayRate / hormoneConcentration //=effect. Life of the inhibitor Number of cells
//        lifeOfInhibitor = 1;//'.5;
        dbc = 1 - 2 * inhibitorDiffusion - lifeOfInhibitor
    }
    //Rem ----------------Plot -------------
    // kx = 800;

    // ctx.beginPath();
    // for (var ix=1; ix<kx; ix++) {   //Pigment is drawn when over a threshold
    //     if (a[ix] > 0.5){
    //     ctx.moveTo(x1 + dx * (ix - 1), y1);
    //     ctx.lineTo(x1 + dx * ix, y1);
    //    }
    // }
    // ctx.stroke();
    // ctx.closePath();
}

  
  canvas.width = numLines
  canvas.height = numLines
  var imageData = ctx.getImageData(0, 0, numLines, numLines)

  var buf = new ArrayBuffer(imageData.data.length)
  var buf8 = new Uint8ClampedArray(buf)
  var colorData = new Uint32Array(buf)

  for(var y = 0; y < numLines; ++y){
    for(var x = 0; x < numLines; ++x){
      var value = allA[y][x] * 255
      colorData[y * numLines + x] =
        (value << 24) | // alpha
        (0 << 16) | // blue
        (0 << 8) | // green
        0 // red
    }
  }
  imageData.data.set(buf8)
  ctx.putImageData(imageData, 0, 0)

}




