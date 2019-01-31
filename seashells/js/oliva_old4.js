var size = 400
var x1 = 0;
var end = false;
var dac;
var dc;
var imax = size;
//timev = 0, t1 , t2, t3;
var a = [];  // activator
var b = [];
var c = [];
var z = [];
//start:
var it = size;  //Number of lines to be calculated,
var i = 0;
var bf = 0, j = 0;
var tc = 0, af = 0, tbb = 0;
var aq3;
//          it*ip = Total number of iterations
var ip = 12;  //Number of iterations between the representations
var dx = 1;   //Cell width in pixels;   mit ip=12 ; kx=630 und dx=1 =>
var fieldOffset = 10
var kx = size + fieldOffset*2;  //Number of cells               Simulation in a large field
//var dx = 2;
//var kx = 315, ip = 6;
// var da = 0.015; //Diffusion of the activator
// var ta = 0.1;  //Decay rate of the activator
// var qa = 0.1;  //Basic production of the activator
// var ra = 0.25; //Saturation of autocatalysis
// var db = 0;    //Diffusion of the inhibitor
// var tb = 0.014; //Decay rate of the inhibitor
// var rb = 0.1;  //Michaelis-Menten constant of inhibition
// var sb = 0.1;  //Life of the hormone

var da = 0.01; //Diffusion of the activator
var ta = 0.02;  //Decay rate of the activator
var qa = 0.01;  //Basic production of the activator
var ra = 0.9; //Saturation of autocatalysis
var db = 0.4955;    //Diffusion of the inhibitor
var tb = 0.03; //Decay rate of the inhibitor
var rb = 0.1;  //Michaelis-Menten constant of inhibition
var sb = 0.1;  //Life of the hormone


function run(ctx){

// ----------- initial conditions --------------------------
for(i=1; i<=kx; i++) { //note: added <=
//   randomize();
   a[i] = 1;   //Activator, general initial concentration
   b[i] = 1;  //Inhibitor, general initial concentration
   z[i] = ta * (0.96 + 0.08 * Math.random()); //Small fluctuation of autocatalysis
} //Next i
var aho = 0.5; //Hormone concentration
i = 10;
for(j=1; j<20; j++) {  //initially activated cells, random
    a[i] = 1;
    i = i + 20 * ~~(Math.random()*10) + 1;
    if (i > imax) j = 22;
} //Next
dac = 1 - ta - 2 * da; // These factors are always constant
var dbc = 1 - tb - 2 * db; // needed, they are therefore calculated to start again

/*
Screen 12
continuo:
WINDOW (0, 0)-(640, 480)
Line (0, 0)-(640, 480), 1, BF //blue background
*/
var x1 = 0; //Starting position of the graphic
var a1, b1, af, bf;
//for (y1=15; y1<500; y1++) {
let allA = []  
  for (let y = 0; y < it; y++) {
    allA[y] = []
    for (var iprint=1; iprint<ip; iprint++) { // Beginning of the iteration
        //Rem -----  --- render opaque
        a1 = a[fieldOffset];     //a1 is the concentration of a cell to the left of the current
        b1 = a[fieldOffset];     //Cell. Since the same concentration, no diffusion through the edge
        a[kx + 1] = a[kx];   //Cell to the right of the right margin = margin cell
        b[kx + 1] = b[kx];   //i.e. no exchange between cells with the same concentration
        var bsa = 0;         //BSA => missing later sum of the activations of all cells

        // Rem ---------- reactions ------
        for (i=fieldOffset; i<kx; i++) { // i = current cell kx = right cell
          af = a[i]; //local activator concentration
          bf = b[i]; //local inhibitory concentration

          //Saturating self-reinforcement:
          aq3 = z[i] * af * af / (1 + ra * af * af) + 0.0000001;
          //Calculate the new activator and inhibitor concentrations in the cell i
          a[i] = af * dac + da * (a1 + a[i + 1]) + aq3 / (rb + bf);
          //1/BF = retardant effect of the inhibitor
          b[i] = bf * dbc + db * (b1 + b[i + 1]) + aq3; //new inhibitor concentration
          bsa = bsa + sb * af; //Sum of the activation, the hormone production
          a1 = af;     //actual concentration of the cell i is the concentration
          b1 = bf;     // left cell in computational cell i +1
          allA[y].push(a[i])
        } //Next i
        aho = aho * (1 - sb) + bsa / kx; //New hormone concentration, 1/kx = normalization
        tbb = tb / aho; //=effect. Life of the inhibitor Number of cells
//        tbb = 1;//'.5;
        dbc = 1 - 2 * db - tbb;
    }// Next iprint%
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

  
  canvas.width = it
  canvas.height = it
  var imageData = ctx.getImageData(0, 0, it, it);

  var buf = new ArrayBuffer(imageData.data.length);
  var buf8 = new Uint8ClampedArray(buf);
  var colorData = new Uint32Array(buf);

  for(var y = 0; y < it; ++y){
    for(var x = 0; x < it; ++x){
      var value = allA[y][x] * 255
      colorData[y * it + x] =
        (value << 24) | // alpha
        (0 << 16) | // blue
        (0 << 8) | // green
        0 // red
    }
  }
  imageData.data.set(buf8);
  ctx.putImageData(imageData, 0, 0);

}




