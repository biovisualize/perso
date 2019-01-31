const size = 500 // max x
// diffuseA = .2, // diffusion of activator
// baseA = .5, // basic activator production
// reduceA = .1, // rate of activator removal
// diffuseB = .8, // diffusion of inhibitor
// baseB = .01, // basic inhibitor production
// reduceB = .1 // rate of inibhitor production

diffuseA = .01, // diffusion of activator
baseA = .001, // basic activator production
reduceA = .02, // rate of activator removal
diffuseB = .49, // diffusion of inhibitor
baseB = 0, // basic inhibitor production
reduceB = .03 // rate of inibhitor production

let a = [], // activator at time t
b = [], // inhibitor at time t
s = [], // source density, ability to perform autocatalysis
dA = [], // activators
dB = [] // inhibitors

// initialize dA, dB, maybe constant s
let rndWalk = 0
for(let x = 0; x < size; x++) {
  rndWalk = Math.min(1, rndWalk + (Math.random() * 2 - 1))
  a[x] = rndWalk
  b[x] = 1
  s[x] = Math.random() * 0.05 + 0.95 // slight variations
}

// border wraparound

// function d2 (arr) {
//   let res = []
//   for(let x = 1; x < size-1; x++) {
//     res[x] = arr[x-1] - 2.0 * arr[x] + arr[x+1]
//   }
//   return res
// }

function derivative (arr, x, prev) {
  // console.log(1, arr)
  // console.log(2, x, arr[x + 1], arr[x - 1], (arr[x + 1] - arr[x - 1]) / 2)
  // return arr[x-1] - 2.0 * arr[x] + arr[x+1]

  // return ((arr[x + 1] - arr[x - 1]) / 2) || 0
  return ((arr[x - 1] + arr[x + 1])) || 0
  // return ((prev + arr[x + 1])) || 0
}


let maxA = 0,
  minA = Number.MAX_VALUE,
  maxB = 0,
  minB = Number.MAX_VALUE,
  prevA = 0,
  prevB = 0
for(let y = 0; y < size; y++) {
  dA[y] = []
  dB[y] = []
  for(let x = 0; x < size; x++) {
    const aX = a[x]
    const bX = b[x]
    const sX = s[x]

    const derivativeAX = derivative(dA[Math.max(y-1, 0)], x)
    const derivative2BX = derivative(dB[Math.max(y-1, 0)], x)
    // const derivativeAX = derivative(a, x, prevA)
    // const derivative2BX = derivative(b, x, prevB)
    // console.log(3, sX, aX, bX, baseA, reduceA, diffuseA, derivativeAX)

    const activationValue = sX * (aX * aX / bX + baseA) - reduceA*aX + diffuseA * derivativeAX
    const inhibitorValue = sX * (aX * aX) - reduceB*bX + diffuseB * derivative2BX + baseB

    dA[y].push(activationValue)
    dB[y].push(inhibitorValue)
    prevA = activationValue
    prevB = inhibitorValue
    
    maxA = Math.max(maxA, activationValue)
    if (activationValue > 0) {
      minA = Math.min(minA, activationValue)
    }
    maxB = Math.max(maxB, inhibitorValue)
    if (inhibitorValue > 0) {
      minB = Math.min(minB, inhibitorValue)
    }
  }
  // a = dA[y]
  // b = dB[y]
}

// swap aX[x] = dA[x]

var canvas = document.querySelector('canvas');
canvas.width = 630;
canvas.height = 400;
var ctx = canvas.getContext('2d');
var imageData = ctx.getImageData(0, 0, size, size);

var buf = new ArrayBuffer(imageData.data.length);
var buf8 = new Uint8ClampedArray(buf);
var colorData = new Uint32Array(buf);

for(var y = 0; y < size; ++y){
  for(var x = 0; x < size; ++x){
    var valueA = (dA[y][x] - minA) / (maxA - minA) * 255
    var valueB = (dB[y][x] - minB) / (maxB - minB) * 255
    let value = (valueA + valueB) / 2
    colorData[y * size + x] =
      (value << 24) | // alpha
      (0 << 16) | // blue
      (0 << 8) | // green
      0 // red
  }
}
imageData.data.set(buf8);
ctx.putImageData(imageData, 0, 0);
