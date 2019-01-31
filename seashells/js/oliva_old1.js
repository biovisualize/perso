var x1 =0;
var end = false;
var dac;
var dc;
var imax = 640, timev = 0, t1 , t2, t3;
var a = [];
var b = [];
var c = [];
var z = [];
//start:
var it = 480;  //Number of lines to be calculated,
var i = 0;
var bf = 0, j = 0;
var tc = 0, af = 0, tbb = 0;
var aq3;
//          it*ip = Total number of iterations
var ip = 12;  //Number of iterations between the representations
var dx = 1;   //Cell width in pixels;   mit ip=12 ; kx=630 und dx=1 =>
var kx = 630;  //Number of cells               Simulation in a large field
//var dx = 2;
//var kx = 315, ip = 6;
var da = 0.015; //Diffusion of the activator
var ta = 0.1;  //Decay rate of the activator
var qa = 0.1;  //Basic production of the activator
var ra = 0.25; //Saturation of autocatalysis
var db = 0;    //Diffusion of the inhibitor
var tb = 0.014; //Decay rate of the inhibitor
var rb = 0.1;  //Michaelis-Menten constant of inhibition
var sb = 0.1;  //Life of the hormone

function run(){

//TRect Quelle, Ziel;


//__fastcall TForm1::TForm1(TComponent* Owner)
//	: TForm(Owner)
//{
//}
//---------------------------------------------------------------------------
// void __fastcall Run(void);
//void __fastcall TForm1::Button2Click(TObject *Sender)
//{
//Close ();
end = true;
//Application->Terminate();
//}
//---------------------------------------------------------------------------
//void __fastcall TForm1::Button1Click(TObject *Sender)
//
//{
//Refresh();

// ----------- initial conditions --------------------------
t1 = timev;
for(i=1; i<=kx; i++) { //note: added <=
//   randomize();
   a[i] = 0;   //Activator, general initial concentration
   b[i] = 0.1;  //Inhibitor, general initial concentration
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
var dbc = 1 - tb - 2 * db; // needed, they are
//var dcc = 1 - tc - 2 * dc; // note: dc is undefined
var dcc = 1 - tc - 2; // therefore calculated to start again
var dbcc = dbc;
dcc = dcc * 1;

/*
Screen 12
continuo:
WINDOW (0, 0)-(640, 480)
Line (0, 0)-(640, 480), 1, BF //blue background
*/
var ic = 0;
var x1 = 50, y1 = 15; //Starting position of the graphic
//for (var itot=0; itot>-1; itot++) { //note: never stops

    for (var iprint=1; iprint<ip; iprint++) { // Beginning of the iteration
        //Rem -----  --- render opaque
        var a1 = a[1];     //a1 is the concentration of a cell to the left of the current
        var b1 = a[1];     //Cell. Since the same concentration, no diffusion through the edge
        a[kx + 1] = a[kx];   //Cell to the right of the right margin = margin cell
        b[kx + 1] = b[kx];   //i.e. no exchange between cells with the same concentration
        var bsa = 0;         //BSA => missing later sum of the activations of all cells

        // Rem ---------- reactions ------
        for (i=1; i<kx; i++) { // i = current cell kx = right cell
           var af = a[i]; //local activator concentration
           var bf = b[i]; //local inhibitory concentration

            //Saturating self-reinforcement:
            aq3 = z[i] * af * af / (1 + ra * af * af) + 0.0000001;
            //Calculate the new activator and inhibitor concentrations in the cell i
            a[i] = af * dac + da * (a1 + a[i + 1]) + aq3 / (rb + bf);
            // 1/BF = retardant effect of the inhibitor
            b[i] = bf * dbcc + db * (b1 + b[i + 1]) + aq3; //new inhibitor concentration
            bsa = bsa + sb * af; //Sum of the activation, the hormone production
            a1 = af;     //actual concentration of the cell i is the concentration
            b1 = bf;     // left cell in computational cell i +1
        } //Next i
        //kx = 630;
        //aho =  0.5;
        // aho = 0.05;
        aho = aho * (1 - sb) + bsa / kx; //New hormone concentration, 1/kx = normalization
        tbb = tb / aho; //=effect. Life of the inhibitor Number of cells
        // tbb = 1;//'.5;
        dbcc = 1 - 2 * db - tbb;
    }// Next iprint%
    //Rem ----------------Plot -------------
    kx = 630;
//    if (ic == 0){
//        Application->ProcessMessages(); //Do events
//    }
//    ic++;
//    if (ic==20) ic = 0;
    if (y1<500) { //Next deeper plot a pixel row
        y1 = y1 + 1;
    }
    else {
//        Quelle = Rect(x1,15,x1+640,500);
//        Ziel = Rect(x1,16,x1+640,501);
//        Canvas->CopyRect(Quelle, Canvas, Ziel);
        console.log(x1);
    }

    if (end == true){
//        goto Exit;
    }

////    Canvas->Pen->Color = clWhite;
////    Canvas->MoveTo (x1, y1);
////    Canvas->LineTo(x1 + dx * kx, y1); //white background
//
   for (var ix=1; ix<kx; ix++) {   //Pigment is drawn when over a threshold

       if (a[ix] > 0.5){
//       console.log('color', ix, y1, dx);
//       console.log('moveto', x1 + dx * (ix - 1), y1);
//       console.log('lineto', x1 + dx * ix, y1);
////           Canvas->Pen->Color = (ix * y1 * dx);
////           Canvas->Pen->Color = clMaroon;
////           Canvas->MoveTo (x1 + dx * (ix - 1), y1);
////           Canvas->LineTo (x1 + dx * ix, y1);
       } //END IF
   } // Next ix%
//}// Next itot
/*
Loop
If a$ = "c" Then GoTo continuo
If a$ = "s" Then GoTo start
*/

//_____
t2 = timev;
t3 = t2 - t1;
//Form1->Caption = (String(t3));
//Exit:
//}

 //---------------------------------------------------------------------------
//void __fastcall TForm1::Button3Click(TObject *Sender)
//{
//Quelle = Rect(x1,15,x1+640,500);
//SaveToFile("C:\bild.bmp");
//}
//---------------------------------------------------------------------------
//void __fastcall TForm1::Timer1Timer(TObject *Sender)
//{
//Close();
//timev = timev + 1;
//}
//---------------------------------------------------------------------------

}




