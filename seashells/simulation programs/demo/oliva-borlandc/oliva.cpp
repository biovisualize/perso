//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "oliva.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
int x1 =0;
bool end = false;
float dac;
float dc;
int imax = 640, timev = 0, t1 , t2, t3;
float a[1200];
float b[1200];
float c[1200];
float z[1200];
//start:
float it = 480;  //Zahl der zu rechnenden Zeilen,
int i = 0;
float bf = 0, j = 0;
float tc = 0, af = 0, tbb = 0;
float aq3;
//          it*ip = Zahl der Iterationen insgesamt
float ip = 12;  //Zahl der Iterationen zwischen den Darstellungen
float dx = 1;   //Zellenbreite in Pixel;   mit ip=12 ; kx=630 und dx=1 =>
int kx = 630;  //Zahl der Zellen               Simulation in einem gr”áeren Feld
//int dx = 2;
//int kx = 315, ip = 6;
float da = 0.015; //Diffusion des Aktivators
float ta = 0.1;  //Zerfallsrate des Aktivators
float qa = 0.1;  //Grundproduktion des Aktivators
float ra = 0.25; //S„ttigung der Autokatalyse
float db = 0;    //Diffusion des Inhibitors
float tb = 0.014; //Zerfallsrate des Inhibitors
float rb = 0.1;  //Michaelis-Menten Konstante der Inhibition
float sb = 0.1;  //Lebensdauer des Hormons
TRect Quelle, Ziel;


__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
 void __fastcall Run(void);
void __fastcall TForm1::Button2Click(TObject *Sender)
{
//Close ();
end = true;
Application->Terminate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)

{
Refresh();

 // _______________

// ----------- Anfangsbedingungen --------------------------
t1 = timev;
for (i=1; i<kx; i++) {
   randomize ();
   a[i] = 0;   //Aktivator, generelle Anfangskonzentration
   b[i] = 0.1;  //Inhibitor, generelle Anfangskonzentration
   z[i] = ta * (0.96 + 0.08 * random(1)); //Kleine Fluktuation der Autokatalyse
} //Next i
float aho = 0.5; //Hormon-Konzentration
i = 10; for(j=1; j<20; j++) {  //anfangs aktivierte Zellen, zuf„llig
a[i] = 1; i = i + 20 * random(10) + 1; if (i > imax) j = 22;
} //Next
dac = 1 - ta - 2 * da; // Diese konstanten Faktoren werden immer wieder
float dbc = 1 - tb - 2 * db; // gebraucht, sie werden
float dcc = 1 - tc - 2 * dc; // deshalb zu Anfang einmal berechnet
float dbcc = dbc;
dcc = dcc * 1;
/*
Screen 12
continuo:
WINDOW (0, 0)-(640, 480)
Line (0, 0)-(640, 480), 1, BF //Hintergrund blau
*/
int ic = 0;
float x1 = 50, y1 = 15; //Anfangsposition der Graphik
for (float itot=0; itot>-1; itot++) {


for (int iprint=1; iprint<ip; iprint++) { // Beginn der Iteration
//Rem -----  --- R„nder undurchl„ssing
float a1 = a[1];     //a1 ist die Konzentration einer Zelle links von der aktuellen
float b1 = a[1];     //Zelle. Da Konzentration gleich, keine Diffusion durch Rand.
a[kx + 1] = a[kx];   //Zelle rechts neben rechtem Rand = Randzelle.
b[kx + 1] = b[kx];   //d.h. kein Austausch zwischen Zellen mit gleicher Konz.
float bsa = 0;         //BSA => enh„lt sp„ter Summe der Aktivierungen aller Zellen
// Rem ---------- Reaktionen ------
for (i=1; i<kx; i++) { // i = laufende Zelle, kx = rechte Zelle
   float af = a[i]; //lokale Aktivator-Konzentration
   float bf = b[i]; //lokale Inhibitor-Konzentration

 //S„ttigende Selbstverst„rkung:
aq3 = z[i] * af * af / (1 + ra * af * af) + 0.0000001;
//Ausrechnen der neuen Aktivator- und Inhibitorkonzentrationen in der Zelle i
   a[i] = af * dac + da * (a1 + a[i + 1]) + aq3 / (rb + bf);
   // 1/BF=Hemmender Einfluá des Inhibitors
   b[i] = bf * dbcc + db * (b1 + b[i + 1]) + aq3; //neue Inhibitor- Konz.
   bsa = bsa + sb * af; //Summe der Aktivation, fr Hormon-Produktion
   a1 = af;     //aktuelle Konzentration der Zelle i ist Konzentration der
   b1 = bf;     // linker Zelle bei Berechnung Zelle i+1
   } //Next i
   //kx = 630;
   //aho =  0.5;
 // aho = 0.05;
   aho = aho * (1 - sb) + bsa / kx; //Neue Hormonkonzentration , 1/kx=Normierung
  tbb = tb / aho; //=effect. Lebensdauer des Inhibitors    Gesamtzahl der Zellen
        // tbb = 1;//'.5;
   dbcc = 1 - 2 * db - tbb;
   }// Next iprint%
   //Rem ----------------Plott -------------
   kx = 630;
     if (ic == 0) Application->ProcessMessages(); //Doevents
ic++; if (ic==20) ic = 0;
   if (y1<500) {y1 = y1 + 1;} //N„chster Plot eine Pixelreihe tiefer


   else {
      Quelle = Rect(x1,15,x1+640,500);
        Ziel = Rect(x1,16,x1+640,501);
      Canvas->CopyRect(Quelle, Canvas, Ziel);
      }


   if (end == true) goto Exit;
   Canvas->Pen->Color = clWhite;
   Canvas->MoveTo (x1, y1);
   Canvas->LineTo(x1 + dx * kx, y1); //Hintergrund weiá


   for (int ix=1; ix<kx; ix++) {   //Pigment wird gezeichnet wenn a ber einer Schwelle

   if (a[ix] > 0.5)
   {
   Canvas->Pen->Color = (ix * y1 * dx);
   Canvas->Pen->Color = clMaroon;
   Canvas->MoveTo (x1 + dx * (ix - 1), y1);
   Canvas->LineTo (x1 + dx * ix, y1);
   } //END IF
   } // Next ix%
   }// Next itot
   /*
   Loop
If a$ = "c" Then GoTo continuo
If a$ = "s" Then GoTo start
*/


//_____
t2 = timev;
t3 = t2 - t1;
Form1->Caption = (String(t3));
Exit:
}



 //---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
Quelle = Rect(x1,15,x1+640,500);
//SaveToFile("C:\bild.bmp");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
//Close();
timev = timev + 1;
}
//---------------------------------------------------------------------------




