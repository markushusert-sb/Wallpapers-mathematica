BeginPackage["Patterns`"]
Needs["Settings`"]
formssnub::usage="formssnub[phi] returns shapes (2x triangles, 1x quadrilateral) in fundamental region of snub square of angle phi between skew square and its bounding square"
graficsnub::usage="graficsnub[phi] returns grafic object of fundamental region of snub square of angle phi"
graficsnubface::usage="graficsnubspace[x] where x ist list of list of points plots snubspace indicated by forms in x"
Begin["`Private`"]
nsin[x_] := Sin[x]/(Sin[x] + Cos[x])
ncos[x_] := Cos[x]/(Sin[x] + Cos[x])
solveintersectsnub1[
  phi_] := {Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi])),
  Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi]))}
solveintersectsnub2[
  phi_] := {Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi])),
  1 - Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi]))}
pointsinnersnub[phi_] := {
  {0,1-nsin[phi]},
  solveintersectsnub1[phi],
  solveintersectsnub2[phi]
  }
(* Transformation obtained by:
B={{B1,B2},{B3,B4}};
a={a1,a2};
s=Solve[Join[MapThread[Equal,{a+B.{0,0},{1/2,0}}],MapThread[Equal,{a+\
B.{0,1},{0,1/2}}],MapThread[Equal,{a+B.{1/2,1/2},{1/2,1/2}}]],{a1,a2,\
B1,B2,B3,B4}];
B/.s;
a/.s;*)
pointsoutersnub[phi_:Pi/6] := 
  # - (# . {1, 1} - 1/2) & /@ Simplify[{{1/2, -(1/2)}, {1/2, 1/2}} . # + {1/2, 0} & /@ 
    pointsinnersnub[phi]];
(*# - (# . {1, 1} - 1/2) & /@ serves to mirror to the right fundamental region*)
formssnub[phi_:Pi/6]:={{{0,1/2},#[[1]],#[[3]]},{#[[1]],#[[2]],{0,0},#[[3]]},{#[[1]],#[[2]],{1/2,0}}}&[pointsoutersnub[phi]]
Options[graficsnubface]={colorsq->LightRed,colortr->LightBlue}
graficsnubface[x:{vecpat..},OptionsPattern[]]:=(Graphics[{EdgeForm[Thickness[Small]],Length[x]/.{3->OptionValue@colortr,4->OptionValue@colorsq},Polygon[x]}])
graficsnub[phi_:Pi/6]:=graficsnubface[#]&/@ formssnub[phi]
End[]
EndPackage[]
