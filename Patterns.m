BeginPackage["Patterns`"]
Needs["Settings`"]
formssnub::usage="formssnub[phi] returns shapes (2x triangles, 1x quadrilateral) in fundamental region of snub square of angle phi between skew square and its bounding square"
formssnubwf::usage="formssnubwf[phi] returns wireframe in fundamental region of snub square of angle phi"
formssnubp4m::usage="formssnub[phi] returns shapes (2x triangles, 1x quadrilateral) in fundamental region of p4m for snub square of angle phi between skew square and its bounding square"
formsmaster::usage="formsmaster[form:String,param] selects suitable form based on argument form"
graficsnub::usage="graficsnub[phi] returns grafic object of fundamental region of snub square of angle phi"
graficsnubface::usage="graficsnubspace[x] where x ist list of list of points plots snubspace indicated by forms in x"
graficswireframe::usage="graficsnubspace[x,color] where x ist list of list of points and color is an optional color"
meshgensoutersnub::usage="meshgensoutersnub[phi] returns corners of 2 quad-shells who construct a sunb-square under action of p4g"
Begin["`Private`"]
nsin[x_] := Sin[x]/(Sin[x] + Cos[x])
ncos[x_] := Cos[x]/(Sin[x] + Cos[x])
solveintersectsnub1[
  phi_] := {Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi])),
  Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi]))}
solveintersectsnub2[
  phi_] := {Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi])),
  1 - Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi]))}
(*points wrt coordinate system in inner square, which is rotated by 45 degrees*)
pointsinnersnub[phi_] := {
  {0,ncos[phi]},
  solveintersectsnub1[phi],
  solveintersectsnub2[phi]
  }
meshgensinnersnub[phi_] := {{
	{0,ncos[phi]},
  {nsin[phi],0},
	{1,nsin[phi]},
  {ncos[phi],1}},{
	{0,ncos[phi]},
  {nsin[phi],0},
	{0,-ncos[phi]},
  {-nsin[phi],0}}
  }
(* Transformation obtained by:
B={{B1,B2},{B3,B4}};
a={a1,a2};
s=Solve[Join[MapThread[Equal,{a+B.{0,0},{1/2,0}}],MapThread[Equal,{a+\
B.{0,1},{0,1/2}}],MapThread[Equal,{a+B.{1/2,1/2},{1/2,1/2}}]],{a1,a2,\
B1,B2,B3,B4}];
B/.s;
a/.s;*)
(*# - (# . {1, 1} - 1/2) & /@ serves to mirror to the right fundamental region*)
inntertooutersnub[x:vecpat]:=# - (# . {1, 1} - 1/2) & [Simplify[{{1/2, -(1/2)}, {1/2, 1/2}} . x + {1/2, 0}]]
inntertooutersnubp4m[x:vecpat]:=({{0, 1}, {-1, 0}}.#)+{0,1/2} & [Simplify[{{1/2, -(1/2)}, {1/2, 1/2}} . x + {1/2, 0}]]
simplelaminate[]:={{{0,0},{1,0},{1,1/2},{0,1/2}},{{0,1/2},{1,1/2},{1,1},{0,1}}}

pointsoutersnub[phi_:Pi/6] := inntertooutersnub /@ pointsinnersnub[phi]
pointsoutersnubp4m[phi_:Pi/6] := inntertooutersnubp4m /@ pointsinnersnub[phi]
(*forms triangle and quadrilateral needed to plot snub square, in normed coordinates*)
formsmaster[form_String,params_:{}]:=(form/.{
	"snub"->formssnub[],
	"meshsnub"->meshgensoutersnub[],
	"simple_laminate"->simplelaminate[],
	"snubwf"->formssnubwf[],
	"snubp4m"->formssnubp4m[]
})
formssnub[phi_:Pi/6]:={{{0,1/2},#[[1]],#[[3]]},{#[[1]],#[[2]],{0,0},#[[3]]},{#[[1]],#[[2]],{1/2,0}}}&[pointsoutersnub[phi]]
formssnubwf[phi_:Pi/6]:={{#[[2]],#[[1]],#[[3]]}}&[pointsoutersnub[phi]]
formssnubp4m[phi_:Pi/6]:={{{1/2,1/2},#[[1]],#[[3]]},{#[[1]],#[[2]],{1/2,0},#[[3]]},{#[[1]],#[[2]],{0,0}}}&[pointsoutersnubp4m[phi]]
meshgensoutersnub[phi_:Pi/6]:=Map[inntertooutersnub,meshgensinnersnub[phi],{2}]
Options[graficsnubface]={colorsq->LightRed,colortr->LightBlue,opts->{EdgeForm[Thickness[Small]]}}
graficsnubface[x:{vecpat..},OptionsPattern[]]:=(Graphics[Join[OptionValue@opts,{Length[x]/.{3->OptionValue@colortr,4->OptionValue@colorsq},Polygon[x]}]])
graficswireframe[x:{vecpat..},color: _?validColorQ : RGBColor["green"]]:=Graphics[
	{Thickness[Large],color,Line[x]}
]
graficsnub[phi_:Pi/6]:=graficsnubface[#]&/@ formssnub[phi]
End[]
EndPackage[]
