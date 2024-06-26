BeginPackage["Settings`"]
dim=2
vecpat={Repeated[_?NumericQ,{dim}]}(* pattern denoting vector of euclidean space to be treated*)
matpat={Repeated[{Repeated[_?NumericQ,{dim}]},{dim}]}
windpat={{vecpat, _?NumericQ}..}
(*specify id of corner as well as id of connections linking it to other fundamental regions*)
fundconnectpat={_Integer, {_Integer ..}}
formsQ::usage="check for nicely structured array of points"
validColorQ::usage="check for color"
sequal::usage="check for symbolic egality of expression"
depthlist::usage="get depth of list without expanding its elements"
(*list that has a consistent depth (all whose element are present on the lowest level) and that has only vecpats on second lowest level*)
identitytrafo={{0, 0}, {{1, 0}, {0, 1}}};
affineelepat={vecpat,matpat}
(*https://mathematica.stackexchange.com/questions/38851/what-is-the-proper-way-to-verify-that-two-expressions-are-equal*)
Begin["`Private`"]
formsQ[list_List]:=Length[Level[list, {Depth[N[list]] - 1}]] == Length[Flatten[list]] && ( MatchQ[Level[list, {Depth[N[list]] - 2}], {vecpat ..}])
validColorQ[color_] := MatchQ[color, RGBColor[_, _, _] | GrayLevel[_]]
sequal[a_,b_]:=(True === FullSimplify[a == b])
depthlist[{}] := 2;
depthlist[list_List] := 1 + Max[Map[depthlist, list]];
depthlist[_] := 1;
End[]
EndPackage[]
