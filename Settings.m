BeginPackage["Settings`"]
dim=2
vecpat={Repeated[_?NumericQ,{dim}]}(* pattern denoting vector of euclidean space to be treated*)
matpat={Repeated[{Repeated[_?NumericQ,{dim}]},{dim}]}
(*specify id of corner as well as id of connections linking it to other fundamental regions*)
fundconnectpat={_Integer, {_Integer ..}}
formsQ[list_List]:=Length[Level[list, {Depth[N[list]] - 1}]] == Length[Flatten[list]] && 
 MatchQ[Level[list, {Depth[N[list]] - 2}], {vecpat ..}]
(*list that has a consistent depth (all whose element are present on the lowest level) and that has only vecpats on second lowest level*)
identitytrafo={{0, 0}, {{1, 0}, {0, 1}}};
affineelepat={vecpat,matpat}
Begin["`Private`"]
End[]
EndPackage[]
