BeginPackage["Settings`"]
dim=2
vecpat={Repeated[_?NumericQ,{dim}]}(* pattern denoting vector of euclidean space to be treated*)
matpat={Repeated[{Repeated[_?NumericQ,{dim}]},{dim}]}
(*specify id of corner as well as id of connections linking it to other fundamental regions*)
fundconnectpat={_Integer, {_Integer ..}}
formsQ[list_List]:=Length[Level[list, {Depth[list] - 1}]] == Length[Flatten[list]] && 
 MatchQ[Level[list, {Depth[list] - 2}], {vecpat ..}]
identitytrafo={{0, 0}, {{1, 0}, {0, 1}}};
affineelepat={vecpat,matpat}
Begin["`Private`"]
End[]
EndPackage[]
