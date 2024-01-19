BeginPackage["Settings`"]
dim=2
vecpat={Repeated[_?NumericQ,{dim}]}(* pattern denoting vector of euclidean space to be treated*)
matpat={Repeated[{Repeated[_?NumericQ,{dim}]},{dim}]}
identitytrafo={{0, 0}, {{1, 0}, {0, 1}}};
affineelepat={vecpat,matpat}
Begin["`Private`"]
End[]
EndPackage[]
