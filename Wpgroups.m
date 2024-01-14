BeginPackage["Wpgroups`"]

creategroup::usage="creategroup[a,b,name] returns lattice group name with base vectors a and b as mathematica associations" 
quotientgrafics::usage="quotientgrafics[ia,ib,group] returns list of graphics objects for quotient group of wp-group <group> with <ia> and <ib> times its respective lattice vectors"
quotientpattern::usage="quotientpattern[ia,ib,group] returns list of lists of coordinates of fund region  for quotient group of wp-group <group> with <ia> and <ib> times its respective lattice vectors" 
wpgens::usage="wpgens[name] returns generators of quotient groups of <name> in normed form"
wpfundregnorm::usage="wpfundregnorm[name] returns fundamental region in normed form of group <name>"
wplattice::usage="wplattice[name] returns lattice type of group <name>"
Begin["`Private`"]
wpgens = <|
   (*returns list of generators (in normed form) for quotient group \
of given wp-group
   Form: {{coord1,coord2},matrix representing action on lattice}*)
   "p1" -> {},
   "p2" -> {{{0, 0}, {{-1, 0}, {0, -1}}}},
   "pm" -> {{{0, 0}, {{1, 0}, {0, -1}}}},
   "pg" -> {{{1/2, 0}, {{1, 0}, {0, -1}}}},
   "cm" -> {{{0, 0}, {{1, 1}, {0, -1}}}},
   "pmm" -> {{{0, 0}, {{1, 0}, {0, -1}}}, {{0, 
       0}, {{-1, 0}, {0, -1}}}},
   "pmg" -> {{{0, 0}, {{1, 0}, {0, -1}}}, {{0, 1/
       2}, {{-1, 0}, {0, -1}}}},
   "pgg" -> {{{1/2, 0}, {{1, 0}, {0, -1}}}, {{1/2, 1/
       2}, {{-1, 0}, {0, -1}}}},
   "cmm" -> {{{0, 0}, {{1, 1}, {0, -1}}}, {{0, 
       0}, {{-1, -2}, {0, 1}}}},
   "p4" -> {{{0, 0}, {{0, -1}, {1, 0}}}},
   "p4m" -> {{{0, 0}, {{1, 0}, {0, -1}}}, {{0, 0}, {{0, -1}, {1, 0}}}},
   "p4g" -> {{{1/2, 1/2}, {{1, 0}, {0, -1}}}, {{0, 
       0}, {{0, -1}, {1, 0}}}},
   "p3" -> {{{0, 0}, {{-1, -1}, {1, 0}}}},
   "p3m1" -> {{{0, 0}, {{-1, -1}, {1, 0}}}, {{0, 
       0}, {{0, 1}, {1, 0}}}},
   "p31m" -> {{{0, 0}, {{-1, -1}, {1, 0}}}, {{0, 0}, {{1, 1}, {0, -1}}}},
   "p6" -> {{{0, 0}, {{0, -1}, {1, 1}}}},
   "p6m" -> {{{0, 0}, {{0, -1}, {1, 1}}}, {{0, 0}, {{1, 1}, {0, -1}}}}
   |>;
wpfundregnorm = <|
   (*returns vertices of fundamental domain in normed coordinates*)
   "p1" -> {{0, 0}, {1, 0}, {1, 1}, {0, 1}},
   "p2" -> {{0, 0}, {1, 0}, {1, 1/2}, {0, 1/2}},
   "pm" -> {{0, 0}, {1, 0}, {1, 1/2}, {0, 1/2}},
   "pg" -> {{0, 0}, {1/2, 0}, {1/2, 1}, {0, 1}},
   "cm" -> {{0, 0}, {1, 0}, {0, 1}},
   "pmm" -> {{0, 0}, {1/2, 0}, {1/2, 1/2}, {0, 1/2}},
   "pmg" -> {{0, 0}, {1/2, 0}, {1/2, 1/2}, {0, 1/2}},
   "pgg" -> {{0, 0}, {1, 0}, {1/2, 1/2}},
   "cmm" -> {{0, 0}, {1, 0}, {1, 0}},
   "p4" -> {{0, 0}, {1/2, 0}, {1/2, 1/2}, {0, 1/2}},
   "p4m" -> {{0, 0}, {1/2, 0}, {1/2, 1/2}},
   "p4g" -> {{0, 0}, {1/2, 0}, {0, 1/2}},
   "p3" -> {{1, 0}, {1/3, 1/3}, {0, 1}, {2/3, 2/3}},
   "p3m1" -> {{1, 0}, {1/3, 1/3}, {2/3, 2/3}},
   "p31m" -> {{1, 0}, {0, 1}, {1/3, 1/3}},
   "p6" -> {{0, 0}, {0, 1}, {1/3, 1/3}},
   "p6m" -> {{0, 0}, {0, 1/2}, {1/3, 1/3}}
   |>;
(* Conditions to be fulfilled by lattice vectors*)
wplattice = <|
   (*returns list of generators for quotient group of given wp-group
   Form: (coord1,coord2,matrix representing action on lattice)*)
   "p1" -> "oblique",
   "p2" -> "oblique",
   "pm" -> "rectang",
   "pg" -> "rectang",
   "cm" -> "crectang",
   "pmm" -> "rectang",
   "pmg" -> "rectang",
   "pgg" -> "rectang",
   "cmm" -> "crectang",
   "p4" -> "square",
   "p4m" -> "square",
   "p4g" -> "square",
   "p3" -> "hexa",
   "p3m1" -> "hexa",
   "p31m" -> "hexa",
   "p6" -> "hexa",
   "p6m" -> "hexa"
   |>;
wpconds = {
   (*returns list of conditions which basis vectors of latice need to \
fullfil*)
   q_ :> ((wplattice[q]) /. Latticeconds)
   };
cosetop[x_, y_] := {x[[1]] + x[[2]] . y[[1]], 
  x[[2]] . y[[2]]} (*coset x multiplied with coset y*)
gencosets[gens_] := Module[{donecosets, lastcosets, donetrafo},
        lastcosets = donecosets = {{{0, 0}, {{1, 0}, {0, 1}}}};(* 
  identity coset*)
        donetrafo = {donecosets[[1]][[2]]};
        If[Length[gens] > 0,
        While[Length[lastcosets] > 0,
        donecosets = 
      Join[donecosets, genlayercosets[gens, lastcosets, donetrafo]];
        ]
        ];
        donecosets
  ]
gencosets[gens_, fundnorm_] := Module[{},
        {#[[1]] - 
      Floor[Fold[{Min[#1[[1]], #2[[1]]], Min[#1[[2]], #2[[2]]]} &, {0,
          0}, applyeuclidtofundreg[fundnorm, #]]], #[[2]]} & /@ 
   gencosets[gens]
  ]
genlayercosets[gens_, lastcosets_, donetrafo_] := 
 Module[{potentcoset, cosets2add},
  cosets2add = {};
  Do [(
    potentcoset = cosetop[gens[[i]], lastcosets[[j]]];
    If [! MemberQ[donetrafo, potentcoset[[2]]],
     AppendTo[cosets2add, potentcoset];
     AppendTo[donetrafo, potentcoset[[2]]];
     ];
    potentcoset = cosetop[lastcosets[[j]], gens[[i]]];
    If [! MemberQ[donetrafo, potentcoset[[2]]],
     AppendTo[cosets2add, potentcoset];
     AppendTo[donetrafo, potentcoset[[2]]];
     ]
    )
   , {i, 1, Length[gens]}, {j, 1, Length[lastcosets]}];
  lastcosets = cosets2add
  ]
t = Hold[a . b == 0];
checklatticevec[a_, b_, 
   name_] := (wplattice[name]) /. {oblique -> True, 
    rectang :> a . b == 0, crectang :> a . b == Norm[a]/2, 
    square :>  a . b == 0 && Norm[a] == Norm[b], 
    hexa :>  a . b - Norm[a]/2 == 0 && Norm[a] == Norm[b]};
creategroup[a_, b_, name_] := (
        (*consists of Latticevec a, latticevec b, 
  list of generators of quotient group*)
        If [! checklatticevec[a, b, name],
        Throw["lattice vectors do not fullfill requirements for group"]];
        <|"a" -> a, "b" -> b, "name" -> name, 
   "fundregnorm" -> wpfundregnorm[name], 
   "fundreg" -> ((Transpose[{a, b}] . #) & /@ (wpfundregnorm[name])), 
   "gens" -> wpgens[name], "lvecmat" -> N[Transpose[{a, b}]], 
   "lvecmatinv" -> N[Inverse[Transpose[{a, b}]]]|>
  )
quotienttorusnorm[na_, nb_, group_] := 
 Module[{quotientele, cosets, i, ia, ib},
        (*Generate all elements of quotient group (normalised w.r.t. 
  base vectors) with torus of na and nb*) 
        quotientele = {};
        cosets = gencosets[group["gens"], group["fundregnorm"]];
        Do[
        AppendTo[
     quotientele, {{cosets[[i]][[1]][[1]] + ia, 
       cosets[[i]][[1]][[2]] + ib}, cosets[[i]][[2]]}];
        ,
        {i, Length[cosets]}, {ia, na}, {ib, nb}];
        quotientele
  ]
normedtotrafo[ele_, group_] := (
  {(group["lvecmat"] ) . ele[[1]], (group["lvecmat"]) . 
    ele[[2]] . (group["lvecmatinv"])}
  )
quotienttorus[na_, nb_, group_] := Module[{i, ia, ib},
        (*Generate all elements of quotient group with torus of na and nb \
times base vectors a and b
        in terms of euclidean trafo*)
        normedtotrafo[#, group] & /@ quotienttorusnorm[na, nb, group]
  ]
applyeuclid[x_, euclid_] := euclid[[2]] . x + euclid[[1]]
applyeuclidtofundreg[fundreg_, euclid_] := 
 applyeuclid[#, euclid] & /@ fundreg 
applyquotienttofundreg[fundreg_, quotients_] := 
 applyeuclidtofundreg[fundreg, #] & /@ quotients

SetAttributes[genlayercosets, HoldAll]
quotientpatternnorm[na_, nb_, group_] := 
 applyquotienttofundreg[group["fundregnorm"], 
  quotienttorusnorm[na, nb, group]]
quotientpattern[na_, nb_, group_] := 
 Map[(group["lvecmat"] . #) &, quotientpatternnorm[na, nb, group], {2}]

(*returns list of transformed fundamental regions*)

(*Visualize pattern*)
areapoly[
  points_] := (Total[
   points*RotateLeft[points, {1, 1}] . {{1, 0}, {0, -1}}, 2])
masspointpoly[
  points_] := (Total[(points + RotateLeft[points])*
     Map[Total, 
      points*RotateLeft[points, {1, 1}] . {{1, 0}, {0, -1}}], 
    1]/(3*areapoly[points]))
clenpoly[points_] := Min[Map[Norm, points - RotateLeft[points]]]
horilinepoly[points_] := (points[[2]] - points[[1]])/
  Norm[points[[2]] - points[[1]]]
vertlinepoly1[
  points_] := (points[[2]] - points[[1]]) . {{0, -1}, {1, 0}}/
  Norm[points[[2]] - points[[1]]]
vertlinepoly[points_] := 
 vertlinepoly1[points]*
  Sign[vertlinepoly1[points] . (points[[3]] - points[[2]])]
lpointspoly[points_] := 
 Map[-0.05*
     clenpoly[
      points]*(horilinepoly[points] + 
       vertlinepoly[points]) + # &, {masspointpoly[points] + 
    0.1*clenpoly[points]*horilinepoly[points], masspointpoly[points], 
   masspointpoly[points] + 
    0.2*clenpoly[points]*vertlinepoly[points]}]
graficfund[
  points_] := {(pol = Polygon[points]; 
   Graphics[{LightGreen, EdgeForm[Dashed], pol}]), 
  Graphics[Line[lpointspoly[points]]]}
quotientgrafics[na_, nb_, group_] := 
 graficfund /@ quotientpattern[na, nb, group]
End[]

EndPackage[]

