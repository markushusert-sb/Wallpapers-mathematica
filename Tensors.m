BeginPackage["Tensors`"]
rotmatk::usage="rotmatk[p] yields rotation matrix in kelvin notation for turning about angle p" 
rotmath::usage="rotmath[p] yields rotation matrix in harmonic base for turning about angle p" 
harmdecomp::usage="harmdecomp[tens] yields harmonic decomposition of 4th order tensor in Voigt notation" 
invariants::usage="invariants[tens] yields 5 invariants of 4th order tensor in Voigt notation" 
cauchyschwartz::usage="cauchyschwartz[I2_,J2_,J3_] yields cauchy schwartz inegality" 
thk::usage="tensor as a function of harmonic decomposition in kelvin notation"
ktoh::usage="turn[T,ktoh] turns tensor from kelvin base to harmonic base"
Kscal::usage="Scale to pass from mandel to kelvin-voigt"
turn::usage="turn[a,r] turns 2/th or 4/t tensor a by rotation tensor r"
deductsymmetryclassplane::usage="deductsymmetryclassplane[I2,J2,J3,norm] deducts symmetryclass based on IB"
Begin["`Private`"]
ktoh = Transpose[{{Sqrt[2]/2, 0, Sqrt[2]/2}, {-Sqrt[2]/2, 0, Sqrt[2]/2}, {0, 1, 
    0}}];(*transformation matrix between kelvin and harmonic \
representation*)
Kscal = {{1, 1, Sqrt[2]}, {1, 1, Sqrt[2]}, {Sqrt[2], Sqrt[2], 
    2}};(*to scale tensor into kelvin form from voigt*)
turn[t : _?MatrixQ, r : _?MatrixQ] := 
 FullSimplify[r . t . Transpose[r] ]
(*turn 4th order tensor t*)
turn[t : _?VectorQ, r : _?MatrixQ] := FullSimplify[Transpose[r] . t ]
(*turn 2th order tensor t*)

(*harmonic decomposition of tensor in kelvin notation*)
dk[d1_, d2_] := {d1, -d1, Sqrt[2] d2};
Dmatk[D1_, 
  D2_] := {{D1, -D1, Sqrt[2] D2}, {-D1, 
   D1, -Sqrt[2] D2}, {Sqrt[2] D2, -Sqrt[2] D2, -2 D1}}
ik = {1, 1, 0}(*second order identity tensor*)
iik = IdentityMatrix[3](*forth order symmetric identity tensor*)
kk = 1/2*Outer[Times, ik, ik](*spheric projector*)
jk = iik - kk(*spheric projector*)
tdk[d1_, d2_] := 
 1/2 (Outer[Times, dk[d1, d2], ik] + Outer[Times, ik, dk[d1, d2]])
tkk[k_] := k (Outer[Times, ik, ik])
tyk[y_] := y*jk
thk := Dmatk[D1, D2] + tdk[d1, d2] + tkk[k] + tyk[y]
thh := turn[thk, ktoh]
(*decompose tensor in voigt notation*)
deductsymmetryclassplane[I2_, J2_, J3_, norm_] := Module[{limit=10^(-5)},
   If[Sqrt[J2/norm^2] < limit,
    If[Sqrt[I2/norm^2] < limit,
     "O(2)",
     "D2"]
    , If[J3<0 ||(J3/norm^3)^(1/3) < limit,
     If[Sqrt[I2/norm^2] < limit,
      "D4",
      "D2"],
     "Z2"]]]
angle[x_, y_] := -ArcCos[x/Sqrt[x^2 + y^2]]*If[y != 0, Sign[y], 1]
(*harmonic decomposition of arbitrary tensor in voigt notation*)
harmdecomp[tens_?MatrixQ/;AnyTrue[Flatten[tens],(!NumericQ[#]&)]] :=(
  Solve[thh == turn[(Kscal*tens), ktoh], {D1, D2, d1, d2, y, 
     k}][[1]])
tv = {{T1111, T1122, T1112}, {T1122, T2222, T1222}, {T1112, T1222,
    T1212}};
tvharmdecomp=harmdecomp[tv];
harmdecomp[tens_?MatrixQ/;MatrixQ[tens,(NumericQ[#]&)]]:=Echo[tvharmdecomp /. {T1111->tens[[1,1]],T1122->tens[[1,2]],T2222->tens[[2,2]],T1222->tens[[2,3]],T1112->tens[[1,3]],T1212->tens[[3,3]]}]
invariants[tens_] :=(*harmonic decomposition of numeric tensor in voigt notation*) 
  FullSimplify[{k, y, dk[d1, d2] . dk[d1, d2], 
     Tr[Dmatk[D1, D2] . Dmatk[D1, D2]], 
     dk[d1, d2] . Dmatk[D1, D2] . dk[d1, d2],angle[d1,d2],angle[D1,D2]} /. harmdecomp[tens]];
(*tensor in kelvin system with harmonic composants*)
cauchyschwartz[I2_,J2_,J3_]:=I2^2*J2-2*J3^2
rotmatk[p_] := 
 1/2*{{1 + Cos[2*p], 1 - Cos[2*p], -Sqrt[2]*Sin[2*p]}, {1 - Cos[2*p], 
    1 + Cos[2*p], 
    Sqrt[2]*Sin[2*p]}, {Sqrt[2]*Sin[2*p], -Sqrt[2]*Sin[2*p], 
    2 Cos[2*p]}}
rotmath[p_] := {{Cos[2*p], -Sin[2*p], 0}, {Sin[2*p], Cos[2*p], 0}, {0,
    0, 1}}
End[]

EndPackage[]
