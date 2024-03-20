BeginPackage["Tensors`"]
rotmatk::usage="rotmatk[p] yields rotation matrix in kelvin notation for turning about angle p" 
rotmath::usage="rotmath[p] yields rotation matrix in harmonic base for turning about angle p" 
harmdecomp::usage="harmdecomp[tens] yields harmonic decomposition of 4th order tensor in Mandel notation" 
invariants::usage="invariants[tens] yields 5 invariants of 4th order tensor in Mandel notation" 
cauchyschwartz::usage="cauchyschwartz[I2_,J2_,J3_] yields cauchy schwartz inegality" 
thk::usage="tensor as a function of harmonic decomposition in kelvin notation"
Kscal::usage="Scale to pass from "
Begin["`Private`"]
ktoh = {{Sqrt[2]/2, 0, Sqrt[2]/2}, {-Sqrt[2]/2, 0, Sqrt[2]/2}, {0, 1, 
    0}};(*transformation matrix between kelvin and harmonic \
representation*)
Kscal = {{1, 1, Sqrt[2]/2}, {1, 1, Sqrt[2]/2}, {Sqrt[2]/2, Sqrt[2]/2, 
    2}};(*to scale tensor into kelvin form*)
turn[t : _?MatrixQ, r : _?MatrixQ] := 
 FullSimplify[Transpose[r] . t . r ]
(*turn 4th order tensor t*)
turn[t : _?VectorQ, r : _?MatrixQ] := FullSimplify[Transpose[r] . t ]
(*turn 2th order tensor t*)

(*harmonic decomposition of tensor in voigt notation*)
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
harmdecomp[tens_?MatrixQ/;AnyTrue[Flatten[tens],(!NumericQ[#]&)]] :=(
  Solve[thh == turn[(Kscal*tens), ktoh], {D1, D2, d1, d2, y, 
     k}][[1]])
tv = {{T1111, T1122, T1112}, {T1122, T2222, T1222}, {T1112, T1222,
    T1212}};
tvharmdecomp=harmdecomp[tv];
harmdecomp[tens_?MatrixQ/;MatrixQ[tens,(NumericQ[#]&)]]:=tvharmdecomp /. {T1111->tens[[1,1]],T1122->tens[[1,2]],T2222->tens[[2,2]],T1222->tens[[2,3]],T1112->tens[[1,3]],T1212->tens[[3,3]]}
invariants[tens_] := 
  FullSimplify[{k, y, dk[d1, d2] . dk[d1, d2], 
     Tr[Dmatk[D1, D2] . Dmatk[D1, D2]], 
     dk[d1, d2] . Dmatk[D1, D2] . dk[d1, d2]} /. harmdecomp[tens]];
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
