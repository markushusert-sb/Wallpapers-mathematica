BeginPackage["Windows`"]

restricttowind::usage="restricttowind[wind : {vecpat, vecpat}, points_?formsQ] cuts of any parts of polygons described by forms which are outside of the specified window" 
Needs["Settings`"]

Begin["`Private`"]
restricttowind[wind : windpat, points_?formsQ] := (Fold[restricttoedge[#2[[1]],#2[[2]],#1]&,points,wind])

restricttoedge[n : vecpat, lim_?NumericQ, points_?formsQ] := (
  Map[restricttoedgepoly[n, lim, #] &, points, {depthlist[points] - 3}] //. {{} -> Sequence[],{vecpat} -> Sequence[],{vecpat,vecpat} -> Sequence[]} (*remove empty polygons (who lie entirely outside of the window*)
  )
restricttoedge[n : vecpat, lim_?NumericQ] := Sequence[]
restricttoedgepoly[n : vecpat, lim_?NumericQ,
  polygon : {vecpat ..}] := (Nest[
   Replace[#, {p : vecpat, r : vecpat ...} :>
      treatpoint[n, lim, p, r]] &, polygon, Length[polygon]])
treatpoint[n : vecpat, lim_?NumericQ, p : vecpat,
  r : vecpat ...] := ({r,
   If[N[n . p] > lim,
    Sequence @@ {If[Length[{r}] == 0 || N[n . {r}[[-1]]] >= lim,
       Nothing, intersection[n, lim, p, {r}[[-1]]]],
      If[Length[{r}] == 0 || N[n . {r}[[1]]] >= lim, Nothing,
       intersection[n, lim, p, {r}[[1]]]]}, p]})
intersection[n : vecpat, lim_?NumericQ, p1 : vecpat,p2 : vecpat] := FullSimplify[(lim - p2 . n)/(p1 . n - p2 . n)*p1 - (lim - p1 . n)/(p1 . n - p2 . n)*p2]
End[]

EndPackage[]
