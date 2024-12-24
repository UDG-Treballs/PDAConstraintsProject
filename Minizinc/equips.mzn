include "globals.mzn";
% Variables d'entrada dels fitxers
int: nmachesperday;
int: nmatchesperstadium;
int: ndays;
int: nteams;
int: nstadiums;

array[1..ndays,1..nstadiums] of set of int : fixes;
array[1..nstadiums,1..nstadiums] of int : distancies;

array[1..nteams] of int : tifosi;
array[1..nstadiums] of int : capacitats;

% ViewPoint
% -------------------------------------------------------------------------------------------------------------
% Tenim un matriu d'equip vs equip on a cada posició ens indica a quin estadi i dia juguen aquells dos equips.
% La diagonal és buida (com la meva ànima).
% -------------------------------------------------------------------------------------------------------------
array[1..nteams,1..nteams] of var tuple(1..ndays, 1..nstadiums): teamVsTeam;
var int: seguidorsFora;
array[1..nteams] of var min([distancies[i, j]*nstadiums | i in 1..nstadiums, j in i..nstadiums where distancies[i, j] > 0])..max([distancies[i, j]*nstadiums | i in 1..nstadiums, j in i..nstadiums where distancies[i, j] > 0]): minKilometres;


constraint forall(i in 1..nteams) (
    all_different([teamVsTeam[i, j].1 | j in 1..(i-1)]++
                  [teamVsTeam[j, i].1 | j in i+1..nteams]) /\
                  
    all_different([teamVsTeam[i, j].2 | j in 1..(i-1)]++
                  [teamVsTeam[j, i].2 | j in i+1..nteams])
);

constraint forall(d in 1..ndays, s in 1..nstadiums)(
     if card(fixes[d,s])>1 then teamVsTeam[max(fixes[d,s]),min(fixes[d,s])].1==d /\ teamVsTeam[max(fixes[d,s]),min(fixes[d,s])].2==s
     elseif card(fixes[d,s])==1 then exists( vs in [teamVsTeam[i,fixes[d,s][1]] | i in fixes[d,s][1]+1..nteams]++
			                                             [teamVsTeam[fixes[d,s][1],i] | i in 1..(fixes[d,s][1]-1)])(vs.1==d /\ vs.2==s) 
     else
      true
     endif
     
);
  
constraint seguidorsFora = 
  sum(e1 in 1..nteams-1, e2 in e1+1..nteams)(
    abs(max(tifosi[e1] + tifosi[e2] - capacitats[teamVsTeam[e2, e1].2],0))
);

constraint forall(e in 1..nteams)(
  minKilometres[e] = sum(d in 2..ndays)(
                        let {
                          var 1..nstadiums: d1_estadium;
                          var 1..nstadiums: d2_estadium;
                          constraint exists(e2d1 in 1..nteams) (e!=e2d1 /\ teamVsTeam[max(e, e2d1),min(e, e2d1)].1==(d-1) /\ teamVsTeam[max(e, e2d1),min(e, e2d1)].2==d1_estadium);
		                      constraint exists(e2d2 in 1..nteams) (e!=e2d2 /\ teamVsTeam[max(e, e2d2),min(e, e2d2)].1==(d) /\ teamVsTeam[max(e, e2d2),min(e, e2d2)].2==d2_estadium);
                        } in
                        distancies[d1_estadium, d2_estadium])
                     );

solve minimize(seguidorsFora);


output["\t|"] ++  [
 "Team " ++ show(i) ++ "\t" ++ if i != nteams then "" else "|\n" endif |i in 1..nteams
] ++ ["---------"
] ++ [
 "--------"++if i != nteams then "" else "\n" endif |i in 1..nteams
] ++  [
 if j == 1 then "Team " ++ show(i) ++ "\t" else "" endif ++
 if i < j then "d" ++ show(teamVsTeam[j, i].1) ++ " e" ++ show(teamVsTeam[j,i].2) ++ "\t" 
 elseif i == j then "-----\t" 
 else "d" ++ show(teamVsTeam[i, j].1) ++ " e" ++ show(teamVsTeam[i,j].2) ++ "\t" endif ++
 if j == nteams then "|\n" else "" endif| 
 i in 1..nteams, j in 1..nteams
] ++ ["---------"
] ++ [
 "--------"++if i != nteams then "" else "\n" endif |i in 1..nteams
] ++ [
 "\nKMs TOTALS: \(sum(minKilometres)) \n",
 "SEGUIDORS FORA: \(seguidorsFora)"
];