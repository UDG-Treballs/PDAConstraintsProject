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
%array[1..ndays, 1..nstadiums] of var set of 1..nteams: resultat;

%array[1..ndays,1..nstadiums] of var 0..nteams: availability;
constraint forall(i in 2..nteams) (
    all_different([teamVsTeam[i, j].1 | j in 1..(i-1)]++
                  [teamVsTeam[j, i].1 | j in i+1..nteams])
);
constraint forall(i in 2..nteams) (
    all_different([teamVsTeam[i, j].2 | j in 1..(i-1)]++
                  [teamVsTeam[j, i].2 | j in i+1..nteams])
);


constraint forall(d in 1..ndays, s in 1..nstadiums)(
    card({i | i in 1..nteams-1, j in i+1..nteams where teamVsTeam[j, i].1 == d /\ teamVsTeam[j, i].2 == s})<=1
);

constraint forall(d in 1..ndays, s in 1..nstadiums)(
     if card(fixes[d,s])>1 then teamVsTeam[max(fixes[d,s]),min(fixes[d,s])].1==d /\ teamVsTeam[max(fixes[d,s]),min(fixes[d,s])].2==s
     elseif card(fixes[d,s])==1 then exists( vs in [teamVsTeam[i,fixes[d,s][1]] | i in fixes[d,s][1]+1..nteams])(vs.1==d /\ vs.2==s) \/
                                     exists( vs in [teamVsTeam[fixes[d,s][1],i] | i in 1..(fixes[d,s][1]-1)])(vs.1==d /\ vs.2==s)
     else
      true
     endif
     
);
  
constraint seguidorsFora = 
  sum(e1 in 1..nteams-1, e2 in e1+1..nteams)(
    abs(max(tifosi[e1] + tifosi[e2] - capacitats[teamVsTeam[e2, e1].2],0))
);



/*% Constraint to populate resultat based on teamVsTeam
constraint forall(i in 1..nteams-1, j in i+1..nteams)(
    resultat[teamVsTeam[j, i].1, teamVsTeam[j, i].2] = {i, j}
);*/


solve minimize(seguidorsFora);

