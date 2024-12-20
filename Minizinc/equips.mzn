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
%array[1..ndays,1..nstadiums] of var 0..nteams: availability;

constraint forall(i in 1..nteams) (
    all_different([teamVsTeam[j+1, i].1 | j in i..(nteams - 1)])
);

constraint forall(i in 1..nteams) (
    all_different([teamVsTeam[j+1, i].2 | j in i..(nteams - 1)])
);

constraint forall(i in 1..nteams) (
    all_different([teamVsTeam[i, j].1 | j in 1..(i-1)])
);

constraint forall(i in 1..nteams) (
    all_different([teamVsTeam[i, j].2 | j in 1..(i-1)])
);

constraint forall(d in 1..ndays, s in 1..nstadiums)(
    card({i | i in 1..nteams, j in i..(nteams-1) where teamVsTeam[j+1, i].1 == d /\ teamVsTeam[j+1, i].2 == s})<=1
);

constraint forall(d in 1..ndays, s in 1..nstadiums)(
     if card(fixes[d,s])>1 then teamVsTeam[max(fixes[d,s]),min(fixes[d,s])].1==d /\ teamVsTeam[max(fixes[d,s]),min(fixes[d,s])].2==s
     elseif card(fixes[d,s])==1 then exists( vs in [teamVsTeam[i+1,fixes[d,s][1]] | i in fixes[d,s][1]..(nteams-1)])(vs.1==d /\ vs.2==s) \/
                                     exists( vs in [teamVsTeam[fixes[d,s][1],i] | i in 1..(fixes[d,s][1]-1)])(vs.1==d /\ vs.2==s)
     else
      true
     endif
     
);

/*constraint forall(d in 1..ndays, s in 1..nstadiums)(
    availability[d, s] = card({1 | i in 1..nteams, j in i..(nteams-1) where teamVsTeam[j+1, i].1 == d /\ teamVsTeam[j+1, i].2 == s})
);
*/

solve satisfy;