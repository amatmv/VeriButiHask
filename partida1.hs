--EX1 TRAMPA: Exemples per trampa on no hi ha trampa.
let mans1= [NewM [NewC(Bastos, Manilla),NewC(Bastos, Vuit), NewC(Espases, Tres), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Espases, Cavall), NewC(Copes, Set), NewC(Oros, As), NewC(Bastos, Cinc), NewC(Copes, Sota), NewC(Espases, Quatre), NewC(Bastos, Set)],NewM [NewC(Bastos, Sota), NewC(Bastos, Cavall), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Oros, Cinc), NewC(Espases, Vuit), NewC(Copes, Manilla), NewC(Oros, Sis), NewC(Oros, Sota), NewC(Copes, Cinc), NewC(Oros, Set), NewC(Copes, Quatre)],NewM [NewC(Bastos, As), NewC(Oros, Dos), NewC(Espases, Rei), NewC(Copes, Cavall), NewC(Oros, Vuit), NewC(Espases, As), NewC(Copes, Rei), NewC(Oros, Rei), NewC(Copes, Tres), NewC(Copes, Sis), NewC(Espases, Sis), NewC(Espases, Cinc)],NewM [NewC(Bastos, Dos), NewC(Bastos, Tres), NewC(Espases, Dos), NewC(Copes, Dos), NewC(Bastos, Sis), NewC(Espases, Set), NewC(Oros, Tres), NewC(Oros, Quatre), NewC(Bastos, Rei), NewC(Oros, Cavall), NewC(Oros, Manilla), NewC(Espases, Sota)]]

let partida1 = [NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Vuit), NewC(Bastos, Cavall), NewC(Oros, Dos), NewC(Bastos, Tres), NewC(Espases, Rei), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Copes, Cavall), NewC(Copes, Dos), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Oros, Cinc), NewC(Oros, Vuit), NewC(Bastos, Sis), NewC(Espases, As), NewC(Espases, Set), NewC(Espases, Cavall), NewC(Espases, Vuit), NewC(Copes, Rei), NewC(Oros, Tres), NewC(Copes, Set), NewC(Copes, Manilla), NewC(Oros, Quatre), NewC(Oros, As), NewC(Oros, Sis), NewC(Oros, Rei), NewC(Bastos, Cinc), NewC(Oros, Sota), NewC(Copes, Tres), NewC(Bastos, Rei), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Oros, Cavall), NewC(Copes, Sota), NewC(Oros, Manilla), NewC(Espases, Quatre), NewC(Oros, Set), NewC(Espases, Sis), NewC(Espases, Sota), NewC(Bastos, Set), NewC(Copes, Quatre), NewC(Espases, Cinc)]

let trumfu1 = Or

let init1 = 0

--
call: trampa mans1 trumfu1 partida1
--
--
result: Nothing
--


--EX2 TRAMPA: Exemples per trampa on hi ha trampa del jugador 2 en la basa 2 on tira un Vuit de Copes i esta obligat a tirar bastos
let partida2 = [NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Vuit), NewC(Copes, Vuit), NewC(Oros, Dos), NewC(Bastos, Tres), NewC(Espases, Rei), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Bastos, Cavall), NewC(Copes, Dos), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Oros, Cinc), NewC(Oros, Vuit), NewC(Bastos, Sis), NewC(Espases, As), NewC(Espases, Set), NewC(Espases, Cavall), NewC(Espases, Vuit), NewC(Copes, Rei), NewC(Oros, Tres), NewC(Copes, Set), NewC(Copes, Manilla), NewC(Oros, Quatre), NewC(Oros, As), NewC(Oros, Sis), NewC(Oros, Rei), NewC(Bastos, Cinc), NewC(Oros, Sota), NewC(Copes, Tres), NewC(Bastos, Rei), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Oros, Cavall), NewC(Copes, Sota), NewC(Oros, Manilla), NewC(Espases, Quatre), NewC(Oros, Set), NewC(Espases, Sis), NewC(Espases, Sota), NewC(Bastos, Set), NewC(Copes, Quatre), NewC(Espases, Cinc)]

let mans2= [NewM [NewC(Bastos, Manilla),NewC(Bastos, Vuit), NewC(Espases, Tres), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Espases, Cavall), NewC(Copes, Set), NewC(Oros, As), NewC(Bastos, Cinc), NewC(Copes, Sota), NewC(Espases, Quatre), NewC(Bastos, Set)],NewM [NewC(Bastos, Sota), NewC(Bastos, Cavall), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Oros, Cinc), NewC(Espases, Vuit), NewC(Copes, Manilla), NewC(Oros, Sis), NewC(Oros, Sota), NewC(Copes, Cinc), NewC(Oros, Set), NewC(Copes, Quatre)],NewM [NewC(Bastos, As), NewC(Oros, Dos), NewC(Espases, Rei), NewC(Copes, Cavall), NewC(Oros, Vuit), NewC(Espases, As), NewC(Copes, Rei), NewC(Oros, Rei), NewC(Copes, Tres), NewC(Copes, Sis), NewC(Espases, Sis), NewC(Espases, Cinc)],NewM [NewC(Bastos, Dos), NewC(Bastos, Tres), NewC(Espases, Dos), NewC(Copes, Dos), NewC(Bastos, Sis), NewC(Espases, Set), NewC(Oros, Tres), NewC(Oros, Quatre), NewC(Bastos, Rei), NewC(Oros, Cavall), NewC(Oros, Manilla), NewC(Espases, Sota)]]

let trumfu2 = Or

let init2 = 0

--
call: trampa mans1 trumfu2 partida2 init2
result: Just ([Vuit de Bastos,Vuit de Copes,Dos d'Oros,Tres de Bastos],2,1)
--


--EX3 CARTES GUANYADES: Exemple per cartes guanyades on primer agafem una partida, barrajem les cartes amb una llavor i comprovem les cartes guanyades per a cada parella
let partida3 = [NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Vuit), NewC(Bastos, Cavall), NewC(Oros, Dos), NewC(Bastos, Tres), NewC(Espases, Rei), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Copes, Cavall), NewC(Copes, Dos), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Oros, Cinc), NewC(Oros, Vuit), NewC(Bastos, Sis), NewC(Espases, As), NewC(Espases, Set), NewC(Espases, Cavall), NewC(Espases, Vuit), NewC(Copes, Rei), NewC(Oros, Tres), NewC(Copes, Set), NewC(Copes, Manilla), NewC(Oros, Quatre), NewC(Oros, As), NewC(Oros, Sis), NewC(Oros, Rei), NewC(Bastos, Cinc), NewC(Oros, Sota), NewC(Copes, Tres), NewC(Bastos, Rei), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Oros, Cavall), NewC(Copes, Sota), NewC(Oros, Manilla), NewC(Espases, Quatre), NewC(Oros, Set), NewC(Espases, Sis), NewC(Espases, Sota), NewC(Bastos, Set), NewC(Copes, Quatre), NewC(Espases, Cinc)]

let partida3 = [NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Vuit), NewC(Bastos, Cavall), NewC(Oros, Dos), NewC(Bastos, Tres), NewC(Espases, Rei), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Copes, Cavall), NewC(Copes, Dos), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Oros, Cinc), NewC(Oros, Vuit), NewC(Bastos, Sis), NewC(Espases, As), NewC(Espases, Set), NewC(Espases, Cavall), NewC(Espases, Vuit), NewC(Copes, Rei), NewC(Oros, Tres), NewC(Copes, Set), NewC(Copes, Manilla), NewC(Oros, Quatre), NewC(Oros, As), NewC(Oros, Sis), NewC(Oros, Rei), NewC(Bastos, Cinc), NewC(Oros, Sota), NewC(Copes, Tres), NewC(Bastos, Rei), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Oros, Cavall), NewC(Copes, Sota), NewC(Oros, Manilla), NewC(Espases, Quatre), NewC(Oros, Set), NewC(Espases, Sis), NewC(Espases, Sota), NewC(Bastos, Set), NewC(Copes, Quatre), NewC(Espases, Cinc)]

let partidaAleatoria = shuffle partida3 1997

--
call: fst $ cartesGuanyades Or partidaAleatoria 0
result: [Set d'Espases,Vuit de Bastos,Sota d'Oros,Dos d'Oros,Rei d'Oros,Quatre de Bastos,Sis de Copes,Cavall de Copes,Dos de Bastos,As de Copes,Set de Bastos,Manilla de Copes,Manilla d'Oros,As de Bastos,Cinc de Copes,Tres d'Espases,Manilla d'Espases,Cavall de Bastos,Set d'Oros,Sota d'Espases,Cavall d'Espases,Dos d'Espases,As d'Espases,As d'Oros,Sis de Bastos,Manilla de Bastos,Tres d'Oros,Sota de Copes,Sota de Bastos,Cinc d'Oros,Vuit d'Oros,Rei de Bastos,Sis d'Espases,Vuit de Copes,Cavall d'Oros,Set de Copes]
--
call: snd $ cartesGuanyades Or partidaAleatoria 0
result: [Sis d'Oros,Rei d'Espases,Quatre d'Oros,Quatre de Copes,Cinc d'Espases,Quatre d'Espases,Vuit d'Espases,Cinc de Bastos,Tres de Copes,Rei de Copes,Tres de Bastos,Dos de Copes]
--
comprovant: length $ (fst $ cartesGuanyades Or partidaAleatoria 0)++(snd $ cartesGuanyades Or partidaAleatoria 0)
result: 48
--

--EX4 PUNTS: Exemple per a comprovar el recompte de punts de les cartes.
let partida4 = [NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Vuit), NewC(Bastos, Cavall), NewC(Oros, Dos), NewC(Bastos, Tres), NewC(Espases, Rei), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Copes, Cavall), NewC(Copes, Dos), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Oros, Cinc), NewC(Oros, Vuit), NewC(Bastos, Sis), NewC(Espases, As), NewC(Espases, Set), NewC(Espases, Cavall), NewC(Espases, Vuit), NewC(Copes, Rei), NewC(Oros, Tres), NewC(Copes, Set), NewC(Copes, Manilla), NewC(Oros, Quatre), NewC(Oros, As), NewC(Oros, Sis), NewC(Oros, Rei), NewC(Bastos, Cinc), NewC(Oros, Sota), NewC(Copes, Tres), NewC(Bastos, Rei), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Oros, Cavall), NewC(Copes, Sota), NewC(Oros, Manilla), NewC(Espases, Quatre), NewC(Oros, Set), NewC(Espases, Sis), NewC(Espases, Sota), NewC(Bastos, Set), NewC(Copes, Quatre), NewC(Espases, Cinc)]

--
call: punts partida4
result: 60
--

--EX5 PUNTSPARELLES: Exemple per comprovar la puntuacio de cada parella segons les cartes guanyades.
let mans5= [NewM [NewC(Bastos, Manilla),NewC(Bastos, Vuit), NewC(Espases, Tres), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Espases, Cavall), NewC(Copes, Set), NewC(Oros, As), NewC(Bastos, Cinc), NewC(Copes, Sota), NewC(Espases, Quatre), NewC(Bastos, Set)],NewM [NewC(Bastos, Sota), NewC(Bastos, Cavall), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Oros, Cinc), NewC(Espases, Vuit), NewC(Copes, Manilla), NewC(Oros, Sis), NewC(Oros, Sota), NewC(Copes, Cinc), NewC(Oros, Set), NewC(Copes, Quatre)],NewM [NewC(Bastos, As), NewC(Oros, Dos), NewC(Espases, Rei), NewC(Copes, Cavall), NewC(Oros, Vuit), NewC(Espases, As), NewC(Copes, Rei), NewC(Oros, Rei), NewC(Copes, Tres), NewC(Copes, Sis), NewC(Espases, Sis), NewC(Espases, Cinc)],NewM [NewC(Bastos, Dos), NewC(Bastos, Tres), NewC(Espases, Dos), NewC(Copes, Dos), NewC(Bastos, Sis), NewC(Espases, Set), NewC(Oros, Tres), NewC(Oros, Quatre), NewC(Bastos, Rei), NewC(Oros, Cavall), NewC(Oros, Manilla), NewC(Espases, Sota)]]

let partida5= [NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Vuit), NewC(Bastos, Cavall), NewC(Oros, Dos), NewC(Bastos, Tres), NewC(Espases, Rei), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Manilla), NewC(Copes, Vuit), NewC(Copes, Cavall), NewC(Copes, Dos), NewC(Copes, As), NewC(Bastos, Quatre), NewC(Oros, Cinc), NewC(Oros, Vuit), NewC(Bastos, Sis), NewC(Espases, As), NewC(Espases, Set), NewC(Espases, Cavall), NewC(Espases, Vuit), NewC(Copes, Rei), NewC(Oros, Tres), NewC(Copes, Set), NewC(Copes, Manilla), NewC(Oros, Quatre), NewC(Oros, As), NewC(Oros, Sis), NewC(Oros, Rei), NewC(Bastos, Cinc), NewC(Oros, Sota), NewC(Copes, Tres), NewC(Bastos, Rei), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Oros, Cavall), NewC(Copes, Sota), NewC(Oros, Manilla), NewC(Espases, Quatre), NewC(Oros, Set), NewC(Espases, Sis), NewC(Espases, Sota), NewC(Bastos, Set), NewC(Copes, Quatre), NewC(Espases, Cinc)]

--
call: puntsParelles mans5 Or partida5 0
result: Just (31,29)
--
comprovant1: punts (fst $ cartesGuanyades Or partida5 0)
result1: 31
comprovant2: punts (snd $ cartesGuanyades Or partida5 0)
result2: 29
--
