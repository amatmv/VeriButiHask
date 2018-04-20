{-------------------------------------------------------------}
{--                                                         --}
{--  IMPLEMENTACIÓ D'UN VALIDADOR DE PARTIDES DE BUTIFARRA  --}
{--                                                         --}
{-------------------------------------------------------------}
import System.Random
import System.IO.Unsafe
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Char

--------------------------
-- Definicions de tipus --
--------------------------


{-------- PAL: Pal de les cartes ------
  És mostrable i comparable.
-}
data Pal = Oros | Bastos | Copes | Espases
           deriving (Show, Eq)


{-------- TRUMFU: Pal de la partia ------
  El Trumfu pot ser un Pal o "Butifarra".
  És mostrable i comparable.
-}
data Trumfu = Or | Ba | Co | Es | Bu
              deriving (Eq)

instance Show Trumfu where
  show Or = "Oros"
  show Ba = "Bastos"
  show Co = "Copes"
  show Es = "Espases"
  show Bu = "Butifarra"


{--------- TIPUSCARTA: Numero de la carta ------
  Cada Pal té 12 numeros, que van des del 1 fins al 12.
  És mostrable, comparable i ordenable.
-}
data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei
                  deriving (Show, Eq)
instance Ord TipusCarta where
  compare tipuscarta1 tipuscarta2
    | (getCardValue(tipuscarta1)) == (getCardValue(tipuscarta2)) = EQ
    | (getCardValue(tipuscarta1)) <= (getCardValue(tipuscarta2)) = LT
    | otherwise = GT


{------ CARTA: Representació en Haskell d'una carta de la baralla espanyola. ------
  Una Carta Pertany a un Pal i és un tipus de carta (que representa el nombre de  l'1 al 12)
  És comparable, mostrable i ordenable.
-}
data Carta = NewC (Pal, TipusCarta)

instance Eq Carta where
  (NewC (p1,t1)) == (NewC (p2,t2)) = ((t1 == t2) && (p1 == p2))

instance Show Carta where
  show (NewC (pal, tipus)) =  ((show tipus) ++ (if (show pal) !! 0 `elem` ['A', 'E', 'I', 'O', 'U'] then " d\'" else " de ") ++ (show pal))

instance Ord Carta where
  compare (NewC (p1,t1)) (NewC (p2,t2))
    | t1 == t2 = EQ
    | t1 <= t2 = LT
    | otherwise = GT


{---- MA: Representació de la ma d'un jugador. ----
  S'entén per "Ma" el conjunt de Cartes que té el jugador a la seva mà.
  És comparable i mostrable.
-}
data Ma = NewM [Carta] deriving Eq

instance Show Ma where
  show (NewM []) = "{}"
  show (NewM (x:xs)) = ("{" ++ (show x) ++ "-" ++ (show2 (NewM xs)) ++ "}")
-- Funció auxiliar per a fer el Show de la ma.
show2 :: Ma -> String
show2 (NewM []) = ""
show2 (NewM (x:[])) = (show x)
show2 (NewM (x:xs)) = ((show x) ++ "-" ++ (show2 (NewM xs)))


{--- BASA: Representació d'una ronda de tirades dels 4 jugadors ----
  La Basa sempre conté quatre cartes i és iniciada per un jugador.
  És comparable.
-}
data Basa = NewB (Int, Carta, Carta, Carta, Carta) deriving Eq

instance Show Basa where
  show (NewB (j,w,x,y,z))= ("Tirada iniciada per el jugador: " ++ (show j) ++ "\n" ++ "  Tirada 1: " ++ (show w) ++ "\n" ++ "  Tirada 2: " ++ (show x) ++ "\n" ++ "  Tirada 3: " ++ (show y) ++ "\n" ++ "  Tirada 4: " ++ (show z))


----------------------------------------
--- Definicio de la constant baralla ---
----------------------------------------

baralla = [NewC(Oros, As), NewC(Oros, Dos), NewC(Oros, Tres), NewC(Oros, Quatre), NewC(Oros, Cinc), NewC(Oros, Sis), NewC(Oros, Set), NewC(Oros, Vuit), NewC(Oros, Manilla), NewC(Oros, Sota), NewC(Oros, Cavall), NewC(Oros, Rei), NewC(Espases, As), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Quatre), NewC(Espases, Cinc), NewC(Espases, Sis), NewC(Espases, Set), NewC(Espases, Vuit), NewC(Espases, Manilla), NewC(Espases, Sota), NewC(Espases, Cavall), NewC(Espases, Rei), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Tres), NewC(Bastos, Quatre), NewC(Bastos, Cinc), NewC(Bastos, Sis), NewC(Bastos, Set), NewC(Bastos, Vuit), NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, Cavall), NewC(Bastos, Rei), NewC(Copes, As), NewC(Copes, Dos), NewC(Copes, Tres), NewC(Copes, Quatre), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Copes, Set), NewC(Copes, Vuit), NewC(Copes, Manilla), NewC(Copes, Sota), NewC(Copes, Cavall), NewC(Copes, Rei)]


-----------------------------------
--- Implementació de la logica de la partida ---
-----------------------------------

{--- jugar: Inicialitza una partida ---}
jugar = do
  putStrLn "---------------------------------------------------------"
  putStrLn "--------------------- VeriButiHask ----------------------"
  putStrLn "---------------------------------------------------------"
  putStrLn "                              _____"
  putStrLn "  _____                _____ |6    |"
  putStrLn " |2    | _____        |5    || & & |"
  putStrLn " |  &  ||3    | _____ | & & || & & | _____"
  putStrLn " |     || & & ||4    ||  &  || & & ||7    |"
  putStrLn " |  &  ||     || & & || & & ||____9|| & & | _____"
  putStrLn " |____Z||  &  ||     ||____S|       |& & &||8    | _____"
  putStrLn "        |____E|| & & |              | & & ||& & &||9    |"
  putStrLn "               |____h|              |____L|| & & ||& & &|"
  putStrLn "                           _____           |& & &||& & &|"
  putStrLn "                   _____  |K  WW|          |____8||& & &|"
  putStrLn "           _____  |Q  ww| | o {)|                 |____6|"
  putStrLn "    _____ |J  ww| | o {(| |o o%%| _____"
  putStrLn "   |10 & || o {)| |o o%%| | |%%%||A _  |"
  putStrLn "   |& & &||o o% | | |%%%| |_%%%>|| ( ) |"
  putStrLn "   |& & &|| | % | |_%%%O|        |(_'_)|"
  putStrLn "   |& & &||__%%[|                |  |  |"
  putStrLn "   |___0I|                       |____V|"
  putStrLn " "
  putStrLn "---------------------------------------------------------"
  putStrLn "---------------------------------------------------------"
  putStrLn " "
  putStrLn "Entra una llavor per a generar la baralla: "
  llavor <- getLine
  putStrLn "S'ha generat la baralla. "
  putStrLn "Procedint a inicialitzar la partida..."
  putStrLn "Repartint les cartes..."
  let mans = crearMans(repartirCartes (shuffle baralla (read llavor::Int)))
  let ma1 = (mans !! 0)
  let ma2 = (mans !! 1)
  let ma3 = (mans !! 2)
  let ma4 = (mans !! 3)
  putStrLn "Cartes repartides als jugadors!"
  putStrLn " "
  putStrLn "La teva ma és la següent: "
  putStrLn " "
  print(ma1)
  putStrLn " "
  putStrLn "El primer jugador a tirar ets tu Jugador 1!"
  putStrLn " "
  putStrLn "---------------------------------"
  putStrLn "------   ESCOLLIR TRUMFU   ------"
  putStrLn "---------------------------------"
  putStrLn " "
  putStrLn "Escull un dels seguents Trumfus:"
  escollirTrumfu ma1 ma2 ma3 ma4 0 1 (0,0) (0,0)


{--- escollirTrumfu: S'encarrega de gestionar la logica d'escollit un Trumfu ---
  Gestiona l'E/S si es tracta d'un usuari o ho fa automatitzat si es el torn
  d'escollir de la IA.
-}
escollirTrumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada = do
  putStrLn "->> Oros, Bastos, Copes, Espases, Butifarra <<-"
  if escollidor == 0 then do --ESCULL USUARI
    trumfu <- getLine
    putStrLn " "
    if trumfu == "Oros" then do
      putStrLn "En aquesta partida el Trumfu escollit es Oros!"
      contrarTime Or ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
    else if trumfu == "Bastos" then do
      putStrLn "En aquesta partida el Trumfu escollit es Bastos!"
      contrarTime Ba ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
    else if trumfu == "Copes" then do
      putStrLn "En aquesta partida el Trumfu escollit es Copes!"
      contrarTime Co ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
    else if trumfu == "Espases" then do
      putStrLn "En aquesta partida el Trumfu escollit es Espases!"
      contrarTime Es ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
    else if trumfu == "Butifarra" then do
      putStrLn "En aquesta partida el Trumfu escollit es Butifarra!"
      contrarTime Bu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
    else do
      putStrLn "No es una opcio correcte, torna a escollir un  dels seguents Trumfus:"
      escollirTrumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
  else if escollidor == 1 then do -- ESCULL IA 2 EN FUNCIO DE LA SEVA MA
    let trumfu = iaEscullTrumfu ma2
    putStrLn ("En aquesta partida el Trumfu escollit es " ++ show(trumfu) ++"!")
    contrarTime trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
  else if escollidor == 2 then do -- ESCULL IA 3 EN FUNCIO DE LA SEVA MA
    let trumfu = iaEscullTrumfu ma3
    putStrLn ("En aquesta partida el Trumfu escollit es " ++ show(trumfu) ++"!")
    contrarTime trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada
  else do -- ESCULL IA 4 EN FUNCIO DE LA SEVA MA
    let trumfu = iaEscullTrumfu ma4
    putStrLn ("En aquesta partida el Trumfu escollit es " ++ show(trumfu) ++"!")
    contrarTime trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada


{--- contrarTime: S'encarrega de gestionar la logica de Contrar, Recontrar, Sant Viçenc i Barraca ---
  Gestiona l'E/S si es tracta d'un usuari o ho fa automatitzat si es tracta d'una
  decisio de la IA.
-}
contrarTime trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada = do
  putStrLn " "
  putStrLn "-----------------------------------------------------------"
  putStrLn "------   CONTRAR, RECONTRAR, SANT VICENÇ O BARRACA   ------"
  putStrLn "-----------------------------------------------------------"
  putStrLn " "
  if (escollidor == 0) || (escollidor == 2) then do -- O L'USUARI O LA PARELLA DE L'USARI HAN ESCOLLIT EL TRUMFU
    if esContra2 trumfu ma2 ma4 then do -- LA MAQUINA HA DE DECIDIR SI CONTRA O NO
      putStrLn "La parella formada per el jugador 2 i 4 creuen que poden rascar mes punts de la partida, Contren!"
      putStrLn "Jugadors 1 i 3, penseu fer alguna cosa al respecte? Recontreu?"
      putStrLn "(S/N)"
      let parellaVa = esRecontra1 trumfu ma3
      let joVaig = (unsafePerformIO hiVas)
      if parellaVa || joVaig then do -- SI L'USUARI O LA SEVA PARELLA HI VAN ES RECONTRA
        if parellaVa && joVaig then do
          putStrLn "La parella formada per el jugador 1 i 3 ho tenen molt clar i Recontren!!"
        else if parellaVa then do
          putStrLn "El jugador 3 ho te molt clar i Recontra!!"
        else do
          putStrLn "El jugador 1 ho te molt clar i Recontra!!"
        if (trumfu /= Bu) then do -- SI EL TRUMFU NO ES BUTIFARRA PODEM SEGUIR PER SANT VICENÇ
          if santVicens2 trumfu ma2 ma4 then do -- LA MAQUINA HA DE DECIDIR SI FER SANT VICENÇ O NO
            putStrLn "La parella formada per el jugador 2 i 4 van molt forts i canten Sant Vicenç!!!"
            putStrLn "Jugadors 1 i 3, us planteu? M'ha semblat escoltar Barraca?"
            putStrLn "(S/N)"
            let parellaVa = esBarraca1 trumfu ma3
            let joVaig = (unsafePerformIO hiVas)
            if parellaVa || joVaig then do -- SI L'USUARI O LA SEVA PARELLA HI VAN ES CANTA BARRACA
              if parellaVa && joVaig then do
                putStrLn "La parella formada per el jugador 1 i 3 tiren la casa per la finestra i canten BARRACA!!!!"
              else if parellaVa then do
                putStrLn "El jugador 3 tira la casa per la finestra i canta BARRACA!!!!"
              else do
                putStrLn "El jugador 1 tira la casa per la finestra i canta BARRACA!!!!"
              putStrLn " "
              putStrLn "Fins aqui hem arribat, procedim doncs a començar la ma amb un multiplicador de puntuacio x16!!"
              putStrLn "Prem INTRO per continuar..."
              dummy <- getLine
              mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 16
            else do -- NI L'USUARI NI LA SEVA PARELLA HAN CANTAT BARRACA
              putStrLn "La parella formada per el jugador 1 i 3  no ho han vist clar i han deixat passar la oportunitat de cantar Barraca..."
              putStrLn " "
              putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x8!"
              putStrLn "Prem INTRO per continuar..."
              dummy <- getLine
              mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 8
          else do -- LA MAQUINA NO HA FET SANT VICENÇ
            putStrLn "La parella formada per el jugador 1 i 3  no ho han vist clar i han deixat passar la oportunitat de cantar Sant Vicenç..."
            putStrLn " "
            putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x4!"
            putStrLn "Prem INTRO per continuar..."
            dummy <- getLine
            mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 4
        else do -- SI EL TRUMFU ES BUTIFARRA COMENÇEM LA PARTIDA RECONTRADA
          putStrLn "Com que el Trumfu es Butifarra, ja no es pot augmentar mes l'index de multiplicacio."
          putStrLn " "
          putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x8!"
          putStrLn "Prem INTRO per continuar..."
          dummy <- getLine
          mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 8
      else do -- NI L'USUARI NI LA SEVA PARELLA HAN CONTRAT
        putStrLn "La parella formada per el jugador 1 i 3  no ho han vist clar i han deixat passar la oportunitat de Recontrar..."
        if (trumfu /= Bu) then do -- SI EL TRUMFU ES BUTIFARRA ES SUMA 2 AL INDEX DE MULTIPLICACIO
          putStrLn " "
          putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x2!"
          putStrLn "Prem INTRO per continuar..."
          dummy <- getLine
          mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 2
        else do
          putStrLn " "
          putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x(2+2), ja que el Trumfu es Butifarra!"
          putStrLn "Prem INTRO per continuar..."
          dummy <- getLine
          mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 4
    else do -- LA MAQUINA NO HA CONTRAT
      putStrLn "La parella formada per el jugador 2 i 4 no ho han vist clar i han deixat passar la oportunitat de Contrar..."
      if (trumfu /= Bu) then do -- SI EL TRUMFU ES BUTIFARRA ES SUMA 2 AL INDEX DE MULTIPLICACIO
        putStrLn " "
        putStrLn "Procedim doncs a començar la ma sense multiplicadors de puntuacio."
        putStrLn "Prem INTRO per continuar..."
        dummy <- getLine
        mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 1
      else do
        putStrLn " "
        putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x(0+2), ja que el Trumfu es Butifarra!"
        putStrLn "Prem INTRO per continuar..."
        dummy <- getLine
        mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 2
  else do -- LA MAQUINA QUE NO ES PARELLA DE L'USUARI HA ESCOLLIT EL TRUMFU
    putStrLn "Jugadors 1 i 3, voleu contrar?"
    putStrLn "(S/N)"
    let parellaVa = esContra1 trumfu ma3
    let joVaig = (unsafePerformIO hiVas)
    if parellaVa || joVaig then do -- SI L'USUARI O LA SEVA PARELLA HI VAN ES CONTRA
      if parellaVa && joVaig then do
        putStrLn "La parella formada per el jugador 1 i 3 creuen que poden rascar mes punts de la partida i Contren!"
      else if parellaVa then do
        putStrLn "El jugador 3 creu que pot rascar mes punts de la partida i Contra!"
      else do
        putStrLn "El jugador 1 creu que pot rascar mes punts de la partida i Contra!"
      if esRecontra2 trumfu ma2 ma4 then do -- LA MAQUINA HA DE DECIDIR SI RECONTRA O NO
        putStrLn "La parella formada per el jugador 2 i 4 ho tenen molt clar i recontren!!"
        if (trumfu /= Bu) then do -- SI EL TRUMFU NO ES BUTIFARRA PODEM SEGUIR PER SANT VICENÇ
          putStrLn "Jugadors 1 i 3, penseu fer alguna cosa al respecte? M'ha semblat escoltar Sant Vicenç?"
          putStrLn "(S/N)"
          let parellaVa = santVicens1 trumfu ma3
          let joVaig = (unsafePerformIO hiVas)
          if parellaVa || joVaig then do -- SI L'USUARI O LA SEVA PARELLA HI VAN ES CANTA SANT VICENÇ
            if parellaVa && joVaig then do
              putStrLn "La parella formada per el jugador 1 i 3 van molt forts i canten Sant Vicenç!!!"
            else if parellaVa then do
              putStrLn "El jugador 3 va molt fort i canta Sant Vicenç!!!"
            else do
              putStrLn "El jugador 1 va molt fort i canta Sant Vicenç!!!"
            if esBarraca2 trumfu ma2 ma4 then do -- LA MAQUINA HA DE DECIDIR SI CANTA BARRACA O NO
              putStrLn "La parella formada per el jugador 2 i 4 tiren la casa per la finestra i canten BARRACA!!!!"
              putStrLn " "
              putStrLn "Fins aqui hem arribat, procedim doncs a començar la ma amb un multiplicador de puntuacio x16!!"
              putStrLn "Prem INTRO per continuar..."
              dummy <- getLine
              mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 16
            else do -- LA MAQUINA NO HA CANTAT BARRACA
              putStrLn "La parella formada per el jugador 2 i 4 no ho han vist clar i han deixat passar la oportunitat de cantar Barraca..."
              putStrLn " "
              putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x8!"
              putStrLn "Prem INTRO per continuar..."
              dummy <- getLine
              mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 8
          else do -- NI L'USUARI NI LA SEVA PARELLA HAN CANTAT SANT VICENÇ
            putStrLn "La parella formada per el jugador 1 i 3 no ho han vist clar i han deixat passar la oportunitat de cantar Sant Vicenç..."
            putStrLn " "
            putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x4!"
            putStrLn "Prem INTRO per continuar..."
            dummy <- getLine
            mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 4
        else do -- SI EL TRUMFU ES BUTIFARRA COMENÇEM LA PARTIDA RECONTRADA
          putStrLn "Com que el Trumfu es Butifarra, ja no es pot augmentar mes l'index de multiplicacio."
          putStrLn " "
          putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x8!"
          putStrLn "Prem INTRO per continuar..."
          dummy <- getLine
          mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 8
      else do -- LA MAQUINA NO RECONTRA
        putStrLn "La parella formada per el jugador 2 i 4 no ho han vist clar i han deixat passar la oportunitat de Recontrar..."
        if (trumfu /= Bu) then do -- SI EL TRUMFU ES BUTIFARRA ES SUMA 2 AL INDEX DE MULTIPLICACIO
          putStrLn " "
          putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x2!"
          putStrLn "Prem INTRO per continuar..."
          dummy <- getLine
          mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 2
        else do
          putStrLn " "
          putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x(2+2), ja que el Trumfu es Butifarra!"
          putStrLn "Prem INTRO per continuar..."
          dummy <- getLine
          mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 4
    else do -- NI L'USUARI NI LA SEVA PARELLA HAN CONTRAT
      putStrLn "La parella formada per el jugador 1 i 3 no ho han vist clar i han deixat passar la oportunitat de Contrar..."
      if (trumfu /= Bu) then do -- SI EL TRUMFU ES BUTIFARRA ES SUMA 2 AL INDEX DE MULTIPLICACIO
        putStrLn " "
        putStrLn "Procedim doncs a començar la ma sense multiplicadors de puntuacio."
        putStrLn "Prem INTRO per continuar..."
        dummy <- getLine
        mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 1
      else do
        putStrLn " "
        putStrLn "Procedim doncs a començar la ma amb un multiplicador de puntuacio x(0+2), ja que el Trumfu es Butifarra!"
        putStrLn "Prem INTRO per continuar..."
        dummy <- getLine
        mainLoop trumfu ma1 ma2 ma3 ma4 escollidor tirador puntuacio puntuacioAcomulada 2


{--- hiVas: Suport per el contrarTime ---
  Gestiona l'E/S de les decisions que pren l'Usuari tot controlant els
  errors d'entrada
-}
hiVas = do
  eleccio <- getLine
  if eleccio == "S" || eleccio == "s" then do
    return True
  else if eleccio == "N" || eleccio == "n" then do
    return False
  else do
    putStrLn "No es una opcio valida, torna a respondre:"
    hiVas


{--- mainLoop: S'encarrega del loop principal de la partida ---
  En aquest bloc es gestionen les tirades fins que els jugadors es queden sense
  cartes a la ma. Quan aixo passa es recompten els punts i es tornen a
  repartir cartes o be s'acaba la partida ja que algun de les parelles haura
  arribat als 101 punts.
-}
mainLoop trumfu ma1 ma2 ma3 ma4 oldEscollidor tirador oldPuntuacio puntuacioAcomulada multiplicador = do
  if ((fst puntuacioAcomulada) == 0) && ((snd puntuacioAcomulada) == 0) then do -- PROMT INICIAL DEL LOOP
    putStrLn " "
    putStrLn "------------------------------------"
    putStrLn "------   COMENÇA LA PARTIDA   ------"
    putStrLn "------------------------------------"
    putStrLn " "
  else if (length(getCartesMa ma1)) == 12 then do -- PROMT D'HAVER REPARTIT UN ALTRE COP
    putStrLn " "
    putStrLn "--------------------------------------------------------"
    putStrLn "------   CONTINUEM LA PARTIDA AMB LES NOVES MANS  ------"
    putStrLn "--------------------------------------------------------"
    putStrLn " "
  else do -- PROMT DE NOVA RONDA
    putStrLn " "
    putStrLn "-----------------------------------------------"
    putStrLn "-----------------  NOVA RONDA -----------------"
    putStrLn "-----------------------------------------------"
    putStrLn " "

  --TORN 1
  let tornTirada1 = tirador
  let llistaJugadors = [ma1,ma2,ma3,ma4]
  putStrLn ("--- TORN DEL JUGADOR "++show(tornTirada1+1)++" ---")
  let cartaTirada1 = unsafePerformIO(realitzarTirada (llistaJugadors !! tornTirada1) tornTirada1 [] trumfu)
  putStrLn (show cartaTirada1)
  putStrLn ("El jugador "++show(tornTirada1+1)++" ha tirat la següent carta: "++show(cartaTirada1))
  putStrLn ("Actualment sobre la taula hi han les següents cartes: "++show(cartaTirada1))
  putStrLn " "

  --TORN 2
  let tornTirada2 = mod (tornTirada1 + 1) 4
  putStrLn ("--- TORN DEL JUGADOR "++show(tornTirada2+1)++" ---")
  let cartaTirada2 = unsafePerformIO(realitzarTirada (llistaJugadors !! tornTirada2) tornTirada2 [cartaTirada1] trumfu)
  putStrLn (show cartaTirada2)
  putStrLn ("El jugador "++show(tornTirada2+1)++" ha tirat la següent carta: "++show(cartaTirada2))
  putStrLn ("Actualment sobre la taula hi han les següents cartes: "++show([cartaTirada1,cartaTirada2]))
  putStrLn " "

  --TORN 3
  let tornTirada3 = mod (tornTirada2 + 1) 4
  putStrLn ("--- TORN DEL JUGADOR "++show(tornTirada3+1)++" ---")
  let cartaTirada3 = unsafePerformIO(realitzarTirada (llistaJugadors !! tornTirada3) tornTirada3 [cartaTirada1,cartaTirada2] trumfu)
  putStrLn (show cartaTirada3)
  putStrLn ("El jugador "++show(tornTirada3+1)++" ha tirat la següent carta: "++show(cartaTirada3))
  putStrLn ("Actualment sobre la taula hi han les següents cartes: "++show([cartaTirada1,cartaTirada2,cartaTirada3]))
  putStrLn " "

  --TORN 4
  let tornTirada4 = mod (tornTirada3 + 1) 4
  putStrLn ("--- TORN DEL JUGADOR "++show(tornTirada4+1)++" ---")
  let cartaTirada4 = unsafePerformIO(realitzarTirada (llistaJugadors !! tornTirada4) tornTirada4 [cartaTirada1,cartaTirada2,cartaTirada3] trumfu)
  putStrLn (show cartaTirada4)
  putStrLn ("El jugador "++show(tornTirada4+1)++" ha tirat la següent carta: "++show(cartaTirada4))
  putStrLn ("Actualment sobre la taula hi han les següents cartes: "++show([cartaTirada1,cartaTirada2,cartaTirada3,cartaTirada4]))
  putStrLn " "

  --ESTAT DE LA PARTIDA AL FINAL DE LA RONDA
  putStrLn "-----------------------------"
  putStrLn "----- RESUM FI DE RONDA -----"
  putStrLn "-----------------------------"
  let basa = (NewB (tornTirada1,cartaTirada1,cartaTirada2,cartaTirada3,cartaTirada4))
  let guanyador = jugadorGuanyaBasa basa trumfu
  putStrLn ("El jugador que ha guanyat la basa actual ha estat el Jugador "++show(guanyador+1))

  --COMPROBAR LA PUNTUACIO QUE HI HA EN LA BASA
  let puntsGuanyats = (((punts (cartesBasa basa))+1))
  if (guanyador == 0) || (guanyador == 2) then do
    putStrLn ("La parella formada per el Jugador 1 i el Jugador 3 ha guanyat "++show(puntsGuanyats)++" punts en aquesta basa!")
  else do
    putStrLn ("La parella formada per el Jugador 2 i el Jugador 4 ha guanyat "++show(puntsGuanyats)++" punts en aquesta basa!")

  --CALCULAR LA PUNTUACIO QUE TENEN LES PARELLES AMB LA MA ACTUAL
  let newPuntuacio = (puntsTuples puntsGuanyats guanyador oldPuntuacio)

  --TREURE LES CARTES TIRADES DE LES MANS DELS JUGADORS I CREACIO DE LES NOVES MANS
  let cartesTirades = [cartaTirada1,cartaTirada2,cartaTirada3,cartaTirada4]
  let novesMans = eliminarCartesTiradesDeLesMans llistaJugadors cartesTirades tornTirada1
  let newMa1 = (novesMans !! 0)
  let newMa2 = (novesMans !! 1)
  let newMa3 = (novesMans !! 2)
  let newMa4 = (novesMans !! 3)

  --MOSTRAR LES PUNTUACIONS QUE ES TENEN AMB LA MA ACTUAL
  putStrLn "Les puntuacions actuals son: "
  putStrLn ("Parella 1 (J1 i J3): "++show(fst newPuntuacio))
  putStrLn ("Parella 2 (J2 i J4): "++show(snd newPuntuacio))
  putStrLn "Prem INTRO per continuar amb la seguent ronda..."
  dummy <- getLine

  --FINALITZACIO DE LA RONDA DE TIRADES
  if (getCartesMa newMa1) == [] then do --COMPROBAR SI S'HAN TIRAT TOTES LES CARTES SI ES QUE SI ES TORNA A BARREJAR LA BARALLA, REPARTIR LES CARTES I ES TORNA A TRIAR EL TRUMFU
    putStrLn " "
    putStrLn "-----------------------------------"
    putStrLn "------   RECOMPTE DE PUNTS   ------"
    putStrLn "-----------------------------------"
    putStrLn " "
    putStrLn "S'han jugat totes les cartes, procedirem a realitzar el recompte de punts..."
    putStrLn "En aquesta ma les parelles han aconseguit les següents puntuacions:"
    putStrLn ("Parella 1 (J1 i J3): "++show(fst newPuntuacio))
    putStrLn ("Parella 2 (J2 i J4): "++show(snd newPuntuacio))
    let newEscollidor = mod (oldEscollidor + 1) 4
    if (fst newPuntuacio) > (snd newPuntuacio) then do --PARELLA 1 GUANYA LA MA ACTUAL
      let tmp = ((div ((fst newPuntuacio)-(snd newPuntuacio)) 2)*multiplicador) + (fst puntuacioAcomulada)
      if tmp >= 101 then do--PARELLA 1 GUANYA LA PARTIDA
        putStrLn ("Un cop fet el recomte i repartits els punts, ja tenim una parella guanyadora!!!")
        putStrLn ("Amb una puntuacio de "++show(tmp)++" la parella 1 (J1 i J3) han guanyat la partida!!!")
        putStrLn ("La parella 2 (J2 i J4) ha perdut amb una puntuació de "++show(snd puntuacioAcomulada))
        putStrLn "Fi de l'execucio"
      else do--TORNEM A REPARTIR LES CARTES
        putStrLn " "
        putStrLn "Les puntuacions actuals de les dues parelles son:"
        putStrLn ("Parella 1 (J1 i J3): "++show(tmp))
        putStrLn ("Parella 2 (J2 i J4): "++show(snd puntuacioAcomulada))
        putStrLn " "
        putStrLn "Com que cap de les dues parelles ha arribat als 101 punts,"
        putStrLn "procedim a repartir novament les cartes i a escollir un nou Trumfu!"
        putStrLn "Prem INTRO per continuar..."
        dummy <- getLine
        tornarRepartirCartes newEscollidor (tmp,(snd puntuacioAcomulada))
    else if (fst newPuntuacio) < (snd newPuntuacio) then do --PARELLA 2 GUANYA LA MA ACTUAL
      let tmp = ((div ((snd newPuntuacio)-(fst newPuntuacio)) 2)*multiplicador) + (snd puntuacioAcomulada)
      if tmp >= 101 then do--PARELLA 2 GUANYA LA PARTIDA
        putStrLn ("Un cop fet el recomte i repartits els punts, ja tenim una parella guanyadora!!!")
        putStrLn ("Amb una puntuacio de "++show(tmp)++" la parella 1 (J1 i J3) han guanyat la partida!!!")
        putStrLn ("La parella 2 (J2 i J4) ha perdut amb una puntuació de "++show(snd puntuacioAcomulada))
        putStrLn "Fi de l'execucio"
      else do--TORNEM A REPARTIR LES CARTES
        putStrLn " "
        putStrLn "Les puntuacions actuals de les dues parelles son:"
        putStrLn ("Parella 1 (J1 i J3): "++show(fst puntuacioAcomulada))
        putStrLn ("Parella 2 (J2 i J4): "++show(tmp))
        putStrLn " "
        putStrLn "Com que cap de les dues parelles ha arribat als 101 punts,"
        putStrLn "procedim a repartir novament les cartes i a escollir un nou Trumfu!"
        putStrLn "Prem INTRO per continuar..."
        dummy <- getLine
        tornarRepartirCartes newEscollidor ((fst puntuacioAcomulada), tmp)
    else do --EMPAT EN LA MA ACTUAL
      putStrLn " "
      putStrLn "Les puntuacions actuals de les dues parelles son:"
      putStrLn ("Parella 1 (J1 i J3): "++show(fst puntuacioAcomulada))
      putStrLn ("Parella 2 (J2 i J4): "++show(snd puntuacioAcomulada))
      putStrLn " "
      putStrLn "Com que cap de les dues parelles ha arribat als 101 punts,"
      putStrLn "procedim a repartir novament les cartes i a escollir un nou Trumfu!"
      putStrLn "Prem INTRO per continuar..."
      dummy <- getLine
      tornarRepartirCartes newEscollidor puntuacioAcomulada
  else do --ELS JUGADORS ENCARA TENEN CARTES A LA MA, SEGUIM JUGANT
    mainLoop trumfu newMa1 newMa2 newMa3 newMa4 oldEscollidor guanyador newPuntuacio puntuacioAcomulada multiplicador


{--- tornarRepartirCartes: S'encarrega de tornar a repartir les cartes---
  En aquest bloc es gestionen la generacio d'una nova baralla aixi com repartirla
  als jugadors de la partida. Tambe gestiona ser el proxim a tirar.
-}
tornarRepartirCartes newEscollidor puntuacioAcomulada = do
  putStrLn " "
  putStrLn "----------------------------------"
  putStrLn "------   REPARTINT CARTES   ------"
  putStrLn "----------------------------------"
  putStrLn " "
  putStrLn "Entra una llavor per a generar la baralla: "
  llavor <- getLine
  putStrLn "S'ha generat la baralla."
  putStrLn "Procedint a inicialitzar la partida..."
  putStrLn "Repartint les cartes..."
  let mans = crearMans(repartirCartes (shuffle baralla (read llavor::Int)))
  let ma1 = (mans !! 0)
  let ma2 = (mans !! 1)
  let ma3 = (mans !! 2)
  let ma4 = (mans !! 3)
  putStrLn "Cartes repartides als jugadors!"
  putStrLn " "
  putStrLn "La teva ma és la següent: "
  putStrLn " "
  print(ma1)
  putStrLn " "
  putStrLn "El primer jugador a tirar ets tu!"
  putStrLn " "
  putStrLn "---------------------------------"
  putStrLn "------   ESCOLLIR TRUMFU   ------"
  putStrLn "---------------------------------"
  putStrLn " "
  putStrLn "Escull un dels seguents Trumfus:"
  escollirTrumfu ma1 ma2 ma3 ma4 newEscollidor (mod (newEscollidor + 1) 4) (0,0) puntuacioAcomulada


{--- realitzarTirada: S'encarrega de gestionar la tirada---
  En aquest bloc es gestionen l'E/S de l'usuari a l'hora de realitzar la tirada
  i gestiona tambe les tirades de la maquina.
-}
realitzarTirada ma jugador cartesJugades trumfu = do
  if jugador == 0 then do --TIRADA DE L'USUARI
    putStrLn "Actualment tens les següents cartes a la ma: "
    print(ma)
    putStrLn "D'aquestes només es legal tirar les següents: "
    print(realJugadesPossibles ma trumfu cartesJugades)
    putStrLn "Quina carta jugaras? (Tinguen en compte que la primera carta de la llista es la 0)"
    return (unsafePerformIO(escollirCarta (realJugadesPossibles ma trumfu cartesJugades)))
  else do --TIRADA DE LA IA
    return (escollirCartaIA ma cartesJugades trumfu)


{--- escollirCarta: S'encarrega de gestionar l'eleccio de la carta---
  En aquest bloc es gestionen l'E/S de l'usuari a l'hora de realitzar l'eleccio
  de la carta a tirar
-}
escollirCarta cartes = do
  input <- getLine
  if (length input) > 2 then do
    putStrLn "ERROR: Index no valid. Torna a escollir."
    escollirCarta cartes
  else if (length input) == 2 then do
    if (isDigit(input !! 0)) && (isDigit(input !! 1)) then do
      let index = (read input::Int)
      if (index + 1) > length(cartes) then do
        putStrLn "ERROR: Index no valid. Torna a escollir."
        escollirCarta cartes
      else do
        return (cartes!!index)
    else do
      putStrLn "ERROR: Index no valid. Torna a escollir."
      escollirCarta cartes
  else do
    if (isDigit(input !! 0)) then do
      let index = (read input::Int)
      if (index + 1) > length(cartes) then do
        putStrLn "ERROR: Index no valid. Torna a escollir."
        escollirCarta cartes
      else do
        return (cartes!!index)
    else do
    putStrLn "ERROR: Index no valid. Torna a escollir."
    escollirCarta cartes


--------------------------
--- Funcions Auxiliars ---
--------------------------

{--- puntsTuples: Retorna una tuple amb les puntuacions de les dues parelles ---
  Donat uns punta guanyats, el jugador guanyador i la puntuacio que ja tenien
  retorna una tupla amb les puntuacions actualitzades.
-}
puntsTuples :: Int -> Int -> (Int,Int) -> (Int,Int)
puntsTuples pguanyats 0 p = ((fst(p)+pguanyats),(snd(p)))
puntsTuples pguanyats 2 p = ((fst(p)+pguanyats),(snd(p)))
puntsTuples pguanyats 1 p = ((fst(p)),((snd(p))+pguanyats))
puntsTuples pguanyats 3 p = ((fst(p)),((snd(p))+pguanyats))


{--- teManillaOAsDeTrunfus: Retorna una boolea si a la ma hi ha una manilla o
   o un as del pal del trumfu
-}
teManillaOAsDeTrunfus :: Trumfu -> Ma -> Bool
teManillaOAsDeTrunfus t (NewM l)
  | ([x | x<-l, ((trumfu2Pal t == (getPal x)) && ((getTipus x == As) || (getTipus x == As)))] == []) = False
  | otherwise = True


-- TODO: documentar aquests mètodes: Pol
esRecontra1 :: Trumfu -> Ma -> Bool
esRecontra1 trumfu maX = if (length (cartesPalMa maX (trumfu2Pal trumfu)) >=6) || ((length (cartesPalMa maX (trumfu2Pal trumfu)) >=4) && (teManillaOAsDeTrunfus trumfu maX)) then True else False
esContra2 :: Trumfu -> Ma -> Ma -> Bool
esContra2 trumfu maX maY = if (length (cartesPalMa maX (trumfu2Pal trumfu)) >=4) || (length (cartesPalMa maY (trumfu2Pal trumfu)) >=4) ||  ((length (cartesPalMa maX (trumfu2Pal trumfu)) >=3) && (teManillaOAsDeTrunfus trumfu maX)) || ((length (cartesPalMa maY (trumfu2Pal trumfu)) >=3) && (teManillaOAsDeTrunfus trumfu maY)) then True else False
santVicens2 :: Trumfu -> Ma -> Ma -> Bool
santVicens2 trumfu maX maY = if ((length (cartesPalMa maX (trumfu2Pal trumfu)) >=7) || (length (cartesPalMa maY (trumfu2Pal trumfu)) >=7)) then True else False
santVicens1 :: Trumfu -> Ma -> Bool
santVicens1 trumfu maX = if (length (cartesPalMa maX (trumfu2Pal trumfu)) >=7) then True else False
esBarraca1 :: Trumfu -> Ma -> Bool
esBarraca1 trumfu maX = if length (cartesPalMa maX (trumfu2Pal trumfu)) >=7 then True else False
esContra1 :: Trumfu -> Ma -> Bool
esContra1 trumfu maX = if (length (cartesPalMa maX (trumfu2Pal trumfu)) >=4) || (length (cartesPalMa maX (trumfu2Pal trumfu)) >=3) then True else False
esRecontra2 :: Trumfu -> Ma -> Ma -> Bool
esRecontra2 trumfu maX maY = if (length (cartesPalMa maX (trumfu2Pal trumfu)) >=6) || ((length (cartesPalMa maX (trumfu2Pal trumfu)) >=4) && (teManillaOAsDeTrunfus trumfu maX)) || if (length (cartesPalMa maX (trumfu2Pal trumfu)) >=6) || ((length (cartesPalMa maY (trumfu2Pal trumfu)) >=4) && (teManillaOAsDeTrunfus trumfu maY)) then True else False then True else False
esBarraca2 :: Trumfu -> Ma -> Ma -> Bool
esBarraca2 trumfu maX maY = if length (cartesPalMa maX (trumfu2Pal trumfu)) >=7 || length (cartesPalMa maY (trumfu2Pal trumfu)) >=7 then True else False
iaEscullTrumfu :: Ma -> Trumfu
iaEscullTrumfu llista
  | (nOros == nBastos) && (nOros == nCopes) && (nEspases == nOros) = Bu
  | (nOros >= nBastos) && (nOros >= nEspases) && (nOros >= nCopes) = Or
  | (nBastos >= nOros) && (nBastos >= nEspases) && (nBastos >= nCopes) = Ba
  | (nEspases >= nBastos) && (nEspases >= nOros) && (nEspases >= nCopes) = Es
  | (nCopes >= nBastos) && (nCopes >= nEspases) && (nCopes >= nOros) = Co
  where
    nOros = length (cartesPalMa llista Oros)
    nBastos = length (cartesPalMa llista Bastos)
    nEspases = length (cartesPalMa llista Copes)
    nCopes = length (cartesPalMa llista Espases)
--

escollirCartaIA :: Ma -> [Carta] -> Trumfu -> Carta
escollirCartaIA ma cartes trumfu = (realJugadesPossibles ma trumfu cartes) !! 0

getCartesMa :: Ma -> [Carta]
getCartesMa (NewM x) = x

crearMans :: [[Carta]] -> [Ma]
crearMans llista = [(NewM (llista !! 0)),(NewM (llista !! 1)),(NewM (llista !! 2)),(NewM (llista !! 3))]

{- repartirCartes
  Input: Una llista de Cartes
  Output: Reparteix la llista inicial en 4 llistes.
-}
repartirCartes :: [Carta] -> [[Carta]]
repartirCartes (w:x:y:z:xs) = repartirCartes' [[w],[x],[y],[z]] xs

{- repartirCartes'
  Input: #TODO Adrià
  Output: #TODO Adrià
-}
repartirCartes' :: [[Carta]] -> [Carta] -> [[Carta]]
repartirCartes' llista [] = llista
repartirCartes' llista (w:x:y:z:xs) = repartirCartes' [((llista !! 0) ++ [w]), ((llista !! 1) ++ [x]), ((llista !! 2) ++ [y]), ((llista !! 3) ++ [z])] xs


{- shuffle
  Input: Una llista de cartes
  Output: La llista entrada barrejada
-}
shuffle :: [Carta] -> Int -> [Carta]
shuffle (carta:[]) llavor = [carta]
shuffle (carta:cs) llavor = (cartes !! rand) : shuffle deleteRandomCard llavor
    where
        cartes = (carta:cs)
        rand = mod (unsafePerformIO (getStdRandom (randomR (0, (length cartes)-1))) * llavor) (length cartes)
        deleteRandomCard = (fst $ splitAt rand cartes) ++ (tail $ snd $ splitAt rand cartes)

{- getCardValue
  Input: Un TipusCarta
  Output: Valor numèric corresponent al tipus de la carta
-}
getCardValue :: TipusCarta ->  Int
getCardValue tipus = case tipus of
      Manilla -> 12
      As -> 11
      Rei -> 10
      Cavall -> 9
      Sota -> 8
      Vuit -> 7
      Set -> 6
      Sis -> 5
      Cinc -> 4
      Quatre -> 3
      Tres -> 2
      Dos -> 1

{- getCardPunctuation
  Input: El TipusCarta
  Output: El valor numèric corresponent a la puntuació d'aquest TipusCarta
-}
getCardPunctuation :: TipusCarta ->  Int
getCardPunctuation tipus = case tipus of
      Sota -> 1
      Cavall -> 2
      Rei -> 3
      As -> 4
      Manilla -> 5
      otherwise -> 0

{- punts
  Input: Conjunt de cartes
  Output: Puntuació del conjunt de cartes
-}
punts :: [Carta] -> Int
punts [] = 0
punts ((NewC (pal, tipus)):cv) = (getCardPunctuation tipus) + (punts cv)

{- cartesGuanyades
  Input: Donat un trumfu, un conjunt de cartes i el numero de tirador inicial.
  Output: Retorna una tupla que conté les cartes que ha guanyat cada grup
-}
cartesGuanyades :: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
cartesGuanyades trumfu [] _ = ([],[])
cartesGuanyades trumfu (carta:xs) tirador
  | (length (carta:xs)) == 4  = if mod _guanyador 2 == 0 then (fst _cas_base ++ _cartes_guanyades, snd _cas_base) else (fst _cas_base, snd _cas_base ++ _cartes_guanyades)
  | mod (length (carta:xs)) 4 == 0 = if mod _guanyador 2 == 0 then (fst _crida_recursiva ++ _cartes_guanyades, snd _crida_recursiva) else (fst _crida_recursiva, snd _crida_recursiva ++ _cartes_guanyades)
  | otherwise = error "Error desconegut"
  where
    _cas_base = (cartesGuanyades trumfu [] 0)
    _crida_recursiva = cartesGuanyades trumfu (drop 4 (carta:xs)) _guanyador
    _guanyador = jugadorGuanyaBasa _baseActual trumfu
    _baseActual = (NewB (tirador, carta, xs !! 0, xs !! 1, xs !! 2))
    _cartes_guanyades = cartesBasa (NewB (_guanyador, carta, xs !! 0, xs !! 1, xs !! 2))

{-
  Input: Les mans dels jugadors, un trumfu, les cartes tirades en tota la
         partida, i el nombre del primer jugador en tirar.
  Output: Retorna una tupla amb la puntuació dels dos equips si no hi ha hagut
          trampa. Si no, retorna Nothing.
-}
puntsParelles :: [Ma] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
puntsParelles mans t cartes jugador = Just (punts $ fst _cartes_guanyades, punts $ snd _cartes_guanyades)
  where
    _cartes_guanyades = cartesGuanyades t cartes jugador

{- getPal
  Input: Una Carta.
  Output: Pal de la Carta.
-}
getPal :: Carta -> Pal
getPal (NewC (pal, tipus)) = pal

{- getTipus
  Input: Una Carta.
  Output: Tipus de la carta.
-}
getTipus :: Carta -> TipusCarta
getTipus (NewC (pal, tipus)) = tipus

{- cartesPalBasa
  Input: Donada una Basa i el Pal
  Output: Les cartes d'aquest Pal en la Basa
-}
cartesPalBasa :: Basa -> Pal -> [Carta]
cartesPalBasa (NewB (j,w,x,y,z)) pal = [x | x<-[w,x,y,z], (pal == (getPal x))]

{- cartesPalMa
   Input: Una Ma i un Pal
   Output: Retorna les cartes de la Mà que són del Pal
-}
cartesPalMa :: Ma -> Pal -> [Carta]
cartesPalMa (NewM llista) pal= [x | x<-llista, (pal == (getPal x))]

{- cartesPal
  Input: Un conjunt de Cartes i un Pal
  Output: Conjunt de Cartes que són del Pal especificat
-}
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal llista pal = [x | x<-llista, (pal == (getPal x))]

{- basa1
  Input: Una basa
  Output: Retorna la primera carta de la basa
-}
basa1 :: Basa -> Carta
basa1 (NewB (_,w,_,_,_)) = w

{- basa2
  Input: Una basa
  Output: Retorna la segona carta de la basa
-}
basa2 :: Basa -> Carta
basa2 (NewB (_,_,x,_,_)) = x

{- basa3
  Input: Una basa
  Output: Retorna la tercera carta de la basa
-}
basa3 :: Basa -> Carta
basa3 (NewB (_,_,_,y,_)) = y

{- basa4
  Input: Una basa
  Output: Retorna la quarta carta de la basa
-}
basa4 :: Basa -> Carta
basa4 (NewB (_,_,_,_,z)) = z

{- iniciadorBasa
  Input: Una basa
  Output: Retorna el jugador que tira la primera carta de la basa
-}
iniciadorBasa :: Basa -> Int
iniciadorBasa (NewB (j,_,_,_,_)) = j

{- cartesBasa
  Input: Una basa
  Output: Retorna una llista que conté les cartes de la basa
-}
cartesBasa :: Basa -> [Carta]
cartesBasa (NewB (j,w,x,y,z)) = [w,x,y,z]

{- palGuanyadorBasa
  Input:
  Output: Retorna una llista que conté les cartes de la basa
-}
palGuanyadorBasa :: Basa -> Trumfu -> Pal
palGuanyadorBasa b t
  | ((cartesPalBasa b (trumfu2Pal t)) /= []) = (trumfu2Pal t)
  | otherwise = (getPal (basa1 b))

{- trumfu2Pal
  Input: Un trumfu
  Output: Converteix el tipus trumfu al tipus Pal
-}
trumfu2Pal :: Trumfu -> Pal
trumfu2Pal trumfu = case trumfu of
  Or -> Oros
  Ba -> Bastos
  Co -> Copes
  Es -> Espases
  Bu -> error "No és un Pal"


{- pal2Trumfu
  Input: Un Pal
  Output: Converteix el tipus pal al tipus Trumfu
-}
pal2Trumfu :: Pal -> Trumfu
pal2Trumfu pal = case pal of
  Oros  -> Or
  Bastos -> Ba
  Copes -> Co
  Espases -> Es

{- jugadorGuanyaBasa
  Input: La basa i el trumfo
  Output: Retorna el jugador que ha guanyat la basa
-}
jugadorGuanyaBasa :: Basa -> Trumfu -> Int
jugadorGuanyaBasa b t = tiradorCarta (cartaGuanyadora (basa1 b) (cartesBasa b) (palGuanyadorBasa b t)) b

{- cartaGuanyadora
  Input: #TODO Adria
  Output: #TODO Adria
-}
cartaGuanyadora :: Carta -> [Carta] -> Pal -> Carta
cartaGuanyadora c [] p = c
cartaGuanyadora c (x:xs) p = if (mata c x p) == c then cartaGuanyadora c xs p else cartaGuanyadora x xs p

{- mata
  Input: Dos cartes tirades i el al de la partida
  Output: Retorna la carta que "mata" a l'altre
-}
mata :: Carta -> Carta -> Pal -> Carta
mata c1 c2 p
  | (getPal c1 == p) && (getPal c2 /= p) = c1
  | (getPal c1 /= p) && (getPal c2 == p) = c2
  | c1 >= c2 = c1
  | c2 > c1 = c2


{- tiradorCarta
  Input: La carta tirada a la Basa actual
  Output: Retorna el jugador que ha tirat la carta
-}
tiradorCarta :: Carta -> Basa -> Int
tiradorCarta c b
  | (c == (basa1 b)) = (iniciadorBasa b)
  | (c == (basa2 b)) = mod ((iniciadorBasa b)+1) 4
  | (c == (basa3 b)) = mod ((iniciadorBasa b)+2) 4
  | (c == (basa4 b)) = mod ((iniciadorBasa b)+3) 4

-- #TODO Erase that
--jugadesPossibles:: Ma->Trumfu->Basa->[Carta]
--jugadesPossibles (NewM llista) t b

{- filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal
  Input: #TODO Pol
  Output: #TODO Pol
-}
filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal :: [Carta] -> Carta -> Trumfu -> [Carta]
filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal l c t
  | (getPal c == trumfu2Pal t) && (getPal (head l) /= trumfu2Pal t) = l
  | (filter (>c) l == []) = l
  | otherwise = (filter (>c) l)

{- filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes
  Input: #TODO Pol
  Output: #TODO Pol
-}
filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes :: [Carta] -> Carta -> Trumfu -> [Carta]
filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes l c t
  | ([x | x<-l, (trumfu2Pal t == (getPal x))] == []) = l
  | otherwise = [x | x<-l, (trumfu2Pal t == (getPal x))]

{- quiEstaGuanyant
  Input: #TODO Pol
  Output: #TODO Pol
-}
-- Donat un enter (mida llista 2 o 3), una llista de cartes i el trumfu retorna el jugador segons ordre de tirada que esta guanyant
quiEstaGuanyant :: Int -> [Carta] -> Trumfu -> Int
quiEstaGuanyant m (x:xs) t
  | (m == 2) && (getPal x == getPal (head xs)) = if x > head xs then 1 else 2
  | (m == 2) && (getPal x /= getPal (head xs)) = if (getPal x == trumfu2Pal t) && (getPal (head xs) /= trumfu2Pal t) then 1
                                                 else if (getPal (head xs) == trumfu2Pal t) && (getPal x /= trumfu2Pal t) then 2
                                                 else 1
  | (m == 3) && (quiEstaGuanyant 2 (take 2 (x:xs)) t == 1) = if quiEstaGuanyant 2 (x:(tail (xs))) t == 1 then 1 else 3
  | (m == 3) && (quiEstaGuanyant 2 (take 2 (x:xs)) t == 2) = if quiEstaGuanyant 2 xs t == 1 then 2 else 3


-- Ma: ma del jugador  -- Trumfu: pal dominant de la partida -- [Carta] cartes de la base pot ser buida -- Return [Carta] possibles cartes
-- Si cartes basa buida podem tirar qualsevol
-- Si cartes basa size 1 (obligats a matar si podem):
        -- Si tenim del mateix pal i podem matar qualsevol que pugui matar
        -- si fallem tirem qualsevol trumfu si tenim
        -- si tampoc tenim trumfu tirem qualsevol
-- Si cartes basa size 2 hem de mirar si el company [0] esta guanyant o no:
        -- Si company esta guanyant podem tirar qualsevol carta del mateix pal que a tirat i si fallem qualsevol altre
        -- altrament
            -- si [1] no ha fallat hem de matar si podem amb cartes del mateix pal sino hem de tirar trumfu si tenim sino qualsevol
            -- si [1] ha fallat (amb trumfu) i tenim encara cartes del pal iniciador podem tirar qualsevol del pal
            -- Si [1] ha fallat (amb trumfu) i fallem tambe hem de matar amb trumfu si podem altrament qualsevol

-- Si cartes base size 3 (el nostre company es [1]):
        -- si el company [1] esta guanyant podem tirar qualsevol carta del mateix pal que a tirat i si fallem qualsevol altre
        -- Altrament:
        -- mirem quina carta esta guanyant(Hem de matar si podem):
                -- Si esta guanyant una carta del mateix pal que ha tirat el company:
                    -- si tenim d'aquell pal i podem matar nomes podem tirar aquestes altrament si fallem trumfu si tenim altrament qualsevol
                    -- si esta guanyant un fallo (de trumfu):
                    -- si tenim del pal de la base qualsevol del pal
                    -- Si fallem i tenim un trumfu que pugui matar qualsevol trumfu que pugui matar
                    -- altrament qualsevol carta
realJugadesPossibles :: Ma -> Trumfu -> [Carta] -> [Carta]
realJugadesPossibles (NewM llista) _ [] = [x | x<-llista] -- Si no s'ha tirat cap carta començem nosaltres i podem tirar qualsevol carta
realJugadesPossibles (NewM llista) t (x:xs)
  | (length xs + 1 == 1) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] x t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) x t
  | (length xs + 1 == 2) && (quiEstaGuanyant 2 (x:xs) t == 2) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] (head xs) t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) (head xs) t
  | (length xs + 1 == 2) && (quiEstaGuanyant 2 (x:xs) t == 1) = if cartesPalMa (NewM llista) (getPal x) == [] then [c | c<-llista] else cartesPalMa (NewM llista) (getPal x)
  | (length xs + 1 == 3) && (quiEstaGuanyant 3 (x:xs) t == 1) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] x t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) x t
  | (length xs + 1 == 3) && (quiEstaGuanyant 3 (x:xs) t == 3) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] (last xs) t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) (last xs) t
  | otherwise = if cartesPalMa (NewM llista) (getPal x) == [] then [c | c<-llista] else cartesPalMa (NewM llista) (getPal x)


-- Donades les mans dels jugadors ordenades 0-3, les cartes que s'han tirat, el jugador que ha començat retorna les mans sense les cartes tirades
eliminarCartesTiradesDeLesMans :: [Ma] -> [Carta] -> Int -> [Ma]
eliminarCartesTiradesDeLesMans ml b j
  | (j == 0) = [NewM(filter (/=_carta1) _maJugador1), NewM(filter (/=_carta2) _maJugador2), NewM(filter (/=_carta3) _maJugador3), NewM(filter (/=_carta4) _maJugador4)]
  | (j == 1) = [NewM(filter (/=_carta4) _maJugador1), NewM(filter (/=_carta1) _maJugador2), NewM(filter (/=_carta2) _maJugador3), NewM(filter (/=_carta3) _maJugador4)]
  | (j == 2) = [NewM(filter (/=_carta3) _maJugador1), NewM(filter (/=_carta4) _maJugador2), NewM(filter (/=_carta1) _maJugador3), NewM(filter (/=_carta2) _maJugador4)]
  | (j == 3) = [NewM(filter (/=_carta2) _maJugador1), NewM(filter (/=_carta3) _maJugador2), NewM(filter (/=_carta4) _maJugador3), NewM(filter (/=_carta1) _maJugador4)]
  where
    _maJugador1 = getCartesMa (ml !! 0)
    _maJugador2 = getCartesMa (ml !! 1)
    _maJugador3 = getCartesMa (ml !! 2)
    _maJugador4 = getCartesMa (ml !! 3)
    _carta1 = b !! 0
    _carta2 = b !! 1
    _carta3 = b !! 2
    _carta4 = b !! 3

-- Donat les mans dels jugadors, el trunfu de la partida, les cartes de la base, el jugador i el numero de base
-- retorna (base, jugador i nbase) si hi ha hagut trampas.
baseEsTrampa :: [Ma] -> Trumfu -> [Carta] -> Int -> Int -> ([Ma], ([Carta],Int, Int))
baseEsTrampa ml t b j nbase
  | (j == 0) = if (filter (==_carta2) (realJugadesPossibles _maJugador2 t _primer_ha_tirat)) == [] then ([], (b, nbase, 1))
               else if (filter (==_carta3) (realJugadesPossibles _maJugador3 t _segon_ha_tirat)) == [] then ([], (b, nbase, 2))
               else if (filter (==_carta4) (realJugadesPossibles _maJugador4 t _tercer_ha_tirat)) == [] then ([], (b, nbase, 3))
               else (_updated_mans, (b, nbase, 2))
  | (j == 1) = if (filter (==_carta2) (realJugadesPossibles _maJugador3 t _primer_ha_tirat)) == [] then ([], (b, nbase, 2))
               else if (filter (==_carta3) (realJugadesPossibles _maJugador4 t _segon_ha_tirat)) == [] then ([], (b, nbase, 3))
               else if (filter (==_carta4) (realJugadesPossibles _maJugador1 t _tercer_ha_tirat)) == [] then ([], (b, nbase, 0))
               else (_updated_mans, (b, nbase, 2))
  | (j == 2) = if (filter (==_carta2) (realJugadesPossibles _maJugador4 t _primer_ha_tirat)) == [] then ([], (b, nbase, 3))
               else if (filter (==_carta3) (realJugadesPossibles _maJugador1 t _segon_ha_tirat)) == [] then ([], (b, nbase, 0))
               else if (filter (==_carta4) (realJugadesPossibles _maJugador2 t _tercer_ha_tirat)) == [] then ([], (b, nbase, 1))
               else (_updated_mans, (b, nbase, 2))
  | (j == 3) = if (filter (==_carta2) (realJugadesPossibles _maJugador1 t _primer_ha_tirat)) == [] then ([], (b, nbase, 0))
               else if (filter (==_carta3) (realJugadesPossibles _maJugador2 t _segon_ha_tirat)) == [] then ([], (b, nbase, 1))
               else if (filter (==_carta4) (realJugadesPossibles _maJugador3 t _tercer_ha_tirat)) == [] then ([], (b, nbase, 2))
               else (_updated_mans, (b, nbase, 2))
  where
    _maJugador1 = ml !! 0
    _maJugador2 = ml !! 1
    _maJugador3 = ml !! 2
    _maJugador4 = ml !! 3
    _no_ha_tirat_ningu = []
    _primer_ha_tirat = take 1 b
    _segon_ha_tirat = take 2 b
    _tercer_ha_tirat = take 3 b
    _carta1 = b !! 0
    _carta2 = b !! 1
    _carta3 = b !! 2
    _carta4 = b !! 3
    _updated_mans = eliminarCartesTiradesDeLesMans ml b j

-- Donada les mans dels jugadors, el trumfu de la partida, les cartes jugades a la partida i el jugador que ha començat
-- retorna una tupla amb la base, numero de base i jugador que ha fet trampa
trampa :: [Ma] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
trampa ml t c j
  | getCartesMa (head ml) == [] = Nothing
  | (fst _itsaTrap == []) = Just (snd _itsaTrap) -- trampa
  | otherwise = trampa (fst _itsaTrap) t (drop 4 c) (jugadorGuanyaBasa (NewB (j,(_baseActual !! 0),(_baseActual !! 1),(_baseActual !! 2), _baseActual !! 3)) t) -- correcte
  where
    _baseActual = take 4 c
    _nbase = 13 - length (getCartesMa (head ml))
    _itsaTrap = baseEsTrampa ml t _baseActual j _nbase
