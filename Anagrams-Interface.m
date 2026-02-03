(* ::Package:: *)

(* ::Text:: *)
(*(* ::Package::Interface*)*)
(**)
(*(* :Title: Anagram-Interface *)*)
(*(* :Context: Interface` *)*)
(*(* :Author: Winx *)*)
(*(* :Summary: definizione delle componenti grafiche per la costituzione dell'interfaccia ed esposizione della funzione che deve essere richiamata per avviare il gioco degli anagrammi*)*)
(*(* :Copyright: Winx 2024 *)*)
(*(* :Package Version: 2 *)*)
(*(* :Mathematica Version: 13 *)*)
(*(* :History: last modified 22/04/2024 *)*)
(*(* :Keywords: anagram, word, game, dictionary, interface *)*)
(*(* :Sources:  /*)*)
(*(* :Limitations: La versione non permette di selezionare la lingua di gioco *)*)
(*(* :Discussion:  /*)*)
(*(* :Requirements: /*)*)
(*(*\[NonBreakingSpace]:Warning: /\[NonBreakingSpace]*)*)


BeginPackage["Interface`"]

(*Carica il pacchetto che contiene le funzioni relative alla generazione e gestione degli anagrammi*)
Needs["LogicAnagram`"]

	myAnagrams::usage=  "Funzione che avvia il gioco degli Anagrammi."
	
	Begin["`Private`"]
		
		(*Componenti grafiche*)
		buttonStyle = {Background -> RGBColor[0.95,0.87,0.76], ImageSize -> Medium}; 
		coloreSfondo1 = RGBColor["#c2a083"];
		coloreSfondo2 = RGBColor["#e7d9cd"];
		
		(*Inizializzazione delle variabili*)
		
		inputWord = "";             (*Parola inserita dall'utente come tentativo*)
		stampa = "";                (*Variabile che viene assegnata quando deve essere mostrato un messaggio all'utente*)
		anagram="";                 (*Contenitore per la grafica delle parole mostrate all'utente*)
		seed=0;                     (*Seme per la generazione della sequenza pseudorandomica per la selezione delle parole*)
		maxSeed=1000;               (*Seed massimo selezionabile dall'utente*)
		contatore = 1;              (*Contatore per identificare la posizione all'interno della sequenza generata*)
		
		(*Liste di messaggi per l'utente*)
		frasiVittoria = {"Hai vinto!", "Complimenti!", "Grande lavoro!", "Fantastico!", "Eccezionale!"};
		frasiErrore = {"Grrr!", "Riprova!", "Prova di nuovo!", "Continua a tentare!","Rabbia."};
		frasiNoAnagram = {"Mmm...Non \[EGrave] un anagramma", "Questo non mi sembra un anagramma", "Controlla meglio, non \[EGrave] un anagramma"};
	
		(*Funzione che genera la grafica di una tessera, prendendo in input una lettera*)
		myTessera[letter_] := Deploy[Image[Graphics[{coloreSfondo1, 
		Rectangle[{0, 0}, {2.05, 3.05}, RoundingRadius -> 0.2], coloreSfondo2, 
		Rectangle[{0, 0}, {2, 3.001}, RoundingRadius -> 0.2], 
		Text[Style[ToUpperCase[letter], FontFamily -> "Times New Roman", 
		100, FontColor -> Black], {1, 1.525}]}, ImageSize -> 100]]];
		
		(*Funzione che restituisce la grafica completa della parola, suddividendola in caratteri e ricomponendo le grafiche generate da myTessera*)
		myShowWord[text_]:=  Return[Grid[Partition[Table[myTessera[lettera], {lettera, Characters[text]}], StringLength[text]], Spacings -> {1, 1}]];
		
		(*Richiede al pacchetto LogicAnagrams di generare una nuova parola e la passa a myShowWord per generarne la grafica*)
		myGenerate[] := myShowWord[myGetAnagram[]];
			
		(*Controlla la parola inserita dall'utente e aggiorna il messaggio da mostrare*)
		myFunzConfronto[input_] := stampa= myCheck[ToLowerCase[input]];
		
		(*Verifica che la parola inserita dall'utente sia una soluzione dell'anagramma utilizzando le funzioni del pacchetto LogicAnagrams*)
		myCheck[input_] := Module[{},anagram=myShowWord[input]; 
									(*Verifica che siano inserite solo lettere, che le lettere corrispondano alle lettere dell'anagramma e infine se sia una soluzione corretta*)
									Return[RandomChoice[frasiVittoria]]]/; LetterQ[input] && myAnagramQ[input] && myCheckAnagram[input]; 
							
		(*Verifica che la parola sia un anagramma utilizzando la funzione myAnagramQ del pacchetto LogicAnagrams*)
		myCheck[input_] := RandomChoice[frasiErrore]/;LetterQ[input] && myAnagramQ[input];
		(*Verifica che l'utente abbia inserito solo lettere e non caratteri speciali*)
		myCheck[input_] := RandomChoice[frasiNoAnagram]/;LetterQ[input];
		(*Condizione che si verifica quando LetterQ non \[EGrave] soddisfatto, quindi sono stati inseriti caratteri speciali*)
		myCheck[input_] := "Non inserire dei caratteri speciali :(";
		
		(*Resetta le variabili di gioco alle condizioni di partenza generando una nuova parole e aumentando il contatore*)
		myReset[] := DynamicModule[{}, stampa = ""; inputWord = ""; anagram = myGenerate[]; contatore=contatore+1];
		
		(*Pulisce il tentativo dell'utente inserito nella barra interna al panel*)
		myResetInput[] := DynamicModule[{}, stampa = ""; inputWord = ""];
		
		(*Funzione per l'impostazione e la modifica del seed*)
		mySeed[] := DynamicModule[{},
		(*Genero una Dialog che chiede in input all'utente di inserire un seed tramite uno Slider da 0 a maxSeed*)
		seed = DialogInput[DialogNotebook[{
				TextCell["Seleziona un numero per definire il seed:"],
				Slider[Dynamic[seed],{0,maxSeed,1}, Appearance->Large, ImageSize->500], 
				Row[{Button["-", seed=seed-1],Dynamic[seed], Button["+", seed=seed+1]}, "  "],
				DefaultButton[DialogReturn[seed]]}]];
		
		(*Controllo se il seed sia stato effettivamente inserito altrimenti assegna il valore di default*)
		seed = If[NumberQ[seed], seed, 0];
		SeedRandom[seed];
		];



	(**** Funzione principale ****)
		myAnagrams[] := DynamicModule[{}, 
		(*Definizione del seed*)	
		mySeed[];
		
		(*Definisco un anagramma iniziale*)
		anagram = myGenerate[];
		
		(*Organizzazione interfaccia grafica*)
		Column[{Row[{ 
				(*Definizione dei bottoni del men\[UGrave] principale*)
				Deploy[Button["Genera Nuovo Esercizio", { 
					anagram = myGenerate[], (*Genera un nuovo anagramma*)
					inputWord="",           (*Azzera i tentativi di inserimento dell'utente*)
					stampa = "",            (*Azzera i messaggi per l'utente*)
					contatore=contatore+1}, (*Incrementa il contatore, dato che viene generata una nuova parola*)
					buttonStyle]], 
				Deploy[Button["Modifica Seed", {
					mySeed[],               (*Compare la DialogInput per modificare il seed*)
					contatore=1,            (*Il contatore viene reimpostato a 1*)
					anagram = myGenerate[]},(*Genera un nuovo anagramma con il seed appena selezionato*)
					Method->"Queued",        (*Opzione necessaria per gestire le eccezioni generate da un eccessivo numero di click sul pulsante*)
					buttonStyle]], 
				Deploy[Button["Mostra Risultato", {
					anagram = myShowWord[myGetSolution[]]}, (*Le tessere vengono ridisegnate per mostrare la parola soluzione*)
					buttonStyle]], 
				Deploy[Button["Reset", 
					myReset[],      (*Chiama la funzione di reset*)
					buttonStyle]],
				
				(*Definizione del men\[UGrave] a tendina per la selezione dei livelli, relativo alla variabile level interna al pacchetto LogicAnagrams*)
				PopupMenu[Dynamic[LogicAnagram`level], {1 -> "Livello 1" , 2 -> "Livello 2", 3 -> "Livello 3", 4 -> "Livello 4", 5 -> "Livello 5"}],
				
				(*Stampa del seed attuale e del contatore relativo alla posizione della sequenza generata*)
				Dynamic[StringJoin["Seed: ", ToString[seed],".", ToString[contatore]]]}, "  "],
					Row[{}],
					Column[{
						(*Stampa della grafica della parola anagrammata*)
						Dynamic[anagram],
						(*Pannello per l'inserimento del tentativo dell'utente*)
						Panel[Column[{InputField[Dynamic[inputWord],String,FieldSize->{Automatic,Large}, Enabled-> True, FieldHint->"scrivi qui...",ImageSize->{Automatic,Large},FrameMargins->{{12,8},{Automatic,Automatic}}], 
						(*Bottone per la verifica del tentativo*)
						Row[{Deploy[Button["Verifica Soluzione", 
											myFunzConfronto[inputWord], (*Richiama la funzione per controllare il tentativo inserito dall'utente*)
											buttonStyle]], 
							(*Bottone per la pulizia del campo di inserimento*)
							Deploy[Button["Pulisci", 
											myResetInput[], (*Richiama la funzione per pulire la barra di inserimento del tentativo*)
											buttonStyle]]}, "   "]}]]
			}],
			(*Stampa eventuali messaggi per l'utente*)
			Dynamic[Style[stampa, FontSize->32]]
		}]
		]
 
	
	End[]
	Protected["Interface`"];
	SetAttributes[Evaluate@Names["Interface`*"], Locked];
EndPackage[]
