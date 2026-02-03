(* ::Package:: *)

 (* ::Package::LogicAnagram *)

(* :Title: Anagram-Logic *)
(* :Context: LogicAnagram` *)
(* :Author: Winx *)
(* :Summary: logica alla base del gioco di anagrammi *)
(* :Copyright: Winx 2024 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 13 *)
(* :History: last modified 22/4/2024 *)
(* :Keywords: anagram, word, game, dictionary *)
(* :Sources:  /*)
(* :Limitations:  La versione non permette di selezionare la lingua di gioco*)
(* :Discussion:  /*)
(* :Requirements: /*)
(*\[NonBreakingSpace]:Warning:\[NonBreakingSpace] /*)


BeginPackage["LogicAnagram`"]
	
	myCheckAnagram::usage = "Restituisce un booleano che indica se l'input \[EGrave] o non \[EGrave] una soluzione";
	
	myAnagramQ::usage = "Controlla che l'input sia un anagramma della soluzione";
			
	myGetSolution::usage = "Restituisce la soluzione";
	
	myGetAnagram::usage = "Restituisce una parola anagrammata";
	
	level::usage = "Imposta il livello";
	
	Begin["Private`"]
		
	word="";                              (*parola del dizionario su cui si sta giocando*) 
	minLen=5;                             (*lunghezza della parole collegate al primo livello (livello 1)*) 
	maxLen=10;                            (*lunghezza della parole collegate all'ultimo livello (livello 5)*)
	language="Italian";                   (*stringa indicante la lingua di gioco*) 
	nomeFileDictionary="dictonaryIT.txt"; (*stringa indicante il nome del file dizionario da caricare*) 
	level=1;                              (*intero che rappresenta il livello corrente di gioco compreso tra 1 e 5*) 

	(*Lista completa di tutte le parole utilizzate nel gioco (ogni elemento della lista rappresenta una singola parola).
	 Le parole sono importate dal file "dictionary.txt", creato con 1145 parole comuni (nomi, verbi, aggettivi...)
	 Le parole sono scritte in minuscolo, non presentano lettere accentate e hanno una lunghezza compresa tra 5 e 9*)
	 dictionary= Import[NotebookDirectory[]<>nomeFileDictionary,"List"];

	(* Lista che contiene l'unione delle parole italiane restituite dalla funzione WordList 
	e della lista "dictionary" importata dal file. 
	\[CapitalEGrave] necessaria per verificare se una soluzione fornita dall'utente \[EGrave] una parola esistente*)
	realDictionary =Union[WordList[Language->language],dictionary];

	(*Mappa che associa una numero intero compreso tra minLen e maxLen a una lista di parole del dizionario di quella lunghezza,
	 la mappa \[EGrave] utilizzata per eseguire dei controlli sulle parole inserite dall'utente*)
	mapDictionary=AssociationMap[myWordsILenght, Range[minLen,maxLen]];

	(*Input: Integer length: \[EGrave] la lunghezza delle parole da selezionare.
		La funzione restituisce una lista con tutte le parole del dizionario di lunghezza length*)
	myWordsILenght[length_Integer]:=Select[dictionary, StringLength[#]==length &];
			
	(* Input: /
		La funzione seleziona casualmente una parola dal dizionario e ne restituisce un anagramma. 
		La parola originale viene salvata nella variabile "word" per eseguire i successivi controlli. 
		Inoltre, la funzione verifica che l'anagramma prodotto non sia una parola esistente; 
		in caso affermativo, ne genera uno nuovo.*)
	myGetAnagram[] :=
		Module[{anagramma},
			word = myNewWord[];
			anagramma = myAnagram[word];
			While[MemberQ[realDictionary, anagramma], anagramma = myGetAnagram[]];
			Return[anagramma];
			]
			
	(*Input: String wordInput:soluzione proposta dall'utente
		La funzione controlla che la stringa wordInput sia una soluzione e restituisce il booleano associato.
		Una parola \[EGrave] soluzione se \[EGrave] la parola di partenza o una parola italiana di senso compiuto,
		ottenuta dalla permutazione delle lettere della parola di partenza*)
	myCheckAnagram[wordInput_String]:=myAnagramQ[wordInput]  && (SameQ[wordInput,word] || MemberQ[realDictionary, wordInput])

	(*Input: String wordInput: soluzione proposta dall'utente
		La funzione verifica se la stringa "wordInput" \[EGrave] un anagramma della soluzione e restituisce il booleano corrispondente.*)
	myAnagramQ[wordInput_String] :=Sort[Characters[wordInput]]==Sort[Characters[word]]
			
	(*Input: \ 
		La funzione restituisce la soluzione*)
	myGetSolution[] := word;
	
	(*Restituisce una parola casuale da anagrammare di lunghezza associata al livello impostato.
	Il gioco dispone di 5 livelli. Ogni livello genera parole di lunghezza level+4.*)
	myNewWord[]:=RandomChoice[mapDictionary[[level]]];
	
	(*Input: String wordInput: parola da anagrammare
		La funzione genera un anagramma di wordInput permutando le sue lettere in maniera casuale*)
	myAnagram[wordInput_String]:=StringJoin@RandomSample@Characters@wordInput;
	
	End[]
	Protected["LogicAnagram`"];
	SetAttributes[Evaluate@Names["LogicAnagram`*"], Locked];
EndPackage[]

