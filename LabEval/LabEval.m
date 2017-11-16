(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Nov 16, 2017 *)

BeginPackage["LabEval`"]
(* Exported symbols added here with SymbolName::usage *) 

ReadAndEvaluate::usage = "Evaluates a spreadsheet"
ReadAndEvaluate::fileNotfound = "Error: The given file path can not be opened.
	Be sure to use absolut paths."
ReadAndEvaluate::fileWriteError = "Error: Failed to save results to spreadsheet!
	Is the result file opened by another program?"
	
TestFunction ::usage = "asdffg"

Begin["`Private`"]
(* Implementation of the package *)

PrintINV[verbose_, args___] := If[verbose, Print[args]];

TestFunction[a_,b_] := Module[{aa=a,bb=b},
	Return[aa+bb+2];
];

ReadAndEvaluate[aPathName_String, aOptions___] :=
  	Module[{pathName = aPathName, options = aOptions},
   	
   	(* Read additional arguments *)
   	argVerbose = Verbose /. {options} /. Verbose -> False;
   	argOpenXLS =  
    OpenResultFile /. {options} /. OpenResultFile -> True;
   	
   	Print["Verbose: ", Verbose, "   OpenResult: ", OpenResult];
   	Print["Options: <<",  options, ">>; Verbose: ", argVerbose, 
    "; openFile: ", argOpenXLS];
   	
   	(* Read spreadsheet file *)
   	rawInput = Import[pathName];
   	If[rawInput === $Failed,
    		Message[ReadAndEvaluate::fileNotfound, Style[fullName, Red]];
    		Return[$Failed];
    	];
   	PrintINV[argVerbose, "Imported file '", pathName, 
    "' as spreadsheet"];
   	
   	(* Parse configuration info and extract data *)
   	firstColumn = rawInput[[2]][[1]][[2]];
   	startOfConfig = rawInput[[2]][[2]][[2]];
   	varnameLine = 1;
   	assignLine = 2;
   	functionLine = 3;
   	startOfData = rawInput[[2]][[3]][[2]] - startOfConfig + 1;
   	input = rawInput[[1]][[startOfConfig ;;, firstColumn ;;]];
   	
   	(* Link value variables to their error variables *)
   	assignList = input[[assignLine]];
   	errorlist = {};
   	For[iLoop = 1, iLoop <= Length[assignList], iLoop++,
    		If[StringMatchQ[assignList[[iLoop]], RegularExpression["\\w+"]],
      			AppendTo[errorlist, ToExpression[assignList[[iLoop]]]],
      			AppendTo[errorlist, 0]
      		];
    	];
   	
   	PrintINV[argVerbose, "Error List: ", errorlist];
   	
   	(* Parse and evaluate functions *)
   	fcontl = input[[functionLine]];
   	varnames = Map[ToExpression, input[[varnameLine]]];
   	localFunctions = {};
   
   	For[iLoop = 1, iLoop <= Length[fcontl], iLoop++,
    		If[fcontl[[iLoop]] != "",
      			expb = ToExpression[fcontl[[iLoop]], TraditionalForm];
      			Fooa = Function[Evaluate[varnames], Evaluate[expb]];
      			ErrFooa = 
       Function[Evaluate[varnames], Evaluate[Sqrt[Plus @@ MapThread[
            				(D[Fooa @@ varnames, #1]^2*#2^2) &, {varnames, 
             errorlist}
            			]]]];
      			AppendTo[localFunctions, iLoop \[RightArrow] Fooa];
      			errorIndex = 
       Position[varnames, errorlist[[iLoop]] ] [[1]][[1]];
      			For[jLoop = startOfData, jLoop <= Length[input], jLoop++,
       				input[[jLoop]][[iLoop]] = Fooa @@ input[[jLoop]];
       				input[[jLoop]][[errorIndex]] = ErrFooa @@ input[[jLoop]];
       			];
      		];
    	];
   	
   	(* Save results to spreadsheet *)
   	exportPath = 
    StringInsert[pathName, "-Result", Last[StringPosition[pathName, FileExtension[pathName]]][[1]] - 1];
   	PrintINV[argVerbose, "Exporting results to ", exportPath];
   	
   	exportResult = Export[exportPath, input];
   	If[exportResult === $Failed,
    	Message[ReadAndEvaluate::fileWriteError, Style[fullName, Red]];
    	Return[$Failed];
    ];
   	If[argOpenXLS,
    	SystemOpen[exportPath];,
    	Print["Your spreadsheet has been evaluated. Find the results in \n'", exportPath, "'"];
	];
];

End[]

Protect[Verbose]
Protect[OpenResultFile]

EndPackage[]

