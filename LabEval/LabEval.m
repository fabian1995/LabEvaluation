(* Wolfram Language Package *)

(*
	(C) Copyright 2017 Fabian Hummer
	
	This file is part of LabEval - Simple Measurement Series Evanualtion Script

	LabEval is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    LabEval is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with LabEval.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["LabEval`"]
(* Exported symbols added here with SymbolName::usage *) 

ReadAndEvaluate::usage = "Evaluates a spreadsheet"
ReadAndEvaluate::fileNotfound = "Error: The given file path can not be opened.
	Be sure to use absolut paths."
ReadAndEvaluate::fileWriteError = "Error: Failed to save results to spreadsheet!
	Is the result file opened by another program?"
	
ReadAndEvaluate::version = "0.0.1"
	
TestFunction ::usage = "Test function"

Begin["`Private`"]
(* Implementation of the package *)

PrintINV[verbose_, args___] := If[verbose, Print[args]];

TestFunction[a_,b_] := Module[{aa=a,bb=b},
	Return[aa+bb];
];

ReadAndEvaluate[aPathName_String, aOptions___] :=
  	Module[{pathName = aPathName, options = aOptions},
   	
   	(* Read additional arguments *)
   	argVerbose = Verbose /. {options} /. Verbose -> False;
   	argOpenXLS =  
    OpenResultFile /. {options} /. OpenResultFile -> True;
    
    PrintINV[argVerbose, " --- Running LabEval Script, Version ", ReadAndEvaluate::version, " --- "];
   	
   	PrintINV[argVerbose, "Options: <<",  options, ">>; Verbose: ", argVerbose, 
    "; openFile: ", argOpenXLS];
   	
   	(* Read spreadsheet file *)
   	rawInput = Import[pathName];
   	If[rawInput === $Failed,
    		Message[ReadAndEvaluate::fileNotfound, Style[fullName, Red]];
    		Return[$Failed];
    	];
   	PrintINV[argVerbose, "Imported file '", pathName, "' as spreadsheet"];
   	
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
  			ErrFooa = Function[Evaluate[varnames], Evaluate[
  				Sqrt[Plus @@ MapThread[
  					(D[Fooa @@ varnames, #1]^2*#2^2) &, {varnames, errorlist}
        		]]
        	]];
  			AppendTo[localFunctions, iLoop \[RightArrow] Fooa];
  			errorIndex = Position[varnames, errorlist[[iLoop]] ];
  			
  			For[jLoop = startOfData, jLoop <= Length[input], jLoop++,
   				input[[jLoop]][[iLoop]] = Fooa @@ input[[jLoop]];
   				
   				If[ Length[errorIndex] == 1,
   					input[[jLoop]][[errorIndex [[1]] [[1]] ]] = ErrFooa @@ input[[jLoop]];
   				];
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

