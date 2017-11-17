(* ::Package:: *)

<< LabEval`

Dynamic[pathName];
Dynamic[openFile];
Dynamic[argverbose];
Print["Input file: ", FileNameSetter[Dynamic[pathName]], "   ", Dynamic[pathName]];
Print["open result file: ", Checkbox[Dynamic[openFile]], "   ", Dynamic[openFile]];
Print["Extended output (for developers): ", Checkbox[Dynamic[argverbose]], "   ", Dynamic[argverbose]];

Button["Auswerten",ReadAndEvaluate[pathName,
OpenResultFile -> openFile,
Verbose -> argverbose
]]



