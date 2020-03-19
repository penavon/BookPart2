unit uBuiltinFunctions;

// Developed using Delphi for Windows and Mac platforms.

// *** Ths source is distributed under Apache 2.0 ***

// Copyright (C) 2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com

interface

Uses System.SysUtils, uUtils, uMachineStack, System.generics.Collections, uVM;

type
   TBuiltInFunction = procedure (vm : TVM);

   TBuiltinFunctionRecord = record
       name : string;
       nArguments : integer;
       helpStr : string;
       funcPtr : TBuiltInFunction;
   end;
   TBuiltinList = class (TList<TBuiltInFunctionRecord>)
     public
        function find(const name: string; var index : integer): boolean;
   end;


var builtinList : TBuiltinList;

implementation

Uses Math, uVMExceptions, uStringObject;

procedure argMustBeNumber;
begin
  raise ERuntimeException.Create('argument must be a number');
end;


function TBuiltinList.find(const Name: string; var index : integer) : boolean;
var i : integer;
begin
  for i := 0 to Count-1 do
    if Self[i].name = name then
       begin
       index := i;
       exit (true);
       end;
  result := false;
end;


procedure myInt (vm : TVM);
var x : PMachineStackRecord; tmp : int32;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (x.iValue);
       stDouble  : begin
                   // Do it this way inorder to get a range check error
                   // if dValue can't be case to a 32-bit integer
                   tmp := trunc (x.dValue);
                   vm.push (int32 (tmp));
                   end;
  else
     argMustBeNumber;
  end;
end;


procedure readNumber (vm : TVM);
var s : string;
    iValue : integer;
    dValue : double;
begin
  readln(s);
  while (not TryStrToInt(s, iValue)) and (not TryStrToFloat(s, dValue)) do
      begin
      writeln ('Number error: ' + s + ' is not a number, try again');
      readln (s);
      end;
  if TryStrToInt(s, iValue) then
     vm.push (iValue)
  else
    vm.push (dValue);
end;


procedure readString (vm : TVM);
var s : string;
    sObj : TStringObject;
begin
  readln(s);
  sobj := TStringObject.create (s);
  vm.push (sObj);
end;


procedure mySin (vm : TVM);
var x : PMachineStackRecord;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (sin (x.iValue));
       stDouble  : vm.push (sin (x.dValue));
  else
     argMustBeNumber;
  end;
end;


procedure myCos (vm : TVM);
var x : PMachineStackRecord;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (cos (x.iValue));
       stDouble  : vm.push (cos (x.dValue));
  else
     argMustBeNumber;
  end;
end;


procedure myTan (vm : TVM);
var x : PMachineStackRecord;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (tan (x.iValue));
       stDouble  : vm.push (tan (x.dValue));
  else
     argMustBeNumber;
  end;
end;

procedure mySqrt (vm : TVM);
var x : PMachineStackRecord;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (sqrt (x.iValue));
       stDouble  : vm.push (sqrt (x.dValue));
  else
     argMustBeNumber;
  end;
end;


procedure myExp (vm : TVM);
var x : PMachineStackRecord;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (exp (x.iValue));
       stDouble  : vm.push (exp (x.dValue));
  else
     argMustBeNumber;
  end;
end;


procedure myLn (vm : TVM);
var x : PMachineStackRecord;
begin
  x := vm.pop;
  case x.stackType of
       stInteger : vm.push (ln (x.iValue));
       stDouble  : vm.push (ln (x.dValue));
  else
     argMustBeNumber;
  end;
end;


procedure addBuiltIns (name : string; nArguments : integer; helpStr : string; fcn : TBuiltInFunction);
var builtin : TBuiltInFunctionRecord;
begin
  builtin.name := name;
  builtin.nArguments := nArguments;
  builtin.helpStr := helpStr;
  builtin.funcPtr := fcn;
  builtinList.Add (builtin);
end;


initialization
  builtinList := TBuiltinList.Create;
  addBuiltIns ('readNumber', 0, 'read an integer from the console', readNumber);
  addBuiltIns ('readString', 0, 'read a string from the console', readString);
  addBuiltIns ('int', 1, 'convert float to integer: int (3.4)', myInt);
  addBuiltIns ('sin', 1, 'compute sine of a radian angle: sin (x)', mySin);
  addBuiltIns ('cos', 1, 'compute cosine of a radian angle: cos (x)', myCos);
  addBuiltIns ('tan', 1, 'compute tangent of a radian angle: tan (x)', myTan);
  addBuiltIns ('sqrt',1, 'compute the square rootof a number. Negative values are not supported: sqrt (9)', mySqrt);
  addBuiltIns ('exp', 1, 'compute e raised to the power of a value: exp (10)', myExp);
  addBuiltIns ('ln',  1, 'compute the natural logarithm of a value: ln (123)', myLn);
finalization
  builtinList.Free;
end.

