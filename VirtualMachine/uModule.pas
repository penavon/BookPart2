unit uModule;

// Ths source is distributed under Apache 2.0

// Copyright (C)  2019-2020 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


// A module defines a collection of zero or more user defined functions and code.
// eg
// function add (a, b) return a+b end;
// println (add (4,5));

interface

Uses Classes, SysUtils, uSymbolTable;

type
   TModule = class (TObject)
       name : string;
       code : TProgram;
       symbolTable : TSymbolTable;

       procedure   clearCode;
       constructor Create (name : string);
       destructor  Destroy; override;
   end;


implementation


constructor TModule.Create (name : string);
begin
  self.name := name;
  code := TProgram.Create;
  symbolTable := TSymbolTable.Create;
end;


destructor  TModule.Destroy;
begin
  code.Free;
  symbolTable.Free;
  inherited;
end;


procedure TModule.clearCode;
begin
  code.Clear;
end;

end.
