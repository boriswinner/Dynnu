unit saveunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,figuresunit,Dialogs;

const
  signature = '@DYNNUVECTORIMAGE';

var
  ImageName, LastSavedFile: string;
  FileWasChanged: boolean;

type
  StringArray = figuresunit.StringArray;

procedure LoadFromStringArray(AStringArray: StringArray);
function  ReadFromFile(AFileName: string): StringArray;
function  SaveToStringArray: StringArray;
procedure WriteToFile(AFileName: string);

implementation

procedure LoadFromStringArray(AStringArray: StringArray);
var
  tFigures: array of TFigure;
  tLength,t: integer;
  i,j: integer;
  tParams: StringArray;
  cnt: integer;
begin
  setlength(tFigures,StrToInt(AStringArray[1]));
  cnt := 1;
  for i := low(tFigures) to high(tFigures) do
  begin
    inc(cnt);
    t := StrToInt(AStringArray[cnt]);
    setlength(tParams,t);
    for j := 0 to t-1 do
    begin
      inc(cnt);
      tParams[j] := AStringArray[cnt];
    end;
    tFigures[i] := TFigure(GetClass(tParams[0]).Create);
    tFigures[i].Load(tParams);
  end;
  setlength(Figures,length(tFigures));
  for i := low(tFigures) to high(tFigures) do
    Figures[i] := tFigures[i];
end;

function ReadFromFile(AFileName: string): StringArray;
var
  f: TextFile;
begin
  AssignFile(f,AFileName);
  reset(f);
  setlength(Result,1);
  readln(f,Result[0]);
  if (Result[0]<>signature) then
  begin
    ShowMessage('Invalid file');
    setlength(Result,0);
    CloseFile(f);
    exit;
  end;
  while (not EOF(f)) do
  begin
    setlength(Result,length(Result)+1);
    readln(f,Result[high(Result)]);
  end;
  CloseFile(f);
end;

function SaveToStringArray: StringArray;
var
  f: TextFile;
  i,j: integer;
begin
  setlength(Result, 2);
  Result[0] := signature;
  Result[1] := IntToStr(length(Figures));
  for i := low(Figures) to high(Figures) do
  begin
    setlength(Result,length(Result)+length(Figures[i].Save));
    for j := low((Figures[i]).Save) to high((Figures[i]).Save) do
      Result[high(Result)-(high((Figures[i]).Save)-j)] := Figures[i].Save[j];
  end;
end;

procedure WriteToFile(AFileName: string);
var
  f: TextFile;
  i,j: integer;
  tStrings: StringArray;
begin
  AssignFile(f,AFileName);
  DeleteFile(AFileName);
  rewrite(f);
  tStrings := SaveToStringArray;
  for i := low(tStrings) to high(tStrings) do
    writeln(f,tStrings[i]);
  CloseFile(f);
  ImageName := AFileName;
  LastSavedFile := AFileName;
  FileWasChanged:=false;
end;

end.

