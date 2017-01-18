unit clipboardunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, figuresunit;

type
  TFigureArray = array of TFigure;

  TCLipboard = class
    buffer: TFigureArray;
    procedure SaveToClipboard(AFigures: TFigureArray);
    function LoadFromClipboard: TFigureArray;
  end;

var
  Clipboard: TCLipboard;

implementation

procedure TCLipboard.SaveToClipboard(AFigures: TFigureArray);
var
  i: integer;
begin
  setlength(buffer,0);
  setlength(buffer,length(AFigures));
  for i := low(AFigures) to high(AFigures) do
  begin
   // buffer[i] := TRectangle.Create;
    //buffer[i] := TFigure(AFigures[i].ClassType.Create);
    buffer[i] := (AFigures[i].GetCopy);
  end;
end;

function TCLipboard.LoadFromClipboard: TFigureArray;
var
  i: integer;
begin
  setlength(Result,length(buffer));
  for i := low(buffer) to high(buffer) do
    result[i] := (buffer[i].GetCopy);
end;

initialization

Clipboard := TCLipboard.Create;
end.

