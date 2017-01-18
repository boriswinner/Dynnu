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
    buffer[i] := TRectangle.Create;
//    buffer[i] := TFigure(AFigures[i].ClassType.Create);
    buffer[i].Assign(AFigures[i]);
  end;
end;

function TCLipboard.LoadFromClipboard: TFigureArray;
begin
  Result := buffer;
end;

initialization

Clipboard := TCLipboard.Create;
end.

