unit clipboardunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, figuresunit;

type
  TFigureArray = array of TFigure;

  TCLipboard = class
    buffer: TFigureArray;
    procedure SaveToClipboard;
    procedure LoadFromClipboard;
  end;

var
  Clipboard: TCLipboard;

implementation

procedure TCLipboard.SaveToClipboard;
var
  i: integer;
begin
  for i := low(Figures) to high(Figures) do
  begin
    if (Figures[i].Selected) then
    begin
      setlength(buffer,length(buffer)+1);
      buffer[high(buffer)] := Figures[i].GetCopy;
    end;
  end;
end;

procedure TCLipboard.LoadFromClipboard;
var
  i: integer;
begin
  for i := low(buffer) to high(buffer) do
  begin
    setlength(Figures,length(Figures)+1);
    Figures[high(Figures)] := buffer[i];
  end;
end;

initialization

Clipboard := TCLipboard.Create;
end.

