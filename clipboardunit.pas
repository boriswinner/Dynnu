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
    //procedure LoadFromClipboard;
  end;

var
  Clipboard: TCLipboard;

implementation

procedure TCLipboard.SaveToClipboard(AFigures: TFigureArray);
begin
  setlength(buffer,0);
  //setlength(b
end;

initialization

Clipboard := TCLipboard.Create;
end.

