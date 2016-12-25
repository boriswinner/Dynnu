unit historyunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
const BufferLength = 100;
type
  StringArray = array of string;

  TCycleBuffer = class
  public
    buffer: array [1..BufferLength] of StringArray;
    position: integer;
    Constructor Create; overload;
    procedure UpdateBuffer;
    procedure CutOff(APos: integer);
    function GetElement(APos: integer): StringArray;
  end;

implementation

  constructor TCycleBuffer.Create;
  begin
    inherited;
    position := 1;
  end;

  procedure TCycleBuffer.UpdateBuffer;
  begin
    inc(position);
    if position > BufferLength then
      position := 1;

  end;

  procedure TCycleBuffer.CutOff(APos: integer);
  begin

  end;

  function TCycleBuffer.GetElement(APos: integer): StringArray;
  begin

  end;

end.

