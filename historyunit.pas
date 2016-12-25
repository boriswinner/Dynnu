unit historyunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,saveunit;

const
  BufferLength = 100;

type
  StringArray = saveunit.StringArray;

  TCycleBuffer = class
  public
    buffer: array [1..BufferLength] of StringArray;
    position, savedposition, AvaibleUndos, AvaibleRedos: integer;
    Constructor Create; overload;
    procedure AddToBuffer;
    procedure Undo;
    procedure Redo;
    procedure CutOff;
    procedure DeleteBuffer;
    function ShowAsterisk: boolean;
    function GetElement(APos: integer): StringArray;
  end;

var
  HistoryBuffer: TCycleBuffer;

implementation

  constructor TCycleBuffer.Create;
  begin
    inherited;
    DeleteBuffer;
  end;

  procedure TCycleBuffer.AddToBuffer;
  begin
    inc(position);
    if (AvaibleUndos < BufferLength-1) then
      inc(AvaibleUndos);
    if position > BufferLength then
      position := 1;
    buffer[position] :=  SaveToStringArray;
  end;

  procedure TCycleBuffer.Undo;
  begin
    if (AvaibleUndos>0) then
    begin
      dec(position);
      dec(AvaibleUndos);
      inc(AvaibleRedos);
      if position < 1 then
        position := BufferLength;
      LoadFromStringArray(buffer[position]);
    end;
  end;

  procedure TCycleBuffer.Redo;
  begin
    if (AvaibleRedos > 0) then
    begin
      inc(position);
      if (AvaibleUndos < BufferLength) then
        inc(AvaibleUndos);
      dec(AvaibleRedos);
      if position > BufferLength then
        position := 1;
      LoadFromStringArray(buffer[position]);
    end;
  end;

  procedure TCycleBuffer.CutOff;
  begin
    AvaibleRedos := 0;
  end;

  procedure TCycleBuffer.DeleteBuffer;
  begin
    AvaibleRedos := 0;
    AvaibleUndos := -1;
    position := 0;
    savedposition := 1;
  end;

  function TCycleBuffer.ShowAsterisk: boolean;
  begin
    if (position <> savedposition) then
      Result := true
    else
      Result := false;
  end;

 function TCycleBuffer.GetElement(APos: integer): StringArray;
 begin

 end;

 initialization
 HistoryBuffer := TCycleBuffer.Create;

end.

