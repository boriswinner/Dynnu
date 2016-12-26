unit historyunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,saveunit;

const
  BufferLength = 100;
  ActionCreate = 'Created Canvas';
  ActionOpen = 'Opened File';
  ActionDraw = 'Drew Item';
  ActionChange = 'Changed Properties';
  ActionMove = 'Moved Item';

type
  StringArray = saveunit.StringArray;

  TCycleBuffer = class
  public
    buffer: array [1..BufferLength] of StringArray;
    action: array [1..BufferLength] of String;
    position, savedposition, AvaibleUndos, AvaibleRedos: integer;
    Constructor Create; overload;
    procedure AddToBuffer(AAction: string);
    procedure Undo;
    procedure Redo;
    procedure CutOff;
    procedure DeleteBuffer;
    function ShowAsterisk: boolean;
    procedure SetToState(APos: integer);
    function UpdateSpos: integer;
    function UpdateEpos: integer;
  end;

var
  HistoryBuffer: TCycleBuffer;

implementation

  constructor TCycleBuffer.Create;
  begin
    inherited;
    DeleteBuffer;
  end;

  procedure TCycleBuffer.AddToBuffer(AAction: string);
  begin
    inc(position);
    if (AvaibleUndos < BufferLength-1) then
      inc(AvaibleUndos);
    if position > BufferLength then
      position := 1;
    buffer[position] := SaveToStringArray;
    action[position] := AAction;
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

  function TCycleBuffer.UpdateSpos: integer;
  var
    i: integer;
  begin
    Result := position;
    if (AvaibleUndos=0) then exit;
    for i := 1 to AvaibleUndos do
    begin
      dec(Result);
      if (Result < 1) then Result := BufferLength;
    end;
  end;

  function TCycleBuffer.UpdateEpos: integer;
  var
    i: integer;
  begin
    Result := position;
    if (AvaibleRedos=0) then exit;
    for i := 1 to AvaibleRedos do
    begin
      inc(Result);
      if (Result > BufferLength) then Result := 1;
    end;
  end;

  procedure TCycleBuffer.SetToState(APos: integer);
  begin
    if (position-APos > 0) then
    begin
      AvaibleUndos:=AvaibleUndos-(position-APos);
      AvaibleRedos:=AvaibleRedos+(position-APos);
    end;
    if (position-APos < 0) then
    begin
      AvaibleRedos:=AvaibleRedos-(position-APos);
      AvaibleUndos:=AvaibleUndos+(position-APos);
    end;
    position := APos;
    if position < 1 then
      position := BufferLength;
    if position > BufferLength then
       position := 1;
    LoadFromStringArray(buffer[position]);
  end;

 initialization
 HistoryBuffer := TCycleBuffer.Create;

end.

