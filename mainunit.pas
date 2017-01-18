unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, FPCanvas, TypInfo,
  Spin, aboutunit, figuresunit, toolsunit, scalesunit, historyunit,saveunit,clipboardunit,Types;

type
  TFigureClass = figuresunit.TFigureClass;

  { TMainForm }

  TMainForm = class(TForm)
    AntiAliasingLabel: TLabel;
    ButtonsPanel: TPanel;
    ColorsGridColorDialog: TColorDialog;
    ColorLabel1: TLabel;
    ColorLabel2: TLabel;
    ColorsGrid: TDrawGrid;
    AntiAliasingComboBox: TComboBox;
    HistoryListBox: TListBox;
    MainMenu: TMainMenu;
    FileSubMenu: TMenuItem;
    HelpSubMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    MainPaintBox: TPaintBox;
    EditSubMenu: TMenuItem;
    HistoryPanel: TPanel;
    CopyMenuItem: TMenuItem;
    DeteleMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    MoveToTopMenuItem: TMenuItem;
    MoveToBottomMenuItem: TMenuItem;
    MoveUpMenuItem: TMenuItem;
    MoveDownMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    OpenMenuitem: TMenuItem;
    SaveMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    ShowAllItem: TMenuItem;
    PaintPanel: TPanel;
    ColorsPanel: TPanel;
    HorizontalScrollBar: TScrollBar;
    ShowAllButton: TButton;
    VerticalScrollBar: TScrollBar;
    ZoomLabel: TLabel;
    ZoomSpinEdit: TSpinEdit;
    ToolsPanel: TPanel;
    SaveImageDialog: TSaveDialog;
    OpenImageDialog: TOpenDialog;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AntiAliasingComboBoxChange(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure DeteleMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HistoryListBoxKeyPress(Sender: TObject; var Key: char);
    procedure HistoryListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HistoryListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure MoveDownMenuItemClick(Sender: TObject);
    procedure MoveToBottomMenuItemClick(Sender: TObject);
    procedure MoveToTopMenuItemClick(Sender: TObject);
    procedure MoveUpMenuItemClick(Sender: TObject);
    procedure OpenMenuitemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure RedoMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainPaintBoxResize(Sender: TObject);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ShowAllItemClick(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure ColorsGridDblClick(Sender: TObject);
    procedure ColorsGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ColorsGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MainPaintBoxPaint(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure ZoomSpinEditChange(Sender: TObject);
    procedure HistoryListBoxUpdate(Sender: TListBox);
  private
    { private declarations }
    isDrawing,isInvisible: boolean;
    CurrentTool: TTool;
    Colors: array of TColor;
    ColorsFile: text;
    BotScrollCent,RightScrollCent: integer;
    ScrollBool,RBtn: boolean;
    PropPanel: TPanel;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  OldPaintBoxSize: TPoint;
implementation

{$R *.lfm}
{ TMainForm }

procedure TMainForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) then RBtn := true;
  isDrawing := true;
  if (CurrentTool is TInvisibleTool) then isInvisible:=true else
      isInvisible:=false;
  CurrentTool.FigureCreate(CurrentTool.FigureClass,Point(X,Y));
  Invalidate;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  b: TBitBtn;
begin
  Zoom                        := 100;
  scalesunit.PaintBoxSize.x   := MainPaintBox.Width;
  scalesunit.PaintBoxSize.y   := MainPaintBox.Height;
  CtrlBtn:=false;

  ColorLabel1.Color := PenColor;
  ColorLabel2.Color := BrushColor;
  ColorsGrid.Width  := ColorsGrid.DefaultColWidth*ColorsGrid.ColCount+4;
  ColorsPanel.Width := ColorsGrid.Width;

  AssignFile(ColorsFile,'colors.txt');
  reset(ColorsFile);
  while not eof(ColorsFile) do
  begin
    setlength(Colors,length(Colors)+1);
    readln(ColorsFile,Colors[high(Colors)]);
  end;
  CloseFile(ColorsFile);

  ColorsGrid.RowCount := length(Colors) div ColorsGrid.ColCount;
  ToolsPanel.Width    := 5 + 4*32 + 5;
  for i := low(ToolsRegister) to high(ToolsRegister) do
  begin
    b         := TBitBtn.Create(ToolsPanel);
    b.Parent  := ButtonsPanel;
    b.name    := 'ToolButton'+IntToStr(i+1);
    b.Caption := '';
    b.Tag     := Integer(i);
    b.Left    := 5 + (i mod 4)*32;
    b.Top     := 5 + (i div 4)*32;
    b.Width   := 32;
    b.Height  := 32;
    b.Glyph   := ToolsRegister[i].Bitmap;
    b.OnClick := @ToolButtonClick;
    if (i=low(ToolsRegister)) then b.Click;
  end;
  ButtonsPanel.Height := 40 + (high(ToolsRegister) div 4)*32;

  for i := ord(low(TAntiAliasingMode)) to ord(high(TAntiAliasingMode)) do
  begin
    AntiAliasingComboBox.Items.Add(GetEnumName(TypeInfo(TAntiAliasingMode),i));
  end;
  AntiAliasingComboBox.ItemIndex := ord(Canvas.AntiAliasingMode);
  AntiAliasingComboBox.ReadOnly := true;


  HorizontalScrollBar.max := MainPaintBox.Width;
  VerticalScrollBar.max   := MainPaintBox.Height;

  ImageName := 'Image1.dvimg';
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.AntiAliasingComboBoxChange(Sender: TObject);
begin
  MainPaintBox.Canvas.AntiAliasingMode := TAntiAliasingMode(GetEnumValue(TypeInfo(TAntiAliasingMode),
  (Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex]));
  Invalidate;
end;

procedure TMainForm.CopyMenuItemClick(Sender: TObject);
begin
  Clipboard.SaveToClipboard;
end;

procedure TMainForm.DeteleMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while (i <= high(Figures)) do
  begin
    if (Figures[i].Selected) then
    begin
      Figures[i].FDelete(i);
      HistoryBuffer.AddToBuffer(ActionDelete)
    end
    else
      inc(i);
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  CurrentTool := TRectangleTool.Create;
  BrushColor := clWhite;
  CurrentTool.FigureCreate(TRectangle,Point(0,0));
  CurrentTool.AddPoint(Point(MainPaintBox.Width,MainPaintBox.Height));
  BrushColor := clBlack;
  CurrentTool := ToolsRegister[0];
  Invalidate;
  VerticalScrollBar.Max   := round(WorldToScreen(MaxFloatPoint).y);
  VerticalScrollBar.Min   := round(WorldToScreen(MinFloatPoint).y);
  HorizontalScrollBar.Max := round(WorldToScreen(MaxFloatPoint).x);
  HorizontalScrollBar.Min := round(WorldToScreen(MinFloatPoint).x);
  HistoryBuffer.AddToBuffer(ActionCreate);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=VK_ADD)      or (key=VK_OEM_PLUS)  then
    ZoomSpinEdit.Value := ZoomSpinEdit.Value + 10;
  if (key=VK_SUBTRACT) or (key=VK_OEM_MINUS) then
    ZoomSpinEdit.Value := ZoomSpinEdit.Value - 10;
  if (key=VK_CONTROL) then
    CtrlBtn:= true;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (key=VK_CONTROL) then
    CtrlBtn:= false;
end;

procedure TMainForm.HistoryListBoxKeyPress(Sender: TObject; var Key: char);
begin
  MainPaintBox.Invalidate;
end;

procedure TMainForm.HistoryListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  MainPaintBox.Invalidate;
end;

procedure TMainForm.HistoryListBoxSelectionChange(Sender: TObject; User: boolean
  );
var
  destination,spos,epos: integer;
begin
  destination := (Sender as TListBox).ItemIndex+1;

  while(HistoryBuffer.AvaibleUndos>0) do
  begin
    if (destination = HistoryBuffer.position) then
      break;
    HistoryBuffer.Undo;
  end;
  if (destination <> HistoryBuffer.position) then
  begin
    while(HistoryBuffer.AvaibleRedos>0) do
    begin
      if (destination = HistoryBuffer.position) then
        break;
      HistoryBuffer.Redo;
    end;
  end;
end;

procedure TMainForm.MoveDownMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  if (Figures[1].Selected) then exit;
  for i := low(Figures) to high(Figures) do
  begin
    if (Figures[i].Selected) then
    begin
      Figures[i].MoveDown(i);
      HistoryBuffer.AddToBuffer(ActionMove);
    end;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.MoveToBottomMenuItemClick(Sender: TObject);
var
  i,shift: integer;
begin
  shift := 0;
  if (Figures[1].Selected) then exit;
  for i := high(Figures) downto low(Figures)+1 do
  begin
    if (Figures[i+shift].Selected) then
    begin
      Figures[i+shift].MoveToBottom(i+shift);
      inc(shift);
      HistoryBuffer.AddToBuffer(ActionMove);
    end;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.MoveToTopMenuItemClick(Sender: TObject);
var
  i,shift: integer;
begin
  shift := 0;
  if (Figures[high(Figures)].Selected) then exit;
  for i := low(Figures) to high(Figures) do
  begin
    if (Figures[i-shift].Selected) then
    begin
      Figures[i-shift].MoveToTop(i-shift);
      inc(shift);
      HistoryBuffer.AddToBuffer(ActionMove);
    end;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.MoveUpMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  if (Figures[high(Figures)].Selected) then exit;
  for i := high(Figures) downto low(Figures) do
  begin
    if (Figures[i].Selected) then
    begin
      Figures[i].MoveUp(i);
      HistoryBuffer.AddToBuffer(ActionMove);
    end;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.OpenMenuitemClick(Sender: TObject);
begin
  OpenImageDialog := TOpenDialog.Create(Self);
  with OpenImageDialog do
  begin
    InitialDir := GetCurrentDir;
    Filter := 'Dynnu Vector Image|*.dvimg|';
    Title := 'Open a Dynnu Vector Image';
    DefaultExt:= 'dvimg';
  end;
  if OpenImageDialog.Execute then
  begin
    ImageName:= OpenImageDialog.FileName;
    LastSavedFile := ImageName;
    LoadFromStringArray(ReadFromFile(OpenImageDialog.FileName));
    HistoryBuffer.DeleteBuffer;
    HistoryBuffer.AddToBuffer(ActionOpen);
    MainPaintBox.Invalidate;
  end;
end;

procedure TMainForm.PasteMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  ShowMessage(IntToStr(length(Clipboard.buffer)));
  Clipboard.LoadFromClipboard;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.RedoMenuItemClick(Sender: TObject);
begin
  HistoryBuffer.Redo;
  MainPaintBox.Invalidate;
end;


procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
var
  Reply,BoxStyle: Integer;
  tempObject: TObject;
begin
  SaveImageDialog := TSaveDialog.Create(self);
  with SaveImageDialog do
  begin
    InitialDir := GetCurrentDir;
    Title := 'Save image as Dynnu Vector Image';
    DefaultExt:= 'dvimg';
    Filter := 'Dynnu Vector Image|*.dvimg|';
    FileName:= ImageName;
  end;
  if SaveImageDialog.Execute then
  begin
    if FileExists(SaveImageDialog.FileName) then
    begin
      BoxStyle := MB_ICONQUESTION + MB_YESNO;
      Reply := Application.MessageBox('Overwrite file?',
      'Overwrite file dialog', BoxStyle);
      if (Reply = IDYES) then
      begin
        WriteToFile(SaveImageDialog.FileName);
        HistoryBuffer.savedposition := HistoryBuffer.position;
      end else
      begin
        SaveImageDialog.Free;
        tempObject := TObject.Create;
        SaveAsMenuItemClick(tempObject);
        tempObject.Free;
        exit;
      end;
    end else
    begin
      WriteToFile(SaveImageDialog.FileName);
      HistoryBuffer.savedposition := HistoryBuffer.position;
    end;
  end;
  SaveImageDialog.Free;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin
  if (LastSavedFile=ImageName) then
    WriteToFile(ImageName)
  else
    SaveAsMenuItemClick(TObject.Create);
end;

procedure TMainForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if not ScrollBool then
    begin
      SetOffset(Point(HorizontalScrollBar.Position,VerticalScrollBar.Position));
      Invalidate;
    end;
  ScrollBool:=false;
end;

procedure TMainForm.MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (isDrawing) then
    begin
      if (CurrentTool.ClassName = 'TRectSelectionTool') then
      begin
        FreeAndNil(PropPanel);
        PropPanel := TPanel.Create(ToolsPanel);
        PropPanel.Parent := ToolsPanel;
        PropPanel.Align := alClient;
        PropPanel.Name := 'PropPanel';
        PropPanel.Caption := '';
        CurrentTool.Initialize(PropPanel,MainPaintBox);
      end;
      isDrawing := false;
      CurrentTool.StopDraw(X,Y,MainPaintBox.Height, MainPaintBox.Width,
        RBtn,PropPanel);
      ZoomSpinEdit.Value := scalesunit.Zoom;
      RBtn := false;
      if (not isInvisible) or (CurrentTool is TMoveTool) then
      begin
        if (HistoryBuffer.AvaibleRedos > 0) then
          HistoryBuffer.CutOff;
      end;
      if (not isInvisible) then
        HistoryBuffer.AddToBuffer(ActionDraw);
      Invalidate;
    end;
end;

procedure TMainForm.MainPaintBoxResize(Sender: TObject);
begin
  if (OldPaintBoxSize <> Point(0,0)) then
  begin
    Offset.x := round(Offset.x - (MainPaintBox.Width-OldPaintBoxSize.x) div 2);
    Offset.y := round(Offset.y - (MainPaintBox.Height-OldPaintBoxSize.y) div 2);
  end;
  scalesunit.PaintBoxSize.x   := MainPaintBox.Width;
  scalesunit.PaintBoxSize.y   := MainPaintBox.Height;
  OldPaintBoxSize.x := MainPaintBox.Width;
  OldPaintBoxSize.y := MainPaintBox.Height;
end;

procedure TMainForm.ShowAllButtonClick(Sender: TObject);
begin
  RectZoom(MainPaintBox.Height,MainPaintBox.Width,MinFloatPoint,MaxFloatPoint);
  ZoomSpinEdit.Value:=round(Zoom);
  Invalidate;
end;

procedure TMainForm.ShowAllItemClick(Sender: TObject);
begin
  ShowAllButtonClick(Sender);
end;

procedure TMainForm.ToolButtonClick(Sender: TObject);
var
  atag: integer;
begin
  atag := Integer((Sender as TBitBtn).Tag);
  CurrentTool := ToolsRegister[atag];

  FreeAndNil(PropPanel);
  PropPanel := TPanel.Create(ToolsPanel);
  PropPanel.Parent := ToolsPanel;
  PropPanel.Align := alClient;
  PropPanel.Name := 'PropPanel';
  PropPanel.Caption := '';
  CurrentTool.Initialize(PropPanel,MainPaintBox);
end;

procedure TMainForm.ColorsGridDblClick(Sender: TObject);
var
  aState: TGridDrawState;
begin
  if ColorsGridColorDialog.Execute then
  begin
    PenColor := ColorsGridColorDialog.Color;
    Colors[ColorsGrid.ColCount*((Sender as TDrawGrid).Row)+
      (Sender as TDrawGrid).Col] := ColorsGridColorDialog.Color;
    ColorsGridDrawCell(Sender,(Sender as TDrawGrid).Col,
      (Sender as TDrawGrid).Row,Rect(1,1,1,1),aState);
    ColorLabel1.Color := PenColor;
  end;
end;

procedure TMainForm.ColorsGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  ColorsGrid.Canvas.Brush.Color := Colors[ColorsGrid.ColCount*(aRow)+aCol];
  ColorsGrid.Canvas.FillRect(aRect);
end;

procedure TMainForm.ColorsGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col,Row,i: integer;
begin
  ColorsGrid.MouseToCell(X,Y,Col,Row);
  if (ssLeft  in Shift) then
    PenColor   := Colors[ColorsGrid.ColCount*Row+Col];
  if (ssRight in Shift) then
    BrushColor := Colors[ColorsGrid.ColCount*Row+Col];
  ColorLabel1.Color := PenColor;
  ColorLabel2.Color := BrushColor;
  for i := low(Figures) to high(Figures) do
  begin
    if (Figures[i].Selected) then
    begin
      if (ssLeft in Shift) then
        (Figures[i] as TVisibleFigure).FigurePenColor   := PenColor;
      if (ssRight in Shift) then
        (Figures[i] as TVisibleFigure).FigureBrushColor := BrushColor;
    end;
  end;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (isDrawing) then
  begin
    if (ssLeft in Shift) then
      CurrentTool.AddPoint(Point(X,Y));
    Invalidate;
  end;
end;

procedure TMainForm.MainPaintBoxPaint(Sender: TObject);
var
  i: integer;
begin
  for i := low(Figures) to high(Figures) do
    Figures[i].Draw(MainPaintBox.Canvas);

  if round(MinFloatPoint.X*Zoom/100)<HorizontalScrollBar.Min then
    HorizontalScrollBar.Min:=round(MinFloatPoint.X*Zoom/100);
  if HorizontalScrollBar.Max<round(MaxFloatPoint.X*Zoom/100) then
    HorizontalScrollBar.Max:=round(MaxFloatPoint.X*Zoom/100);

  if round(MinFloatPoint.Y*Zoom/100)<VerticalScrollBar.Min then
    VerticalScrollBar.Min:=round(MinFloatPoint.Y*Zoom/100);
  if VerticalScrollBar.Max<round(MaxFloatPoint.Y*Zoom/100) then
    VerticalScrollBar.Max:=round(MaxFloatPoint.Y*Zoom/100);

  if Offset.x+MainPaintBox.Width>HorizontalScrollBar.Max then
    HorizontalScrollBar.Max:=Offset.x+MainPaintBox.Width;
  if Offset.x<HorizontalScrollBar.Min then
    HorizontalScrollBar.Min:=Offset.x;
  if Offset.y+MainPaintBox.Height>VerticalScrollBar.Max then
    VerticalScrollBar.Max:=Offset.y+MainPaintBox.Height;
  if Offset.y<VerticalScrollBar.Min then
    VerticalScrollBar.Min:=Offset.y;

  ScrollBool:=true;
  HorizontalScrollBar.Position:=Offset.x;
  VerticalScrollBar.Position:=Offset.y;

  if (HistoryBuffer.AvaibleRedos=0) then
    RedoMenuItem.Enabled := false
  else
    RedoMenuItem.Enabled := true;
  if (HistoryBuffer.AvaibleUndos=0) then
    UndoMenuItem.Enabled := false
  else
    UndoMenuItem.Enabled := true;

  if (HistoryBuffer.ShowAsterisk) then
    MainForm.Caption:='Dynnu - '+ImageName+'*'
  else
    MainForm.Caption:='Dynnu - '+ImageName;

  HistoryListBoxUpdate(HistoryListBox);
end;

procedure TMainForm.HistoryListBoxUpdate(Sender: TListBox);
var
  spos,epos,i: integer;
begin
  Sender.Items.Clear;
  spos := HistoryBuffer.UpdateSpos;
  epos := HistoryBuffer.UpdateEpos;

  for i := 1 to BufferLength do
  begin
    if (spos <= epos) and ((i >= spos) and (i <= epos)) then
      Sender.Items.Add(HistoryBuffer.action[i]);
    if (spos > epos) and (((i >= epos) and
      (i <= BufferLength)) or ((i >= 1) and (i <= spos))) then
        Sender.Items.Add(HistoryBuffer.action[i]);
  end;
  Sender.ItemIndex := min(HistoryBuffer.position-1,Sender.Items.Count-1);
  Sender.TopIndex  := max(HistoryBuffer.position-3,0);
end;

procedure TMainForm.ScrollBarChange(Sender: TObject);
begin
  SetOffset(FloatPoint(BotScrollCent-HorizontalScrollBar.Position,
    RightScrollCent-VerticalScrollBar.Position));

  BotScrollCent   := HorizontalScrollBar.Position;
  RightScrollCent := VerticalScrollBar.Position;

  VerticalScrollBar.Max   := round(WorldToScreen(MaxFloatPoint).y);
  VerticalScrollBar.Min   := round(WorldToScreen(MinFloatPoint).y);
  HorizontalScrollBar.Max := round(WorldToScreen(MaxFloatPoint).x);
  HorizontalScrollBar.Min := round(WorldToScreen(MinFloatPoint).x);

  Invalidate;
end;

procedure TMainForm.UndoMenuItemClick(Sender: TObject);
begin
  HistoryBuffer.Undo;
  MainPaintBox.Invalidate;
end;

procedure TMainForm.ZoomSpinEditChange(Sender: TObject);
var
  oldzoom:double;
begin
  oldzoom := Zoom;
  Zoom := abs(ZoomSpinEdit.Value);
  CenterZoom(MainPaintBox.Width,MainPaintBox.Height,oldzoom);
  Invalidate;
end;

end.
