unit toolsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL,
  figuresunit,scalesunit;

type

  TFigureClass = figuresunit.TFigureClass;
  TToolClass   = class of TTool;

  TTool = class
    public
    Bitmap: TBitmap;
    FigureClass: TFigureClass;
    procedure Initialize(APanel: TPanel); virtual;
    procedure CreateLineWidthSpinEdit(APanel: TPanel);
    procedure CreateLineStyleComboBox(APanel: TPanel);
    procedure CreateFillStyleComboBox(APanel: TPanel);
    procedure CreateCornersSpinEdit(APanel: TPanel);
    procedure CreateAngleSpinEdit(APanel: TPanel);
    procedure CreateRSpinEdit(APanel: TPanel);
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); virtual;
    procedure AddPoint(APoint: TPoint); virtual;
    procedure StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean); virtual;
    procedure PenStyleComboBoxSelect(Sender: TObject);
    procedure FillStyleComboBoxSelect(Sender: TObject);
    procedure LineWidthSpinEditSelect (Sender: TObject);
    procedure CornersSpinEditSelect (Sender: TObject);
    procedure AngleSpinEditSelect (Sender: TObject);
    procedure AngleModeSpinEditSelect (Sender: TObject);
    procedure RSpinEditXSelect (Sender: TObject);
    procedure RSpinEditYSelect (Sender: TObject);
    procedure StyleComboBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TTwoPointsTools = class(TTool)
  public
    procedure AddPoint(APoint: TPoint); override;
    procedure Initialize(APanel: TPanel); override;
  end;

  TSpecificTools = class(TTool)
  public
    procedure Initialize(APanel: TPanel); override;
  end;

  THandTool       = class(TSpecificTools)
  public
    Figure: THandFigure;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);override;
    procedure AddPoint(APoint: TPoint); override;
    procedure StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean); override;
  end;

  TPolylineTool   = class(TTool)
  public
    Figure: TPolyline;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
    procedure Initialize(APanel: TPanel); override;
  end;

  TRectangleTool  = class(TTwoPointsTools)
  public
    Figure: TRectangle;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
  end;

  TRoundRectTool  = class(TTwoPointsTools)
  public
    Figure: TRectangle;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
    procedure Initialize(APanel: TPanel); override;
  end;

  TEllipseTool    = class(TTwoPointsTools)
  public
    Figure: TEllipse;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
  end;

  TPolygonTool = class(TTwoPointsTools)
  public
    Figure: TPolygon;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
    procedure Initialize(APanel: TPanel); override;
  end;

  TLineTool       = class(TTwoPointsTools)
  public
    Figure: TLine;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
    procedure Initialize(APanel: TPanel); override;
  end;

  TMagnifierTool  = class(TTwoPointsTools)
  public
    Figure: TRectangle;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
    procedure StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean); override;
    procedure Initialize(APanel: TPanel); override;
  end;

  TPointSelectionTool = class(TTool)
  public
    Figure: TRectangle;
    procedure FigureCreate(AFigureClass: TFigureClass; APoint: TPoint); override;
    procedure AddPoint(APoint: TPoint); override;
    procedure StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean); override;
  end;

  TRectSelectionTool = class(TTwoPointsTools)
  public
    Figure: TRectangle;
    procedure AddPoint(APoint: TPoint); override;
  end;

var
  ToolsRegister: array of TTool;
  OffsetFirstPoint: TPoint;
  AngleSpinEdit: TSpinEdit;
  AngleMode: boolean;
implementation

procedure TTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := AFigureClass.Create;
  with Figures[high(Figures)] do begin
    FigurePenColor := PenColor;
    FigureBrushColor := BrushColor;
    SetLength(Points,1);
    Points[high(Points)] := scalesunit.ScreenToWorld(APoint);
  end;
end;

procedure TPointSelectionTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
var
  i,j: integer;
  tempRect: tRect;
begin
  Inherited;
  with Figures[high(Figures)] do
  begin
    SetLength(Points,2);
    Points[high(Points)] := scalesunit.ScreenToWorld(APoint);
  end;
  for i := low(Figures)+1 to high(Figures)-1 do
  begin
    with Figures[i] do
    begin
      SetRegion;
      if (PtInRegion(FigureRegion,APoint.X,APoint.Y)=true) and (Selected = false) then
        Selected := true
      else if (PtInRegion(FigureRegion,APoint.X,APoint.Y)=true) and (Selected = true) then
        Selected := false;
    end;
  end;
end;

procedure TPolylineTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  with Figures[high(Figures)] as TPolyline do begin
    FigurePenStyle := PenStyle;
    FigurePenWidth := PenWidth;
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure THandTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  OffsetFirstPoint.x:=Offset.x+APoint.x;
  OffsetFirstPoint.y:=Offset.y+APoint.y;
end;

procedure TRectangleTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  with Figures[high(Figures)] as TRectangle do begin
    FigurePenStyle := PenStyle;
    FigureBrushStyle := BrushStyle;
    FigurePenWidth := PenWidth;
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TRoundRectTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  with Figures[high(Figures)] as TRoundRect do begin
    FigurePenStyle := PenStyle;
    FigureBrushStyle := BrushStyle;
    FigurePenWidth := PenWidth;
    FigureR := RectR;
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TEllipseTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  with Figures[high(Figures)] as TEllipse do begin
    FigurePenStyle := PenStyle;
    FigureBrushStyle := BrushStyle;
    FigurePenWidth := PenWidth;
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TLineTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  with Figures[high(Figures)] as TLine do begin
    FigurePenStyle := PenStyle;
    FigurePenWidth := PenWidth;
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TPolygonTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
  with Figures[high(Figures)] as TPolygon do begin
    FigurePenStyle := PenStyle;
    FigureBrushStyle := BrushStyle;
    FigurePenWidth := PenWidth;
    FigureCorners := Corners;
    FigureAngle := Angle;
    FigureAngleMode := AngleMode;
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TMagnifierTool.FigureCreate(AFigureClass: TFigureClass; APoint: TPoint);
begin
  Inherited;
end;

procedure TTool.AddPoint(APoint: TPoint);
begin
  with Figures[high(Figures)] do begin
    SetLength(Points,length(Points)+1);
    Points[high(Points)] := scalesunit.ScreenToWorld(APoint);
  end;
  SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TTwoPointsTools.AddPoint(APoint: TPoint);
begin
  with Figures[high(Figures)] do begin
    SetLength(Points,2);
    Points[high(Points)] := scalesunit.ScreenToWorld(APoint);
  end;
  if (ClassName <> 'TMagnifierTool') and (ClassName <> 'TSelectionTool') then
        SetMaxMinFloatPoints(ScreenToWorld(APoint));
end;

procedure TRectSelectionTool.AddPoint(APoint: TPoint);
begin
  Inherited;
  {with Figures[high(Figures)] do begin
    FigureRegion := CreateRectRgn (round(ScreenToWorld(Points[low(Points)]) .x),
                                   round(ScreenToWorld(Points[low(Points)]) .y),
                                   round(ScreenToWorld(Points[high(Points)]).x),
                                   round(ScreenToWorld(Points[high(Points)]).y));
  end; }
end;
procedure TPointSelectionTool.AddPoint(APoint: TPoint);
begin
end;

procedure THandTool.AddPoint(APoint: TPoint);
begin
  with Figures[high(Figures)] do begin
    SetLength(Points,2);
    Points[high(Points)].x := scalesunit.ScreenToWorld(APoint).x -
      Points[low(Points)].x;
    Points[high(Points)].y := scalesunit.ScreenToWorld(APoint).y -
      Points[low(Points)].y;
    Offset.x := OffsetFirstPoint.x-APoint.x;
    Offset.y := OffsetFirstPoint.y-APoint.y;
  end;
end;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; ABitmapFile: string);
begin
  setlength(ToolsRegister,length(ToolsRegister)+1);
  ToolsRegister[high(ToolsRegister)] := ATool;
  with ToolsRegister[high(ToolsRegister)] do begin
    Bitmap := TBitmap.Create;
    Bitmap.LoadFromFile(ABitmapFile);
    FigureClass := AFigureClass;
  end;
end;

procedure TTool.StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean);
begin
end;

procedure TMagnifierTool.StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean);
var
  t: double;
begin
    with Figures[high(Figures)] do begin
      if(Points[low(Points)].x+5>Points[high(Points)].x) then
      begin
        if (not RBtn) then t := Zoom*2 else t := Zoom / 2;
        if (t>800) then
        begin
          setlength(Figures,length(Figures)-1);
          exit;
        end;
        Zoom := t;
        scalesunit.ToPointZoom(FloatPoint(X,Y));
        RBtn := false;
      end else begin
        RectZoom(AHeight, AWidth, Points[0], Points[1]);
      end;
    setlength(Figures,length(Figures)-1);
  end;
end;

procedure TPointSelectionTool.StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean);
begin
  setlength(Figures,length(Figures)-1);
end;

procedure THandTool.StopDraw(X,Y, AHeight, AWidth: integer; RBtn: boolean);
begin
  setlength(Figures,length(Figures)-1);
end;

procedure TTool.PenStyleComboBoxSelect(Sender: TObject);
begin
  figuresunit.PenStyle := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle),
  (Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex]));
end;

procedure TTool.FillStyleComboBoxSelect(Sender: TObject);
begin
  figuresunit.BrushStyle := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle),
  (Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex]));
end;

procedure TTool.LineWidthSpinEditSelect (Sender: TObject);
begin
  PenWidth := (Sender as TSpinEdit).Value;
end;

procedure TTool.CornersSpinEditSelect (Sender: TObject);
begin
  Corners := (Sender as TSpinEdit).Value;
end;

procedure TTool.AngleSpinEditSelect (Sender: TObject);
begin
  Angle := ((Sender as TSpinEdit).Value*Pi)/180;
end;

procedure TTool.AngleModeSpinEditSelect (Sender: TObject);
begin
  if ((Sender as TCheckBox).Checked = true) then
  begin
    AngleSpinEdit.Enabled := true;
    AngleMode := true;
  end else
  begin
    AngleSpinEdit.Enabled := false;
    AngleMode := false;
  end;
end;

procedure TTool.RSpinEditXSelect (Sender: TObject);
begin
  RectR.x := (Sender as TSpinEdit).Value;
end;

procedure TTool.RSpinEditYSelect (Sender: TObject);
begin
  RectR.y := (Sender as TSpinEdit).Value;
end;

procedure TTool.CreateLineWidthSpinEdit(APanel: TPanel);
var
  LineWidthSpinEdit: TSpinEdit;
  l: Tlabel;
begin
  LineWidthSpinEdit := TSpinEdit.Create(APanel);
  LineWidthSpinEdit.Name := 'LineWidthSpinEdit';
  LineWidthSpinEdit.Parent := APanel;
  LineWidthSpinEdit.Align := alBottom;
  LineWidthSpinEdit.MaxValue := 100;
  LineWidthSpinEdit.MinValue := 1;
  LineWidthSpinEdit.Value := PenWidth;
  LineWidthSpinEdit.OnChange := @LineWidthSpinEditSelect;
  l := TLabel.Create(APanel);
  l.name := 'LineWidthLabel';
  l.Caption := 'Line Width';
  l.Parent := APanel;
  l.Align := alBottom;
end;

procedure TTool.CreateCornersSpinEdit(APanel: TPanel);
var
  CornersSpinEdit: TSpinEdit;
  l: TLabel;
begin
  CornersSpinEdit := TSpinEdit.Create(APanel);
  CornersSpinEdit.Name := 'CornersSpinEdit';
  CornersSpinEdit.Parent := APanel;
  CornersSpinEdit.Align := alBottom;
  CornersSpinEdit.MaxValue := 50;
  CornersSpinEdit.MinValue := 3;
  CornersSpinEdit.Value := Corners;
  CornersSpinEdit.OnChange := @CornersSpinEditSelect;
  l := TLabel.Create(APanel);
  l.name := 'CornersLabel';
  l.Caption := 'Number of corners';
  l.Parent := APanel;
  l.Align := alBottom;
end;

procedure TTool.CreateAngleSpinEdit(APanel: TPanel);
var
  l: TLabel;
  c: TCheckBox;
begin
  AngleSpinEdit := TSpinEdit.Create(APanel);
  AngleSpinEdit.Name := 'AngleSpinEdit';
  AngleSpinEdit.Parent := APanel;
  AngleSpinEdit.Align := alBottom;
  AngleSpinEdit.MaxValue := 360;
  AngleSpinEdit.MinValue := 0;
  AngleSpinEdit.Value := Angle;
  AngleSpinEdit.OnChange := @AngleSpinEditSelect;
  AngleSpinEdit.Enabled := false;
  c := TCheckBox.Create(APanel);
  c.Parent := APanel;
  c.name := 'AngleModeCheckBox';
  c.caption := 'Manual angle control';
  c.onChange := @AngleModeSpinEditSelect;
  c.Align := alBottom;
  l := TLabel.Create(APanel);
  l.name := 'AngleLabel';
  l.Caption := 'Rotate Angle';
  l.Parent := APanel;
  l.Align := alBottom;
end;

procedure TTool.CreateRSpinEdit(APanel: TPanel);
var
  l: TLabel;
  RSpinEditX, RSpinEditY: TSpinEdit;
begin
  RSpinEditX := TSpinEdit.Create(APanel);
  RSpinEditX.Name := 'RSpinEditX';
  RSpinEditX.Parent := APanel;
  RSpinEditX.Align := alBottom;
  RSpinEditX.MaxValue := 500;
  RSpinEditX.MinValue := 0;
  RSpinEditX.Value := RectR.x;
  RSpinEditX.OnChange := @RSpinEditXSelect;

  l := TLabel.Create(APanel);
  l.name := 'RSpinEditLabelX';
  l.Caption := 'Rounding Radius (X)';
  l.Parent := APanel;
  l.Align := alBottom;

  RSpinEditY := TSpinEdit.Create(APanel);
  RSpinEditY.Name := 'RSpinEditY';
  RSpinEditY.Parent := APanel;
  RSpinEditY.Align := alBottom;
  RSpinEditY.MaxValue := 500;
  RSpinEditY.MinValue := 0;
  RSpinEditY.Value := RectR.y;
  RSpinEditY.OnChange := @RSpinEditYSelect;

  l := TLabel.Create(APanel);
  l.name := 'RSpinEditLabelY';
  l.Caption := 'Rounding Radius (Y)';
  l.Parent := APanel;
  l.Align := alBottom;
end;

procedure TTool.CreateLineStyleComboBox(APanel: TPanel);
var
  LineStyleComboBox: TComboBox;
  i: integer;
  l: TLabel;
begin
  LineStyleComboBox := TComboBox.Create(APanel);
  LineStyleComboBox.Name := ('LineStyleComboBox');
  LineStyleComboBox.Parent := APanel;
  LineStyleComboBox.Align := alBottom;
  LineStyleComboBox.ReadOnly := true;
  for i := ord(low(TFPPenStyle)) to ord(high(TFPPenStyle))-3 do
  begin
    LineStyleComboBox.Items.Add(GetEnumName(TypeInfo(TFPPenStyle),i));
  end;
  LineStyleComboBox.Items.Add(GetEnumName(TypeInfo(TFPPenStyle),
    ord(high(TFPPenStyle))));
  LineStyleComboBox.ItemIndex := ord(PenStyle);
  LineStyleComboBox.OnSelect  := @PenStyleComboBoxSelect;
  LineStyleComboBox.Style := csOwnerDrawFixed;
  LineStyleComboBox.OnDrawItem := @StyleComboBoxDrawItem;
  l := TLabel.Create(APanel);
  l.name := 'LineStyleLabel';
  l.Caption := 'Line Style';
  l.Parent := APanel;
  l.Align := alBottom;
end;

procedure TTool.StyleComboBoxDrawItem(Control: TWinControl;
Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  GraphicsRect: TRect;
  t: TComboBox;
begin
    t := (Control as TComboBox);
    GraphicsRect.Left   := ARect.Left   + 2;
    GraphicsRect.Right  := ARect.Left   + 18;
    GraphicsRect.Top    := ARect.Top    + 1;
    GraphicsRect.Bottom := ARect.Bottom - 1;

   t.Canvas.FillRect(ARect);
   t.Canvas.TextRect(ARect, 22, ARect.Top, (Control as TComboBox).Items[Index]);
   t.Canvas.Pen.Style := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle),
     t.Items[Index]));
   t.Canvas.Brush.Style := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle),
     t.Items[Index]));
   t.Canvas.Brush.Color := clBlack;

   //костыль для bsClear
   if TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle),
     t.Items[Index])) = TBrushStyle(bsClear) then
   begin
     t.Canvas.Brush.Style := bsSolid;
     t.Canvas.Brush.Color := clWhite;
   end;

   t.Canvas.Rectangle(GraphicsRect);
end;

procedure TTool.CreateFillStyleComboBox(APanel: TPanel);
var
  FillStyleComboBox: TComboBox;
  i: integer;
  l: TLabel;
begin
  FillStyleComboBox := TComboBox.Create(APanel);
  FillStyleComboBox.Name := ('FillStyleComboBox');
  FillStyleComboBox.Parent := APanel;
  FillStyleComboBox.Align := alBottom;
  FillStyleComboBox.ReadOnly := true;
  for i := ord(low(TBrushStyle)) to ord(high(TBrushStyle))-2 do
  begin
    FillStyleComboBox.Items.Add(GetEnumName(TypeInfo(TBrushStyle),i));
  end;
  FillStyleComboBox.ItemIndex := ord(BrushStyle);
  FillStyleComboBox.OnSelect  := @FillStyleComboBoxSelect;
  FillStyleComboBox.Style := csOwnerDrawFixed;
  FillStyleComboBox.OnDrawItem := @StyleComboBoxDrawItem;
  l := TLabel.Create(APanel);
  l.name := 'FillStyleLabel';
  l.Caption := 'Fill Style';
  l.Parent := APanel;
  l.Align := alBottom;
end;

procedure TTool.Initialize(APanel: TPanel);
begin
end;

procedure TPolylineTool.Initialize(APanel: TPanel);
begin
  CreateLineWidthSpinEdit(APanel);
end;

procedure TTwoPointsTools.Initialize(APanel: TPanel);
begin
  CreateLineWidthSpinEdit(APanel);
  CreateLineStyleComboBox(APanel);
  CreateFillStyleComboBox(APanel);
end;

procedure TRoundRectTool.Initialize(APanel: TPanel);
begin
  Inherited;
  CreateRSpinEdit(APanel);
end;

procedure TPolygonTool.Initialize(APanel: TPanel);
begin
  CreateLineWidthSpinEdit(APanel);
  CreateLineStyleComboBox(APanel);
  CreateFillStyleComboBox(APanel);
  CreateCornersSpinEdit(APanel);
  CreateAngleSpinEdit(APanel);
end;

procedure TSpecificTools.Initialize(APanel: TPanel);
begin
end;
procedure TMagnifierTool.Initialize(APanel: TPanel);
begin
end;
procedure TLineTool.Initialize(APanel: TPanel);
begin
  CreateLineWidthSpinEdit(APanel);
  CreateLineStyleComboBox(APanel);
end;

initialization
RegisterTool (TPolylineTool.Create, TPolyline, 'Pencil.bmp');
RegisterTool (TRectangleTool.Create, TRectangle, 'Rectangle.bmp');
RegisterTool (TEllipseTool.Create, TEllipse, 'Ellipse.bmp');
RegisterTool (TLineTool.Create, TLine, 'Line.bmp');
RegisterTool (TMagnifierTool.Create, TMagnifierFrame, 'Magnifier.bmp');
RegisterTool (THandTool.Create, THandFigure, 'Hand.bmp');
RegisterTool (TPolygonTool.Create, TPolygon, 'Polygon.bmp');
RegisterTool (TRoundRectTool.Create, TRoundRect, 'RoundRect.bmp');
RegisterTool (TRectSelectionTool.Create, TMagnifierFrame, 'Selection.bmp');
RegisterTool (TPointSelectionTool.Create, TRectangle, 'PSelection.bmp');
AngleMode := false;
end.

