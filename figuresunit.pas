unit figuresunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, strutils,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows,
  scalesunit, historyunit;
type
  TFigureClass    = class  of TFigure;
  tempPointsArray = array[0..3] of TPoint;
  PolygonPointsArray = array of TPoint;
  StringArray = historyunit.StringArray;

  TFigure = class (TPersistent)
  public
    Points: array of TFloatPoint;
    Selected: boolean;
    FigureRegion: HRGN;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawSelection(Pt1,Pt2: TPoint; Canvas: TCanvas); virtual;
    procedure SetRegion; virtual; abstract;
    function Save: StringArray; virtual;
    procedure Load(AParams: StringArray); virtual;
  end;

  TVisibleFigure = class (TFigure)
  public
    FigurePenColor,FigureBrushColor: TColor;
    FigurePenWidth: integer;
    procedure Draw(Canvas: TCanvas); override;
    function Save: StringArray override;
    procedure Load(AParams: StringArray); override;
  end;

  TInvisibleFigure = class (TFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

  TPenStyleFigure = class (TVisibleFigure)
  public
    FigurePenStyle: TPenStyle;
    function Save: StringArray; override;
    procedure Load(AParams: StringArray); override;
  end;

  TBrushStyleFigure = class (TPenStyleFigure)
  public
    FigureBrushStyle: TBrushStyle;
    function Save: StringArray; override;
    procedure Load(AParams: StringArray); override;
  end;

  TPolyline       = class(TVisibleFigure)
  public
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TLine           = class(TPenStyleFigure)
  public
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TRectangle      = class(TBrushStyleFigure)
  public
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TEllipse        = class(TBrushStyleFigure)
  public
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TRoundRect      = class(TBrushStyleFigure)
  public
    FigureR: TPoint;
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
    function Save: StringArray; override;
    procedure Load(AParams: StringArray); override;
  end;

  TPolygon = class(TBrushStyleFigure)
  public
    FigureCorners: integer;
    FigureAngle: double;
    FigureAngleMode: boolean;
    procedure Draw(Canvas:TCanvas); override;
    function Rotate(P1,P2: TFloatPoint; angle: double): TFloatPoint;
    function CreatePolygon (P1,P2: TFloatPoint; AFigureCorners: integer; AFigureAngle: double; AFigureAngleMode: boolean): PolygonPointsArray;
    procedure SetRegion; override;
    function Save: StringArray; override;
    procedure Load(AParams: StringArray); override;
  end;

  THandFigure     = class(TInvisibleFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

  TMagnifierFrame = class(TInvisibleFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

  function CreateRectAroundLine(p1,p2: TPoint; FigurePenWidth: integer): tempPointsArray;

const
  inf:integer = 2147483647;
var
  Figures: array of TFigure;
  PenColor,BrushColor: TColor;
  FiguresRegister: array of TFigureClass;
  HandPrevCent: TPoint;
  PenStyle: TPenStyle;
  BrushStyle: TBrushStyle;
  PenWidth: integer;
  Corners: integer;
  Angle: double;
  RectR: TPoint;
implementation
function CreateRectAroundLine(p1,p2: TPoint; FigurePenWidth: integer): tempPointsArray;
begin
  if (abs(p2.x-p1.x)>45) then
  begin
    Result[0].x := p1.x-FigurePenWidth div 2;
    Result[0].y := p1.y-5-FigurePenWidth;
    Result[1].x := p2.x+FigurePenWidth div 2;
    Result[1].y := p2.y-5-FigurePenWidth;
    Result[2].x := p2.x+FigurePenWidth div 2;
    Result[2].y := p2.y+5+FigurePenWidth;
    Result[3].x := p1.x-FigurePenWidth div 2;
    Result[3].y := p1.y+5+FigurePenWidth;
  end else
  begin
    Result[0].x := p1.x-5-FigurePenWidth;
    Result[0].y := p1.y-FigurePenWidth div 2;
    Result[1].x := p2.x-5-FigurePenWidth;
    Result[1].y := p2.y+FigurePenWidth div 2;
    Result[2].x := p2.x+5+FigurePenWidth;
    Result[2].y := p2.y+FigurePenWidth div 2;
    Result[3].x := p1.x+5+FigurePenWidth;
    Result[3].y := p1.y-FigurePenWidth div 2;
  end;
end;

procedure TFigure.DrawSelection(Pt1,Pt2: TPoint; Canvas: TCanvas);
var
  lP,hP: TPoint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Width := 1;
  lP.x := min(Pt1.x,Pt2.x);
  lP.y := min(Pt1.y,Pt2.y);
  hP.x := max(Pt1.x,Pt2.x);
  hP.y := max(Pt1.y,Pt2.y);
  Canvas.Ellipse(lp.X-5,
                 lP.y-5,
                 lP.x+5,
                 lP.y+5);
  Canvas.Ellipse(hP.x-5,
                 hP.y-5,
                 hP.x+5,
                 hP.y+5);
  Canvas.Ellipse(hP.x-5,
                 lP.y-5,
                 hP.x+5,
                 lP.y+5);
  Canvas.Ellipse(lP.x-5,
                 hP.y-5,
                 lP.x+5,
                 hP.y+5);
  Canvas.Pen.Style := psDash;
  Canvas.Frame  (lP.x-5,
                 lP.y-5,
                 hP.x+5,
                 hP.y+5);
end;

procedure TFigure.Draw(Canvas: TCanvas);
begin
end;

procedure TVisibleFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Brush.Color := FigureBrushColor;
end;

procedure TInvisibleFigure.Draw(Canvas: TCanvas);
begin
end;


procedure TPolyline.Draw(Canvas: TCanvas);
var
  i: integer;
  lP,hP: TPoint;
begin
  Inherited;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  lP.x := 0;
  lP.y := 0;
  hP.x := inf;
  hP.y := inf;
  for i := low(Points) to high(Points)-1 do
  begin
    Canvas.Line   (scalesunit.WorldToScreen(Points[i])  .x,
                   scalesunit.WorldToScreen(Points[i])  .y,
                   scalesunit.WorldToScreen(Points[i+1]).x,
                   scalesunit.WorldToScreen(Points[i+1]).y);
  end;
  for i := low(Points) to High(Points) do
  begin
    if (lP.x < WorldToScreen(Points[i]).x) then lP.x := WorldToScreen(Points[i]).x;
    if (lP.y < WorldToScreen(Points[i]).y) then lP.y := WorldToScreen(Points[i]).y;
    if (hP.x > WorldToScreen(Points[i]).x) then hP.x := WorldToScreen(Points[i]).x;
    if (hP.y > WorldToScreen(Points[i]).y) then hP.y := WorldToScreen(Points[i]).y;
  end;
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    DrawSelection(lP,hP,Canvas);
  end;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Rectangle(scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    DrawSelection(WorldToScreen(Points[low(Points)]),WorldToScreen(Points[high(Points)]),Canvas);
  end;
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.RoundRect(scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y,
                   FigureR.x,
                   FigureR.y);
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    DrawSelection(WorldToScreen(Points[low(Points)]),WorldToScreen(Points[high(Points)]),Canvas);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Ellipse  (scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    DrawSelection(WorldToScreen(Points[low(Points)]),WorldToScreen(Points[high(Points)]),Canvas);
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Inherited;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := bsSolid;
  Canvas.Line     (scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    DrawSelection(WorldToScreen(Points[low(Points)]),WorldToScreen(Points[high(Points)]),Canvas);
  end;
end;

procedure THandFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Width := 1;
  Canvas.Ellipse  (scalesunit.WorldToScreen(Points[low(Points)]).x-5,
                   scalesunit.WorldToScreen(Points[low(Points)]).y-5,
                   scalesunit.WorldToScreen(Points[low(Points)]).x+5,
                   scalesunit.WorldToScreen(Points[low(Points)]).y+5);
end;

procedure TPolygon.Draw(Canvas: TCanvas);
var
  P1,P2: TFloatPoint;
  i: integer;
  PolygonPointsScr: PolygonPointsArray;
  lP,hP: TFloatPoint;
begin
  Inherited;
  P1 := Points[low(Points)];
  P2 := Points[high(Points)];
  lP.x := 0;
  lP.y := 0;
  hP.x := inf;
  hP.y := inf;
  PolygonPointsScr := CreatePolygon(P1,P2,FigureCorners,FigureAngle,FigureAngleMode);
  for i := low(PolygonPointsScr) to high (PolygonPointsScr) do
  begin
    if (lP.x < PolygonPointsScr[i].x) then lP.x := PolygonPointsScr[i].x;
    if (lP.y < PolygonPointsScr[i].y) then lP.y := PolygonPointsScr[i].y;
    if (hP.x > PolygonPointsScr[i].x) then hP.x := PolygonPointsScr[i].x;
    if (hP.y > PolygonPointsScr[i].y) then hP.y := PolygonPointsScr[i].y;
  end;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Polygon(PolygonPointsScr);
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    DrawSelection(lP,hP,Canvas);
  end;
end;

procedure TMagnifierFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Width := 1;
  Canvas.Frame    (scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
end;
function TPolygon.Rotate(P1,P2: TFloatPoint; angle: double): TFloatPoint;
begin
  Result.x := P1.x+(P2.x-P1.x)*cos(angle)-(P2.Y-P1.Y)*sin(angle);
  Result.Y := P1.y+(P2.x-P1.x)*sin(angle)+(P2.Y-P1.Y)*cos(angle);
end;

procedure TRectangle.SetRegion;
var
  tempRect: TRect;
begin
  tempRect.TopLeft := WorldToScreen(Points[low(Points)]);
  tempRect.BottomRight := WorldToScreen(Points[high(Points)]);
  FigureRegion := CreateRectRgn (tempRect.Left,tempRect.Top,tempRect.Right,tempRect.Bottom);
end;

procedure TEllipse.SetRegion;
var
  tempRect: TRect;
begin
  tempRect.TopLeft := WorldToScreen(Points[low(Points)]);
  tempRect.BottomRight := WorldToScreen(Points[high(Points)]);
  FigureRegion := CreateEllipticRgn (tempRect.Left,tempRect.Top,tempRect.Right,tempRect.Bottom);
end;

procedure TRoundRect.SetRegion;
var
  tempRect: TRect;
begin
  tempRect.TopLeft := WorldToScreen(Points[low(Points)]);
  tempRect.BottomRight := WorldToScreen(Points[high(Points)]);
  FigureRegion := CreateRoundRectRgn (tempRect.Left,tempRect.Top,tempRect.Right,tempRect.Bottom,RectR.x,RectR.y);
end;

procedure TLine.SetRegion;
var
  tempPoints: tempPointsArray;
  p1,p2: TPoint;
begin
  p1 := WorldToScreen(Points[low(Points)]);
  p2 := WorldToScreen(Points[high(Points)]);
  tempPoints := CreateRectAroundLine(p1,p2,FigurePenWidth);
  FigureRegion := CreatePolygonRgn (tempPoints,length(tempPoints),winding);
end;

procedure TPolyline.SetRegion;
var
  tempPoints: array[0..3] of TPoint;
  p1,p2: TPoint;
  curRgn: HRGN;
  i: integer;
begin
  for i := low(Points) to high(Points)-1 do
  begin
    p1 := WorldToScreen(Points[i]);
    p2 := WorldToScreen(Points[i+1]);
    tempPoints := CreateRectAroundLine(p1,p2,FigurePenWidth);
    if (i=low(Points)) then FigureRegion := CreatePolygonRgn (tempPoints,length(tempPoints),winding);
    curRgn := CreatePolygonRgn (tempPoints,length(tempPoints),winding);
    CombineRgn (FigureRegion,FigureRegion,curRgn,RGN_OR);
    DeleteObject(curRgn);
  end;
end;

procedure TPolygon.SetRegion;
var
  P1,P2: TFloatPoint;
  PolygonPointsScr: PolygonPointsArray;
begin
  P1 := Points[low(Points)];
  P2 := Points[high(Points)];
  PolygonPointsScr := CreatePolygon(P1,P2,FigureCorners,FigureAngle,FigureAngleMode);
  FigureRegion := CreatePolygonRgn (PolygonPointsScr[0],length(PolygonPointsScr),winding);
end;

function TPolygon.CreatePolygon (P1,P2: TFloatPoint; AFigureCorners: integer;
  AFigureAngle: double; AFigureAngleMode: boolean): PolygonPointsArray;
var
  r: double;
  i,k: integer;
  PolygonPoints: array of TFloatPoint;
begin
  r := sqrt(abs(sqr(P2.x-P1.x) + sqr(P2.y-P1.y)));
  k:=360 div AFigureCorners;
  if (not AFigureAngleMode) then AFigureAngle := Arctan2(p1.Y - p2.Y, p1.X - p2.X);
  setlength (PolygonPoints, AFigurecorners);
  setlength (Result, AFigurecorners);
  for i := low(PolygonPoints) to high(PolygonPoints) do
  begin
    PolygonPoints[i].x := P1.x + r*cos(i*k/180*Pi);
    PolygonPoints[i].y := P1.Y + r*sin(i*k/180*Pi);
    Result[i] := WorldToScreen(Rotate(P1,PolygonPoints[i],AFigureAngle));
  end;
  FigureAngle := AFigureAngle;
end;

function TFigure.Save: StringArray;
var
  i: integer;
begin
  setlength(Result,3);
  Result[0] := IntToStr(length(Result)-1);
  Result[1] := ClassName;
  Result[2] := '';
  for i := low(Points) to high(Points) do
    Result[2] := Result[2] + ' ' + FloatToStr(Points[i].x) + ' ' + FloatToStr(Points[i].y);
end;

function TVisibleFigure.Save: StringArray;
begin
  Inherited;
  Result := Inherited;
  setlength(Result,length(Result)+3);
  Result[0] := IntToStr(length(Result)-1);
  Result[high(Result)-2] := ColorToString(FigurePenColor);
  Result[high(Result)-1] := ColorToString (FigureBrushColor);
  Result[high(Result)] := IntToStr(FigurePenWidth);
end;

function TPenStyleFigure.Save: StringArray;
begin
  Inherited;
  Result := Inherited;
  Setlength(Result,length(Result)+1);
  Result[0] := IntToStr(length(Result)-1);
  Result[high(Result)] := GetEnumName(TypeInfo(TFPPenStyle),ord(FigurePenStyle));
end;

function TBrushStyleFigure.Save: StringArray;
begin
  Inherited;
  Result := Inherited;
  Setlength(Result,length(Result)+1);
  Result[0] := IntToStr(length(Result)-1);
  Result[high(Result)] := GetEnumName(TypeInfo(TBrushStyle),ord(FigureBrushStyle));
end;

function TRoundRect.Save: StringArray;
begin
  Inherited;
  Result := Inherited;
  Setlength(Result,length(Result)+1);
  Result[0] := IntToStr(length(Result)-1);
  Result[high(Result)] := IntToStr(FigureR.x) + ' ' + IntToStr(FigureR.y);
end;

function TPolygon.Save: StringArray;
begin
  Inherited;
  Result := Inherited;
  setlength(Result,length(Result)+3);
  Result[0] := IntToStr(length(Result)-1);
  Result[high(Result)-2] := IntToStr(FigureCorners);
  Result[high(Result)-1] := FloatToStr(FigureAngle);
  Result[high(Result)] := BoolToStr(FigureAngleMode,'true','false');
end;

procedure TFigure.Load(AParams: StringArray);
var
  ptsNumber: integer;
  i,cnt: integer;
begin
  delete(AParams[1],1,1);
  ptsNumber := WordCount(AParams[1],[' ']) div 2;
  setlength(Points,ptsNumber);
  i := -1;
  cnt := -1;
  WHILE (cnt+1 < ptsNumber) do
  begin
    inc(cnt);
    inc(i);
    Points[cnt].x := StrToFloatDef(ExtractWord(i+1,aParams[1],[' ']),0);
    inc(i);
    Points[cnt].y := StrToFloatDef(ExtractWord(i+1,aParams[1],[' ']),0);
  end;
end;

procedure TVisibleFigure.Load(AParams: StringArray);
begin
  Inherited;
  FigurePenColor := StringToColor(AParams[2]);
  FigureBrushColor := StringToColor(AParams[3]);
  FigurePenWidth := StrToIntDef(AParams[4],1);
end;

procedure TPenSTyleFigure.Load(AParams: StringArray);
begin
  Inherited;
  FigurePenStyle := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle),
  AParams[5]));
end;

procedure TBrushSTyleFigure.Load(AParams: StringArray);
begin
  Inherited;
  FigureBrushStyle := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle),
  AParams[6]));
end;

procedure TPolygon.Load(AParams: StringArray);
begin
  Inherited;
  FigureCorners := StrToIntDef(AParams[7],3);
  FigureAngle := StrToFloat(AParams[8]);
end;

procedure TRoundRect.Load(AParams: StringArray);
begin
  Inherited;
  FigureR.x := StrToIntDef(ExtractWord(1,aParams[7],[' ']),0);
  FigureR.y := StrToIntDef(ExtractWord(2,aParams[7],[' ']),0);
end;

initialization
PenWidth := 1;
Corners := 3;
end.

