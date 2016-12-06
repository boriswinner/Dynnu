unit figuresunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows,
  scalesunit;
type
  TFigureClass    = class  of TFigure;

  TFigure = class
  public
    Points: array of TFloatPoint;
    FigurePenColor,FigureBrushColor: TColor;
    Selected: boolean;
    FigureRegion: HRGN;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawSelection(Pt1,Pt2: TPoint; Canvas: TCanvas); virtual;
    procedure SetRegion; virtual; abstract;
  end;

  TPolyline       = class(TFigure)
  public
    FigurePenWidth: integer;
    FigurePenStyle: TPenStyle;
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TRectangle      = class(TFigure)
  public
    FigurePenStyle: TPenStyle;
    FigureBrushStyle: TBrushStyle;
    FigurePenWidth: integer;
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TRoundRect      = class(TFigure)
  public
    FigurePenStyle: TPenStyle;
    FigureBrushStyle: TBrushStyle;
    FigurePenWidth: integer;
    FigureR: TPoint;
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TEllipse        = class(TFigure)
  public
    FigurePenStyle: TPenStyle;
    FigureBrushStyle: TBrushStyle;
    FigurePenWidth: integer;
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TLine           = class(TFigure)
  public
    FigurePenWidth: integer;
    FigurePenStyle: TPenStyle;
    procedure Draw(Canvas:TCanvas); override;
    procedure SetRegion; override;
  end;

  TPolygon = class(TFigure)
  public
    FigurePenWidth: integer;
    FigurePenStyle: TPenStyle;
    FigureBrushStyle: TBrushStyle;
    FigureCorners: integer;
    FigureAngle: double;
    FigureAngleMode: boolean;
    procedure Draw(Canvas:TCanvas); override;
    function Rotate(P1,P2: TFloatPoint; angle: double): TFloatPoint;
    procedure SetRegion; override;
  end;

  THandFigure     = class(TFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

  TMagnifierFrame = class(TFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

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

procedure TFigure.DrawSelection(Pt1,Pt2: TPoint; Canvas: TCanvas);
var
  lP,hP: TPoint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Width := 1;
  lP.x := min(WorldToScreen(Pt1).x,WorldToScreen(Pt2).x);
  lP.y := min(WorldToScreen(Pt1).y,WorldToScreen(Pt2).y);
  hP.x := max(WorldToScreen(Pt1).x,WorldToScreen(Pt2).x);
  hP.y := max(WorldToScreen(Pt1).y,WorldToScreen(Pt2).y);
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
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Brush.Color := FigureBrushColor;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var i: integer;
begin
  Inherited;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  for i := low(Points) to high(Points)-1 do
  begin
    Canvas.Line   (scalesunit.WorldToScreen(Points[i])  .x,
                   scalesunit.WorldToScreen(Points[i])  .y,
                   scalesunit.WorldToScreen(Points[i+1]).x,
                   scalesunit.WorldToScreen(Points[i+1]).y);
  end;
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    for i := low(Points) to high(Points)-1 do
      DrawSelection(Points[i],Points[i+1],Canvas);
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
    DrawSelection(Points[low(Points)],Points[high(Points)],Canvas);
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
    DrawSelection(Points[low(Points)],Points[high(Points)],Canvas);
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
    DrawSelection(Points[low(Points)],Points[high(Points)],Canvas);
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
    DrawSelection(Points[low(Points)],Points[high(Points)],Canvas);
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
  r: double;
  i,k: integer;
  PolygonPoints: array of TFloatPoint;
  PolygonPointsScr: array of TPoint;
begin
  Inherited;
  P1 := Points[low(Points)];
  P2 := Points[high(Points)];
  r := sqrt(abs(sqr(P2.x-P1.x) + sqr(P2.y-P1.y)));
  k:=360 div FigureCorners;
  if (not FigureAngleMode) then FigureAngle := Arctan2(p1.Y - p2.Y, p1.X - p2.X);
  setlength (PolygonPoints, Figurecorners);
  setlength (PolygonPointsScr, Figurecorners);
  for i := low(PolygonPoints) to high(PolygonPoints) do
  begin
    PolygonPoints[i].x := P1.x + r*cos(i*k/180*Pi);
    PolygonPoints[i].y := P1.Y + r*sin(i*k/180*Pi);
    PolygonPointsScr[i] := WorldToScreen(Rotate(P1,PolygonPoints[i],FigureAngle));
  end;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Polygon(PolygonPointsScr);
  if (Selected = true) then
  begin
    DeleteObject(FigureRegion);
    for i := low(PolygonPointsScr) to high(PolygonPointsScr)-1 do
      DrawSelection(ScreenToWorld(PolygonPointsScr[i]),
      ScreenToWorld(PolygonPointsScr[i+1]),Canvas);

    DrawSelection(ScreenToWorld(PolygonPointsScr[high(PolygonPointsScr)]),
    ScreenToWorld(PolygonPointsScr[low(PolygonPointsScr)]),Canvas);
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
  tempPoints: array[0..3] of TPoint;
  p1,p2: TPoint;
begin
  p1 := WorldToScreen(Points[low(Points)]);
  p2 := WorldToScreen(Points[high(Points)]);
  if (abs(p2.x-p1.x)>45) then
  begin
    tempPoints[0].x := p1.x-FigurePenWidth div 2;
    tempPoints[0].y := p1.y-5-FigurePenWidth;
    tempPoints[1].x := p2.x+FigurePenWidth div 2;
    tempPoints[1].y := p2.y-5-FigurePenWidth;
    tempPoints[2].x := p2.x+FigurePenWidth div 2;
    tempPoints[2].y := p2.y+5+FigurePenWidth;
    tempPoints[3].x := p1.x-FigurePenWidth div 2;
    tempPoints[3].y := p1.y+5+FigurePenWidth;
  end else
  begin
    tempPoints[0].x := p1.x-5-FigurePenWidth;
    tempPoints[0].y := p1.y-FigurePenWidth div 2;
    tempPoints[1].x := p2.x-5-FigurePenWidth;
    tempPoints[1].y := p2.y+FigurePenWidth div 2;
    tempPoints[2].x := p2.x+5+FigurePenWidth;
    tempPoints[2].y := p2.y+FigurePenWidth div 2;
    tempPoints[3].x := p1.x+5+FigurePenWidth;
    tempPoints[3].y := p1.y-FigurePenWidth div 2;
  end;
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
    if (abs(p2.x-p1.x)>45) then
    begin
      tempPoints[0].x := p1.x-FigurePenWidth div 2;
      tempPoints[0].y := p1.y-5-FigurePenWidth;
      tempPoints[1].x := p2.x+FigurePenWidth div 2;
      tempPoints[1].y := p2.y-5-FigurePenWidth;
      tempPoints[2].x := p2.x+FigurePenWidth div 2;
      tempPoints[2].y := p2.y+5+FigurePenWidth;
      tempPoints[3].x := p1.x-FigurePenWidth div 2;
      tempPoints[3].y := p1.y+5+FigurePenWidth;
    end else
    begin
      tempPoints[0].x := p1.x-5-FigurePenWidth;
      tempPoints[0].y := p1.y-FigurePenWidth div 2;
      tempPoints[1].x := p2.x-5-FigurePenWidth;
      tempPoints[1].y := p2.y+FigurePenWidth div 2;
      tempPoints[2].x := p2.x+5+FigurePenWidth;
      tempPoints[2].y := p2.y+FigurePenWidth div 2;
      tempPoints[3].x := p1.x+5+FigurePenWidth;
      tempPoints[3].y := p1.y-FigurePenWidth div 2;
    end;
    if (i=low(Points)) then FigureRegion := CreatePolygonRgn (tempPoints,length(tempPoints),winding);
    curRgn := CreatePolygonRgn (tempPoints,length(tempPoints),winding);
    CombineRgn (FigureRegion,FigureRegion,curRgn,RGN_OR);
    DeleteObject(curRgn);
  end;
end;

procedure TPolygon.SetRegion;
var
  P1,P2: TFloatPoint;
  r: double;
  i,k: integer;
  PolygonPoints: array of TFloatPoint;
  PolygonPointsScr: array of TPoint;
begin
  P1 := Points[low(Points)];
  P2 := Points[high(Points)];
  r := sqrt(abs(sqr(P2.x-P1.x) + sqr(P2.y-P1.y)));
  k:=360 div FigureCorners;
  if (not FigureAngleMode) then FigureAngle := Arctan2(p1.Y - p2.Y, p1.X - p2.X);
  setlength (PolygonPoints, Figurecorners);
  setlength (PolygonPointsScr, Figurecorners);
  for i := low(PolygonPoints) to high(PolygonPoints) do
  begin
    PolygonPoints[i].x := P1.x + r*cos(i*k/180*Pi);
    PolygonPoints[i].y := P1.Y + r*sin(i*k/180*Pi);
    PolygonPointsScr[i] := WorldToScreen(Rotate(P1,PolygonPoints[i],FigureAngle));
  end;
  FigureRegion := CreatePolygonRgn (PolygonPointsScr[0],length(PolygonPointsScr),winding);
end;

initialization
PenWidth := 1;
Corners := 3;
end.

