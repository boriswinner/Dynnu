unit figuresunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphMath, Forms, Dialogs, Math,
  scalesunit;
type
  TFigureClass    = class  of TFigure;

  TFigure = class
  public
    procedure Draw(Canvas: TCanvas); virtual; abstract;
  public
    Points: array of TFloatPoint;
    FigurePenColor,FigureBrushColor: TColor;
  end;

  TPolyline       = class(TFigure)
  FigurePenWidth: integer;
  FigurePenStyle: TPenStyle;
  public
    procedure Draw(Canvas:TCanvas); override;
  end;

  TRectangle      = class(TFigure)
  FigurePenStyle: TPenStyle;
  FigureBrushStyle: TBrushStyle;
  FigurePenWidth: integer;
  public
    procedure Draw(Canvas:TCanvas); override;
  end;

  TRoundRect      = class(TFigure)
  FigurePenStyle: TPenStyle;
  FigureBrushStyle: TBrushStyle;
  FigurePenWidth: integer;
  FigureR: TPoint;
  public
    procedure Draw(Canvas:TCanvas); override;
  end;

  TEllipse        = class(TFigure)
  FigurePenStyle: TPenStyle;
  FigureBrushStyle: TBrushStyle;
  FigurePenWidth: integer;
  public
    procedure Draw(Canvas:TCanvas); override;
  end;

  TLine           = class(TFigure)
  FigurePenWidth: integer;
  FigurePenStyle: TPenStyle;
  public
    procedure Draw(Canvas:TCanvas); override;
  end;

  TPolygon = class(TFigure)
  FigurePenWidth: integer;
  FigurePenStyle: TPenStyle;
  FigureBrushStyle: TBrushStyle;
  FigureCorners: integer;
  FigureAngle: double;
  FigureAngleMode: boolean;
  public
    procedure Draw(Canvas:TCanvas); override;
    function Rotate(P1,P2: TFloatPoint; angle: double): TFloatPoint;
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

procedure TPolyline.Draw(Canvas: TCanvas);
var i: integer;
begin
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;
  for i := low(Points) to high(Points)-1 do
  begin
    Canvas.Line   (scalesunit.WorldToScreen(Points[i])  .x,
                   scalesunit.WorldToScreen(Points[i])  .y,
                   scalesunit.WorldToScreen(Points[i+1]).x,
                   scalesunit.WorldToScreen(Points[i+1]).y);
  end;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Brush.Color := FigureBrushColor;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Rectangle(scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Brush.Color := FigureBrushColor;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.RoundRect(scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y,
                   FigureR.x,
                   FigureR.y);
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Brush.Color := FigureBrushColor;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Ellipse  (scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := bsSolid;
  Canvas.Line     (scalesunit.WorldToScreen(Points[low(Points)]) .x,
                   scalesunit.WorldToScreen(Points[low(Points)]) .y,
                   scalesunit.WorldToScreen(Points[high(Points)]).x,
                   scalesunit.WorldToScreen(Points[high(Points)]).y);
end;

procedure THandFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clBlack;
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
  Canvas.Pen.Color := FigurePenColor;
  Canvas.Pen.Width := FigurePenWidth;
  Canvas.Brush.Color := FigureBrushColor;
  Canvas.Pen.Style := FigurePenStyle;
  Canvas.Brush.Style := FigureBrushStyle;
  Canvas.Polygon(PolygonPointsScr);
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

initialization
PenWidth := 1;
Corners := 3;
end.

