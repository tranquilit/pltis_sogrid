// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2023  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit ugridchart;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  ComCtrls,
  ExtCtrls,
  Spin,
  StdCtrls,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  TAGraph,
  TARadialSeries,
  TASeries,
  TASources,
  TACustomSource,
  TAChartUtils,
  TATextElements,
  TATools;

type
  TVisGridChartForm = class(TForm)
    cbMarkAttachment: TComboBox;
    cmbOrientation: TComboBox;
    ChartPie: TChart;
    ChartPiePieSeries1: TPieSeries;
    ChartToolset1: TChartToolset;
    cbRotate: TCheckBox;
    cbMarkPositions: TComboBox;
    Cb3D: TCheckBox;
    cbShowLabels: TCheckBox;
    cbMarkPositionsCentered: TCheckBox;
    lblViewAngle: TLabel;
    lblDistance: TLabel;
    lblStartAngle: TLabel;
    lblAngleRange: TLabel;
    seDepth: TSpinEdit;
    seViewAngle: TSpinEdit;
    seDepthBrightnessDelta: TSpinEdit;
    lblInnerRadius: TLabel;
    lblDepth: TLabel;
    lblDepthBrightnessDelta: TLabel;
    lblWords: TLabel;
    lblLabelAngle: TLabel;
    ListChartSource: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    seStartAngle: TSpinEdit;
    seAngleRange: TSpinEdit;
    seWords: TSpinEdit;
    seLabelAngle: TSpinEdit;
    seInnerRadius: TSpinEdit;
    seDistance: TSpinEdit;
    tsPie: TTabSheet;
    procedure cbMarkAttachmentChange(Sender: TObject);
    procedure cbMarkPositionsCenteredChange(Sender: TObject);
    procedure cbMarkPositionsChange(Sender: TObject);
    procedure cbRotateChange(Sender: TObject);
    procedure cbShowLabelsChange(Sender: TObject);
    procedure ChartPieMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Cb3DChange(Sender: TObject);
    procedure cmbOrientationChange(Sender: TObject);
    procedure seDepthBrightnessDeltaChange(Sender: TObject);
    procedure seDepthChange(Sender: TObject);
    procedure seDistanceChange(Sender: TObject);
    procedure seAngleRangeChange(Sender: TObject);
    procedure seInnerRadiusChange(Sender: TObject);
    procedure seStartAngleChange(Sender: TObject);
    procedure seViewAngleChange(Sender: TObject);
    procedure seWordsChange(Sender: TObject);
    procedure seLabelAngleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fSavedDataPoints: TStrings;
    fSaveMarks: record
      Style: TSeriesMarksStyle;
      Format: string;
    end;
  end;

implementation

{$R *.lfm}

{ TVisGridChartForm }

procedure TVisGridChartForm.cbMarkAttachmentChange(Sender: TObject);
begin
  ChartPiePieSeries1.Marks.Attachment :=
    TChartMarkAttachment(cbMarkAttachment.ItemIndex);
end;

procedure TVisGridChartForm.cbMarkPositionsCenteredChange(Sender: TObject);
begin
  ChartPiePieSeries1.MarkPositionCentered := cbMarkPositionsCentered.Checked;
end;

procedure TVisGridChartForm.cbMarkPositionsChange(Sender: TObject);
begin
  ChartPiePieSeries1.MarkPositions :=
    TPieMarkPositions(cbMarkPositions.ItemIndex);
end;

procedure TVisGridChartForm.cbRotateChange(Sender: TObject);
begin
  ChartPiePieSeries1.RotateLabels := cbRotate.Checked;
end;

procedure TVisGridChartForm.cbShowLabelsChange(Sender: TObject);
begin
  if cbShowLabels.Checked then
  begin
    with ChartPiePieSeries1.Marks do
    begin
      Style := fSaveMarks.Style;
      Format := fSaveMarks.Format;
    end;
  end
  else
    ChartPiePieSeries1.Marks.Style := smsNone;
  seWords.Enabled := cbShowLabels.Checked;
  lblWords.Enabled := cbShowLabels.Checked;
  seLabelAngle.Enabled := cbShowLabels.Checked;
  lblLabelAngle.Enabled := cbShowLabels.Checked;
  cbMarkPositions.Enabled := cbShowLabels.Checked;
  cbMarkAttachment.Enabled := cbShowlabels.Checked;
  cbMarkPositionsCentered.Enabled := cbShowLabels.Checked;
  cbRotate.Enabled := cbShowLabels.Checked;
end;

procedure TVisGridChartForm.Cb3DChange(Sender: TObject);
begin
  if cb3D.Checked then
    ChartPiePieSeries1.Depth := seDepth.Value
  else
    ChartPiePieSeries1.Depth := 0;
  seDepth.Enabled := cb3D.Checked;
  lblDepth.Enabled := cb3D.Checked;
  seDepthBrightnessDelta.Enabled := cb3D.Checked;
  lblDepthBrightnessDelta.Enabled := cb3D.Checked;
  lblViewAngle.Enabled := cb3D.Checked;
  seViewAngle.Enabled := cb3D.Checked;
  cmbOrientation.Enabled := cb3D.Checked;
end;

procedure TVisGridChartForm.cmbOrientationChange(Sender: TObject);
begin
  ChartPiePieSeries1.Orientation := TPieOrientation(cmbOrientation.ItemIndex);
end;

procedure TVisGridChartForm.seDepthBrightnessDeltaChange(Sender: TObject);
begin
  ChartPiePieSeries1.DepthBrightnessDelta := seDepthBrightnessDelta.Value;
end;

procedure TVisGridChartForm.seDepthChange(Sender: TObject);
begin
  ChartPiePieSeries1.Depth := seDepth.Value;
end;

procedure TVisGridChartForm.seDistanceChange(Sender: TObject);
begin
  ChartPiePieSeries1.Marks.Distance := seDistance.Value;
end;

procedure TVisGridChartForm.seAngleRangeChange(Sender: TObject);
begin
  ChartPiePieSeries1.AngleRange := seAngleRange.Value;
end;

procedure TVisGridChartForm.seInnerRadiusChange(Sender: TObject);
begin
  ChartPiePieSeries1.InnerRadiusPercent := seInnerRadius.Value;
end;

procedure TVisGridChartForm.ChartPieMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := ChartPiePieSeries1.FindContainingSlice(Point(X, Y));
  if i < 0 then exit;
  ListChartSource.SetXValue(i, 0.2 - ListChartSource[i]^.X);
  ChartPie.Invalidate;
end;

procedure TVisGridChartForm.seStartAngleChange(Sender: TObject);
begin
  ChartPiePieSeries1.StartAngle := seStartAngle.Value;
end;

procedure TVisGridChartForm.seViewAngleChange(Sender: TObject);
begin
  ChartPiePieSeries1.ViewAngle := seViewAngle.Value;
end;

procedure TVisGridChartForm.seLabelAngleChange(Sender: TObject);
begin
  ChartPiePieSeries1.Marks.LabelFont.Orientation := seLabelAngle.Value * 10;
end;

procedure TVisGridChartForm.FormCreate(Sender: TObject);
begin
  fSavedDataPoints := TStringList.Create;
  with fSaveMarks do
  begin
    Style := smsCustom;
    Format := ChartPiePieSeries1.Marks.Format;
  end;
end;

procedure TVisGridChartForm.FormDestroy(Sender: TObject);
begin
  fSavedDataPoints.Free;
end;

procedure TVisGridChartForm.seWordsChange(Sender: TObject);

  function ExtractWords(aText: string; aWordCount: Integer): string;
  var
    v1, vPos: Integer;
  begin
    result := '';
    for v1 := 1 to aWordCount do
    begin
      vPos := Pos(' ', aText);
      if vPos > 0 then
      begin
        result += Copy(aText, 1, vPos - 1) + ' ';
        aText := Copy(aText, vPos + 1, Length(aText));
      end
      else
      begin
        result += aText;
        Break;
      end;
    end;
    result := Trim(result);
  end;

var
  v1: Integer;
begin
  if fSavedDataPoints.Count = 0 then
    fSavedDataPoints.Assign(ListChartSource.DataPoints);
  ListChartSource.DataPoints.Assign(fSavedDataPoints);
  for v1 := 0 to ListChartSource.Count - 1 do
  begin
    with ListChartSource[v1]^ do
      Text := ExtractWords(Text, seWords.Value);
  end;
  ChartPie.Invalidate;
end;

end.

