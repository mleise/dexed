unit ce_profileviewer;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TAGraph, TATransformations, TASeries,
  TATools, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls,
  StdCtrls, TALegend, TADrawUtils, math,
  ce_widget, ce_common, ce_stringrange, ce_dsgncontrols, ce_ddemangle;

type

  TCEProfileViewerWidget = class(TCEWidget)
    btnOpen: TCEToolButton;
    btnRefresh: TCEToolButton;
    button0: TCEToolButton;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ImageList1: TImageList;
    selPieSource: TComboBox;
    datNumCalls: TListChartSource;
    datTreeTime: TListChartSource;
    datFuncTime: TListChartSource;
    datPerCall: TListChartSource;
    Panel1: TPanel;
    pie: TChart;
    list: TListView;
    pieSeries: TPieSeries;
    Splitter1: TSplitter;
    procedure btnOpenClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure pieDrawLegend(ASender: TChart; ADrawer: IChartDrawer;
      ALegendItems: TChartLegendItems; ALegendItemSize: TPoint;
      const ALegendRect: TRect; AColCount, ARowCount: Integer);
    procedure pieSeriesGetMark(out AFormattedMark: String; AIndex: Integer);
    procedure selPieSourceSelect(Sender: TObject);
    procedure selPieSourceSelectionChange(Sender: TObject; User: boolean);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure Splitter1Moved(Sender: TObject);
  private
    logFname: string;
    procedure updateIcons;
    procedure clearViewer;
    procedure updateFromFile(const fname: string);
    procedure updatePie;
    procedure listCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

constructor TCEProfileViewerWidget.create(aOwner: TComponent);
begin
  inherited;
  clearViewer;
  updatePie;
  list.OnCompare:=@listCompare;
  selPieSourceSelect(nil);
end;

procedure TCEProfileViewerWidget.btnRefreshClick(Sender: TObject);
var
  fname: string;
begin
  if logFname.isNotEmpty and logFname.fileExists then
    updateFromFile(logFname)
  else
  begin
    fname := GetCurrentDir + DirectorySeparator + 'trace.log';
    if fileExists(fname) then
    begin
      updateFromFile(fname);
      logFname:=fname;
    end;
  end;
end;

procedure TCEProfileViewerWidget.pieDrawLegend(ASender: TChart;
  ADrawer: IChartDrawer; ALegendItems: TChartLegendItems;
  ALegendItemSize: TPoint; const ALegendRect: TRect; AColCount,
  ARowCount: Integer);
begin

end;

procedure TCEProfileViewerWidget.pieSeriesGetMark(out AFormattedMark: String;
  AIndex: Integer);
begin

end;

procedure TCEProfileViewerWidget.selPieSourceSelect(Sender: TObject);
begin
  case selPieSource.ItemIndex of
    1: pieSeries.Source := datTreeTime;
    2: pieSeries.Source := datFuncTime;
    3: pieSeries.Source := datPerCall;
    else pieSeries.Source := datNumCalls;
  end;
  updateIcons;
end;

procedure TCEProfileViewerWidget.btnOpenClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if logFname.fileExists and logFname.isNotEmpty then
      FileName := logFname;
    if execute then
    begin
      updateFromFile(filename);
      logFname := FileName;
    end;
  finally
    free;
  end;
end;

procedure TCEProfileViewerWidget.updatePie;
begin
  pieSeries.FixedRadius:= max(1, pie.Height div 2 - 10);
end;

procedure TCEProfileViewerWidget.selPieSourceSelectionChange(Sender: TObject;User: boolean);
begin
  updatePie;
end;

procedure TCEProfileViewerWidget.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if accept then
    updatePie;
end;

procedure TCEProfileViewerWidget.Splitter1Moved(Sender: TObject);
begin
  updatePie;
end;

procedure TCEProfileViewerWidget.clearViewer;
begin
  list.Clear;
  pieSeries.Clear;
  datFuncTime.Clear;
  datNumCalls.Clear;
  datTreeTime.Clear;
  datPerCall.Clear;
end;

procedure TCEProfileViewerWidget.updateIcons;
var
  b: TBitmap;
  i: integer;
begin
  ImageList1.Clear;
  b := TBitmap.Create;
  try
    for i:= 0 to pieSeries.Count-1 do
    begin
      b.SetSize(12,12);
      b.Canvas.Brush.Color := pieSeries.GetColor(i);
      b.Canvas.Pen.Color:= clblack;
      b.Canvas.Brush.Style := bsSolid;
      b.Canvas.Pen.Style := psSolid;
      b.Canvas.Pen.Width := 1;
      b.Transparent := false;
      b.Canvas.Rectangle(Rect(0,0,12,12));
      ImageList1.Add(b, nil);
      list.Items.Item[i].ImageIndex:=i;
      b.Clear;
    end;
  finally
    b.Free;
  end;
end;

procedure TCEProfileViewerWidget.updateFromFile(const fname: string);
var
  log: string;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  tps: qword;
  idt: string;
  fnc: qword;
  fft: qword;
  ftt: qword;
  fpc: qword;

  procedure fillRow();
  var
    itm: TListItem;
    c: TColor;
  begin
    list.AddItem('', nil);
    itm := list.Items[list.Items.Count-1];
    itm.SubItems.Add(fnc.ToString);
    itm.SubItems.Add(fft.ToString);
    itm.SubItems.Add(ftt.ToString);
    itm.SubItems.Add(fpc.ToString);
    itm.SubItems.Add(idt);
    c := Random($70F0F0F0) + $F0F0F0F;
    datNumCalls.Add(0, fnc, idt, c);
    datFuncTime.Add(0, fft, idt, c);
    datTreeTime.Add(0, ftt, idt, c);
    datPerCall.Add(0, fpc, idt, c);
  end;

begin
  clearViewer;

  if not fname.fileExists or (fname.extractFileName <> 'trace.log') then
    exit;

  with TStringList.Create do
  try
    loadFromFile(fname);
    log := strictText;
  finally
    free;
  end;

  if log.length = 0 then
    exit;

  // ======== Timer Is 35.... ============
  rng.init(log);
  rng.popUntil('=')^.popUntil(['0','1','2','3','4','5','6','7','8','9']);
  if rng.empty then
    exit;
  idt := rng.nextWord;
  tps := StrToQWordDef(idt, 0);

  // columns headers
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;

  list.BeginUpdate;

  // each function
  while true do
  begin

    idt:= 'unknown function';
    fnc:= 0;
    fft:= 0;
    ftt:= 0;
    fpc:= 0;
    // num calls
    fnc := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // function time
    fft := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // tree time
    ftt := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // per call
    fpc := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // function name
    rng.popWhile(' ')^.empty;
    idt := demangle(rng.takeUntil(#10).yield);

    fillRow;

    if not rng.empty then
      rng.popFront
    else
      break;
  end;

  list.EndUpdate;
  selPieSourceSelect(nil);

end;

procedure TCEProfileViewerWidget.listCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
var
  i1, i2: qword;
  col: Integer;
begin
  col := list.SortColumn;
  if col = 5 then
  begin
    Compare := AnsiCompareStr(item1.SubItems[3], item2.SubItems[3]);
  end
  else if col <> 0 then
  begin
    i1 := item1.SubItems[col-1].ToInt64;
    i2 := item2.SubItems[col-1].ToInt64;
    if (i1 = i2) then
      Compare := 0
    else if (i1 < i2) then
      Compare := -1
    else
      Compare := 1;
  end;
  if list.SortDirection = sdDescending then
    Compare := -Compare;
end;

end.

