unit ce_lcldragdrop;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls,
  ce_common, ce_nativeproject, ce_dubproject, ce_interfaces,
  ce_dialogs, ce_projutils;

type

  TDDHandler = class(TObject, ICEProjectObserver)
  private
    fProj: ICECommonProject;
    fFreeProj: ICECommonProject;
    procedure projNew(project: ICECommonProject);
    procedure projChanged(project: ICECommonProject);
    procedure projClosing(project: ICECommonProject);
    procedure projFocused(project: ICECommonProject);
    procedure projCompiling(project: ICECommonProject);
    procedure projCompiled(project: ICECommonProject; success: boolean);
    //
    function getFilename(src: TObject): string;
  public
    constructor create;
    destructor destroy; override;
    procedure DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
  end;

var
  ddHandler: TDDHandler;

implementation

uses
  ce_observer;

constructor TDDHandler.create;
begin
  EntitiesConnector.addObserver(self);
end;

destructor TDDHandler.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TDDHandler.projNew(project: ICECommonProject);
begin
  fProj := project;
  if not fProj.inGroup then
    fFreeProj := fProj;
end;

procedure TDDHandler.projChanged(project: ICECommonProject);
begin
end;

procedure TDDHandler.projClosing(project: ICECommonProject);
begin
  fProj := nil;
  if project = fFreeProj then
    fFreeProj := nil;
end;

procedure TDDHandler.projFocused(project: ICECommonProject);
begin
  fProj := project;
  if not fProj.inGroup then
    fFreeProj := fProj;
end;

procedure TDDHandler.projCompiling(project: ICECommonProject);
begin
end;

procedure TDDHandler.projCompiled(project: ICECommonProject; success: boolean);
begin
end;

function TDDHandler.getFilename(src: TObject): string;
var
  lst: TListView;
  trv: TTreeView;
begin
  result := '';
  if src.isNil then exit;
  // from mini-explorer
  if src is TListView then
  begin
    lst := TListView(src);
    if lst.Selected.isNotNil and lst.Selected.Data.isNotNil then
      result := PString(lst.Selected.Data)^;
  end
  // from CE/DUB project inspector
  else if src is TTreeView then
  begin
    trv := TTreeView(src);
    if trv.Selected.isNotNil then
    begin
      result := trv.Selected.Text;
      if not result.fileExists and assigned(fProj) then
        result := fProj.filename.extractFilePath + result;
    end;
  end;
  {$IFNDEF WINDOWS}
  if (result.length > 1) and (result[2] = '/') then
    result := result[2..result.length];
  {$ENDIF}
end;

procedure TDDHandler.DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  fname: string;
begin
  fname := getFilename(Source);
  Accept := fname.fileExists and not fname.dirExists;
end;

procedure TDDHandler.DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  fname: string;
  fmt: TCEProjectFileFormat;
begin
  if Source.isNil then exit;
  fname := getFilename(Source);
  if not fname.fileExists then exit;

  fmt := projectFormat(fname);
  if fmt in [pffCe, pffDub] then
  begin
    if assigned(fFreeProj) then
    begin
      if fFreeProj.modified and (dlgFileChangeClose(fFreeProj.filename, UnsavedProj) = mrCancel) then
        exit;
      fFreeProj.getProject.Free;
    end;
    if fmt = pffCe then
      TCENativeProject.create(nil)
    else
      TCEDubProject.create(nil);
    fProj.loadFromFile(fname);
  end
  else getMultiDocHandler.openDocument(fname);
end;

initialization
  ddHandler:= TDDHandler.create;

finalization
  ddHandler.free;

end.

