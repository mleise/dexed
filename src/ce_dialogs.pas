unit ce_dialogs;

{$I ce_defines.inc}

interface

uses
  classes, sysutils, forms, dialogs;


(**
 * Ok/Cancel modal dialog
 *)
function dlgOkCancel(const message: string): TModalResult;

(**
 * Yes/No modal dialog
 *)
function dlgYesNo(const message: string): TModalResult;

(**
 * Info message
 *)
function dlgOkInfo(const message: string): TModalResult;

(**
 * Error message
 *)
function dlgOkError(const message: string): TModalResult;

(**
 * close aFilename Ok/Cancel.
 *)
function dlgFileChangeClose(const fname: string): TModalResult;

const
  DdiagFilter = 'D source|*.d|D interface|*.di|All files|*.*';

implementation


function dlgOkCancel(const message: string): TModalResult;
const
  Btns = [mbOK,mbCancel];
begin
  exit( MessageDlg('Coedit', message, mtConfirmation, Btns, ''));
end;

function dlgYesNo(const message: string): TModalResult;
const
  Btns = [mbYes,mbNo];
begin
  exit( MessageDlg('Coedit', message, mtConfirmation, Btns, ''));
end;

function dlgOkInfo(const message: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', message, mtInformation, Btns, ''));
end;

function dlgOkError(const message: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', message, mtError, Btns, ''));
end;

function dlgFileChangeClose(const fname: string): TModalResult;
const
  fmt = '"%s" latest modifications are not saved.'#13#10#13#10'Close it without saving ?';
begin
  exit(dlgOkCancel(format(fmt, [fname])));
end;

end.

