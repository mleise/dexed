program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols, anchordockpkg,
  tachartlazaruspkg, ce_sharedres, ce_observer, ce_libman, ce_symstring,
  ce_tools, ce_dcd, ce_main, ce_writableComponent,
  ce_inspectors, ce_editoroptions, ce_dockoptions, ce_shortcutseditor, ce_mru,
  ce_processes, ce_dialogs, ce_dubprojeditor, ce_controls, ce_dfmt,
  ce_lcldragdrop, ce_stringrange, ce_dlangmaps, ce_projgroup, ce_projutils,
  ce_d2synpresets, ce_dastworx, ce_dbgitf, ce_ddemangle, ce_dubproject,
  ce_halstead, ce_diff, ce_profileviewer, ce_semver, ce_term;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCEMainForm, CEMainForm);
  Application.Run;
end.

