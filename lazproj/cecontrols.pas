{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cecontrols;

interface

uses
  ce_dsgncontrols, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ce_dsgncontrols', @ce_dsgncontrols.Register);
end;

initialization
  RegisterPackage('cecontrols', @Register);
end.
