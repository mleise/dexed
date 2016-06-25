unit ce_dsgncontrols;
{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, buttons, graphics,
  Menus, LMessages, LCLType, Toolwin;

type

  (**
   * Toolbutton with methods to load the glyph from the shared resources
   *)
  TCEToolButton = class(TToolButton)
  private
    fResourceName: string;
    fScaledSeparator: boolean;
    fPng: TPortableNetworkGraphic;
    procedure setResourceName(const value: string);
    procedure setScaledSeparator(value: boolean);
    procedure setToolBar(value: TToolbar);
  protected
    procedure Paint; override;
  published
    property resourceName: string read fResourceName write setResourceName;
    property scaledSeparator: boolean read fScaledSeparator write setScaledSeparator;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  (**
   * Toolbar with design-time support for TCEToolbutton
   *)
  TCEToolBar = class(TToolBar)
  protected
    fDesignMenu: TPopupMenu;
    procedure dsgnAdd(style: TToolButtonStyle);
    procedure dsgnAddButton(sender: TObject);
    procedure dsgnAddDivider(sender: TObject);
    procedure dsgnAddSeparator(sender: TObject);
    procedure dsgnAddDropdown(sender: TObject);
    procedure dsgnAddCheckbutton(sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  end;

  procedure register;

implementation

procedure register;
begin
  RegisterComponents('Coedit', [TCEToolBar, TCEToolButton]);
end;

constructor TCEToolButton.Create(TheOwner: TComponent);
begin
  inherited;
  fPng := TPortableNetworkGraphic.Create;
end;

destructor TCEToolButton.Destroy;
begin
  fPng.FreeImage;
  fPng.Free;
  inherited;
end;

procedure TCEToolButton.setToolBar(value: TToolbar);
begin
  FToolBar := value;
end;

procedure TCEToolButton.setResourceName(const value: string);
begin
  if fResourceName = value then
    exit;
  fResourceName:=value;
  if csDesigning in ComponentState then
    exit;
  if Style = tbsButton then
  begin
    fPng.FreeImage;
    fPng.LoadFromResourceName(HINSTANCE, fResourceName);
  end;
  FToolBar.Repaint;
end;

procedure TCEToolButton.setScaledSeparator(value: boolean);
begin
  if fScaledSeparator = value then
    exit;
  fScaledSeparator:=value;
  // store ratio if true
end;

procedure TCEToolButton.Paint;
var
  rc: TRect;
  x, y: integer;
begin
  inherited;
  if (fResourceName <> '') and (style = tbsButton) then
  begin
    rc := ClientRect;
    x := ((rc.Right - rc.Left) - fPng.width) div 2;
    y := ((rc.Bottom - rc.Top) - fPng.Height) div 2;
    Canvas.Draw(x, y, fPng);
  end;
end;

constructor TCEToolBar.Create(TheOwner: TComponent);
var
  item: TMenuItem;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    fDesignMenu := TPopupMenu.Create(nil);
    fDesignMenu.Name:= 'CEToolbarDsgnMenu';
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add button';
    item.OnClick:= @dsgnAddButton;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add separator';
    item.OnClick:= @dsgnAddSeparator;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add divider';
    item.OnClick:= @dsgnAddDivider;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add check';
    item.OnClick:= @dsgnAddCheckbutton;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add dropdown';
    item.OnClick:= @dsgnAddDropdown;
    fDesignMenu.Items.Add(item);
  end;
  borderSpacing.Left := 2;
  borderSpacing.Top := 2;
  borderSpacing.Right := 2;
  borderSpacing.Bottom := 0;
  height := 30;
  ButtonHeight := 28;
  ButtonWidth := 28;
  EdgeInner:= esNone;
  EdgeOuter:= esNone;
  Flat := false;
  Transparent := true;
end;

destructor TCEToolBar.Destroy;
begin
  if csDesigning in ComponentState then
    fDesignMenu.Free;
  inherited;
end;

procedure TCEToolBar.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    exit;
  if Message.Keys <> MK_RBUTTON then
    exit;
  Message.Result := 0;
  fDesignMenu.PopUp(Mouse.CursorPos.x,Mouse.CursorPos.y);
end;

procedure TCEToolBar.dsgnAdd(style: TToolButtonStyle);
var
  button: TCEToolButton;
  str: string = '';
  i: integer = 0;
begin
  button := TCEToolButton.Create(owner);
  while true do
  begin
    str := format('button%d',[i]);
    if owner.FindComponent(str) = nil then
      break;
    i += 1;
  end;
  button.Name:= str;
  button.Style := style;
  InsertControl(button);
  ButtonList.add(button);
  button.setToolBar(self);
  if style = tbsDivider then
    width := 16;
end;

procedure TCEToolBar.dsgnAddButton(sender: TObject);
begin
  dsgnAdd(tbsButton);
end;

procedure TCEToolBar.dsgnAddDivider(sender: TObject);
begin
  dsgnAdd(tbsDivider);
end;

procedure TCEToolBar.dsgnAddSeparator(sender: TObject);
begin
  dsgnAdd(tbsSeparator);
end;

procedure TCEToolBar.dsgnAddCheckbutton(sender: TObject);
begin
  dsgnAdd(tbsCheck);
end;

procedure TCEToolBar.dsgnAddDropdown(sender: TObject);
begin
  dsgnAdd(tbsDropDown);
end;

initialization
  RegisterClasses([TCEToolBar, TCEToolButton]);
end.

