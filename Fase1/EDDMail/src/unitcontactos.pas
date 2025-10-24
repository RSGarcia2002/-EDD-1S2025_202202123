unit UnitContactos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormContactos }

  TFormContactos = class(TForm)
    btnAgregar: TButton;
    btnCerrar: TButton;
    btnEliminarContacto: TButton;
    edtEmailBuscar: TEdit;
    lbContactos: TListBox;
    procedure btnAgregarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarContactoClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure Refrescar;
  public
  end;

var
  FormContactos: TFormContactos;

implementation

{$R *.lfm}

uses UDataCore,UnitEliminarContacto, UDomain;

procedure TFormContactos.Refrescar;
begin
  lbContactos.Items.BeginUpdate;
  try
    Contacts_ToStringsFor(Domain_GetCurrentUser, lbContactos.Items);
  finally
    lbContactos.Items.EndUpdate;
  end;
end;

procedure TFormContactos.FormShow(Sender: TObject);
begin
  Position := poScreenCenter;
  Refrescar;
end;

procedure TFormContactos.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormContactos.btnEliminarContactoClick(Sender: TObject);
var r: Integer;
begin
  if not Assigned(FormEliminarContacto) then
    Application.CreateForm(TFormEliminarContacto, FormEliminarContacto);

  FormEliminarContacto.Position := poScreenCenter;
  FormEliminarContacto.ModalResult := mrNone;
  r := FormEliminarContacto.ShowModal;

  if r = mrOk then
    Refrescar;
end;


procedure TFormContactos.FormActivate(Sender: TObject);
begin
  Refrescar; // se llama cada vez que el form toma foco
end;
procedure TFormContactos.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;  // oculta en vez de destruir
end;

procedure TFormContactos.btnAgregarClick(Sender: TObject);
var
  email: string;
  u: PUserNode;
begin
  email := Trim(edtEmailBuscar.Text);

  if email = '' then
  begin
    MessageDlg('Ingresa un email.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if SameText(email, Domain_GetCurrentUser) then
  begin
    MessageDlg('No puedes agregarte a ti mismo como contacto.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // Debe existir como usuario del sistema
  u := User_FindByEmail(email);
  if u = nil then
  begin
    MessageDlg('Ese email no existe en el sistema.', mtError, [mbOK], 0);
    Exit;
  end;

  // Evitar duplicado en la lista DEL USUARIO ACTUAL
  if Contacts_ExistsFor(Domain_GetCurrentUser, email) then
  begin
    MessageDlg('El contacto ya existe en tu lista.', mtInformation, [mbOK], 0);
    Exit;
  end;

  // Agregar para el propietario actual (usuario logueado)
  if not Contacts_AddFor(Domain_GetCurrentUser, u^.Email, u^.Nombre) then
  begin
    MessageDlg('No se pudo agregar el contacto.', mtError, [mbOK], 0);
    Exit;
  end;

  edtEmailBuscar.Clear;
  Refrescar;
end;



end.

