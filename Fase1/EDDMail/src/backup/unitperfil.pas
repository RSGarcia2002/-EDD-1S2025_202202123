unit UnitPerfil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormPerfil }

  TFormPerfil = class(TForm)
    btnActualizarUsuario: TButton;
    btnActualizarTelefono: TButton;
    btnCerrar: TButton;
    edtUsuario: TEdit;
    edtTelefono: TEdit;
    lblNombre: TLabel;
    lblEmail: TLabel;
    lblUsuario: TLabel;
    lblTelefono: TLabel;
    procedure btnActualizarTelefonoClick(Sender: TObject);
    procedure btnActualizarUsuarioClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CargarDatos;
  public
    procedure Refrescar; // <- para llamar desde el menú antes de ShowModal
  end;

var
  FormPerfil: TFormPerfil;

implementation

{$R *.lfm}

uses UDataCore;

procedure TFormPerfil.CargarDatos;
var
  u: PUserNode;
begin
  u := User_FindByEmail(CurrentUserEmail);
  if u = nil then
  begin
    // Si esto aparece, CurrentUserEmail está vacío o no cargaste usuarios
    lblNombre.Caption   := 'Nombre: (no disponible)';
    lblEmail.Caption    := 'Email: (no disponible)';
    lblUsuario.Caption  := 'Usuario:';
    lblTelefono.Caption := 'Teléfono:';
    edtUsuario.Text  := '';
    edtTelefono.Text := '';
    Exit;
  end;

  lblNombre.Caption   := 'Nombre: '   + u^.Nombre;
  lblEmail.Caption    := 'Email: '    + u^.Email;
  lblUsuario.Caption  := 'Usuario: '  + u^.Usuario;
  lblTelefono.Caption := 'Teléfono: ' + u^.Telefono;

  edtUsuario.Text  := u^.Usuario;
  edtTelefono.Text := u^.Telefono;
end;

procedure TFormPerfil.Refrescar;
begin
  CargarDatos;
end;

procedure TFormPerfil.FormShow(Sender: TObject);
begin
  CargarDatos;
end;

procedure TFormPerfil.FormActivate(Sender: TObject);
begin
  // por si el OnShow no quedó enlazado en el .lfm
  CargarDatos;
end;

procedure TFormPerfil.btnActualizarUsuarioClick(Sender: TObject);
var
  u: PUserNode;
begin
  u := User_FindByEmail(CurrentUserEmail);
  if u = nil then Exit;

  if Trim(edtUsuario.Text) = '' then
  begin
    MessageDlg('El campo Usuario no puede estar vacío.', mtWarning, [mbOK], 0);
    Exit;
  end;

  u^.Usuario := Trim(edtUsuario.Text);
  MessageDlg('Usuario actualizado correctamente.', mtInformation, [mbOK], 0);
  CargarDatos;
end;

procedure TFormPerfil.btnActualizarTelefonoClick(Sender: TObject);
var
  u: PUserNode;
begin
  u := User_FindByEmail(CurrentUserEmail);
  if u = nil then Exit;

  if Trim(edtTelefono.Text) = '' then
  begin
    MessageDlg('El campo Teléfono no puede estar vacío.', mtWarning, [mbOK], 0);
    Exit;
  end;

  u^.Telefono := Trim(edtTelefono.Text);
  MessageDlg('Teléfono actualizado correctamente.', mtInformation, [mbOK], 0);
  CargarDatos;
end;

procedure TFormPerfil.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

