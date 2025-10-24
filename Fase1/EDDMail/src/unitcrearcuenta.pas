unit UnitCrearCuenta;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormCrearCuenta }

  TFormCrearCuenta = class(TForm)
    btnCrear: TButton;
    btnCancelar: TButton;
    edtNombre: TEdit;
    edtUsuario: TEdit;
    edtEmail: TEdit;
    edtTelefono: TEdit;
    edtPass: TEdit;
    edtPass2: TEdit;
    lblNombre: TLabel;
    lblUsuario: TLabel;
    lblEmail: TLabel;
    lblTelefono: TLabel;
    lblPass: TLabel;
    lblPass2: TLabel;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnCrearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function EmailValido(const S: string): boolean;
  public
  end;

var
  FormCrearCuenta: TFormCrearCuenta;

implementation

{$R *.lfm}

uses UDataCore;

procedure TFormCrearCuenta.FormShow(Sender: TObject);
begin
  Position := poScreenCenter;
  edtNombre.Clear; edtUsuario.Clear; edtEmail.Clear; edtTelefono.Clear;
  edtPass.Clear; edtPass2.Clear;
  edtNombre.SetFocus;
end;

procedure TFormCrearCuenta.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

function TFormCrearCuenta.EmailValido(const S: string): boolean;
var
  p: Integer;
begin
  p := Pos('@', S);
  Result := (p > 1) and (p < Length(S)) and (Pos('.', Copy(S, p+1, MaxInt)) > 0);
end;

procedure TFormCrearCuenta.btnCrearClick(Sender: TObject);
var
  id, nom, usu, email, tel, pass1, pass2: string;
begin
  nom   := Trim(edtNombre.Text);
  usu   := Trim(edtUsuario.Text);
  email := Trim(edtEmail.Text);
  tel   := Trim(edtTelefono.Text);
  pass1 := edtPass.Text;
  pass2 := edtPass2.Text;

  if (nom='') or (usu='') or (email='') or (pass1='') or (pass2='') then
  begin
    MessageDlg('Completa al menos: Nombre, Usuario, Email y Password.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if not EmailValido(email) then
  begin
    MessageDlg('Email no válido.', mtError, [mbOK], 0);
    Exit;
  end;

  if Length(pass1) < 6 then
  begin
    MessageDlg('El password debe tener al menos 6 caracteres.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if pass1 <> pass2 then
  begin
    MessageDlg('Las contraseñas no coinciden.', mtError, [mbOK], 0);
    Exit;
  end;

  // unicidad
  if User_ExistsEmail(email) then
  begin
    MessageDlg('Ya existe una cuenta con ese email.', mtError, [mbOK], 0);
    Exit;
  end;

  if User_ExistsUsuario(usu) then
  begin
    MessageDlg('Ya existe una cuenta con ese usuario.', mtError, [mbOK], 0);
    Exit;
  end;

  // generar Id y crear
  id := FormatDateTime('yyyymmddhhnnsszzz', Now);
  UserList_AddOrUpdate(id, nom, usu, email, tel, pass1);

  MessageDlg('Cuenta creada correctamente. Ya puedes iniciar sesión.', mtInformation, [mbOK], 0);
  Close;
end;

procedure TFormCrearCuenta.FormCreate(Sender: TObject);
begin

end;

end.

