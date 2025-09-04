unit UnitLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormLogin }

  TFormLogin = class(TForm)
    btnEntrar: TButton;
    btnSalir: TButton;
    btnCrearCuenta: TButton;
    edtEmail: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblEmail: TLabel;
    lblPassword: TLabel;
    procedure btnCrearCuentaClick(Sender: TObject);
    procedure btnEntrarClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AbrirMenuRootModal;
  public end;

var
  FormLogin: TFormLogin;

implementation


{$R *.lfm}
uses UnitMenuRoot, UnitMenuUsuario, UDataCore;


procedure TFormLogin.FormCreate(Sender: TObject);
begin
  Caption := 'EDDMail - Login';
end;

procedure TFormLogin.AbrirMenuRootModal;
var F: TFormMenuRoot;
begin
  F := TFormMenuRoot.Create(Self);
  try
    Hide;
    F.ShowModal;
  finally
    Show;
    F.Free;
  end;
end;

procedure TFormLogin.btnEntrarClick(Sender: TObject);
var
  nom, usu, tel: string;
begin
  // Root
  if (edtEmail.Text='root@edd.com') and (edtPassword.Text='root123') then
  begin
    AbrirMenuRootModal;
    Exit;
  end;

  // Usuario normal
  if User_ValidarLogin(edtEmail.Text, edtPassword.Text, nom, usu, tel) then
  begin
    CurrentUserEmail := edtEmail.Text;
    if not Assigned(FormMenuUsuario) then
      Application.CreateForm(TFormMenuUsuario, FormMenuUsuario);
    FormMenuUsuario.Configurar(nom, usu);
    Hide;
    FormMenuUsuario.ShowModal;
    Show;
    Exit;
  end;

  // Ninguno
  MessageDlg('Credenciales inválidas.', mtError, [mbOK], 0);
end;

procedure TFormLogin.btnCrearCuentaClick(Sender: TObject);
begin
  if not Assigned(FormCrearCuenta) then
    Application.CreateForm(TFormCrearCuenta, FormCrearCuenta);
  FormCrearCuenta.Position := poScreenCenter;
  FormCrearCuenta.ShowModal;
end;

procedure TFormLogin.btnSalirClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

