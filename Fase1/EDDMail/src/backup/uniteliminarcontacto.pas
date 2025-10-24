unit UnitEliminarContacto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  UDomain, UBST_Contacts, UReports, UDataCore; // UDataCore por Contacts_ExistsFor

type

  { TFormEliminarContacto }

  TFormEliminarContacto = class(TForm)
    btnEliminar: TButton;
    btnCerrar: TButton;
    edtEmail: TEdit;
    Label1: TLabel;
    LabelEmail: TLabel;
    lblTitulo: TLabel;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Limpiar;
  public
  end;

var
  FormEliminarContacto: TFormEliminarContacto;

implementation

{$R *.lfm}

procedure TFormEliminarContacto.FormShow(Sender: TObject);
begin
  Limpiar;
  edtEmail.SetFocus;
end;

procedure TFormEliminarContacto.Limpiar;
begin
  edtEmail.Clear;
end;

procedure TFormEliminarContacto.btnEliminarClick(Sender: TObject);
var email: string;
begin
  email := Trim(edtEmail.Text);
  if email = '' then begin ShowMessage('Ingresa el correo.'); Exit; end;

  if not Contacts_ExistsFor(CurrentUserEmail, email) then
  begin
    ShowMessage('Ese correo no es tu contacto.'); Exit;
  end;

  if Contacts_RemoveFor(CurrentUserEmail, email) then
  begin
    ShowMessage('Contacto eliminado.');
    ModalResult := mrOk;   // <<--- AVISA AL FORM PADRE
    Exit;
  end
  else
    ShowMessage('No se pudo eliminar.');
end;

procedure TFormEliminarContacto.FormShow(Sender: TObject);
begin
  ModalResult := mrNone;   // limpia el resultado cada vez
  edtEmail.Clear;
  edtEmail.SetFocus;
end;

procedure TFormEliminarContacto.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

