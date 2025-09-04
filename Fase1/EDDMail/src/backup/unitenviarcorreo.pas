unit UnitEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TFormEnviarCorreo = class(TForm)
    btnCancelar: TButton;
    btnEnviar: TButton;
    edtPara: TEdit;
    edtAsunto: TEdit;
    LabelPara: TLabel;
    LabelAsunto: TLabel;
    LabelMensaje: TLabel;
    memMensaje: TMemo;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
  private
    function DestinatarioEsContacto(const AEmail: string): boolean;
  public
  end;

var
  FormEnviarCorreo: TFormEnviarCorreo;

implementation

{$R *.lfm}

uses
  UDataCore, DateUtils;

function TFormEnviarCorreo.DestinatarioEsContacto(const AEmail: string): boolean;
begin
  Result := Contacts_Exists(AEmail);
end;
procedure TFormEnviarCorreo.btnEnviarClick(Sender: TObject);
var
  para, asu, msg, id, fec: string;
  u: PUserNode;
begin
  para := Trim(edtPara.Text);
  asu  := Trim(edtAsunto.Text);
  msg  := Trim(memMensaje.Lines.Text);

  if (para='') or (asu='') or (msg='') then
  begin
    MessageDlg('Completa Para, Asunto y Mensaje.', mtWarning,[mbOK],0);
    Exit;
  end;

  // Debe existir y ser contacto DEL USUARIO ACTUAL
  u := User_FindByEmail(para);
  if (u = nil) or (not Contacts_ExistsFor(CurrentUserEmail, para)) then
  begin
    MessageDlg('Solo puedes enviar a correos que EXISTEN y son tu CONTACTO.', mtError,[mbOK],0);
    Exit;
  end;

  id  := FormatDateTime('yyyymmddhhnnss', Now);
  fec := FormatDateTime('yyyy-mm-dd hh:nn', Now);

  // guarda en la bandeja global (destinatario real = para)
  Inbox_PushBack(id, CurrentUserEmail, para, asu, fec, msg);

  // debug opcional
  MessageDlg(Format('Correo guardado para %s. Tiene ahora %d correos.',
    [para, Inbox_CountFor(para)]), mtInformation, [mbOK], 0);

  Close;
end;



procedure TFormEnviarCorreo.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

end.

