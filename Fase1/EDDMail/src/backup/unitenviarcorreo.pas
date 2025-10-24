unit UnitEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UDomain, UAVL_Borradores, UReports;

type
  { TFormEnviarCorreo }
  TFormEnviarCorreo = class(TForm)
    btnCancelar: TButton;
    btnEnviar: TButton;
    btnGuardarBorrador: TButton;
    edtPara: TEdit;
    edtAsunto: TEdit;
    LabelPara: TLabel;
    LabelAsunto: TLabel;
    LabelMensaje: TLabel;
    memMensaje: TMemo;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnGuardarBorradorClick(Sender: TObject);
    procedure btnVerBorradoresClick(Sender: TObject);
  private
    function DestinatarioEsContacto(const AEmail: string): boolean;
    procedure AddLine(const D: TDraft); // visitor para listar borradores
  public
  end;

var
  FormEnviarCorreo: TFormEnviarCorreo;

implementation

{$R *.lfm}

uses
  UDataCore, DateUtils;

{ ---- Aux ---- }

function TFormEnviarCorreo.DestinatarioEsContacto(const AEmail: string): boolean;
begin
  Result := Contacts_ExistsFor(CurrentUserEmail, AEmail);
end;

procedure TFormEnviarCorreo.AddLine(const D: TDraft);
begin
  Memo1.Lines.Add(Format('ID=%d | %s -> %s | %s',
    [D.Id, D.Remitente, D.Destinatario, D.Asunto]));
end;

{ ---- Botones ---- }

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
  if (u = nil) or (not DestinatarioEsContacto(para)) then
  begin
    MessageDlg('Solo puedes enviar a correos que EXISTEN y son tu CONTACTO.', mtError,[mbOK],0);
    Exit;
  end;

  id  := FormatDateTime('yyyymmddhhnnss', Now);
  fec := FormatDateTime('yyyy-mm-dd hh:nn', Now);

  // guarda en la bandeja global (destinatario real = para)
  Inbox_PushBack(id, CurrentUserEmail, para, asu, fec, msg);

  MessageDlg(Format('Correo guardado para %s. Tiene ahora %d correos.',
    [para, Inbox_CountFor(para)]), mtInformation, [mbOK], 0);

  Close;
end;

procedure TFormEnviarCorreo.btnGuardarBorradorClick(Sender: TObject);
var
  D: TDraft;
begin
  if Trim(edtAsunto.Text) = '' then
  begin
    ShowMessage('Escribe un asunto.'); Exit;
  end;
  if Trim(memMensaje.Text) = '' then
  begin
    ShowMessage('Escribe un mensaje.'); Exit;
  end;

  D.Id           := Domain_NewDraftId;        // ID incremental simple
  D.Remitente    := CurrentUserEmail;         // quien inició sesión
  D.Destinatario := Trim(edtPara.Text);
  D.Asunto       := Trim(edtAsunto.Text);
  D.Mensaje      := memMensaje.Text;

  AVL_Insert(GlobalDrafts, D);                // guarda en el AVL real
  GenerateDraftsReportOnly;                   // reescribe avl_borradores.dot

  ShowMessage(Format('Borrador #%d guardado.', [D.Id]));
end;

procedure TFormEnviarCorreo.btnVerBorradoresClick(Sender: TObject);
begin
  Memo1.Clear;
  // Recorre el AVL en orden y lista en el Memo
  AVL_InOrder(GlobalDrafts, @AddLine);
end;

procedure TFormEnviarCorreo.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

end.

