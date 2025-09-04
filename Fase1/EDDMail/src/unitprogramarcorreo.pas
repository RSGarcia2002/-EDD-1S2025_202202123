unit UnitProgramarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, ExtCtrls;

type
  TFormProgramarCorreo = class(TForm)
    btnProgramar: TButton;
    btnCancelar: TButton;
    edtPara: TEdit;
    edtAsunto: TEdit;
    LabelPara: TLabel;
    LabelAsunto: TLabel;
    LabelFH: TLabel;
    LabelMsg: TLabel;
    memMensaje: TMemo;
    deFecha: TDateEdit;
    teHora: TTimeEdit;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnProgramarClick(Sender: TObject);
  private
  public
  end;

var
  FormProgramarCorreo: TFormProgramarCorreo;

implementation

{$R *.lfm}

uses UDataCore;

procedure TFormProgramarCorreo.btnProgramarClick(Sender: TObject);
var
  para, asu, msg, id, fprog: string;
  u: PUserNode;
  fechaHora: TDateTime;
begin
  para := Trim(edtPara.Text);
  asu  := Trim(edtAsunto.Text);
  msg  := Trim(memMensaje.Lines.Text);

  if (para='') or (asu='') or (msg='') then
  begin
    MessageDlg('Completa Para, Asunto y Mensaje.', mtWarning, [mbOK],0);
    Exit;
  end;

  // Debe existir y ser contacto DEL USUARIO ACTUAL
  u := User_FindByEmail(para);
  if (u = nil) or (not Contacts_ExistsFor(CurrentUserEmail, para)) then
  begin
    MessageDlg('Solo puedes programar para correos que EXISTEN y son tu CONTACTO.', mtError,[mbOK],0);
    Exit;
  end;

  // fecha y hora seleccionadas
  fechaHora := deFecha.Date + Frac(teHora.Time);
  fprog := FormatDateTime('yyyy-mm-dd hh:nn', fechaHora);

  id := FormatDateTime('yyyymmddhhnnss', Now);
  Prog_Enqueue(id, CurrentUserEmail, para, asu, fprog, msg);

  MessageDlg('Correo programado en la cola.', mtInformation, [mbOK], 0);
  Close;
end;

procedure TFormProgramarCorreo.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

end.

