unit UnitComMensajes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UBST_Communities, UDomain, DateUtils;

type

  { TFormComMensajes }

  TFormComMensajes = class(TForm)
    btnPublicar: TButton;
    btnRefrescar: TButton;
    btnCerrar: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblEstado: TLabel;
    lstMensajes: TListBox;
    lstComunidades: TListBox;
    memMensajes: TMemo;
    memNuevo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnPublicarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure lstMensajesClick(Sender: TObject);

  private
    procedure CargarComunidades;
    procedure RefrescarMensajes;
  public
  end;

var
  FormComMensajes: TFormComMensajes;

implementation

{$R *.lfm}

uses UDataCore;

procedure TFormComMensajes.CargarComunidades;
  procedure InOrder(C: PCBST);
  begin
    if C = nil then Exit;
    InOrder(C^.L);
    lstComunidades.Items.Add(C^.Data.Nombre); // o C^.Key
    InOrder(C^.R);
  end;
begin
  lstComunidades.Clear;
  InOrder(GlobalCommunities);
end;

procedure TFormComMensajes.RefrescarMensajes;
var
  comu: string;  // <- renombrada (NO uses "name")
begin
  memMensajes.Clear;
  if lstComunidades.ItemIndex < 0 then Exit;

  comu := lstComunidades.Items[lstComunidades.ItemIndex];

  // Requiere uses UDomain, UBST_Communities, SysUtils
  CBST_MessagesToStrings(GlobalCommunities, comu, memMensajes.Lines);

  lblEstado.Caption := Format('Comunidad: %s   Mensajes: %d',
    [comu, memMensajes.Lines.Count]);
end;


procedure TFormComMensajes.FormShow(Sender: TObject);
begin
  CargarComunidades;
  RefrescarMensajes;
end;

procedure TFormComMensajes.lstMensajesClick(Sender: TObject);
var
  Nom: string;
begin
  memMensajes.Clear;
  if lstComunidades.ItemIndex < 0 then Exit;
  Nom := lstComunidades.Items[lstComunidades.ItemIndex];
  CBST_MessagesToStrings(GlobalCommunities, Nom, memMensajes.Lines);
end;

procedure TFormComMensajes.btnRefrescarClick(Sender: TObject);
begin
  CargarComunidades;
  RefrescarMensajes;
end;

procedure TFormComMensajes.btnPublicarClick(Sender: TObject);
var
  Nom: string;
  Ok: Boolean;
  F: string;
begin
  if lstComunidades.ItemIndex < 0 then
  begin
    ShowMessage('Selecciona una comunidad.');
    Exit;
  end;

  if Trim(memNuevo.Text) = '' then
  begin
    ShowMessage('Escribe un mensaje.');
    Exit;
  end;

  Nom := lstComunidades.Items[lstComunidades.ItemIndex];
  F   := FormatDateTime('yyyy-mm-dd hh:nn', Now);

  CBST_AddMessage(GlobalCommunities, Nom, CurrentUserEmail, memNuevo.Text, F, Ok);
  if Ok then
  begin
    memNuevo.Clear;
    // refrescar el panel de mensajes
    CBST_MessagesToStrings(GlobalCommunities, Nom, memMensajes.Lines);
  end
  else
    ShowMessage('No se pudo publicar (comunidad no encontrada).');
end;

procedure TFormComMensajes.btnCerrarClick(Sender: TObject);
begin
  Close;
end;


end.

