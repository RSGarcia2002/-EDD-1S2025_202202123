unit UnitInbox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  UDataCore, UDomain;

type

  { TFormInbox }

  TFormInbox = class(TForm)
    btnCerrar: TButton;
    btnFavorito: TButton;
    btnRefrescar: TButton;
    btnDemo: TButton;
    LabelLista: TLabel;
    LabelDetalle: TLabel;
    lstInbox: TListBox;
    memDetalle: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnDemoClick(Sender: TObject);
    procedure btnFavoritoClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstInboxClick(Sender: TObject);
  private
    procedure RefrescarLista;
    procedure MostrarDetalle;
  public
  end;

var
  FormInbox: TFormInbox;

implementation

{$R *.lfm}

procedure TFormInbox.RefrescarLista;
var
  n: Integer;
begin
  if Assigned(lstInbox) then lstInbox.Clear;
  if Assigned(memDetalle) then memDetalle.Clear;

  if CurrentUserEmail = '' then
  begin
    Caption := 'Bandeja de entrada (usuario no definido)';
    Exit;
  end;

  // 1) Conteo y llenado (firma: TStrings primero, luego el email)
  n := Inbox_CountFor(CurrentUserEmail);
  Inbox_ToStringsFor(lstInbox.Items, CurrentUserEmail);

  // 2) Fallback visible si el helper no agregó nada
  if (n > 0) and (lstInbox.Items.Count = 0) then
    lstInbox.Items.Add('(⚠ filtro no mostró elementos — revisar helper)');

  // 3) Encabezado
  Caption := Format('Bandeja de entrada (%d)', [n]);
end;



procedure TFormInbox.MostrarDetalle;
var
  s: string;
begin
  memDetalle.Clear;
  if lstInbox.ItemIndex < 0 then Exit;
  s := lstInbox.Items[lstInbox.ItemIndex];
  // Si tu ToStringsFor pone todo en una línea, muéstrala tal cual
  memDetalle.Lines.Text := s;
end;

procedure TFormInbox.FormShow(Sender: TObject);
begin
  RefrescarLista;
end;

procedure TFormInbox.lstInboxClick(Sender: TObject);
begin
  MostrarDetalle;
end;

procedure TFormInbox.btnRefrescarClick(Sender: TObject);
begin
  RefrescarLista;
end;

procedure TFormInbox.btnFavoritoClick(Sender: TObject);
var
  s: string;
begin
  if lstInbox.ItemIndex < 0 then
  begin
    ShowMessage('Selecciona un correo.'); Exit;
  end;
  s := lstInbox.Items[lstInbox.ItemIndex];
  // Por ahora solo marcamos visualmente.
  ShowMessage('Marcado como favorito:' + LineEnding + s);
end;


procedure TFormInbox.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormInbox.btnDemoClick(Sender: TObject);
var id,fec: string;
begin
  if CurrentUserEmail = '' then Exit;

  id  := FormatDateTime('yyyymmddhhnnss', Now);
  fec := FormatDateTime('yyyy-mm-dd hh:nn', Now);
  Inbox_PushBack(id, 'test1@edd.com', CurrentUserEmail, 'Hola 1', fec, 'Mensaje 1');

  id  := FormatDateTime('yyyymmddhhnnss', Now + EncodeTime(0,0,1,0));
  Inbox_PushBack(id, 'test2@edd.com', CurrentUserEmail, 'Hola 2', fec, 'Mensaje 2');

  RefrescarLista;
end;


end.

