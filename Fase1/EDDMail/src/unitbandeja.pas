unit UnitBandeja;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UDataCore, Math, DateUtils;

type
  { TFormBandeja }
  TFormBandeja = class(TForm)
    btnCerrar: TButton;
    btnEliminar: TButton;
    btnOrdenar: TButton;
    btnFavorito: TButton;
    lblNoLeidos: TLabel;
    lbBandeja: TListBox;
    memDetalle: TMemo;
    procedure btnFavoritoClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnOrdenarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbBandejaClick(Sender: TObject);
    procedure lbBandejaDblClick(Sender: TObject);
  private
    FMap: TFPList; // mapa paralelo de PMailNode
    procedure CargarLista;
    procedure MostrarDetalle;
    function SelectedMailNode: PMailNode;
  public
  end;

var
  FormBandeja: TFormBandeja;

implementation

{$R *.lfm}

uses
  UnitPapelera, UDomain, UBTree_Favoritos, UReports;

{ ===== Ciclo de vida ===== }

procedure TFormBandeja.FormCreate(Sender: TObject);
begin
  FMap := TFPList.Create;

  // Asegura que los eventos queden bien aunque el .lfm estuviera mal
  if Assigned(lbBandeja) then
  begin
    lbBandeja.OnClick    := @lbBandejaClick;
    lbBandeja.OnDblClick := @lbBandejaDblClick;
  end;
  btnOrdenar.OnClick   := @btnOrdenarClick;
  btnEliminar.OnClick  := @btnEliminarClick;
  btnFavorito.OnClick  := @btnFavoritoClick;
  btnCerrar.OnClick    := @btnCerrarClick;
end;

procedure TFormBandeja.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMap);
end;

procedure TFormBandeja.FormShow(Sender: TObject);
begin
  Caption := 'Bandeja - ' + Domain_GetCurrentUser;
  CargarLista;

end;


{ ===== Helpers ===== }

procedure TFormBandeja.CargarLista;
var
  cur: PMailNode;
  i, noLeidos: Integer;
  display, ownerEmail, dsto: string;
begin
  ownerEmail := LowerCase(Trim(Domain_GetCurrentUser));

  FMap.Clear;
  lbBandeja.Items.BeginUpdate;
  try
    lbBandeja.Clear;
    memDetalle.Clear;

    if ownerEmail = '' then
    begin
      lbBandeja.Items.Add('(Usuario no establecido. Inicia sesión)');
      lblNoLeidos.Caption := 'No leídos: 0';
      Exit;
    end;

    i := 0;
    noLeidos := 0;
    cur := InboxHead;
    while cur <> nil do
    begin
      dsto := LowerCase(Trim(cur^.Destinatario));
      if dsto = ownerEmail then
      begin
        Inc(i);
        if SameText(cur^.Estado, 'L') then
          Inc(noLeidos);

        FMap.Add(cur);
        display := Format('%d) [%s] %s - %s (%s)',
          [i, cur^.Estado, cur^.Remitente, cur^.Asunto, cur^.Fecha]);
        lbBandeja.Items.Add(display);
      end;
      cur := cur^.Next;
    end;

    if i = 0 then
      lbBandeja.Items.Add('(No hay correos para ' + Domain_GetCurrentUser + ')');

    lblNoLeidos.Caption := 'No leídos: ' + IntToStr(noLeidos);
  finally
    lbBandeja.Items.EndUpdate;
  end;

  if FMap.Count > 0 then
  begin
    lbBandeja.ItemIndex := 0;
    MostrarDetalle;
  end;
end;





function TFormBandeja.SelectedMailNode: PMailNode;
var
  idx: Integer;
  txt: string;
begin
  Result := nil;
  if (FMap = nil) or (FMap.Count = 0) then Exit;

  idx := lbBandeja.ItemIndex;
  if (idx < 0) or (idx >= lbBandeja.Items.Count) then Exit;

  // Evitar seleccionar el placeholder "(Bandeja vacía)" o similares
  txt := lbBandeja.Items[idx];
  if (txt <> '') and (txt[1] = '(') then Exit;

  if (idx < FMap.Count) then
    Result := PMailNode(FMap[idx]);
end;



procedure TFormBandeja.MostrarDetalle;
var
  node: PMailNode;
begin
  memDetalle.Clear;
  node := SelectedMailNode;
  if node = nil then Exit;

  memDetalle.Lines.BeginUpdate;
  try
    memDetalle.Lines.Add('ID:        ' + node^.Id);
    memDetalle.Lines.Add('Estado:    ' + node^.Estado);
    memDetalle.Lines.Add('Fecha:     ' + node^.Fecha);
    memDetalle.Lines.Add('De:        ' + node^.Remitente);
    memDetalle.Lines.Add('Para:      ' + node^.Destinatario);
    memDetalle.Lines.Add('Asunto:    ' + node^.Asunto);
    memDetalle.Lines.Add('---');
    memDetalle.Lines.Add(node^.Mensaje);
  finally
    memDetalle.Lines.EndUpdate;
  end;
end;

{ ===== Eventos UI ===== }

procedure TFormBandeja.lbBandejaClick(Sender: TObject);
begin
  MostrarDetalle;
end;

procedure TFormBandeja.lbBandejaDblClick(Sender: TObject);
var
  node: PMailNode;
begin
  node := SelectedMailNode;
  if node = nil then Exit;

  if node^.Estado = 'NL' then node^.Estado := 'L'
  else node^.Estado := 'NL';

  CargarLista; // repinta lista y mapa
end;

procedure TFormBandeja.btnOrdenarClick(Sender: TObject);
  procedure SortBySubject;
  var i, j: Integer; A, B: PMailNode;
  begin
    for i := 0 to FMap.Count - 2 do
      for j := i + 1 to FMap.Count - 1 do
      begin
        A := PMailNode(FMap[i]);
        B := PMailNode(FMap[j]);
        if CompareText(A^.Asunto, B^.Asunto) > 0 then
          FMap.Exchange(i, j);
      end;
  end;

var
  i: Integer;
  N: PMailNode;
begin
  if (FMap = nil) or (FMap.Count <= 1) then Exit;

  SortBySubject;

  lbBandeja.Items.BeginUpdate;
  try
    lbBandeja.Clear;
    for i := 0 to FMap.Count - 1 do
    begin
      N := PMailNode(FMap[i]);
      lbBandeja.Items.Add(Format('%d) [%s] %s - %s (%s)',
        [i + 1, N^.Estado, N^.Remitente, N^.Asunto, N^.Fecha]));
    end;
  finally
    lbBandeja.Items.EndUpdate;
  end;

  lbBandeja.ItemIndex := 0;
  MostrarDetalle;
end;

procedure TFormBandeja.btnEliminarClick(Sender: TObject);
var
  node: PMailNode;
  id, rem, dest, asu, fec, msg, est: string;
begin
  node := SelectedMailNode;
  if node = nil then
  begin
    MessageDlg('Selecciona un correo primero.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if Inbox_RemoveNode(node, id, rem, dest, asu, fec, msg, est) then
  begin
    Trash_Push(id, rem, dest, asu, fec, msg, est);
    if Assigned(FormPapelera) then
      Trash_ToStrings(FormPapelera.lbPapelera.Items);
    CargarLista;
    MessageDlg('Correo movido a Papelera.', mtInformation, [mbOK], 0);
  end
  else
    MessageDlg('No se pudo eliminar el correo.', mtError, [mbOK], 0);
end;

procedure TFormBandeja.btnFavoritoClick(Sender: TObject);
var
  node: PMailNode;
  MF: TMailFav;
  favId: Int64;
begin
  node := SelectedMailNode;
  if node = nil then
  begin
    MessageDlg('Selecciona un correo.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // ID robusto: intenta convertir el Id del mail a Int64, si no, crea uno
  favId := StrToInt64Def(node^.Id, 0);
  if favId = 0 then
    favId := StrToInt64Def(StringReplace(FormatDateTime('yyyymmddhhnnss', Now), ' ', '', [rfReplaceAll]), 0);
  if favId = 0 then
    favId := MilliSecondOf(Now); // último fallback pequeño pero único en la sesión

  MF.Id           := favId;
  MF.Remitente    := node^.Remitente;
  MF.Destinatario := node^.Destinatario;
  MF.Asunto       := node^.Asunto;
  MF.Mensaje      := node^.Mensaje;
  MF.Fecha        := node^.Fecha;

  BTree_Insert_WIP(GlobalFavs, MF);
  GenerateFavoritesReportOnly;

  MessageDlg('Añadido a Favoritos (y reporte actualizado).', mtInformation, [mbOK], 0);
end;

procedure TFormBandeja.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

