unit UnitFavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,Dialogs,
  UDomain, UBTree_Favoritos, UReports;

type
  { TFormFavoritos }
  TFormFavoritos = class(TForm)
    btnCerrar: TButton;
    btnExportarReporte: TButton;
    btnRefrescar: TButton;
    btnEliminar: TButton;
    LabelLista: TLabel;
    LabelDetalle: TLabel;
    lbFavs: TListBox;
    memDetalle: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnExportarReporteClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFavsClick(Sender: TObject);
  private
    FItems: array of TMailFav;      // espejo en memoria para mostrar detalle
    procedure ClearItems;
    procedure VisitAddToList(const M: TMailFav);
    function SelectedFavId: Int64;
    procedure VisitAdd(const M: TMailFav);
    procedure RefrescarLista;
    procedure MostrarDetalle;
  public
  end;

var
  FormFavoritos: TFormFavoritos;

implementation

{$R *.lfm}

{=== Helpers internos ==============================================================}

procedure TFormFavoritos.ClearItems;
begin
  SetLength(FItems, 0);
end;

procedure TFormFavoritos.VisitAddToList(const M: TMailFav);
var
  s: string;
begin
  // Ejemplo visible:  ID=20251011103455 | remitente - asunto
  s := Format('ID=%s | %s - %s', [IntToStr(M.Id), M.Remitente, M.Asunto]);
  lbFavs.Items.AddObject(s, nil); // ← NO guardes el ID en Objects (evita truncado)
end;


procedure TFormFavoritos.RefrescarLista;
begin
  lbFavs.Items.BeginUpdate;
  try
    lbFavs.Clear;
    memDetalle.Clear;
    ClearItems;

    if GlobalFavs = nil then
    begin
      lbFavs.Items.Add('(Sin favoritos)');
      Exit;
    end;

    // Recorre el B-Tree (WIP) y llena la lista
    // NOTA: requiere que UBTree_Favoritos exponga BTree_ForEach_WIP (ver nota al final)
    BTree_ForEach_WIP(GlobalFavs, @VisitAdd);

    if lbFavs.Items.Count = 0 then
      lbFavs.Items.Add('(Sin favoritos)');
  finally
    lbFavs.Items.EndUpdate;
  end;

  if (lbFavs.Items.Count > 0) and (FItems <> nil) then
  begin
    lbFavs.ItemIndex := 0;
    MostrarDetalle;
  end;
end;

procedure TFormFavoritos.MostrarDetalle;
var
  obj: TObject;
  idx: Integer;
  M: TMailFav;
begin
  memDetalle.Clear;
  if (lbFavs.ItemIndex < 0) then Exit;

  obj := lbFavs.Items.Objects[lbFavs.ItemIndex];
  if obj = nil then Exit;

  idx := PtrInt(obj);
  if (idx < 0) or (idx >= Length(FItems)) then Exit;

  M := FItems[idx];

  memDetalle.Lines.BeginUpdate;
  try
    memDetalle.Lines.Add(Format('ID: %d', [M.Id]));
    memDetalle.Lines.Add('Fecha:  ' + M.Fecha);
    memDetalle.Lines.Add('De:     ' + M.Remitente);
    memDetalle.Lines.Add('Para:   ' + M.Destinatario);
    memDetalle.Lines.Add('Asunto: ' + M.Asunto);
    memDetalle.Lines.Add('---');
    memDetalle.Lines.Add(M.Mensaje);
  finally
    memDetalle.Lines.EndUpdate;
  end;
end;

{=== Eventos ======================================================================}

procedure TFormFavoritos.FormShow(Sender: TObject);
begin
  Caption := 'Favoritos - ' + Domain_GetCurrentUser;
  RefrescarLista;
end;

procedure TFormFavoritos.lbFavsClick(Sender: TObject);
begin
  MostrarDetalle;
end;

procedure TFormFavoritos.btnRefrescarClick(Sender: TObject);
begin
  RefrescarLista;
end;
function TFormFavoritos.SelectedFavId: Int64;
var
  s, digits: string;
  p: Integer;
begin
  Result := -1;
  if lbFavs.ItemIndex < 0 then Exit;

  s := lbFavs.Items[lbFavs.ItemIndex];
  p := Pos('ID=', s);
  if p = 0 then Exit;
  Inc(p, 3); // salta "ID="

  digits := '';
  while (p <= Length(s)) and (s[p] in ['0'..'9']) do
  begin
    digits += s[p];
    Inc(p);
  end;

  Result := StrToInt64Def(digits, -1);
end;



procedure TFormFavoritos.btnExportarReporteClick(Sender: TObject);
begin
  // Usa tu helper de reportes (mismo que desde Bandeja)
  GenerateFavoritesReportOnly;
  ShowMessage('Reporte actualizado: Fase 2\graphviz\btree_favoritos.dot');
end;

procedure TFormFavoritos.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFavoritos.btnEliminarClick(Sender: TObject);
var
  idSel: Int64;
  removed: Boolean;
begin
  idSel := SelectedFavId;
  if idSel <= 0 then
  begin
    ShowMessage('Selecciona un favorito.');
    Exit;
  end;

  BTree_Remove_WIP(GlobalFavs, idSel, removed);
  if removed then
  begin
    GenerateFavoritesReportOnly;
    RefrescarLista;
    ShowMessage('Eliminado de favoritos.');
  end
  else
    ShowMessage('No se pudo eliminar (no encontrado).');
end;



end.

