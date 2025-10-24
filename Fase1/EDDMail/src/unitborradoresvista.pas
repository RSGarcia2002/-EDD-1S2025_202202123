unit UnitBorradoresVista;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UDomain, UAVL_Borradores, UReports, DateUtils, UDataCore ;

type

  { TFormBorradoresVista }

  TFormBorradoresVista = class(TForm)
    btnCerrar: TButton;
    btnExportarReporte: TButton;
    btnRefrescar: TButton;
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnPostOrden: TButton;
    btnEnviar: TButton;
    lblEstado: TLabel;
    LabelDetalle: TLabel;
    lstBorradores: TListBox;
    memDetalle: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnExportarReporteClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnPreOrdenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstBorradoresClick(Sender: TObject);
  private
    FSelectedId: Integer;
    function GetSelectedId: Integer;
    procedure AddLine(const D: TDraft);
    procedure RefrescarLista;
    procedure MostrarDetalleSeleccionado;
    procedure UpdateEstado(const Recorrido: string);
    procedure VisitAddToList(const D: TDraft);
  public
  end;

var
  FormBorradoresVista: TFormBorradoresVista;

implementation

{$R *.lfm}

procedure TFormBorradoresVista.FormCreate(Sender: TObject);
begin
  FSelectedId := -1;

  if Assigned(lstBorradores)     then lstBorradores.OnClick     := @lstBorradoresClick;
  if Assigned(btnRefrescar)      then btnRefrescar.OnClick      := @btnRefrescarClick;
  if Assigned(btnExportarReporte) then btnExportarReporte.OnClick := @btnExportarReporteClick;
  if Assigned(btnCerrar)         then btnCerrar.OnClick         := @btnCerrarClick;
  if Assigned(btnEnviar)         then btnEnviar.OnClick         := @btnEnviarClick;

  {$IF DECLARED(btnInOrden)}   btnInOrden.OnClick   := @btnInOrdenClick;   {$IFEND}
  {$IF DECLARED(btnPreOrden)}  btnPreOrden.OnClick  := @btnPreOrdenClick;  {$IFEND}
  {$IF DECLARED(btnPostOrden)} btnPostOrden.OnClick := @btnPostOrdenClick; {$IFEND}
end;

function TFormBorradoresVista.GetSelectedId: Integer;
var
  obj: TObject;
begin
  Result := -1;
  if (lstBorradores.ItemIndex < 0) then Exit;
  obj := lstBorradores.Items.Objects[lstBorradores.ItemIndex];
  if obj <> nil then
    Result := PtrInt(obj);
end;


procedure TFormBorradoresVista.AddLine(const D: TDraft);
begin
  if not Assigned(lstBorradores) then Exit;
  lstBorradores.Items.AddObject(
    Format('ID=%d | %s -> %s | %s', [D.Id, D.Remitente, D.Destinatario, D.Asunto]),
    TObject(PtrInt(D.Id))
  );
end;

procedure TFormBorradoresVista.VisitAddToList(const D: TDraft);
begin
  AddLine(D);
end;

procedure TFormBorradoresVista.RefrescarLista;
begin
  if Assigned(lstBorradores) then lstBorradores.Clear;
  if Assigned(memDetalle) then memDetalle.Clear;
  FSelectedId := -1;

  if GlobalDrafts <> nil then
    AVL_InOrder(GlobalDrafts, @AddLine);

  UpdateEstado('In-Orden');
end;

procedure TFormBorradoresVista.MostrarDetalleSeleccionado;
var
  idx: Integer; obj: TObject; idSel: Integer; D: TDraft;
begin
  if Assigned(memDetalle) then memDetalle.Clear;

  if (not Assigned(lstBorradores)) or (lstBorradores.Items.Count=0) then Exit;
  idx := lstBorradores.ItemIndex;
  if idx < 0 then Exit;

  obj := lstBorradores.Items.Objects[idx];
  if obj = nil then Exit;
  idSel := PtrInt(obj);
  if idSel <= 0 then Exit;

  if not AVL_Find(GlobalDrafts, idSel, D) then Exit;

  FSelectedId := idSel;
  memDetalle.Lines.BeginUpdate;
  try
    memDetalle.Lines.Add(Format('ID: %d', [D.Id]));
    memDetalle.Lines.Add('Remitente: ' + D.Remitente);
    memDetalle.Lines.Add('Destinatario: ' + D.Destinatario);
    memDetalle.Lines.Add('Asunto: ' + D.Asunto);
    memDetalle.Lines.Add('---');
    memDetalle.Lines.Add(D.Mensaje);
  finally
    memDetalle.Lines.EndUpdate;
  end;
end;

procedure TFormBorradoresVista.UpdateEstado(const Recorrido: string);
var n: Integer;
begin
  if Assigned(lstBorradores) then n := lstBorradores.Items.Count else n := 0;
  // muestra el conteo en el título del form y el recorrido en lblEstado
  Caption := Format('Borradores (%d)', [n]);
  if Assigned(lblEstado) then
    lblEstado.Caption := 'Recorrido: ' + Recorrido;
end;

procedure TFormBorradoresVista.FormShow(Sender: TObject);
begin
  try
    if GlobalDrafts = nil then
    begin
      if Assigned(lstBorradores) then lstBorradores.Clear;
      if Assigned(memDetalle) then memDetalle.Clear;
      Caption := 'Borradores (0)';
      if Assigned(lblEstado) then lblEstado.Caption := 'Recorrido: —';
      Exit;
    end;
    RefrescarLista;
  except
    on E: Exception do
      MessageDlg('Error al cargar borradores:'#13#10 + E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFormBorradoresVista.lstBorradoresClick(Sender: TObject);
begin
  MostrarDetalleSeleccionado;
end;

procedure TFormBorradoresVista.btnRefrescarClick(Sender: TObject);
begin
  RefrescarLista;
end;

procedure TFormBorradoresVista.btnPreOrdenClick(Sender: TObject);
begin
  lstBorradores.Clear; memDetalle.Clear;
  if GlobalDrafts<>nil then AVL_PreOrder(GlobalDrafts, @VisitAddToList);
  UpdateEstado('Pre-Orden');
end;

procedure TFormBorradoresVista.btnInOrdenClick(Sender: TObject);
begin
  lstBorradores.Clear; memDetalle.Clear;
  if GlobalDrafts<>nil then AVL_InOrder(GlobalDrafts, @VisitAddToList);
  UpdateEstado('In-Orden');
end;

procedure TFormBorradoresVista.btnPostOrdenClick(Sender: TObject);
begin
  lstBorradores.Clear; memDetalle.Clear;
  if GlobalDrafts<>nil then AVL_PostOrder(GlobalDrafts, @VisitAddToList);
  UpdateEstado('Post-Orden');
end;

procedure TFormBorradoresVista.btnExportarReporteClick(Sender: TObject);
begin
  GenerateDraftsReportOnly;
  ShowMessage('Reporte actualizado: Fase 2\graphviz\avl_borradores.dot');
end;

procedure TFormBorradoresVista.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBorradoresVista.btnEnviarClick(Sender: TObject);
var
  idSel : Integer;
  D     : TDraft;
  fec   : string;
begin
  idSel := GetSelectedId;
  if idSel <= 0 then
  begin
    ShowMessage('Selecciona un borrador.');
    Exit;
  end;

  // Buscar los datos del borrador
  if not AVL_Find(GlobalDrafts, idSel, D) then
  begin
    ShowMessage('No se encontró el borrador seleccionado.');
    Exit;
  end;

  // Guardar en Inbox (estado por defecto: NL)
  fec := FormatDateTime('yyyy-mm-dd hh:nn', Now);
  // id de correo: puedes usar el Id del draft o uno nuevo basado en fecha/hora
  Inbox_PushBack(IntToStr(D.Id), D.Remitente, D.Destinatario, D.Asunto, fec, D.Mensaje);

  // Eliminar del AVL y regenerar reporte
  if AVL_RemoveById(GlobalDrafts, D.Id) then
  begin
    GenerateDraftsReportOnly;
    RefrescarLista;  // tu método que re-llena la lista
    ShowMessage('Borrador enviado y removido del árbol.');
  end
  else
    ShowMessage('No se pudo eliminar el borrador del árbol.');
end;


end.

