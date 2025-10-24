unit unitBorradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UDomain, UAVL_Borradores, UReports;

type
  { TFormBorradoresView }
  TFormBorradoresView = class(TForm)
    btnCerrar: TButton;
    btnRefrescar: TButton;
    btnExportarReporte: TButton;
    LabelLista: TLabel;
    LabelDetalle: TLabel;
    lstBorradores: TListBox;
    memDetalle: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnExportarReporteClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstBorradoresClick(Sender: TObject);
  private
    procedure AddLine(const D: TDraft);
    procedure RefrescarLista;
    procedure MostrarDetalleSeleccionado;
  public
  end;

var
  FormBorradoresView: TFormBorradoresView;

implementation

{$R *.lfm}

{ ---------- helpers ---------- }

procedure TFormBorradoresView.AddLine(const D: TDraft);
begin
  // Solo para ver: mostramos línea y guardamos el ID en Objects
  lstBorradores.Items.AddObject(
    Format('ID=%d | %s -> %s | %s',
      [D.Id, D.Remitente, D.Destinatario, D.Asunto]),
    TObject(PtrInt(D.Id))
  );
end;

procedure TFormBorradoresView.RefrescarLista;
begin
  lstBorradores.Clear;
  memDetalle.Clear;
  AVL_InOrder(GlobalDrafts, @AddLine);
end;

procedure TFormBorradoresView.MostrarDetalleSeleccionado;
var
  idx: Integer;
  idSel: Integer;
  D: TDraft;
begin
  memDetalle.Clear;
  idx := lstBorradores.ItemIndex;
  if (idx < 0) then Exit;
  idSel := PtrInt(lstBorradores.Items.Objects[idx]);
  if AVL_Find(GlobalDrafts, idSel, D) then
  begin
    memDetalle.Lines.Add(Format('ID: %d', [D.Id]));
    memDetalle.Lines.Add('Remitente: ' + D.Remitente);
    memDetalle.Lines.Add('Destinatario: ' + D.Destinatario);
    memDetalle.Lines.Add('Asunto: ' + D.Asunto);
    memDetalle.Lines.Add('---');
    memDetalle.Lines.Add(D.Mensaje);
  end;
end;

{ ---------- eventos ---------- }

procedure TFormBorradoresView.FormShow(Sender: TObject);
begin
  RefrescarLista;
end;

procedure TFormBorradoresView.lstBorradoresClick(Sender: TObject);
begin
  MostrarDetalleSeleccionado;
end;

procedure TFormBorradoresView.btnRefrescarClick(Sender: TObject);
begin
  RefrescarLista;
end;

procedure TFormBorradoresView.btnExportarReporteClick(Sender: TObject);
begin
  // solo exporta el .dot actual del AVL real
  GenerateDraftsReportOnly;
  ShowMessage('Reporte actualizado: Fase 2\graphviz\avl_borradores.dot');
end;

procedure TFormBorradoresView.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

