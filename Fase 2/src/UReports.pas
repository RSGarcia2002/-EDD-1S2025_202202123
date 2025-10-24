unit UReports;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // Estructuras
  UBST_Contacts,
  UAVL_Borradores,
  UBTree_Favoritos,
  UBST_Communities;

// Llamar desde los botones de la GUI
procedure GenerateAllWIPReports;

// Reportes individuales (usados en runtime)
procedure GenerateDraftsReportOnly;
procedure GenerateContactsReportOnly;
procedure GenerateFavoritesReportOnly;
procedure GenerateCommunitiesReportOnly;

implementation

uses
  UDomain, Process; // Para ejecutar "dot" y generar PNG

{ ========================== HELPERS DE RUTAS ========================== }

function EnsureDir(const P: string): string;
begin
  if not DirectoryExists(P) then
    ForceDirectories(P);
  Result := P;
end;

function GoUp(const P: string; Levels: Integer): string;
var
  i: Integer;
  R: string;
begin
  R := IncludeTrailingPathDelimiter(P);
  for i := 1 to Levels do
    R := ExpandFileName(R + '..' + DirectorySeparator);
  Result := R;
end;

// ✅ Guarda siempre en: <raíz del repo>\Fase 2\graphviz\
function GraphvizDir: string;
var
  exeDir, repoRoot, outDir: string;
begin
  exeDir   := ExtractFilePath(ParamStr(0));  // ...\Fase1\EDDMail\lib\x86_64-win64\
  repoRoot := GoUp(exeDir, 3);               // sube hasta raíz del proyecto
  outDir   := repoRoot + 'Fase 2' + DirectorySeparator +
                        'graphviz' + DirectorySeparator;
  Result := EnsureDir(outDir);
end;

{ ========================== GENERAR PNG OPCIONAL ========================== }

procedure TryGeneratePNGFromDOT(const DotFile, PngFile: string);
var
  P: TProcess;
begin
  if not FileExists(DotFile) then Exit;

  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotFile);
    P.Parameters.Add('-o');
    P.Parameters.Add(PngFile);
    P.Options := [poNoConsole, poUsePipes];
    try
      P.Execute;
    except
      // Si no tienes Graphviz, simplemente ignora el error
    end;
  finally
    P.Free;
  end;
end;

{ ========================== REPORTES INDIVIDUALES ========================== }

procedure GenerateDraftsReportOnly;
var
  outDir, dotFile, pngFile: string;
begin
  outDir  := GraphvizDir;
  dotFile := outDir + 'avl_borradores.dot';
  pngFile := outDir + 'avl_borradores.png';
  AVL_ToDot(GlobalDrafts, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);
end;

procedure GenerateContactsReportOnly;
var
  outDir, dotFile, pngFile: string;
begin
  outDir  := GraphvizDir;
  dotFile := outDir + 'bst_contactos.dot';
  pngFile := outDir + 'bst_contactos.png';
  BST_ToDot(GlobalContacts, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);
end;

procedure GenerateFavoritesReportOnly;
var
  outDir, dotFile, pngFile: string;
begin
  outDir  := GraphvizDir;
  dotFile := outDir + 'btree_favoritos.dot';
  pngFile := outDir + 'btree_favoritos.png';
  BTree_ToDot_WIP(GlobalFavs, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);
end;

procedure GenerateCommunitiesReportOnly;
var
  outDir, dotFile, pngFile: string;
begin
  outDir  := GraphvizDir;
  dotFile := outDir + 'bst_comunidades.dot';
  pngFile := outDir + 'bst_comunidades.png';
  CBST_ToDot(GlobalCommunities, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);
end;

{ ========================== DEMO: GENERATE ALL ========================== }

procedure GenerateAllWIPReports;
var
  // Raíces
  RCont: PBSNode;
  RAVL : PAVL;
  RB   : PBNode;
  RCom : PCBST;

  // Datos de ejemplo
  C: TContacto;
  D: TDraft;
  M: TMailFav;

  outDir, dotFile, pngFile: string;
begin
  outDir := GraphvizDir;

  // Inicializa
  RCont := nil; RAVL := nil; RB := nil; RCom := nil;

  // --- CONTACTOS (BST) ---
  BST_Init(RCont);
  C.Nombre := 'Ana';    C.Email := 'ana@edd.com';    C.Telefono := '1111'; BST_Insert(RCont, C);
  C.Nombre := 'Luis';   C.Email := 'luis@edd.com';   C.Telefono := '2222'; BST_Insert(RCont, C);
  C.Nombre := 'Marcos'; C.Email := 'marcos@edd.com'; C.Telefono := '3333'; BST_Insert(RCont, C);
  dotFile := outDir + 'bst_contactos.dot';
  pngFile := outDir + 'bst_contactos.png';
  BST_ToDot(RCont, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);

  // --- BORRADORES (AVL) ---
  AVL_Init(RAVL);
  D.Id := 1; D.Asunto := 'Borrador 1'; D.Remitente := 'ana@edd.com';    D.Destinatario := 'luis@edd.com';   D.Mensaje := 'Hola'; AVL_Insert(RAVL, D);
  D.Id := 2; D.Asunto := 'Borrador 2'; D.Remitente := 'luis@edd.com';   D.Destinatario := 'marcos@edd.com'; D.Mensaje := 'Reporte'; AVL_Insert(RAVL, D);
  D.Id := 3; D.Asunto := 'Borrador 3'; D.Remitente := 'marcos@edd.com'; D.Destinatario := 'ana@edd.com';    D.Mensaje := 'Aviso'; AVL_Insert(RAVL, D);
  dotFile := outDir + 'avl_borradores.dot';
  pngFile := outDir + 'avl_borradores.png';
  AVL_ToDot(RAVL, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);

  // --- FAVORITOS (B-TREE) ---
  BTree_Init(RB);
  M.Id := 101; M.Remitente := 'ana@edd.com'; M.Asunto := 'Proyecto'; M.Mensaje := 'Adjunto avance'; M.Fecha := '2025-10-11'; BTree_Insert_WIP(RB, M);
  M.Id := 102; M.Remitente := 'luis@edd.com'; M.Asunto := 'Pago'; M.Mensaje := 'Enviado'; M.Fecha := '2025-10-10'; BTree_Insert_WIP(RB, M);
  M.Id := 103; M.Remitente := 'marcos@edd.com'; M.Asunto := 'Evento'; M.Mensaje := 'Invitación'; M.Fecha := '2025-10-09'; BTree_Insert_WIP(RB, M);
  dotFile := outDir + 'btree_favoritos.dot';
  pngFile := outDir + 'btree_favoritos.png';
  BTree_ToDot_WIP(RB, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);

  // --- COMUNIDADES (BST) ---
  CBST_Init(RCom);
  CBST_Insert_WIP(RCom, 'Backend');
  CBST_Insert_WIP(RCom, 'Frontend');
  dotFile := outDir + 'bst_comunidades.dot';
  pngFile := outDir + 'bst_comunidades.png';
  CBST_ToDot(RCom, dotFile);
  TryGeneratePNGFromDOT(dotFile, pngFile);
end;

end.

