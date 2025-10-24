unit UReports;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // estructuras
  UBST_Contacts,
  UAVL_Borradores,
  UBTree_Favoritos,
  UBST_Communities;

// Llama esto desde tu botón en la GUI
procedure GenerateAllWIPReports;
// Llama esto al guardar un borrador para regenerar solo el AVL real
procedure GenerateDraftsReportOnly;
procedure GenerateContactsReportOnly;
procedure GenerateFavoritesReportOnly;
procedure GenerateCommunitiesReportOnly;

implementation

uses
  UDomain// GlobalDrafts

{ ===== Helpers de rutas ===== }

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

// ✅ Siempre guarda en <raíz del repo>/Fase 2/graphviz/
function GraphvizDir: string;
var
  exeDir, repoRoot, outDir: string;
begin
  // típico exe: .../Fase1/EDDMail/lib/x86_64-win64/
  exeDir   := ExtractFilePath(ParamStr(0));
  repoRoot := GoUp(exeDir, 3); // sube a la raíz del repo
  outDir   := repoRoot + 'Fase 2' + DirectorySeparator +
                        'graphviz' + DirectorySeparator;
  Result := EnsureDir(outDir);
end;

{ ===== Reportes ===== }

procedure GenerateDraftsReportOnly;
var
  OutDir: string;
begin
  OutDir := GraphvizDir;
  // Exporta el AVL real que vive en memoria (GlobalDrafts)
  AVL_ToDot(GlobalDrafts, OutDir + 'avl_borradores.dot');
end;
procedure GenerateContactsReportOnly;
var OutDir: string;
begin
  OutDir := GraphvizDir;
  BST_ToDot(GlobalContacts, OutDir + 'bst_contactos.dot');
end;

procedure GenerateAllWIPReports;
var
  // Raíces de estructuras
  RCont: PBSNode;   // contactos (BST)
  RAVL : PAVL;      // borradores (AVL demo)
  RB   : PBNode;    // favoritos (B-Tree demo)
  RCom : PCBST;     // comunidades (BST demo)

  // Datos de ejemplo
  C    : TContacto;
  D    : TDraft;
  M    : TMailFav;

  OutDir: string;
begin
  OutDir := GraphvizDir;

  // Inicializar punteros
  RCont := nil;
  RAVL  := nil;
  RB    := nil;
  RCom  := nil;

  // --- Contactos (BST real con datos demo) ---
  BST_Init(RCont);
  C.Nombre := 'Ana';    C.Email := 'ana@edd.com';    C.Telefono := '1111'; BST_Insert(RCont, C);
  C.Nombre := 'Luis';   C.Email := 'luis@edd.com';   C.Telefono := '2222'; BST_Insert(RCont, C);
  C.Nombre := 'Marcos'; C.Email := 'marcos@edd.com'; C.Telefono := '3333'; BST_Insert(RCont, C);
  BST_ToDot(RCont, OutDir + 'bst_contactos.dot');
  BST_Dispose(RCont);

  // --- Borradores (AVL REAL, datos demo) ---
  AVL_Init(RAVL);
  D.Id:=1; D.Asunto:='Borrador 1'; D.Remitente:='ana@edd.com';   D.Destinatario:='luis@edd.com';   D.Mensaje:='WIP'; AVL_Insert(RAVL, D);
  D.Id:=2; D.Asunto:='Borrador 2'; D.Remitente:='luis@edd.com';  D.Destinatario:='marcos@edd.com'; D.Mensaje:='WIP'; AVL_Insert(RAVL, D);
  D.Id:=3; D.Asunto:='Borrador 3'; D.Remitente:='marcos@edd.com';D.Destinatario:='ana@edd.com';    D.Mensaje:='WIP'; AVL_Insert(RAVL, D);
  AVL_ToDot(RAVL, OutDir + 'avl_borradores.dot');

  // --- Favoritos (B-Tree WIP) ---
  BTree_Init(RB);
  M.Id:=101; M.Remitente:='a'; M.Destinatario:='b'; M.Asunto:='x'; M.Mensaje:='y'; M.Fecha:='hoy'; BTree_Insert_WIP(RB, M);
  M.Id:=102; M.Remitente:='';  M.Destinatario:='';  M.Asunto:='';  M.Mensaje:='';  M.Fecha:='';     BTree_Insert_WIP(RB, M);
  M.Id:=103; BTree_Insert_WIP(RB, M);
  M.Id:=104; BTree_Insert_WIP(RB, M);
  M.Id:=105; BTree_Insert_WIP(RB, M);
  BTree_ToDot_WIP(RB, OutDir + 'btree_favoritos.dot');

  // --- Comunidades (BST WIP) ---
  CBST_Init(RCom);
  CBST_Insert_WIP(RCom, 'Backend');
  CBST_Insert_WIP(RCom, 'Frontend');
  CBST_ToDot(RCom, OutDir + 'bst_comunidades.dot');
end;
procedure GenerateFavoritesReportOnly;
var
  OutDir: string;
begin
  OutDir := GraphvizDir;  // reusa tu helper interno
  BTree_ToDot_WIP(GlobalFavs, OutDir + 'btree_favoritos.dot');
end;
procedure GenerateCommunitiesReportOnly;
var outDir: string;
begin
  outDir := GraphvizDir; // Fase 2\graphviz
  CBST_ToDot(GlobalCommunities, outDir + 'bst_comunidades.dot');
end;

end.

