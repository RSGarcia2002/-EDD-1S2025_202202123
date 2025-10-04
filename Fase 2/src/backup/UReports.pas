unit UReports;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  UBST_Contacts,       // BST Contactos
  UAVL_Borradores,     // AVL Borradores (WIP)
  UBTree_Favoritos,    // B-Tree Favoritos (WIP)
  UBST_Communities;    // BST Comunidades (WIP)

// Llama esto desde tu botón en la GUI
procedure GenerateAllWIPReports;

implementation

function EnsureDir(const P: string): string;
begin
  if not DirectoryExists(P) then ForceDirectories(P);
  Result := P;
end;

function GoUp(const P: string; Levels: Integer): string;
var i: Integer; R: string;
begin
  R := IncludeTrailingPathDelimiter(P);
  for i := 1 to Levels do
    R := ExpandFileName(R + '..' + DirectorySeparator);
  Result := R;
end;

// ✅ Guarda SIEMPRE en <raiz del repo>/Fase2/graphviz/
function GraphvizDir: string;
var exeDir, repoRoot, outDir: string;
begin
  exeDir  := ExtractFilePath(ParamStr(0));        // .../Fase1/EDDMail/lib/x86_64-win64/  (típico)
  repoRoot := GoUp(exeDir, 2);                    // sube dos niveles: .../Fase1/EDDMail/
  repoRoot := GoUp(repoRoot, 1);                  // sube uno más:    .../<raiz del repo>/
  outDir   := repoRoot + 'Fase2' + DirectorySeparator +
                        'graphviz' + DirectorySeparator;
  Result := EnsureDir(outDir);
end;





procedure GenerateAllWIPReports;
var
  // Raíces de estructuras
  RCont: PBSNode;   // contactos (BST)
  RAVL : PAVL;      // borradores (AVL WIP)
  RB   : PBNode;    // favoritos (B-Tree WIP)
  RCom : PCBST;     // comunidades (BST WIP)

  // Datos de ejemplo
  C    : TContacto;
  D    : TDraft;
  M    : TMailFav;

  OutDir: string;
begin
  OutDir := GraphvizDir;

  // Inicializar punteros para que el compilador no advierta
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

  // --- Borradores (AVL WIP) ---
  AVL_Init(RAVL);
  D.Id := 1; D.Asunto := 'Borrador 1'; D.Remitente := 'ana@edd.com'; D.Destinatario := 'luis@edd.com'; D.Mensaje := 'WIP';
  AVL_Insert_WIP(RAVL, D);
  D.Id := 2; D.Asunto := 'Borrador 2'; D.Remitente := 'luis@edd.com'; D.Destinatario := 'marcos@edd.com'; D.Mensaje := 'WIP';
  AVL_Insert_WIP(RAVL, D);
  AVL_ToDot(RAVL, OutDir + 'avl_borradores.dot');
  // (si luego implementas Dispose/Free, libéralo aquí)

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

end.

