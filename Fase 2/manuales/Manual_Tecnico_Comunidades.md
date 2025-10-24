# Manual Técnico — Módulo Comunidades (BST + Lista de Mensajes)

## 1. Overview técnico
- **BST de comunidades** (clave: `Nombre`, comparación case-insensitive).
- **Lista simple de mensajes** por comunidad (push en cabeza para O(1)).
- **Métricas**: cada nodo mantiene `NumMensajes` actualizado.
- **Reporte DOT** del BST para auditoría/visualización.

## 2. Estructuras
```pascal
type
  PMsg = ^TMsg;
  TMsg = record
    Correo   : string;
    Texto    : string;
    FechaPub : string;   // 'YYYY-MM-DD HH:NN:SS'
    Next     : PMsg;
  end;

  TComunidad = record
    Nombre        : string;
    FechaCreacion : TDateTime;
    NumMensajes   : Integer;
  end;

  PCBST = ^TCBST;
  TCBST = record
    Key     : string;   // == Nombre
    Data    : TComunidad;
    MsgHead : PMsg;
    L, R    : PCBST;
  end;
```

## 3. API pública (unidad `UBST_Communities`)
```pascal
procedure CBST_Init(var Root: PCBST);
function  CBST_Find(Root: PCBST; const Nombre: string): PCBST;
procedure CBST_Insert(var Root: PCBST; const Nombre: string; Fecha: TDateTime);
function  CBST_PublishMsg(var Root: PCBST; const Nombre, Correo, Texto: string;
                          Fecha: TDateTime; out ErrMsg: string): Boolean;
procedure CBST_ForEachInOrder(Root: PCBST; Proc: TProc<PCBST>);
procedure CBST_ToDot(Root: PCBST; const DotPath: string);
procedure CBST_Free(var Root: PCBST);

// Mensajes
procedure Msg_ForEach(Head: PMsg; Proc: TProc<PMsg>);
```

### 3.1 Contratos
- `CBST_Insert`: ignora si el `Nombre` ya existe (no duplica).
- `CBST_PublishMsg`: **False** y `ErrMsg='La comunidad no existe'` si `Nombre` no está en el BST.
- `CBST_ToDot`: genera un **digraph** con nodos tipo caja y aristas a L/R.

## 4. Complejidad
- **Insert/Find**: promedio O(log n), peor O(n) si desbalanceado.
- **Publicar mensaje**: O(1) para insertar al inicio de la lista + O(log n)/O(n) búsqueda.
- **ToDot**: O(n) sobre comunidades.

## 5. Integración con formularios
### 5.1 Root
- `FormCreate`: `CBST_Init(CommRoot)`.
- `btnComunidadesClick`: InputQuery → `CBST_Insert`.
- `btnExportComunidadesClick`: `CBST_ToDot` + `dot -Tpng`.
- `btnVerMsgsComunidadClick`: localizar nodo y listar con `Msg_ForEach`.

### 5.2 Usuario
- `btnPublicarClick`: `CBST_PublishMsg(CommRoot, Nombre, Correo, Texto, Now, Err)`.

## 6. Formato DOT generado
Ejemplo aproximado:
```dot
digraph Comunidades {
  node [shape=box, style="rounded,filled", fillcolor="#E8F0FE"];
  rankdir=TB;
  "general" [label="📌 general\nCreada: 2025-10-10\n#Mensajes: 3"];
  "general" -> "anuncios";
  ...
}
```

## 7. Pruebas recomendadas
- Insertar 5+ comunidades en diferentes órdenes (mejor y peor caso).
- Publicar 3+ mensajes en 2 comunidades distintas.
- Validar `NumMensajes` y orden L/R en DOT.
- Borde: intentar publicar en comunidad inexistente.

## 8. Troubleshooting
- **E016: dot no encontrado** → instale Graphviz o añada a PATH.
- **Nested procedure type** en callbacks → declare `TProc` según FPC/Lazarus o use procedimientos globales.
- **Comparación con acentos** → `CompareText` maneja case-insensitive; si requiere collate específico, reemplace por normalización previa.

## 9. Extensiones sugeridas (opcional)
- Balanceo (AVL/Red-Black) si n crece mucho.
- Persistencia en archivo (JSON/CSV) para comunidades y mensajes.
- Paginación de mensajes en UI.
