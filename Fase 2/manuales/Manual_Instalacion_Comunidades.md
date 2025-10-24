# Manual de Instalación y Configuración — Comunidades

## 1. Requisitos
- **Lazarus/FPC** (versión usada en el proyecto).
- **Graphviz** para exportar PNG: ejecutable `dot` disponible en **PATH**.
- Permisos de escritura en el directorio del proyecto.

## 2. Archivos y ubicación
- Añadir la unidad `UBST_Communities.pas` a `src/` (o carpeta de unidades).
- Incluir la unidad en los **uses** de los formularios Root/Usuario que correspondan.
- Carpeta de reportes: `Root-Reportes/` (se crea automáticamente si no existe).

## 3. Integración rápida
1. `CBST_Init(CommRoot)` en `FormCreate` del Root.
2. Enlazar botones:
   - **Crear Comunidad** → `CBST_Insert(...)`
   - **Ver mensajes** → `CBST_Find` + `Msg_ForEach`
   - **Exportar Comunidades** → `CBST_ToDot` + proceso `dot -Tpng`
3. En el formulario de Usuario, publicar con `CBST_PublishMsg`.

## 4. Validación posterior
- Crear comunidad **demo** y confirmar en UI.
- Publicar 1–2 mensajes y validar el contador.
- Exportar reporte y revisar `Comunidades.png`.

## 5. Troubleshooting
- **No compila**: verifique la ruta de `UBST_Communities.pas` y el `uses`.
- **No genera PNG**: instale Graphviz o verifique PATH.
- **Permisos**: ejecute con permisos suficientes para crear `Root-Reportes/`.

## 6. Desinstalación (opcional)
- Remover referencias a `UBST_Communities` de los formularios.
- Eliminar `UBST_Communities.pas` y la carpeta `Root-Reportes/` si ya no se usa.
