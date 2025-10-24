# Manual de Administrador (Root) — Módulo Comunidades

## 1. Responsabilidades del rol Root
- **Alta de comunidades** (crear nodos del BST).
- **Supervisión**: revisar mensajes por comunidad.
- **Exportes/Reportes**: generar DOT/PNG del árbol de comunidades.
- **Mantenimiento** (opcional): respaldo/limpieza cuando lo defina el curso/proyecto.

## 2. Operaciones desde la interfaz Root
### 2.1 Crear Comunidad
- Botón **“Crear Comunidad”** → captura de **nombre** → confirmación.
- Rechaza: nombre vacío, duplicado (case-insensitive).

### 2.2 Ver mensajes por comunidad
- Botón **“Ver mensajes de comunidad”** → ingresar nombre → listado (correo, fecha, texto).

### 2.3 Exportar Comunidades (Reporte)
- Botón **“Exportar Comunidades”** → genera:
  - `Root-Reportes/Comunidades.dot` (texto DOT).
  - `Root-Reportes/Comunidades.png` (si `dot` de Graphviz está disponible).
- Formato del nodo: **nombre, fecha de creación y # de mensajes**.

### 2.4 Estructura de carpetas recomendada
```
/EDDMail/
  /Root-Reportes/
     Comunidades.dot
     Comunidades.png
  /bin/
  /src/
```
> Asegure que **Root-Reportes/** exista; si no, el sistema lo crea automáticamente.

## 3. Buenas prácticas
- Definir un **nomenclador** de comunidades (p. ej., `general`, `soporte`, `anuncios`).
- **Evitar duplicados**: use el buscador al crear.
- Generar **reportes** después de lotes de cambios para documentar el estado del árbol.

## 4. Troubleshooting (rápido)
- **No genera PNG**: verifique que **Graphviz** esté instalado y accesible por `dot` en PATH.
- **No aparece la comunidad**: puede haberse creado con mayúsculas/minúsculas distintas; la búsqueda es **case-insensitive** pero valide el nombre ingresado.
- **Contador de mensajes incorrecto**: si hubo manipulación manual, regenere y pruebe publicando un mensaje de control.

## 5. Seguridad y control
- No se permiten **nombres vacíos** o **duplicados**.
- El cargado masivo de comunidades no está previsto en esta versión (se sugiere script controlado).

## 6. Checklist para entrega
- [ ] Botón **Crear Comunidad** operativo.
- [ ] Botón **Ver mensajes** operativo.
- [ ] Botón **Exportar Comunidades** genera DOT y PNG.
- [ ] Carpeta **Root-Reportes/** con archivos generados.
- [ ] Pruebas de **publicación** desde un Usuario estándar completadas.
