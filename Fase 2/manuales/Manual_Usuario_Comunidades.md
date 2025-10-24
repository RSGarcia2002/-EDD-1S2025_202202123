# Manual de Usuario — Módulo Comunidades (EDDMail)

## 1. ¿Qué es “Comunidades”?
Es un espacio tipo foro donde puedes **crear comunidades** (temas o grupos) y **publicar mensajes** (con correo, texto y fecha).

## 2. Funciones principales
- **Crear comunidad** *(solo Root)*.
- **Publicar mensaje** en una comunidad existente.
- **Ver mensajes de una comunidad** en orden del más reciente al más antiguo.
- **Exportar reporte** de comunidades *(solo Root)*.

## 3. Flujo de uso
1. **Entrar como Root** para crear las comunidades base (p. ej., *general*, *anuncios*).
2. **Entrar como Usuario** para publicar mensajes en las comunidades existentes.
3. **Consultar mensajes**: elige una comunidad y revisa el listado.

## 4. Pasos detallados
### 4.1 Crear una comunidad (Root)
1. Abrir el módulo Root.
2. Clic en **“Crear Comunidad”**.
3. Escribir el **nombre** y confirmar.
4. Verás el mensaje **“Comunidad creada.”**

> Si el nombre está vacío o **ya existe**, el sistema mostrará un aviso y no la creará.

### 4.2 Publicar un mensaje (Usuario)
1. Elige la **comunidad**.
2. Ingresa tu **correo** y redacta el **mensaje**.
3. Clic en **“Publicar”**.
4. Confirmación: **“Mensaje publicado.”**

> Si la comunidad **no existe**, se indicará **“No se pudo publicar: La comunidad no existe.”**

### 4.3 Ver mensajes de una comunidad
1. Clic en **“Ver mensajes de comunidad”**.
2. Escribe el **nombre de la comunidad**.
3. Se mostrará el encabezado con **fecha de creación** y **# de mensajes**, seguido de los mensajes más recientes primero.

### 4.4 Exportar reporte de Comunidades (Root)
1. Clic en **“Exportar Comunidades”**.
2. Se genera `Root-Reportes/Comunidades.dot` y, si está **Graphviz** instalado, `Root-Reportes/Comunidades.png`.
3. Revisa la imagen para validar el árbol (BST) de comunidades.

## 5. Consejos
- Usa nombres **claros** para las comunidades.
- Si no ves mensajes, verifica que estás buscando en la **comunidad correcta**.
- Pide al Root que cree la comunidad si aún no existe.

## 6. Preguntas frecuentes
- **¿Puedo renombrar una comunidad?** No en esta versión.
- **¿Se puede borrar un mensaje?** No desde la UI estándar; es una tarea de mantenimiento técnico.
