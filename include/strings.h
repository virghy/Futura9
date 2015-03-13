*-- (c) Microsoft Corporation 1996

*-- STRINGS.H
*-- Incluir este archivo donde sea necesario para
*-- la localización

*-- Nombres de barra de herramientas


#DEFINE TB_FORMDESIGNER_LOC  "Diseñador de formularios"
#DEFINE TB_STANDARD_LOC      "Standard"
#DEFINE TB_LAYOUT_LOC        "Distribución"
#DEFINE TB_QUERY_LOC		 "Diseñador de consultas"
#DEFINE TB_VIEWDESIGNER_LOC  "Diseñador de vistas"
#DEFINE TB_COLORPALETTE_LOC  "Paleta de colores"
#DEFINE TB_FORMCONTROLS_LOC  "Controles de formularios"
#DEFINE TB_DATADESIGNER_LOC  "Diseñador de bases de datos"
#DEFINE TB_REPODESIGNER_LOC  "Diseñador de informes"
#DEFINE TB_REPOCONTROLS_LOC  "Controles de informes"
#DEFINE TB_PRINTPREVIEW_LOC  "Vista preliminar"
#DEFINE WIN_COMMAND_LOC		 "Comandos"			&& Ventana Comandos


*-- Mensajes de cuadro de mensajes
#DEFINE FILENOTEXIST_LOC	"El archivo no existe: "
#DEFINE BADPASSWORD_LOC		"La contraseña no es válida.(Consulte con el  Administrador de Sistemas)"
#DEFINE BADNAME_LOC			"No se ha encontrado el nombre."
#DEFINE BADUPDATE_LOC		"No se puede actualizar - revirtiendo al original."
#DEFINE SAVECHANGES_LOC		"¿Desea guardar primero sus cambios?"
#DEFINE ASKDELETE_LOC		"¿Realmente desea eliminar esta información?"
#DEFINE TAGNOTFOUND_LOC		"La etiqueta de índice no se ha encontrado."
#DEFINE REPORTNOTFOUND_LOC	"El archivo de informe no se ha encontrado."
#DEFINE PRINTERNOTREADY_LOC	"La impresora no está preparada."
#DEFINE NORECSMATCHED_LOC	"Ningún registro cumple los criterios."
#DEFINE DELETEREC_LOC		"¿Realmente desea eliminar este registro?"
#DEFINE DELETEWARN_LOC      "Eliminar advertencia"
#DEFINE AVAILABLECREDIT_LOC	"Crédito disponible"
#DEFINE NOTYET_LOC          "En construcción"
#DEFINE CUSTIDEXISTS_LOC    "El ID de cliente ya existe. Escriba otro distinto."
#DEFINE NOLASTORDER_LOC		"El cliente no tiene ningún pedido previo."
#DEFINE CUSTFIRSTORDER_LOC  "El último pedido del cliente es el actual."
#DEFINE TODAYORLATER_LOC    "La fecha debe ser hoy o posterior."
#DEFINE DATERANGEERROR_LOC  "La fecha 'Hasta' no puede ser anterior a la fecha 'Desde'."
#DEFINE ADDCUSTOMER_LOC		"¿Desea agregar este cliente al archivo maestro de clientes?"
#DEFINE NOTHINGTOPRINT_LOC  "No hay nada que imprimir."
#DEFINE PASSWORDEMPTY_LOC	"La nueva contraseña no puede estar vacía."
#DEFINE PSWDNOTCNFRM_LOC    "No se puede confirmar la nueva contraseña. Inténtelo de nuevo."
#DEFINE NOPSWDENTERED_LOC	"Todavía no ha introducido la contraseña anterior. ¿Desea continuar?"
#DEFINE FILESAVEDAS_LOC		"El archivo se ha guardado como "
#DEFINE ORDHASITEMS_LOC		"Un pedido debe tener al menos un artículo."
#DEFINE CUSTOVERMAX_LOC		"El cliente ha superado el importe máximo de pedido en "
#DEFINE CUSTUNDERMIN_LOC    "El total de pedidos del cliente debe ser al menos "
#DEFINE SAVEANYWAY_LOC      "¿Desea guardar de todas maneras?"
#DEFINE VALIDATING_LOC      "Validando ..."
#DEFINE PRINTING_LOC        "Imprimiendo ..."
#DEFINE VIEWCODEPRINT_LOC	"Este informe puede ser muy largo. ¿Desea continuar?"
#DEFINE VIEWCSDTYPRINT_LOC  "Este informe puede ser muy largo. ¿Desea continuar?"
#DEFINE METHOD_LOC          "Método: "
#DEFINE LINENUM_LOC			"Línea: "
#DEFINE CUSTNOORD_LOC		"El cliente no tiene ningún pedido."
#DEFINE SELCUSTFIRST_LOC    "Debe seleccionar primero un cliente."
#DEFINE CANNOTQUIT_LOC      "Realmente desea salir del sistema ?"
#DEFINE ADDNEWREC_LOC		"Este era el último registro. ¿Desea agregar uno nuevo?"
#DEFINE ENTERADDMODE_LOC    "No hay ningún registro en el archivo. Pasará al modo 'Agregar'."
#DEFINE NOEMPLOYEES_LOC		"No hay ningún empleado en el archivo."
#DEFINE INSEMPLOYEE_LOC		"Todos los empleados deben estar asignados a un grupo."
#DEFINE INSPRODUCT_LOC      "A todos los productos se les debe asignar un proveedor y una categoría."
#DEFINE INSORDER_LOC        "Todos los pedidos deben tener un cliente y una compañía de envío. (Información de entrega)"
#DEFINE TABLERULEFAIL_LOC	"Falló la regla de la tabla."
#DEFINE ITEMNOTSAVED_LOC	"Los artículos marcados no se han agregado al pedido. ¿Desea descartar los artículos marcados?"
#DEFINE CLASSBROWERR_LOC	"Esta clase no se puede utilizar fuera de la aplicación Tastrade."
*-- Mensajes de error de desencadenantes
#DEFINE INSERTTRIGFAIL_LOC  "Falló el desencadenante Insert."
#DEFINE UPDATETRIGFAIL_LOC  "Falló el desencadenante Update."
#DEFINE DELETETRIGFAIL_LOC  "Falló el desencadenante Delete."
#DEFINE DELCATEGORY_LOC		"Los productos pertenecen a esta categoría. No se puede eliminar."
#DEFINE DELCUSTOMER_LOC		"El cliente tiene pedidos. No se puede eliminar."
#DEFINE DELEMPLOYEE_LOC		"El empleado existe en los pedidos. No se puede eliminar."
#DEFINE DELPRODUCT_LOC      "El producto existe en los artículos de pedidos. No se puede eliminar."
#DEFINE DELSUPPLIER_LOC		"Los productos los suministra este proveedor. No se puede eliminar."
#DEFINE DELSHIPPER_LOC      "La compañía de envío existe en los pedidos. No se puede eliminar."


*-- Otras cadenas
#DEFINE ADDPICTURE_LOC      "Agregar imagen"
#DEFINE CHANGEPICTURE_LOC	"Cambiar imagen"
#DEFINE SELECTBUTTON_LOC    "Seleccionar"
#DEFINE VERSION_LOC			"2.0"
#DEFINE COPYRIGHT_LOC		"Copyright © 1999-2006 - Virginio González"
#DEFINE RIGHTSRSRVD_LOC		"Reservados todos los derechos"+CHR(13)+"Asistencia Técnica : 0981-426145"+chr(13)+ "E-Mail : vgonzalez@futura.com.py" +chr(13)+ "www.futura.com.py" 
#DEFINE ADDITEM_LOC			"Agregar artículo"
#DEFINE REGISTRO_LOC      	"ARIANNA"
#DEFINE INFSAVED_LOC        "Se ha guardado la información."
#DEFINE REINDEXING_LOC      "Reindexando: "
#DEFINE DONE_LOC            "Terminado"
#DEFINE LOWERNOTFOUND_LOC	" no se encuentra."
#DEFINE ABOUT_LOC			"Acerca de "
#DEFINE VERSIONLABEL_LOC    "Versión "
#DEFINE LOWERFOR_LOC        " para "
#DEFINE REGISTRO_CLAVE      "REGISTRO.CLAVE"



