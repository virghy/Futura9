  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\venus\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      s  :  winspool  \\venus\HP DeskJet 840C/841C/842C/843C  USB001                  ng Writer Port:                       \\venus\HP DeskJet 840C/841C/8   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoMono MediaType STANDARD Photo1200Mode Off ColorMode Mono PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                           $   �$               $   �$              Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Times New Roman      Arial      Arial      Arial      numero      Times New Roman      "N� DE PEDIDO"      Arial      "G C A   S. A."      Arial      "..\bitmaps\logogca.bmp"      v"Aparipy 1.444 c/ Caaguaz� 
Telefax: (595-21) 552 - 207  
E-mail: ing_gcasa@cmm.com.py
      Asunci�n - Paraguay
"      "@I"      Arial      Fecha      "@ZYS"      Arial      "Fecha de Pedido"      Arial      !"Servicio de Ingenier�a Integral"      Arial      "RUC: 80027085-1 "      Arial      " ORDEN DE COMPRA"      Arial      " PROVEEDOR"      Arial      "ENVIAR"      Arial      	proveedor      Arial      DireccionEnvio      Arial      	"Nombre:"      Arial      "Direccion:"      Arial      	direccion      Arial      CiudadEnvio      Arial      "Direccion:"      Arial      	"Ciudad:"      Arial      	"Ciudad:"      Arial      	"Estado:"      Arial      "CP:"      Arial      TelefonoEnvio      Arial      "Telefono:"      Arial      telefono      Arial      "Telefono:"      Arial      fax      Arial      "Fax:"      Arial      Q"Importante: Favor adjuntar esta Orden de Compra a la Nota de Remisi�n o Factura"      Arial      "Cantidad
"      "@I"      Arial      "C�digo/Articulo
"      "@I"      Arial      "Descripcion
"      "@I"      Arial      "Precio Unitario
"      "@I"      Arial      	"TOTAL
"      "@I"      Arial      "
"      Arial      cantidad      	"999,999"             Arial      
IdProducto      Arial      producto      Arial      precio      "999,999,999.99"      Arial      precio * Cantidad      "999,999,999.99"      Arial      "Detalles de Pago"      Arial      Exenta + Gravada      "999,999,999.99"      Arial      "Detalles de Recepcion"      Arial      
"SubTotal"      Arial      "1 - Contado Gs. - TC"      Arial      "Expedici�n y Tramitaci�n
"      Arial      "2 - Contado US$"      Arial      "1 - Recibido Por"      Arial      "2 - Recibido Por"      Arial      "Otros Servicios
"      Arial      "3 - Cuenta N�m."      Arial      IVA      "999,999,999.99"      Arial      "IVA"      Arial      "4 - Cr�dito"      Arial      "Firma"      Arial      "Firma"      Arial      Exenta+ Gravada      "999,999,999.99"      Arial      "Cheque c/o Banco"      Arial      	"TOTAL
"      Arial      "Fecha Presentaci�n"      Arial      "N�mero Cheque"      Arial      "Fecha"      Arial      "Fecha"      Arial      "Aprobado por"      Arial      "Fecha"      Arial      "Presupuesto N�"      Arial      Usuario      Arial      Contacto      Arial      "Enviar por"      Arial      "Representante"      Arial      "Notas o Comentarios"      Arial      
referencia      Arial      #"Futura Software www.futura.com.py"      Arial      dataenvironment      aTop = 30
Left = -55
Width = 1016
Height = 634
DataSource = .NULL.
Name = "Dataenvironment"
     iPROCEDURE Init


TEXT TO cmdSQL noshow
	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, 
				d.Descripcion as Producto, referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,
	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario
	FROM         dbo.cp_orden_compra o INNER JOIN
	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN
	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor
	                      INNER JOIN ST_PRODUCTO p on d.IdEmpresa = p.IdEmpresa and d.IdProducto = p.IdProducto
	                      left join usuarios u on o.Audit_Usuario = u.Employee_Id collate Modern_Spanish_CI_AS
	                      	where o.IdOrden = ?m.IdOrden	
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     !���                              ^   %   e      �     �          �  U  
	 M(�  ��� �� 	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, �e �_ 				d.Descripcion as Producto, referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,�� �| 	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario�4 �. 	FROM         dbo.cp_orden_compra o INNER JOIN�Q �K 	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN�u �o 	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor�r �l 	                      INNER JOIN ST_PRODUCTO p on d.IdEmpresa = p.IdEmpresa and d.IdProducto = p.IdProducto�q �k 	                      left join usuarios u on o.Audit_Usuario = u.Employee_Id collate Modern_Spanish_CI_AS�; �5 	                      	where o.IdOrden = ?m.IdOrden	� � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO
  �  � U  SETEO Init,     �� BeforeOpenTablesP    ��1 � �Q!AQ!�A �q 5 q 1                       /        V  ^      )                     