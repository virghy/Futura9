  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\venus\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      s  :   winspool  \\venus\HP DeskJet 840C/841C/842C/843C  USB001                  ng Writer Port:                       \\venus\HP DeskJet 840C/841C/8   � pC�  �
od   ,  ,  Letter                                                                          DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin UPPER RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize LETTER Resolution r300x300 PM PlainNormalColor MediaType STANDARD Photo1200Mode Off ColorMode Color24 PQ Normal HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                      $   �$               $   �$              Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Times New Roman      Arial      Arial      Arial      numero      Times New Roman      
"ALUMINIO"      Arial      "N� DE PEDIDO"      Arial      "SAN LORENZO"      Arial      O"..\..\..\users\vgonzalez\documents\empresas\aluminio san lorenzo\logo-asl.jpg"      Fecha      "@ZYS"      Arial      "Fecha de Pedido"      Arial      �"Av. Fdo. de Pinedo e/ Ntra. Sra. de la Asunci�n
Telef.: (595 21) 582-116 / 570-864/0981-534-822
e-mail: aluminiosanlorenzo@gmail.com
pedro.irala@hotmail.com

www.aluminio-sanlorenzo.com

San Lorenzo - Paraguay"      "@I"      Arial      "RUC: 921671-5"      Arial      " ORDEN DE COMPRA"      Arial      " PROVEEDOR"      Arial      "ENVIAR"      Arial      	proveedor      Arial      DireccionEnvio      Arial      	"Nombre:"      Arial      "Direccion:"      Arial      CiudadEnvio      Arial      	direccion      Arial      "Direccion:"      Arial      "Telefono:"      Arial      TelefonoEnvio      Arial      telefono      Arial      fax      Arial      "Telefono:"      Arial      "Fax:"      Arial      Q"Importante: Favor adjuntar esta Orden de Compra a la Nota de Remisi�n o Factura"      Arial      "
"      Arial      "Cantidad
"      "@I"      Arial      "U.M.
"      "@I"      Arial      "C�digo/Articulo
"      "@I"      Arial      "Descripcion
"      "@I"      Arial      "Precio Unitario
"      "@I"      Arial      	"TOTAL
"      "@I"      Arial      cantidad      "999,999.99"      Arial      Unidad      Arial      
IdProducto      Arial      producto      Arial      precio      "999,999,999.99"      Arial      precio * Cantidad      "999,999,999.99"      Arial      ("Detalles de Recepcion en Obra/Deposito"      Arial      Exenta + Gravada      "999,999,999.99"      Arial      
"SubTotal"      Arial      "Expedici�n y Tramitaci�n
"      Arial      "1 - Recibido Por"      Arial      "2 - Recibido Por"      Arial      "Otros Servicios
"      Arial      "Firma"      Arial      "Firma"      Arial      IVA      "999,999,999.99"      Arial      "IVA"      Arial      "Fecha"      Arial      "Fecha"      Arial      Exenta+ Gravada      "999,999,999.99"      Arial      	"TOTAL
"      Arial      "Preparado por"      Arial      "Aprobado por"      Arial      "Presupuesto N�"      Arial      Usuario      Arial      Contacto      Arial      "Representante"      Arial      "Notas o Comentarios"      Arial      
referencia      Arial      #"Futura Software www.futura.com.py"      Arial      dataenvironment      aTop = 30
Left = -55
Width = 1016
Height = 634
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init


TEXT TO cmdSQL noshow
	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, 
				d.Descripcion as Producto, referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,
	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario,
	                      Unidad
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
     G���    .  .                        �8   %   �      �     �          �  U  .	 M(�  ��� �� 	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, �e �_ 				d.Descripcion as Producto, referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,�� �} 	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario,�# � 	                      Unidad�4 �. 	FROM         dbo.cp_orden_compra o INNER JOIN�Q �K 	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN�u �o 	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor�r �l 	                      INNER JOIN ST_PRODUCTO p on d.IdEmpresa = p.IdEmpresa and d.IdProducto = p.IdProducto�q �k 	                      left join usuarios u on o.Audit_Usuario = u.Employee_Id collate Modern_Spanish_CI_AS�; �5 	                      	where o.IdOrden = ?m.IdOrden	� � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO
  �  � U  SETEO Init,     �� BeforeOpenTablest    ��1 � �Q11AQ!�A �q 5 q 1                       O        v  ~      )   .                  