  Z                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
      R  :  winspool  \\venus\HP DeskJet 840C/841C/842C/843C  USB001                       \\venus\HP DeskJet 840C/841C/8   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin UPPER RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize LETTER Resolution r300x300 PM PlainNormalColor MediaType STANDARD Photo1200Mode Off ColorMode Color24 PQ Normal HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                      $   �$               $   �$         Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Times New Roman      Arial      numero      Times New Roman      "CASA MATRIZ"      Arial      "N� DE PEDIDO"      Arial      "..\bitmaps\header.jpg"      O"25 de Mayo N� 2823 e/Rod� y 33 Orientales
Tel. 228 251/252/260 - Fax 204 028"      Arial      $"SERVICIOS GENERALES DE COMPUTACION"      Arial      "SUCURSAL HERNANDARIAS"      Arial      ;"Avda. Paraguay esq. Costa Rica - Telefax (595 631) 23 498"      Arial      " ORDEN DE COMPRA"      Arial      " PROVEEDOR"      Arial      "ENVIAR"      Arial      	proveedor      Arial      	"Nombre:"      Arial      DireccionEnvio      Arial      "Direccion:"      Arial      	direccion      Arial      "Direccion:"      Arial      CiudadEnvio      Arial      	"Ciudad:"      Arial      	"Ciudad:"      Arial      	"Estado:"      Arial      "CP:"      Arial      telefono      Arial      TelefonoEnvio      Arial      "Telefono:"      Arial      fax      Arial      "Telefono:"      Arial      "Fax:"      Arial      "Cantidad
"      "@I"      Arial      "C�digo/Articulo
"      "@I"      Arial      "Pago
"      "@I"      Arial      "Descripcion
"      "@I"      Arial      "Precio Unitario
"      "@I"      Arial      	"TOTAL
"      "@I"      Arial      "
"      Arial      cantidad      	"999,999"             Arial      
IdProducto      Arial      producto      Arial      precio      "999,999,999.99"      Arial      precio * Cantidad      "999,999,999.99"      Arial      "Detalles de Pago"      Arial      Exenta + Gravada      "999,999,999.99"      Arial      
"SubTotal"      Arial      "1 - Contado Gs. - TC"      Arial      "Expedici�n y Tramitaci�n
"      Arial      "2 - Contado US$"      Arial      "Otros Servicios
"      Arial      "3 - Cuenta N�m."      Arial      IVA      "999,999,999.99"      Arial      "IVA"      Arial      "4 - Cr�dito"      Arial      Exenta+ Gravada      "999,999,999.99"      Arial      	"TOTAL
"      Arial      "Cheque c/o Banco"      Arial      "Fecha Presentaci�n"      Arial      "Fecha de Env�o"      Arial      Fecha      "@ZYS"      Arial      "N�mero Cheque"      Arial      "Aprobado por"      Arial      "Fecha"      Arial      "Presupuesto N�"      Arial      Usuario      Arial      Contacto      Arial      "Enviar por"      Arial      "Representante"      Arial      "Notas o Comentarios"      Arial      
referencia      Arial      #"Futura Software www.futura.com.py"      Arial      dataenvironment      aTop = 30
Left = -55
Width = 1016
Height = 634
DataSource = .NULL.
Name = "Dataenvironment"
     iPROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


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
     !���                              ^   %   e      �     �          �  U  
  �  � U  SETEO
	 M(�  ��� �� 	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, �e �_ 				d.Descripcion as Producto, referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,�� �| 	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario�4 �. 	FROM         dbo.cp_orden_compra o INNER JOIN�Q �K 	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN�u �o 	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor�r �l 	                      INNER JOIN ST_PRODUCTO p on d.IdEmpresa = p.IdEmpresa and d.IdProducto = p.IdProducto�q �k 	                      left join usuarios u on o.Audit_Usuario = u.Employee_Id collate Modern_Spanish_CI_AS�; �5 	                      	where o.IdOrden = ?m.IdOrden	� � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO BeforeOpenTables,     �� InitA     ��1 q 2 � �Q!AQ!�A �q 4                       $         ?   ^      )                     