                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   �DRIVER=winspool
DEVICE=TOSHIBA e-STUDIO451c PS3
OUTPUT=192.168.0.17
ORIENTATION=1
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=1
DUPLEX=1
YRESOLUTION=600
TTOPTION=2
COLLATE=0
      J  ,  winspool  TOSHIBA e-STUDIO451c PS3  192.168.0.17                       �TOSHIBA e-STUDIO451c PS3         � S�� 	 �
od   X  X   Letter                                                                            PRIV�0                                                                                       '''  '        @                                  \K hC                             x�d�      �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @  SMTJ     0T O S H I B A   e - S T U D I O 4 5 1 c   P S 3   Resolution 600dpi PageSize Letter PageRegion  InputSlot Auto MediaType Plain Duplex None Collate True OutputBin Bin2 Stapling Off HolePunch Off PrintMode Normal DINDigit1 0 DINDigit2 0 DINDigit3 0 DINDigit4 0 DINDigit5 0 DeptCode False DCDigit1 0 DCDigit2 0 DCDigit3 0 DCDigit4 0 DCDigit5 0 ColorResType ColorLowGeneral DistinguishThinLines True BlackOverPrint True PureBlackGray BlackGrayAuto TonerSave False BlankPage False Smoothing True                                                                            Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Times New Roman      Arial      numero      Times New Roman      "CASA MATRIZ"      Arial      "N� DE PRESUPUESTO"      Arial      "..\bitmaps\header.jpg"      O"25 de Mayo N� 2823 e/Rod� y 33 Orientales
Tel. 228 251/252/260 - Fax 204 028"      Arial      $"SERVICIOS GENERALES DE COMPUTACION"      Arial      "SUCURSAL HERNANDARIAS"      Arial      ;"Avda. Paraguay esq. Costa Rica - Telefax (595 631) 23 498"      Arial      "PRESUPUESTO"      Arial      
"PACIENTE"      Arial      "ENVIAR"      Arial      	proveedor      Arial      	"Nombre:"      Arial      DireccionEnvio      Arial      "Direccion:"      Arial      	direccion      Arial      "Direccion:"      Arial      CiudadEnvio      Arial      	"Ciudad:"      Arial      	"Ciudad:"      Arial      	"Estado:"      Arial      "CP:"      Arial      telefono      Arial      TelefonoEnvio      Arial      "Telefono:"      Arial      fax      Arial      "Telefono:"      Arial      "Fax:"      Arial      "Cantidad
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