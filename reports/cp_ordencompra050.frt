  g                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=1
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "..\bitmaps\logo-promec.jpg"      numero      
"@L 99999"      Arial      " ORDEN DE COMPRA No."      Arial      " PROVEEDOR"      Arial      "ENVIAR"      Arial      	proveedor      Arial      	"Nombre:"      Arial      DireccionEnvio      Arial      "Direccion:"      Arial      	direccion      Arial      "Direccion:"      Arial      CiudadEnvio      Arial      	"Ciudad:"      Arial      	"Ciudad:"      Arial      	"Estado:"      Arial      "CP:"      Arial      telefono      Arial      TelefonoEnvio      Arial      "Telefono:"      Arial      fax      Arial      "Telefono:"      Arial      "Fax:"      Arial      "Cantidad
"      "@I"      Arial      "C�digo/Articulo
"      "@I"      Arial      "Pago
"      "@I"      Arial      "Descripcion
"      "@I"      Arial      "Precio Unitario
"      "@I"      Arial      	"TOTAL
"      "@I"      Arial      "
"      Arial      cantidad      	"999,999"             Arial      
IdProducto      Arial      producto      Arial      precio      "999,999,999.99"      Arial      precio * Cantidad      "999,999,999.99"      Arial      descripcion1      Arial      "Detalles de Pago"      Arial      Exenta + Gravada      "999,999,999.99"      Arial      
"SubTotal"      Arial      "1 - Contado Gs. - TC"      Arial      "Expedici�n y Tramitaci�n
"      Arial      "2 - Contado US$"      Arial      "Otros Servicios
"      Arial      "3 - Cuenta N�m."      Arial      IVA      "999,999,999.99"      Arial      "IVA"      Arial      Fecha      "@ZYS"      Arial      "4 - Cr�dito"      Arial      "Fecha de Env�o"      Arial      Exenta+ Gravada      "999,999,999.99"      Arial      "TOTAL: ",IdMoneda      Arial      "Cheque c/o Banco"      Arial      "Fecha Presentaci�n"      Arial      "Monto de la Venta Aceptada"      Arial      "N�mero Cheque"      Arial      "Preparado por"      Arial      "Autorizado por"      Arial      "Fecha"      Arial      "Presupuesto N�"      Arial      Usuario      Arial      Contacto      Arial      "Enviar por"      Arial      "Representante"      Arial      "Notas o Comentarios"      Arial      
referencia      Arial      #"Futura Software www.futura.com.py"      Arial      dataenvironment      aTop = 30
Left = -55
Width = 1016
Height = 634
DataSource = .NULL.
Name = "Dataenvironment"
     hPROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


TEXT TO cmdSQL noshow
	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, 
				d.Descripcion as Producto, d.Descripcion1,referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,
	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario,o.IDMoneda
	FROM         dbo.cp_orden_compra o INNER JOIN
	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN
	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor
	                      INNER JOIN ST_PRODUCTO p on d.IdEmpresa = p.IdEmpresa and d.IdProducto = p.IdProducto
	                      left join usuarios u on o.Audit_Usuario = u.Employee_Id collate Modern_Spanish_CI_AS
	                      	where o.IdOrden = ?m.IdOrden	
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

ENDPROC
     ;���    "  "                        j�   %         �     �          �  U  
  �  � U  SETEO$	 M(�  ��� �� 	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, case when ISNULL(p.Catalogo,'')='' then d.IdProducto else p.Catalogo end as IdProducto, d.Cantidad, d.Precio, �t �n 				d.Descripcion as Producto, d.Descripcion1,referencia, o.Contacto,DireccionEnvio,CiudadEnvio,TelefonoEnvio,�� �� 	                      o.Numero, prov.Direccion, prov.Telefono, prov.Fax, RTRIM(u.First_Name) + ' ' + u.Last_Name as Usuario,o.IDMoneda�4 �. 	FROM         dbo.cp_orden_compra o INNER JOIN�Q �K 	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN�u �o 	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor�r �l 	                      INNER JOIN ST_PRODUCTO p on d.IdEmpresa = p.IdEmpresa and d.IdProducto = p.IdProducto�q �k 	                      left join usuarios u on o.Audit_Usuario = u.Employee_Id collate Modern_Spanish_CI_AS�; �5 	                      	where o.IdOrden = ?m.IdOrden	� � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO BeforeOpenTables,     �� InitA     ��1 q 2 � �A�AQ!�A �q 2                       $         ?   ]      )   "                  