  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                       `\\futura5\HP DeskJet 840C/841C   � XC� 	 �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                                                                                                     Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Orden de Compra"      Arial      oApp.Nombreempresa      Arial      	proveedor      Arial      fecha      Arial      "Proveedor:"      Arial      	"Fecha
"      "@I"      Arial      direccion,telefono,fax      Arial      IdORden      Arial      "Direccion:"      Arial      	"N�mero:"      Arial      "Producto
"      "@I"      Arial      "Cantidad
"      "@I"      Arial      
"Precio
"      "@I"      Arial      "Importe
"      "@I"      Arial      "
"      Arial      producto             Arial      cantidad      	"999,999"             Arial      precio      "999,999,999.99"      Arial      precio * Cantidad      "999,999,999.99"      Arial      Exenta + Gravada      "99,999,999,999.99"      Arial      "SubTotal:
"      Arial      IVA      "99,999,999,999.99"      Arial      "IVA:
"      Arial      Exenta+ Gravada + IVA      "99,999,999,999.99"      Arial      
"Total:
"      Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


TEXT TO cmdSQL noshow
	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, d.IdProducto, d.Cantidad, d.Precio, 
				d.Descripcion as Producto, 
	                      o.IdOrden, prov.Direccion, prov.Telefono, prov.Fax
	FROM         dbo.cp_orden_compra o INNER JOIN
	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN
	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor
	                      	where o.IdOrden = ?m.IdOrden	
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
     }���    d  d                           %   �           �          �  U  
  �  � U  SETEOj	 M(�  ��� �� 	SELECT     o.Fec_Ord AS Fecha, o.Exenta, o.Gravada, o.Iva, prov.IdProveedor + prov.Razon AS Proveedor, d.IdProducto, d.Cantidad, d.Precio, �% � 				d.Descripcion as Producto, �O �I 	                      o.IdOrden, prov.Direccion, prov.Telefono, prov.Fax�4 �. 	FROM         dbo.cp_orden_compra o INNER JOIN�Q �K 	                      dbo.cp_det_ord d ON o.IdOrden = d.IdOrden INNER JOIN�u �o 	                      dbo.cp_proveedor prov ON o.IdEmpresa = prov.IdEmpresa AND o.Proveedor = prov.IdProveedor�; �5 	                      	where o.IdOrden = ?m.IdOrden	� � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO BeforeOpenTables,     �� InitA     ��1 q 2 � !	Q�AQ�A �q 4                       $         ?   �      )   d                  