  n                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Arial      Arial      Arial      Arial      nroremision      Arial      cliente      Arial      ruc      Arial      direccionPartida      Arial      	direccion      Arial      fechainiciotraslado      "@D"      Arial      fechafintraslado      "@D"      Arial      
marcavehic      Arial      nrochapa      Arial      razsocialtransp      Arial      ructransportista      Arial      nombreconduct      Arial      ciconductor      Arial      direccionconductor      Arial      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      'XX'      Arial      IdComprobante="001"      otrosmotivos      Arial      IdComprobante='013'      transform(cantidad,'9999.99')      Arial      Producto      Arial      dataenvironment      aTop = 173
Left = 115
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     jPROCEDURE Init
DO seteo
_ASCIICOLS = 145
_asciirows = 45
TEXT TO cmdSQL noshow
SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, 
                      det.Imprime, c.Direccion, c.Telefono, c.Ruc, isnull(RTRIM(det.Descripcion),'')+'-'+isnull(RTRIM(p.descripcion1),'') AS Producto, det.Iva, det.real, r.NroRemision,
                      r.IdComprobante, r.fechainiciotraslado, r.fechafintraslado, r.marcavehic,r.nrochapa,r.razsocialtransp,
                      r.ructransportista,r.nombreconduct,r.ciconductor,r.direccionconductor,r.direccionpartida,r.otrosMotivos
FROM         os_remision AS r INNER JOIN
                      st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN
					  st_producto AS p ON det.idproducto = p.idproducto and det.idempresa=p.idempresa INNER JOIN 
                      vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa 
                      where r.IdRemision = ?m.IdRemision  
ENDTEXT
sql(cmdSQL,'cFactura')

SELECT cFactura
ENDPROC
     .���                              |�   %   �      �     �          �  U  L �  � T�>����� T�?��-��	 M(� ��� �� SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, �� ��                       det.Imprime, c.Direccion, c.Telefono, c.Ruc, isnull(RTRIM(det.Descripcion),'')+'-'+isnull(RTRIM(p.descripcion1),'') AS Producto, det.Iva, det.real, r.NroRemision,�� �|                       r.IdComprobante, r.fechainiciotraslado, r.fechafintraslado, r.marcavehic,r.nrochapa,r.razsocialtransp,�� �}                       r.ructransportista,r.nombreconduct,r.ciconductor,r.direccionconductor,r.direccionpartida,r.otrosMotivos�. �( FROM         os_remision AS r INNER JOIN�` �Z                       st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN�h �b 					  st_producto AS p ON det.idproducto = p.idproducto and det.idempresa=p.idempresa INNER JOIN �h �b                       vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa �@ �:                       where r.IdRemision = ?m.IdRemision  � � ��C � � cFactura� �� F� � U  SETEO CMDSQL SQL CFACTURA Init,     ��1 q � � � �	�!1���A �r 1                       _      )                     