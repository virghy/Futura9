  g                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=1
      Arial      Arial      Arial      Arial      nroremision      Arial      cliente      Arial      ruc      Arial      direccionPartida      Arial      	direccion      Arial      fechainiciotraslado      "@D"      Arial      fechafintraslado      "@D"      Arial      
marcavehic      Arial      nrochapa      Arial      razsocialtransp      Arial      ructransportista      Arial      nombreconduct      Arial      ciconductor      Arial      direccionconductor      Arial      'X'      Arial      IdComprobante="SV"      'X'      Arial      IdComprobante="IM"      'X'      Arial      IdComprobante='TR'      'X'      Arial      IdComprobante="EM"      'X'      Arial      IdComprobante="EX"      'X'      Arial      IdComprobante='CN'      'X'      Arial      IdComprobante="TF"      'X'      Arial      IdComprobante="EB"      'X'      Arial      IdComprobante='EC'      'X'      Arial      IdComprobante='DV'      'X'      Arial      IdComprobante="RP"      'X'      Arial      IdComprobante="FE"      otrosmotivos      Arial      IdComprobante='013'      transform(cantidad,'9999.99')      Arial      Serie      Arial      Producto,' - ',IdProducto      Arial      dataenvironment      aTop = 173
Left = 115
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo
_ASCIICOLS = 145
_asciirows = 45
TEXT TO cmdSQL noshow
SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, 
                      c.Telefono, c.Ruc, ISNULL(RTRIM(det.Descripcion), '') AS Producto, '(SN ' + dbo.St_SeriesDescripcion(det.IdDetalle) + ')' AS Serie, det.Iva, det.real, r.NroRemision, 
                      r.idcomprobante, r.FechaInicioTraslado, r.FechaFinTraslado, r.direccionpartida, r.OtrosMotivos, f.marca AS marcavehic, f.rua AS nrochapa, RTRIM(trP.Nombre) 
                      + ' ' + ISNULL(trP.Apellido, '') AS razsocialtransp, trP.NroDoc AS ructransportista, RTRIM(chP.Nombre) + ' ' + ISNULL(chP.Apellido, '') AS nombreconduct, 
                      RTRIM(chP.Direccion1) + ' ' + ISNULL(chP.Direccion2, '') AS direccionconductor, chP.NroDoc AS ciconductor, r.idempresa
FROM         os_remision AS r INNER JOIN
                      st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN
                      st_Producto AS p ON det.IdProducto = p.IdProducto AND det.IdEmpresa = p.IdEmpresa INNER JOIN
                      vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa LEFT OUTER JOIN
                      BS_Personas AS trP INNER JOIN
                      VT_Repartidor AS tr ON trP.IdPersona = tr.IdPersona ON r.idempresa = tr.IdEmpresa AND r.NombreConduct = tr.IdRepartidor LEFT OUTER JOIN
                      tr_flotas AS f ON r.MarcaVehic = f.idflota AND r.idempresa = f.idempresa LEFT OUTER JOIN
                      BS_Personas AS chP INNER JOIN
                      VT_Repartidor AS ch ON chP.IdPersona = ch.IdPersona ON r.idempresa = ch.IdEmpresa AND r.NombreConduct = ch.IdRepartidor
                      where r.IdRemision = ?m.IdRemision  
ENDTEXT
sql(cmdSQL,'cFactura')

SELECT cFactura


*!*	SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, 
*!*	                      det.Imprime, c.Direccion, c.Telefono, c.Ruc, isnull(RTRIM(det.Descripcion),'') AS Producto, '(SN '+ dbo.St_SeriesDescripcion(det.IdDetalle)+')' as Serie, det.Iva, det.real, r.NroRemision,
*!*	                      r.IdComprobante, r.fechainiciotraslado, r.fechafintraslado, r.marcavehic,r.nrochapa,r.razsocialtransp,
*!*	                      r.ructransportista,r.nombreconduct,r.ciconductor,r.direccionconductor,r.direccionpartida,r.otrosMotivos
*!*	FROM         os_remision AS r INNER JOIN
*!*	                      st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN
*!*						  st_producto AS p ON det.idproducto = p.idproducto and det.idempresa=p.idempresa INNER JOIN 
*!*	                      vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa 
*!*	                      where r.IdRemision = ?m.IdRemision  
*!*	ENDTEXT
ENDPROC
     ����    u  u                        i   %   �      ,     �          �  U  � �  � T�>����� T�?��-��	 M(� ��� �� SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, �� ��                       c.Telefono, c.Ruc, ISNULL(RTRIM(det.Descripcion), '') AS Producto, '(SN ' + dbo.St_SeriesDescripcion(det.IdDetalle) + ')' AS Serie, det.Iva, det.real, r.NroRemision, �� ��                       r.idcomprobante, r.FechaInicioTraslado, r.FechaFinTraslado, r.direccionpartida, r.OtrosMotivos, f.marca AS marcavehic, f.rua AS nrochapa, RTRIM(trP.Nombre) �� ��                       + ' ' + ISNULL(trP.Apellido, '') AS razsocialtransp, trP.NroDoc AS ructransportista, RTRIM(chP.Nombre) + ' ' + ISNULL(chP.Apellido, '') AS nombreconduct, �� ��                       RTRIM(chP.Direccion1) + ' ' + ISNULL(chP.Direccion2, '') AS direccionconductor, chP.NroDoc AS ciconductor, r.idempresa�. �( FROM         os_remision AS r INNER JOIN�` �Z                       st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN�x �r                       st_Producto AS p ON det.IdProducto = p.IdProducto AND det.IdEmpresa = p.IdEmpresa INNER JOIN�w �q                       vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa LEFT OUTER JOIN�9 �3                       BS_Personas AS trP INNER JOIN�� ��                       VT_Repartidor AS tr ON trP.IdPersona = tr.IdPersona ON r.idempresa = tr.IdEmpresa AND r.NombreConduct = tr.IdRepartidor LEFT OUTER JOIN�t �n                       tr_flotas AS f ON r.MarcaVehic = f.idflota AND r.idempresa = f.idempresa LEFT OUTER JOIN�9 �3                       BS_Personas AS chP INNER JOIN�� ��                       VT_Repartidor AS ch ON chP.IdPersona = ch.IdPersona ON r.idempresa = ch.IdEmpresa AND r.NombreConduct = ch.IdRepartidor�@ �:                       where r.IdRemision = ?m.IdRemision  � � ��C � � cFactura� �� F� � U  SETEO CMDSQL SQL CFACTURA Init,     ��1 q � � � a!�a!	��q�1
A�1	A �r =                       �      )   u                  