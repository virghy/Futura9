  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      lPrecio      Precio      0      importe0      Eiif(iva=0,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      importe5      Eiif(iva=5,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      	importe10      Fiif(iva=10,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      Total      Importe      0      Arial      Arial      Arial      numero      Arial      "San Lorenzo,", day(Fecha)      Arial      Nombremes(month(Fecha))      Arial      year(Fecha)      Arial      Cliente      Arial      RUC      Arial      	Direccion      Arial      telefono      Arial      
IdProducto      Arial      transform(cantidad,'9999.99')      Arial      
cantidad>0      Producto      Arial      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Iva=10 and cantidad>0      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Iva=0 and Cantidad>0      mletras      Arial      ?transform(Total,iif(IdMoneda='GS','999,999,999','999,999,999'))      Arial      PlazoEntrega      Arial      Condiciones      Arial      dataenvironment      _Top = 220
Left = 1
Width = 519
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init

TEXT TO cmdSQL noshow
SELECT     pe.IdPedido, pe.NroPedido as Numero, cl.RazSocial AS cliente, ISNULL(pe.Direccion, '') AS direccion, ISNULL(cl.Telefono, '') AS telefono, RTRIM(pe.IdVendedor) 
                      + '-' + RTRIM(j.Nombre) + ' ' + j.Apellido AS vendedor, det.Cantidad, det.Precio, ISNULL(pe.Obs, '') AS OBS1, ISNULL(det.Obs, '') AS OBS2, 
                      pe.FechaPedido as Fecha,det.PorcentajeDescuento, 
                      pe.HoraPedido, pe.FechaEntrega, vt_Iva.Valor AS Iva, det.IdProducto, ISNULL(DET.descripcion,p.Descripcion) AS Producto, cl.Fax, pe.Importe,ISNULL(pe.Descuento,0) as Descuento,
                      pe.ATT,pe.Validez,cl.Ruc,'GS' as IdMoneda,pe.Condiciones,pe.PlazoEntrega
FROM         vt_Iva RIGHT OUTER JOIN
                      st_Producto AS p ON vt_Iva.Iva = p.Iva RIGHT OUTER JOIN
                      VT_Pedido AS pe INNER JOIN
                      VT_PedidoDet AS det ON pe.IdPedido = det.IdPedido ON p.IdProducto = det.IdProducto AND p.IdEmpresa = det.IdEmpresa LEFT OUTER JOIN
                      vt_clientes AS cl ON pe.IdCliente = cl.IdCliente AND cl.IdEmpresa = pe.IdEmpresa LEFT OUTER JOIN
                      vt_Vendedores AS i ON pe.IdVendedor = i.IdVendedor and pe.IdEmpresa = i.IDEmpresa  LEFT OUTER JOIN
                      BS_Personas AS j ON i.idpersona = j.IdPersona
WHERE     (pe.IdPedido = ?m.IdPedido) AND (pe.IdEstado <> 'A')
ENDTEXT


sql(cmdSQL,'cFactura')


SELECT cFactura
*SET STEP ON 
m.Total = Importe
m.moneda = IdMoneda
m.lValidez=Validez
m.nro=0
m.nro = RECCOUNT()
m.cCondiciones=Condiciones
m.cPlazoEntrega=PlazoEntrega
*COUNT ALL FOR NVL(Descuento,0)<>0 TO m.NroDesc

*m.Nro = m.Nro + m.NroDesc

DO WHILE m.nro<15
	APPEND BLANK
	replace Importe WITH m.Total,Validez WITH m.lValidez, Condiciones WITH m.cCondiciones,PlazoEntrega WITH m.cPlazoEntrega
	m.nro= m.nro + 1
ENDDO 
	
PUBLIC mLetras


mLetras= ALLTRIM(numeral(int(m.total)))

m.decimales=IIF(m.moneda='GS',0,2)
IF m.decimales > 0
	mletras = "DOLARES " + mletras 
	mLetras = mLetras + ' CON ' + SUBSTR(STR(m.total - INT(m.total),3,2),2) + '/100'
ELSE 
	mletras = "GUARANIES " + mletras 
ENDIF


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo
ENDPROC
     	P���    7	  7	                        n�   %   b      �  *   �          �  U  w	 M(�  ��� �� SELECT     pe.IdPedido, pe.NroPedido as Numero, cl.RazSocial AS cliente, ISNULL(pe.Direccion, '') AS direccion, ISNULL(cl.Telefono, '') AS telefono, RTRIM(pe.IdVendedor) �� ��                       + '-' + RTRIM(j.Nombre) + ' ' + j.Apellido AS vendedor, det.Cantidad, det.Precio, ISNULL(pe.Obs, '') AS OBS1, ISNULL(det.Obs, '') AS OBS2, �M �G                       pe.FechaPedido as Fecha,det.PorcentajeDescuento, �� ��                       pe.HoraPedido, pe.FechaEntrega, vt_Iva.Valor AS Iva, det.IdProducto, ISNULL(DET.descripcion,p.Descripcion) AS Producto, cl.Fax, pe.Importe,ISNULL(pe.Descuento,0) as Descuento,�d �^                       pe.ATT,pe.Validez,cl.Ruc,'GS' as IdMoneda,pe.Condiciones,pe.PlazoEntrega�* �$ FROM         vt_Iva RIGHT OUTER JOIN�S �M                       st_Producto AS p ON vt_Iva.Iva = p.Iva RIGHT OUTER JOIN�6 �0                       VT_Pedido AS pe INNER JOIN�� ��                       VT_PedidoDet AS det ON pe.IdPedido = det.IdPedido ON p.IdProducto = det.IdProducto AND p.IdEmpresa = det.IdEmpresa LEFT OUTER JOIN�| �v                       vt_clientes AS cl ON pe.IdCliente = cl.IdCliente AND cl.IdEmpresa = pe.IdEmpresa LEFT OUTER JOIN�~ �x                       vt_Vendedores AS i ON pe.IdVendedor = i.IdVendedor and pe.IdEmpresa = i.IDEmpresa  LEFT OUTER JOIN�I �C                       BS_Personas AS j ON i.idpersona = j.IdPersona�D �> WHERE     (pe.IdPedido = ?m.IdPedido) AND (pe.IdEstado <> 'A')� � ��C �  � cFactura� �� F� � T�� �� �� T�� �� �� T�� �� �� T��	 �� �� T��	 �CN�� T��
 �� �� T�� �� �� +���	 ����� �3 >� ���� �� ���� �� ����
 �� ���� �� T��	 ���	 ��� � 7� � T� �CCC�� 8� ���% T�� �C�� � GS� � � �6�� %��� � ��M� T� �� DOLARES � ��9 T� �� �  CON CC�� C�� 8��Z�\� /100�� �p� T� ��
 GUARANIES � �� � U  CMDSQL SQL CFACTURA TOTAL IMPORTE MONEDA IDMONEDA LVALIDEZ VALIDEZ NRO CCONDICIONES CONDICIONES CPLAZOENTREGA PLAZOENTREGA MLETRAS NUMERAL	 DECIMALES
  �  � U  SETEO Init,     �� BeforeOpenTablesM    ��1 � q
��A�1a�	���AA �s � � � � � � � EQ 1QA r sR1��� �A 4 q 1                       �     (   �  �  9    )   7	                  