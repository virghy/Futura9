  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Courier New      	IdFActura      lPrecio      real      0      importe0      iif(iva=0,lprecio*cantidad,0)      0      importe5      iif(iva=5,lprecio*cantidad,0)      0      	importe10      iif(iva=10,lprecio*cantidad,0)      0      Total      TotalFactura      0      Arial      Arial      Courier New      numero      Arial      'XX'      Arial      Fecha      "@D"      Arial      IIF(plazo=0,'','X')      Courier New      IIF(plazo=0,'X','')      Courier New      Cliente      Arial      	direccion      Arial      ciudad      Arial      telefono      Arial      
idproducto      Arial      transform(cantidad,'9999.99')      Arial      
cantidad>0      Producto      Arial      @transform(lPRecio,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      mletras      Arial      >transform(Total,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      dataenvironment      _Top = 220
Left = 1
Width = 519
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo
_ASCIICOLS = 145
_asciirows = 45
TEXT TO cmdSQL noshow
SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente,c.ciudad, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, 
                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, 
                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion),'')+'-'+isnull(rtrim(pr.descripcion1),'') AS Producto, det.Iva, 
                      det.[real], TotalFactura,v.IdMoneda
FROM         dbo.vt_clientes c RIGHT OUTER JOIN
                      dbo.vt_factura v INNER JOIN
                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente INNER JOIN 
                      st_producto AS pr ON det.idproducto = pr.idproducto and det.idempresa=pr.idempresa LEFT OUTER JOIN
                      dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN
                      dbo.BS_Personas p INNER JOIN
                      dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor
where v.IdFactura = ?m.IdFActura
ENDTEXT
sql(cmdSQL,'cFactura')
SELECT cFactura
*SET STEP ON 
m.Total = TotalFactura
m.moneda = IdMoneda
m.nro=0
m.nro = RECCOUNT()
*!*	DO WHILE m.nro<29
*!*		APPEND BLANK
*!*		replace precio WITH 0, real WITH 0, totalFactura WITH m.total
*!*		m.nro= m.nro + 1
*!*	ENDDO 
	
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
     ���    �  �                        �   %   W      �  "   e          �  U  � �  � T�>����� T�?��-��	 M(� ��� �� SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente,c.ciudad, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, �� ��                       RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, �� ��                       det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion),'')+'-'+isnull(rtrim(pr.descripcion1),'') AS Producto, det.Iva, �? �9                       det.[real], TotalFactura,v.IdMoneda�5 �/ FROM         dbo.vt_clientes c RIGHT OUTER JOIN�7 �1                       dbo.vt_factura v INNER JOIN�� ��                       dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente INNER JOIN �~ �x                       st_producto AS pr ON det.idproducto = pr.idproducto and det.idempresa=pr.idempresa LEFT OUTER JOIN�� ��                       dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN�8 �2                       dbo.BS_Personas p INNER JOIN�� ��                       dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor�& �  where v.IdFactura = ?m.IdFActura� � ��C � � cFactura� �� F� � T�� �� �� T�� �� �� T�� �� �� T�� �CN�� 7�	 � T�	 �CCC�� 8�
 ���% T�� �C�� � GS� � � �6�� %��� � ���� T�	 �� DOLARES �	 ��9 T�	 ��	 �  CON CC�� C�� 8��Z�\� /100�� ��� T�	 ��
 GUARANIES �	 �� � U  SETEO CMDSQL SQL CFACTURA TOTAL TOTALFACTURA MONEDA IDMONEDA NRO MLETRAS NUMERAL	 DECIMALES Init,     ��1 q � � � �
1
��Qq�	�a�Q	aA �q � � � � w sR1��� �A 3                       �      )   �                  