  "�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=5
COLOR=2
      Arial      lPrecio      real      0      importe0      Eiif(iva=0,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      importe5      Eiif(iva=5,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      	importe10      Fiif(iva=10,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      Total      TotalFactura      0      Arial      Arial      Arial      numero      Arial      "San Lorenzo,", day(Fecha)      Arial      Nombremes(month(Fecha))      Arial      year(Fecha)      Arial      Cliente      Arial      RUC      Arial      	Direccion      Arial      telefono      Arial      Remision      Arial      IIF(plazo=0,'X','')      Arial      IIF(plazo=0,'','X')      Arial      
IdProducto      Arial      transform(cantidad,'9999.99')      Arial      
cantidad>0      Producto      Arial      @transform(lPRecio,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      
Cantidad>0      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Iva=0 and Cantidad>0      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))     q<VFPData>
	<reportdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999,999')" execute="Dec0" execwhen="m.dec=0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
	<reportdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999.99')" execute="Dec2" execwhen="m.Dec&gt;0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
</VFPData>
      Arial      Iva=5      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Iva=10 and cantidad>0      Atransform(importe0,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Atransform(importe5,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Btransform(importe10,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      mletras      Arial      >transform(Total,iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      Rtransform(round(importe10*10/110,0),iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      IdMoneda='GS'      Rtransform(round(importe10*10/110,2),iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      IdMoneda<>'GS'      Ptransform(round(importe5*5/105,2),iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      IdMoneda<>'GS'      Ptransform(round(importe5*5/105,0),iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      IdMoneda='GS'      ktransform(round(importe10*10/110,0) +round(importe5*5/105,0),iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      IdMoneda='GS'      ktransform(round(importe10*10/110,2) +round(importe5*5/105,2),iif(IdMoneda='GS','999,999,999','999,999.99'))      Arial      IdMoneda<>'GS'      dataenvironment      _Top = 220
Left = 1
Width = 519
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     
"PROCEDURE Init
DO seteo
	TEXT TO cmdSQL noshow
	SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, 
	                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, 
	                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion), ' ') AS Producto, det.Iva, 
	                      det.[real], TotalFactura,v.IdMoneda,Vence=dateadd(dd,cond.Inicial,v.Fecha),cond.Inicial,
	                      Remision = dbo.Vt_RemisionDescripcion(v.IdFactura),v.IdFActura,Det.Descuento,v.IdPedido 
	FROM         dbo.vt_clientes c RIGHT OUTER JOIN
	                      dbo.vt_factura v left JOIN
	                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura and det.Imprime=1 ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente INNER JOIN 
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
COUNT ALL FOR NVL(Descuento,0)<>0 TO m.NroDesc

m.Nro = m.Nro + m.NroDesc

DO WHILE m.nro<7
	APPEND BLANK
	replace IdProducto WITH "", TotalFactura WITH m.Total,IdMoneda WITH m.Moneda
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


*!*	CREATE CURSOR Cabecera (IdFactura I ) 
*!*	SELECT Cabecera
*!*	INSERT INTO cabecera(IdFactura) values(m.IdFActura )
*!*	INSERT INTO cabecera(IdFactura) values(m.IdFActura )
*!*	INSERT INTO cabecera(IdFactura) values(m.IdFActura )


*!*	SELECT cFactura
*!*	INDEX on IdFactura TAG Nro

*!*	SELECT Cabecera
*!*	SET RELATION TO IdFactura INTO cFactura ADDITIVE  
ENDPROC
     	X���    ?	  ?	                        �`   %   �      �  (   �          �  U  � �  �	 M(� ��� �� 	SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, �� �� 	                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, �� �� 	                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion), ' ') AS Producto, det.Iva, �u �o 	                      det.[real], TotalFactura,v.IdMoneda,Vence=dateadd(dd,cond.Inicial,v.Fecha),cond.Inicial,�u �o 	                      Remision = dbo.Vt_RemisionDescripcion(v.IdFactura),v.IdFActura,Det.Descuento,v.IdPedido �6 �0 	FROM         dbo.vt_clientes c RIGHT OUTER JOIN�7 �1 	                      dbo.vt_factura v left JOIN�� �� 	                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura and det.Imprime=1 ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente INNER JOIN � �y 	                      st_producto AS pr ON det.idproducto = pr.idproducto and det.idempresa=pr.idempresa LEFT OUTER JOIN�� �� 	                      dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN�9 �3 	                      dbo.BS_Personas p INNER JOIN�� �� 	                      dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor�> �8 	                      where v.IdFactura = ?m.IdFActura � � ��C � � cFactura� �� F� � T�� �� �� T�� �� �� T�� �� �� T�� �CN�� �C�	 � �� �(��
 � T�� ��� ��
 �� +��� ���� �% >� ���  �� ���� �� ���� �� T�� ��� ��� � 7� � T� �CCC�� 8� ���% T�� �C�� � GS� � � �6�� %��� � ���� T� �� DOLARES � ��9 T� �� �  CON CC�� C�� 8��Z�\� /100�� ��� T� ��
 GUARANIES � �� � U  SETEO CMDSQL SQL CFACTURA TOTAL TOTALFACTURA MONEDA IDMONEDA NRO	 DESCUENTO NRODESC
 IDPRODUCTO MLETRAS NUMERAL	 DECIMALES Init,     ��1 q � 1
A
�QQaq�
�q�a	�A �q � � � � �rBQ QQA r sR1��� �A ?                       
      )   ?	                  