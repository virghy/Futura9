  #�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=5
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      lPrecio      real      0      importe0      Eiif(iva=0,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      importe5      Eiif(iva=5,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      	importe10      Fiif(iva=10,(lprecio*cantidad)-lprecio*cantidad*nvl(Descuento,0)/100,0)      0      Total      TotalFactura      0      Arial      Courier New      Courier New       IIF(plazo=0,'CONTADO','CREDITO')      Courier New      numero      Courier New      @day(fecha), " de ", Nombremes(month(Fecha)), " de ", year(fecha)      Courier New      IIF(plazo=0,'X','')      Courier New      IIF(plazo=0,'','X')      Courier New      Cliente      Courier New      RUC      Courier New      Direccion, ' - ' + Ciudad      Courier New      Remision      Courier New      telefono      Courier New      
IdProducto      Courier New      transform(cantidad,'9999.99')      Courier New      
cantidad>0      Producto      Courier New      @transform(lPRecio,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      
Cantidad>0      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      Iva=0 and Cantidad>0      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))     q<VFPData>
	<reportdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999,999')" execute="Dec0" execwhen="m.dec=0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
	<reportdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999.99')" execute="Dec2" execwhen="m.Dec&gt;0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
</VFPData>
      Courier New      Iva=5      Itransform(lPRecio*Cantidad,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      Iva=10 and cantidad>0      Atransform(importe0,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      Atransform(importe5,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      Btransform(importe10,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      mletras      Courier New      >transform(Total,iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      Ptransform(round(importe5*5/105,2),iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      IdMoneda<>'GS'      Ptransform(round(importe5*5/105,0),iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      IdMoneda='GS'      Rtransform(round(importe10*10/110,0),iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      IdMoneda='GS'      Rtransform(round(importe10*10/110,2),iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      IdMoneda<>'GS'      ktransform(round(importe10*10/110,0) +round(importe5*5/105,0),iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      IdMoneda='GS'      ktransform(round(importe10*10/110,2) +round(importe5*5/105,2),iif(IdMoneda='GS','999,999,999','999,999.99'))      Courier New      IdMoneda<>'GS'      dataenvironment      _Top = 220
Left = 1
Width = 519
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     
TPROCEDURE Init
DO seteo
_ASCIICOLS = 145
_asciirows = 45
TEXT TO cmdSQL noshow
SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, 
                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, 
                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, RTRIM(c.Direccion) AS Direccion,c.Ciudad, c.Telefono, c.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion), ' ') AS Producto, det.Iva, 
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
     	����    k	  k	                        ��   %   �      "	  *   �          �  U  	 �  � T�>����� T�?��-��	 M(� ��� �� SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, �� ��                       RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, �� ��                       det.GravadaIncluido, det.RegimenTurismo, det.Imprime, RTRIM(c.Direccion) AS Direccion,c.Ciudad, c.Telefono, c.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion), ' ') AS Producto, det.Iva, �t �n                       det.[real], TotalFactura,v.IdMoneda,Vence=dateadd(dd,cond.Inicial,v.Fecha),cond.Inicial,�t �n                       Remision = dbo.Vt_RemisionDescripcion(v.IdFactura),v.IdFActura,Det.Descuento,v.IdPedido �5 �/ FROM         dbo.vt_clientes c RIGHT OUTER JOIN�6 �0                       dbo.vt_factura v left JOIN�� ��                       dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura and det.Imprime=1 ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente INNER JOIN �~ �x                       st_producto AS pr ON det.idproducto = pr.idproducto and det.idempresa=pr.idempresa LEFT OUTER JOIN�� ��                       dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN�8 �2                       dbo.BS_Personas p INNER JOIN�� ��                       dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor�= �7                       where v.IdFactura = ?m.IdFActura � � ��C � � cFactura� �� F� � T�� �� �� T�� �� �� T�� �� �� T�� �CN�� �C�	 � �� �(��
 � T�� ��� ��
 �� +��� ���3� �% >� ���  �� ���� �� ���� �� T�� ��� ��� � 7� � T� �CCC�� 8� ���% T�� �C�� � GS� � � �6�� %��� � ���� T� �� DOLARES � ��9 T� �� �  CON CC�� C�� 8��Z�\� /100�� �� T� ��
 GUARANIES � �� � U  SETEO CMDSQL SQL CFACTURA TOTAL TOTALFACTURA MONEDA IDMONEDA NRO	 DESCUENTO NRODESC
 IDPRODUCTO MLETRAS NUMERAL	 DECIMALES Init,     ��1 q � � � !
1
QAAQa�
�a�Q	�A �q � � � � �rBQ QQA r sR1��� �A ?                       I
      )   k	                  