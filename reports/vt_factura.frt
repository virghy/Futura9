  %;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Epson LX-300+
OUTPUT=LPT1:
ORIENTATION=1
PAPERSIZE=120
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=120
COLOR=1
YRESOLUTION=72
TTOPTION=2
COLLATE=1
      s  !   winspool  Epson LX-300+  LPT1:                  3C  USB001                   Writer Port:                       �Epson LX-300+                    � $C� x �4d   x   H   A4                                                            ����                DINU" � $  �Z�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      �   SMTJ     � E p s o n   L X - 3 0 0 +   InputBin MANUAL RESDLL UniresDLL Orientation PORTRAIT Resolution Option2 PaperSize A4 PrintQuality Option1 ColorMode Color Halftone HT_PATSIZE_AUTO                                                    Courier New      	IdFActura      lPrecio      real      0      importe0      iif(iva=0,lprecio*cantidad,0)      0      importe5      iif(iva=5,lprecio*cantidad,0)      0      	importe10      iif(iva=10,lprecio*cantidad,0)      0      Total      TotalFactura      0      Courier New      numero      Courier New      IIF(plazo=0,'','X')      Courier New      Fecha      Courier New      IIF(plazo=0,'X','')      Courier New      RUC      Courier New      Cliente      Courier New      	Direccion      Courier New      
IdProducto      Courier New      Cantidad      "99999"      Courier New      
cantidad>0      Producto      Courier New      lPRecio      "@Z 999,999.99"      Courier New      	m.dec > 0      lPRecio      "@Z 999,999,999"      Courier New      	m.dec = 0      lPRecio*Cantidad      "@Z 999,999,999.99"      Courier New      Iva=0 and m.dec>0      lPRecio*Cantidad      "@Z 999,999,999"      Courier New      Iva=0 and m.dec=0      lPRecio*Cantidad     q<VFPData>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999,999')" execute="Dec0" execwhen="m.dec=0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999.99')" execute="Dec2" execwhen="m.Dec&gt;0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
</VFPData>
      "999,999.99"      Courier New      Iva=5 and m.dec > 0      lPRecio*Cantidad     q<VFPData>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999,999')" execute="Dec0" execwhen="m.dec=0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="Transform(lPrecio*Cantidad,'999,999.99')" execute="Dec2" execwhen="m.Dec&gt;0" class="" classlib="" declass="" declasslib="" penrgb="-1" fillrgb="-1" pena="255" filla="0" fname="Courier New" fsize="10" fstyle="0"/>
</VFPData>
      "999,999,999"      Courier New      Iva=5 and m.dec = 0      lPRecio*Cantidad      "@Z 999,999,999,999"      Courier New      Iva=10 and m.dec = 0      lPRecio*Cantidad      "@Z 999,999,999.99"      Courier New      Iva=10 and m.dec > 0      importe0      "@Z 999,999,999,999"      Courier New      m.dec=0      importe0      "@Z 999,999,999.99"      Courier New      m.dec>0      importe5      "@Z 999,999,999,999"      Courier New      m.dec=0      importe5      "@Z 999,999,999.99"      Courier New      m.dec>0      	importe10      "@Z 999,999,999,999"      Courier New      m.dec=0      	importe10      "@Z 999,999,999.99"      Courier New      m.dec>0      mletras      Courier New      Total      "@Z 999,999,999,999"      Courier New      m.dec=0      Total      "@Z 999,999,999.99"      Courier New      m.dec>0      round(importe5*5/105,2)      "@Z 999,999,999"      Courier New      m.dec=0      round(importe5*5/105,2)      "@Z 999,999.99"      Courier New      m.dec>0      round(importe10*10/110,0)      "999,999,999"      Courier New      m.dec=0      round(importe10*10/110,2)      "999,999.99"      Courier New      m.dec>0      3round(importe10*10/110,2) + round(importe5*5/105,2)      "999,999.99"      Courier New      m.dec>0      3round(importe10*10/110,2) + round(importe5*5/105,2)      "999,999,999"      Courier New      m.dec=0      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo
_ASCIICOLS = 145
_asciirows = 45
TEXT TO cmdSQL noshow
SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, 
                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, 
                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, det.Descripcion AS Producto, det.Iva, 
                      det.[real], TotalFactura
FROM         dbo.vt_clientes c RIGHT OUTER JOIN
                      dbo.vt_factura v INNER JOIN
                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente LEFT OUTER JOIN
                      dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN
                      dbo.BS_Personas p INNER JOIN
                      dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor
                      where v.IdFactura = ?m.IdFActura 
ENDTEXT
sql(cmdSQL,'cFactura')
SELECT cFactura
m.Total = TotalFactura
m.nro=0
m.nro = RECCOUNT()
DO WHILE m.nro<29
	APPEND BLANK
	replace precio WITH 0, real WITH 0, totalFactura WITH m.total
	m.nro= m.nro + 1
ENDDO 
	
PUBLIC mLetras

*Set Step On 

mLetras= ALLTRIM(numeral(int(m.total)))
IF m.dec > 0
	mletras = "DOLARES " + mletras 
	mLetras = mLetras + ' CON ' + SUBSTR(STR(m.total - INT(m.total),3,2),2) + '/100'
ELSE 
	mletras = "GUARANIES " + mletras 
ENDIF


ENDPROC
     w���    ^  ^                        �   %   �        $   �          �  U  2 �  � T�>����� T�?��-��	 M(� ��� �� SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, �� ��                       RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, �� ��                       det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, det.Descripcion AS Producto, det.Iva, �4 �.                       det.[real], TotalFactura�5 �/ FROM         dbo.vt_clientes c RIGHT OUTER JOIN�7 �1                       dbo.vt_factura v INNER JOIN�� ��                       dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente LEFT OUTER JOIN�� ��                       dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN�8 �2                       dbo.BS_Personas p INNER JOIN�� ��                       dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor�= �7                       where v.IdFactura = ?m.IdFActura � � ��C � � cFactura� �� F� � T�� �� �� T�� �� �� T�� �CN�� +��� ����� �# >� ��� �� ��� �� ���� �� T�� ��� ��� � 7�	 � T�	 �CCC�� 8�
 ��� %��� � ��� T�	 �� DOLARES �	 ��9 T�	 ��	 �  CON CC�� C�� 8��Z�\� /100�� �+� T�	 ��
 GUARANIES �	 �� � U  SETEO CMDSQL SQL CFACTURA TOTAL TOTALFACTURA NRO PRECIO REAL MLETRAS NUMERAL DEC Init,     ��1 q � � � !
1
A
AQq�	a�Q	�A �q � � � AQ 1QA r t1��� �A 3                       �      )   ^                  