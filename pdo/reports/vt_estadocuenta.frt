  U   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Arial                                                         xDetalle.idcliente                                            
"xDetalle"                                                    anterior                                                      "999,999,999.99"                                                                                                            Arial                                                         dec > 0                                                       saldo                                                         "@Z 999,999,999,999.99"                                                                                                     Arial                                                         dec > 0                                                       saldo                                                         "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       "Estado de Cuenta de Clientes"                                                                                              Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         "Debe"                                                        Arial                                                         "Haber"                                                       Arial                                                         
"Cliente:"                                                    Arial                                                         &alltrim(idCliente) + " - " + razSocial                                                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                           Arial                                                         fecha                                                                                                                       Arial                                                         debe                                                          "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      haber                                                         "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      debe                                                          "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       haber                                                         "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       saldo                                                         "999,999,999,999"                                                                                                           Arial                                                         dec <= 0                                                      idcomprobante                                                                                                               Arial                                                         saldo                                                         "999,999,999,999"                                                                                                           Arial                                                         dec <= 0                                                      anterior                                                      "999,999,999,999"                                                                                                           Arial                                                         dec <= 0                                                      "Referencia"                                                  Arial                                                         "Saldo"                                                       Arial                                                         "Saldo Anterior"                                                                                                            Arial                                                         "Saldos del Periodo"                                                                                                        Arial                                                         numero                                                                                                                      Arial                                                         "Cpbte."                                                      Arial                                                         debe                                                          "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       haber                                                         "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       debe                                                          "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      haber                                                         "@Z 999,999,999,999"                                                                                                        Arial                                                         dec = 0                                                       
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
referencia                                                    Arial                                                         debe                                                          iif(tipo = 'A', total, 0)                                     0                                                             haber                                                         iif(tipo='B', xDetalle.total,0)                               0                                                             saldo                                                         debe -haber                                                   xDetalle.anterior                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               aTop = 144
Left = -42
Width = 792
Height = 483
DataSource = .NULL.
Name = "Dataenvironment"
                            �PROCEDURE Init
LOCAL strsql,widcliente
*Set Step On 
widcliente=m.idcliente

If Empty(m.idcliente)
	Messagebox('Ingrese Codigo Cliente',0,'Futura')
	Return
Endif

SET DATABASE TO DATOS 
strsql =  'SELECT sum(a.exenta+a.gravada+a.iva) AS importe '+;
		 'FROM vt_factura a '+;
		 'WHERE  a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa'+;
		 ' and a.fecha < ?m.dfecha  '
=sql(strsql, 'vfact')

m.sal_fact = NVL(vfact.importe,0)

strsql =  'SELECT sum(b.importe_pag) importe  '+;
		 'FROM vt_pagos a, vt_det_pagos b '+;
		 'WHERE a.idpago = b.idpago '+;
		 ' and a.fecha < ?m.dfecha '+;
		 ' AND a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa '
=sql(strsql, 'vpago')
m.sal_pago = Nvl(vpago.importe,0)
IF USED('vfact')
	USE IN 'vfact'
ENDIF
IF USED('vpago')
	USE IN 'vpago'
ENDIF
m.tip_a = 'A'
m.tip_b = 'B'
IF m.Detalle='CD'
	TEXT TO cmdSQL noshow
	SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, 
			 ?m.tip_a AS tipo, SPACE(12) AS referencia 
			 FROM vt_factura a, vt_clientes b 
			 WHERE  a.idcliente = ?m.idcliente 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 union all  
			 SELECT  a.fecha, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , 
			 ?m.tip_b AS tipo, 
			 'Fact. '  +STR(b.numero) AS referencia 
			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
			 vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND a.idcliente = ?m.idcliente 
	  	  and a.IdEmpresa = ?oApp.Empresa
	  	  union all
	  	  	SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, 
			 ?m.tip_b AS tipo, 'Contado' AS referencia 
			 FROM vt_factura a, vt_clientes b 
			 WHERE  a.idcliente = ?m.idcliente 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)

	ENDTEXT


ELSE
	TEXT TO cmdSQL noshow
	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, 
			 ?m.tip_a AS tipo, SPACE(12) AS referencia 
			 FROM vt_factura a, vt_clientes b 
			 WHERE  a.idcliente = ?m.idcliente 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 union all 
			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, a.TotalValores importe , 
			 ?m.tip_b AS tipo, 
			 ' ' AS referencia 
			 FROM vt_pagos a, vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND a.idcliente = ?m.idcliente 
	  	  and a.IdEmpresa = ?oApp.Empresa
	  	  union all
	  	  	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, 
			 ?m.tip_b AS tipo, 'Contado' AS referencia 
			 FROM vt_factura a, vt_clientes b 
			 WHERE  a.idcliente = ?m.idcliente 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
	ENDTEXT
ENDIF



	
 = sql(cmdSQL, 'vt_restadocuenta')
 
 	TEXT TO cmdSQL noshow
			 SELECT  a.IdPago, a.fecha, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, b.importe_pag importe , 
			 ?m.tip_b AS tipo, 
			 'Fact. '  +STR(b.numero) AS referencia 
			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
			 vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND a.idcliente = ?m.idcliente 
	  	  and a.IdEmpresa = ?oApp.Empresa

	ENDTEXT
 = sql(cmdSQL, 'cDetalle')	
 
 SELECT  m.sal_fact-m.sal_pago anterior, fecha, ;
		  idcliente,razsocial, idcomprobante,  numero, ;
		  tipo, referencia,;
		  0 DEC ,;
			importe total ;
		  from vt_restadocuenta  ;
  	ORDER BY fecha INTO CURSOR xDetalle

*!*	SELECT cDetalle
*!*	INDEX on IdPago TAG Idpago


*!*	SELECT xDetalle
*!*	SET RELATION TO idPago INTO  cDetalle 				 

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                                       Q���    8  8                        �   %   �      �  t   �          �  U  � ��  � � T� ��� �� %�C�� ���] �. ��C� Ingrese Codigo Cliente� � Futura�x�� B� � G(� DATOS�� T�  ��0 SELECT sum(a.exenta+a.gravada+a.iva) AS importe � FROM vt_factura a �A WHERE  a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa�  and a.fecha < ?m.dfecha  �� ��C �  � vfact� �� T�� �C� � � ���� T�  ��# SELECT sum(b.importe_pag) importe  �  FROM vt_pagos a, vt_det_pagos b � WHERE a.idpago = b.idpago �  and a.fecha < ?m.dfecha �@  AND a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa �� ��C �  � vpago� �� T�� �C�	 � � ��� %�C� vfact���u� Q�� vfact�� � %�C� vpago����� Q�� vpago�� � T��
 �� A�� T�� �� B�� %��� � CD��d	�	 M(� �� � 	SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �Q �K 			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, �4 �. 			 ?m.tip_a AS tipo, SPACE(12) AS referencia �+ �% 			 FROM vt_factura a, vt_clientes b �, �& 			 WHERE  a.idcliente = ?m.idcliente �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa� � 			 union all  �5 �/ 			 SELECT  a.fecha, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �Q �K 			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , � � 			 ?m.tip_b AS tipo, �1 �+ 			 'Fact. '  +STR(b.numero) AS referencia �M �G 			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  � � 			 vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 AND a.idcliente = ?m.idcliente �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� � 	  	  union all� � 	  	  	SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �Q �K 			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, �4 �. 			 ?m.tip_b AS tipo, 'Contado' AS referencia �+ �% 			 FROM vt_factura a, vt_clientes b �, �& 			 WHERE  a.idcliente = ?m.idcliente �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�_ �Y 			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)� �  � � ���	 M(� ��! � 	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �Q �K 			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, �4 �. 			 ?m.tip_a AS tipo, SPACE(12) AS referencia �+ �% 			 FROM vt_factura a, vt_clientes b �, �& 			 WHERE  a.idcliente = ?m.idcliente �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa� � 			 union all �? �9 			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �; �5 			 a.num_recibo AS numero, a.TotalValores importe , � � 			 ?m.tip_b AS tipo, � � 			 ' ' AS referencia �) �# 			 FROM vt_pagos a, vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 AND a.idcliente = ?m.idcliente �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� � 	  	  union all�' �! 	  	  	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �Q �K 			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, �4 �. 			 ?m.tip_b AS tipo, 'Contado' AS referencia �+ �% 			 FROM vt_factura a, vt_clientes b �, �& 			 WHERE  a.idcliente = ?m.idcliente �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�_ �Y 			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)� � �! ��C � � vt_restadocuenta� ��	 M(� ��? �9 			 SELECT  a.IdPago, a.fecha, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �: �4 			 a.num_recibo AS numero, b.importe_pag importe , � � 			 ?m.tip_b AS tipo, �1 �+ 			 'Fact. '  +STR(b.numero) AS referencia �M �G 			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  � � 			 vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 AND a.idcliente = ?m.idcliente �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� �  � � ��C � � cDetalle� ��z o� vt_restadocuenta��� �� �Q� �� ��� ��� ��� ��� ��� ��� ��� �Q� �� �Q� ��� ���� xDetalle� U  STRSQL
 WIDCLIENTE	 IDCLIENTE DATOS SQL SAL_FACT VFACT IMPORTE SAL_PAGO VPAGO TIP_A TIP_B DETALLE CMDSQL M ANTERIOR FECHA	 RAZSOCIAL IDCOMPROBANTE NUMERO TIPO
 REFERENCIA DEC TOTAL VT_RESTADOCUENTA XDETALLE
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � � �A A � dar6aqA� A A� A Q� �A��Qq�QQq���q��Q�A��Qq��a A � � A��Qq�A�q����q��QqA��Qq��A A � �q����q��a A ��: q 2                       J     r   q  {  �    )   8                       t   �  �  �    )   h         