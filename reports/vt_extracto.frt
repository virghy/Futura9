  6   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=1
COLOR=1
                         Arial                                                         xDetalle.idcliente                                            xDetalle.IdMoneda                                             
"xDetalle"                                                    !"Extracto de Cuenta de Clientes "                             Arial                                                         oApp.Nombreempresa                                            Arial                                                         
"Per�odo:"                                                    Arial                                                         1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                           Arial                                                         "Debe"                                                        Arial                                                         "Haber"                                                       Arial                                                         "Saldo"                                                       Arial                                                         	"Fecha
"                                                     Arial                                                         "Cpbte."                                                      Arial                                                         "Referencia"                                                  Arial                                                         
"Cliente:"                                                    Arial                                                         &alltrim(idCliente) + " - " + razSocial                                                                                      Arial                                                         "IdMoneda: ", Idmoneda                                        Arial                                                         "Saldo Anterior"                                              Arial                                                         .f.                                                           anterior                                                      "999,999,999,999"                                             Arial                                                         .f.                                                           anterior                                                      "999,999,999.99"                                                                                                            Arial                                                         .f.                                                           fecha                                                         Arial                                                         /alltrim(idcomprobante),"-",alltrim(str(numero))               Arial                                                         
referencia                                                    Arial                                                         debe                                                          "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      debe                                                          "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       haber                                                         "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       haber                                                         "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      saldo                                                         "999,999,999,999"                                             Arial                                                         dec <= 0                                                      saldo                                                         "@Z 999,999,999.99"                                           Arial                                                         dec > 0                                                       "Saldos del Periodo"                                                                                                        Arial                                                         debe                                                          "@Z 999,999,999.99"                                           Arial                                                         dec > 0                                                       debe                                                          "@Z 999,999,999,999"                                          Arial                                                         dec <= 0                                                      haber                                                         "@Z 999,999,999,999"                                          Arial                                                         dec = 0                                                       haber                                                         "@Z 999,999,999.99"                                           Arial                                                         dec > 0                                                       saldo                                                         "999,999,999,999"                                             Arial                                                         dec <= 0                                                      saldo                                                         "@Z 999,999,999,999.99"                                                                                                     Arial                                                         dec > 0                                                       'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         debe                                                          iif(tipo = 'A', total, 0)                                     0                                                             haber                                                         iif(tipo='B', xDetalle.total,0)                               0                                                             saldo                                                         debe -haber                                                   0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               Top = 144
Left = -42
Width = 792
Height = 483
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
                                                              $/PROCEDURE Init
LOCAL strsql,widcliente
*Set Step On 
*widcliente=m.idcliente

*!*	If Empty(m.idcliente)
*!*		Messagebox('Ingrese Codigo Cliente',0,'Futura')
*!*		Return
*!*	Endif

IF EMPTY(m.Idcliente)
	m.IdCliente=null
ENDIF
	

SET DATABASE TO DATOS
TEXT TO cmdSQL noshow
		SELECT sum(TotalFactura) AS importe,
		 a.IdMoneda, a.Idcliente  
		 FROM vt_factura a
		 WHERE  (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
		 and a.IdEmpresa = ?oApp.Empresa
		 and a.fecha < ?m.dfecha
		 and exists(Select IdFactura from vt_forma_pago f where f.IdFactura = a.IdFactura)
		 group by a.IdMoneda, a.Idcliente  

ENDTEXT
 
=sql(cmdSQL , 'vfact')

m.sal_fact = NVL(vfact.importe,0)

m.sal_fact = 0 

TEXT TO cmdSQL noshow
	SELECT sum(b.importe_pag) importe,
	a.IdMoneda, a.IdCliente  
	FROM vt_pagos a, vt_det_pagos b 
	WHERE a.idpago = b.idpago 
	and a.fecha < ?m.dfecha 
	AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
	and a.IdEmpresa = ?oApp.Empresa
	group by a.IdMoneda,a.IdCliente  

ENDTEXT

=sql(cmdSQL, 'vpago')
*m.sal_pago = Nvl(vpago.importe,0)
*!*	IF USED('vfact')
*!*		USE IN 'vfact'
*!*	ENDIF
*!*	IF USED('vpago')
*!*		USE IN 'vpago'
*!*	ENDIF
m.tip_a = 'A'
m.tip_b = 'B'

*!*	*****************
*!*	SET DATABASE TO DATOS 
*!*	strsql =  'SELECT sum(a.exenta+a.gravada+a.iva) AS importe '+;
*!*			 'FROM vt_factura a '+;
*!*			 'WHERE  a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa'+;
*!*			 ' and a.fecha < ?m.dfecha  '
*!*	=sql(strsql, 'vfact')

*!*	m.sal_fact = NVL(vfact.importe,0)

*!*	strsql =  'SELECT sum(b.importe_pag) importe  '+;
*!*			 'FROM vt_pagos a, vt_det_pagos b '+;
*!*			 'WHERE a.idpago = b.idpago '+;
*!*			 ' and a.fecha < ?m.dfecha '+;
*!*			 ' AND a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa '
*!*	=sql(strsql, 'vpago')
*!*	m.sal_pago = Nvl(vpago.importe,0)
*!*	IF USED('vfact')
*!*		USE IN 'vfact'
*!*	ENDIF
*!*	IF USED('vpago')
*!*		USE IN 'vpago'
*!*	ENDIF
*!*	m.tip_a = 'A'
*!*	m.tip_b = 'B'
*************


IF m.Detalle='CD'

	IF m.SIContado='S'
	
	TEXT TO cmdSQL noshow
	SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a inner join vt_clientes b 
			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 WHERE 
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 union 
			 SELECT  a.fecha, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , 
			 ?m.tip_b AS tipo, 
			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
			 vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  
		  	 and a.IdEmpresa = ?oApp.Empresa
	  	  	 union all
	  	  	 SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, 
			 ?m.tip_b AS tipo, 'Contado' AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a, vt_clientes b 
			 WHERE  
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 			 and a.numero_Ref is null
	ENDTEXT			 			 
	ELSE
TEXT TO cmdSQL noshow
	SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a inner join vt_clientes b 
			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 WHERE 
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 union 
			 SELECT  a.fecha, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , 
			 ?m.tip_b AS tipo, 
			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
			 vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  
		  	 and a.IdEmpresa = ?oApp.Empresa

	ENDTEXT
	ENDIF





ELSE
	IF m.SIContado='S'
	
	TEXT TO cmdSQL noshow
	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda
			 FROM vt_factura a, vt_clientes b 
			 WHERE  
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 union all 
			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, a.TotalValores importe , 
			 ?m.tip_b AS tipo, 
			 ' ' AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a, vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
	  	  and a.IdEmpresa = ?oApp.Empresa
	  	  union all
	  	  	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, 'Contado' AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a, vt_clientes b 
			 WHERE 
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 			 and a.numero_Ref is null

	ENDTEXT
	ELSE
		TEXT TO cmdSQL noshow
	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda
			 FROM vt_factura a, vt_clientes b 
			 WHERE  
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 union all 
			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, a.TotalValores importe , 
			 ?m.tip_b AS tipo, 
			 ' ' AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a, vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
	  	  and a.IdEmpresa = ?oApp.Empresa
	ENDTEXT
	
	ENDIF
	
ENDIF



	
 = sql(cmdSQL, 'vt_restadocuenta')
 SELECT vt_restadocuenta

 
 
*!*	 	TEXT TO cmdSQL noshow
*!*				 SELECT  a.IdPago, a.fecha, a.idcliente, d.razsocial, 
*!*				 a.tip_reci AS idcomprobante, 
*!*				 a.num_recibo AS numero, b.importe_pag importe , 
*!*				 ?m.tip_b AS tipo, 
*!*				 'Fact. '  +STR(b.numero) AS referencia,
*!*				 a.IdMoneda 
*!*				 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
*!*				 vt_clientes d 
*!*				 WHERE 
*!*				 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
*!*				 and a.fecha between ?m.dfecha  and ?m.hfecha 
*!*				 AND a.idcliente = ?m.idcliente 
*!*		  	  and a.IdEmpresa = ?oApp.Empresa

*!*		ENDTEXT
* = sql(cmdSQL, 'cDetalle')	


 SELECT  nvl(f.Importe,0)-NVL(c.Importe,0) anterior, fecha, ;
		  r.idcliente,razsocial, idcomprobante,  numero, ;
		  tipo, referencia,;
		  0 as DEC ,;
		  r.importe total, ;
	      r.IdMoneda;
		  from vt_restadocuenta r ;
		  LEFT JOIN vfact f ON r.IdMoneda=f.IdMoneda AND r.idcliente= f.idcliente;
		  LEFT JOIN vPago c ON r.IdMoneda=c.IdMoneda  AND r.idcliente= c.idcliente; 	
  	ORDER BY r.IdCliente,r.IdMoneda, fecha INTO CURSOR xDetalle

SELECT xDetalle

*!*	SELECT cDetalle
*!*	INDEX on IdPago TAG Idpago


*!*	SELECT xDetalle
*!*	SET RELATION TO idPago INTO  cDetalle 				 

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
               =���    $   $                         ��   %   '      �  �   O          �  U  � ��  � � %�C�� ���) � T�� ���� � G(� DATOS�	 M(� ��, �& 		SELECT sum(TotalFactura) AS importe,�" � 		 a.IdMoneda, a.Idcliente  � � 		 FROM vt_factura a�F �@ 		 WHERE  (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �( �" 		 and a.IdEmpresa = ?oApp.Empresa�  � 		 and a.fecha < ?m.dfecha�Z �T 		 and exists(Select IdFactura from vt_forma_pago f where f.IdFactura = a.IdFactura)�+ �% 		 group by a.IdMoneda, a.Idcliente  � �  � � ��C � � vfact� �� T�� �C� � � ��� T�� �� ��	 M(� ��) �# 	SELECT sum(b.importe_pag) importe,�  � 	a.IdMoneda, a.IdCliente  �' �! 	FROM vt_pagos a, vt_det_pagos b �! � 	WHERE a.idpago = b.idpago � � 	and a.fecha < ?m.dfecha �A �; 	AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �& �  	and a.IdEmpresa = ?oApp.Empresa�( �" 	group by a.IdMoneda,a.IdCliente  � �  � � ��C � � vpago� �� T��	 �� A�� T��
 �� B�� %��� � CD��9� %��� � S���	 M(� �� � 	SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda �5 �/ 			 FROM vt_factura a inner join vt_clientes b �D �> 			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa� �
 			 WHERE �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa� �
 			 union �5 �/ 			 SELECT  a.fecha, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �Q �K 			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , � � 			 ?m.tip_b AS tipo, �U �O 			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,� � 			 a.IdMoneda �M �G 			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  � � 			 vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �E �? 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  �+ �% 		  	 and a.IdEmpresa = ?oApp.Empresa� � 	  	  	 union all� � 	  	  	 SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �Q �K 			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, �4 �. 			 ?m.tip_b AS tipo, 'Contado' AS referencia,� � 			 a.IdMoneda �+ �% 			 FROM vt_factura a, vt_clientes b � � 			 WHERE  �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�_ �Y 			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)�& �  			 			 and a.numero_Ref is null� � �5�	 M(� �� � 	SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda �5 �/ 			 FROM vt_factura a inner join vt_clientes b �D �> 			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa� �
 			 WHERE �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�[ �U 			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)� �
 			 union �5 �/ 			 SELECT  a.fecha, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �Q �K 			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , � � 			 ?m.tip_b AS tipo, �U �O 			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,� � 			 a.IdMoneda �M �G 			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  � � 			 vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �E �? 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  �+ �% 		  	 and a.IdEmpresa = ?oApp.Empresa� �  � � � ��� %��� � S���	 M(� ��! � 	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda�+ �% 			 FROM vt_factura a, vt_clientes b � � 			 WHERE  �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa� � 			 union all �? �9 			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �; �5 			 a.num_recibo AS numero, a.TotalValores importe , � � 			 ?m.tip_b AS tipo, � � 			 ' ' AS referencia,� � 			 a.IdMoneda �) �# 			 FROM vt_pagos a, vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �D �> 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� � 	  	  union all�' �! 	  	  	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �4 �. 			 ?m.tip_a AS tipo, 'Contado' AS referencia,� � 			 a.IdMoneda �+ �% 			 FROM vt_factura a, vt_clientes b � �
 			 WHERE �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�_ �Y 			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)�& �  			 			 and a.numero_Ref is null� �  � � ���	 M(� ��! � 	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda�+ �% 			 FROM vt_factura a, vt_clientes b � � 			 WHERE  �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�[ �U 			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)� � 			 union all �? �9 			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �; �5 			 a.num_recibo AS numero, a.TotalValores importe , � � 			 ?m.tip_b AS tipo, � � 			 ' ' AS referencia,� � 			 a.IdMoneda �) �# 			 FROM vt_pagos a, vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �D �> 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� � � �! ��C � � vt_restadocuenta� �� F� �o� vt_restadocuentaQ� X�� vfactQ�  �� � �� � � � �� 	�X�� vPagoQ�  �� � �� � � � �� 	��C�� � �C�� � ��Q� �� ��� � ��� ��� ��� ��� ��� ��� �Q� �� � �Q� �� � ���� � ��� � ��� ���� xDetalle� F� � U  STRSQL
 WIDCLIENTE	 IDCLIENTE DATOS CMDSQL SQL SAL_FACT VFACT IMPORTE TIP_A TIP_B DETALLE	 SICONTADO VT_RESTADOCUENTA ANTERIOR FECHA R	 RAZSOCIAL IDCOMPROBANTE NUMERO TIPO
 REFERENCIA DEC TOTAL IDMONEDA F VPAGO C XDETALLE
  �  � U  SETEO Init,     �� BeforeOpenTables    ��1 � � A � � �!�a���a A br� � �q�a�a A b�QB� �1�QQAq�Qq�QQ��qQ�q�AQ�Qq��aA � � �1�QQAq��Qq�QQ��qQ�a A A � A� 1�A�Qq�A�q���Q�qA�Qq1AQ�Qq��aa A � � 1�A�Qq��A�q���Q�qA�A B B q �1r : q 2                       �#     �   $  $$  $   )   $                                                                                    %ORIENTATION=0
PAPERSIZE=1
COLOR=1
                         Arial                                                         xDetalle.idcliente                                            xDetalle.IdMoneda                                             
"xDetalle"                                                    debe                                                          iif(tipo = 'A', total, 0)                                     0                                                             haber                                                         iif(tipo='B', xDetalle.total,0)                               0                                                             saldo                                                         debe -haber                                                   0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         !"Extracto de Cuenta de Clientes "                             Arial                                                         oApp.Nombreempresa                                            Arial                                                         
"Per�odo:"                                                    Arial                                                         1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                           Arial                                                         "Debe"                                                        Arial                                                         "Haber"                                                       Arial                                                         "Saldo"                                                       Arial                                                         	"Fecha
"                                                     Arial                                                         "Cpbte."                                                      Arial                                                         "Referencia"                                                  Arial                                                         
"Cliente:"                                                    Arial                                                         &alltrim(idCliente) + " - " + razSocial                                                                                      Arial                                                         "IdMoneda: ", Idmoneda                                        Arial                                                         "Saldo Anterior"                                              Arial                                                         .f.                                                           anterior                                                      "999,999,999,999"                                             Arial                                                         .f.                                                           anterior                                                      "999,999,999.99"                                                                                                            Arial                                                         .f.                                                           fecha                                                         Arial                                                         /alltrim(idcomprobante),"-",alltrim(str(numero))               Arial                                                         
referencia                                                    Arial                                                         debe                                                          "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      debe                                                          "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       haber                                                         "@Z 999,999,999.99"                                                                                                         Arial                                                         dec > 0                                                       haber                                                         "@Z 999,999,999,999"                                                                                                        Arial                                                         dec <= 0                                                      saldo                                                         "999,999,999,999"                                             Arial                                                         dec <= 0                                                      saldo                                                         "@Z 999,999,999.99"                                           Arial                                                         dec > 0                                                       "Saldos del Periodo"                                                                                                        Arial                                                         debe                                                          "@Z 999,999,999.99"                                           Arial                                                         dec > 0                                                       debe                                                          "@Z 999,999,999,999"                                          Arial                                                         dec <= 0                                                      haber                                                         "@Z 999,999,999,999"                                          Arial                                                         dec = 0                                                       haber                                                         "@Z 999,999,999.99"                                           Arial                                                         dec > 0                                                       saldo                                                         "999,999,999,999"                                             Arial                                                         dec <= 0                                                      saldo                                                         "@Z 999,999,999,999.99"                                                                                                     Arial                                                         dec > 0                                                       'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         dataenvironment                                               Top = 144
Left = -42
Width = 792
Height = 483
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
                                                              $/PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
LOCAL strsql,widcliente
*Set Step On 
*widcliente=m.idcliente

*!*	If Empty(m.idcliente)
*!*		Messagebox('Ingrese Codigo Cliente',0,'Futura')
*!*		Return
*!*	Endif

IF EMPTY(m.Idcliente)
	m.IdCliente=null
ENDIF
	

SET DATABASE TO DATOS
TEXT TO cmdSQL noshow
		SELECT sum(TotalFactura) AS importe,
		 a.IdMoneda, a.Idcliente  
		 FROM vt_factura a
		 WHERE  (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
		 and a.IdEmpresa = ?oApp.Empresa
		 and a.fecha < ?m.dfecha
		 and exists(Select IdFactura from vt_forma_pago f where f.IdFactura = a.IdFactura)
		 group by a.IdMoneda, a.Idcliente  

ENDTEXT
 
=sql(cmdSQL , 'vfact')

m.sal_fact = NVL(vfact.importe,0)

m.sal_fact = 0 

TEXT TO cmdSQL noshow
	SELECT sum(b.importe_pag) importe,
	a.IdMoneda, a.IdCliente  
	FROM vt_pagos a, vt_det_pagos b 
	WHERE a.idpago = b.idpago 
	and a.fecha < ?m.dfecha 
	AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
	and a.IdEmpresa = ?oApp.Empresa
	group by a.IdMoneda,a.IdCliente  

ENDTEXT

=sql(cmdSQL, 'vpago')
*m.sal_pago = Nvl(vpago.importe,0)
*!*	IF USED('vfact')
*!*		USE IN 'vfact'
*!*	ENDIF
*!*	IF USED('vpago')
*!*		USE IN 'vpago'
*!*	ENDIF
m.tip_a = 'A'
m.tip_b = 'B'

*!*	*****************
*!*	SET DATABASE TO DATOS 
*!*	strsql =  'SELECT sum(a.exenta+a.gravada+a.iva) AS importe '+;
*!*			 'FROM vt_factura a '+;
*!*			 'WHERE  a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa'+;
*!*			 ' and a.fecha < ?m.dfecha  '
*!*	=sql(strsql, 'vfact')

*!*	m.sal_fact = NVL(vfact.importe,0)

*!*	strsql =  'SELECT sum(b.importe_pag) importe  '+;
*!*			 'FROM vt_pagos a, vt_det_pagos b '+;
*!*			 'WHERE a.idpago = b.idpago '+;
*!*			 ' and a.fecha < ?m.dfecha '+;
*!*			 ' AND a.idcliente = ?m.idcliente and a.IdEmpresa = ?oApp.Empresa '
*!*	=sql(strsql, 'vpago')
*!*	m.sal_pago = Nvl(vpago.importe,0)
*!*	IF USED('vfact')
*!*		USE IN 'vfact'
*!*	ENDIF
*!*	IF USED('vpago')
*!*		USE IN 'vpago'
*!*	ENDIF
*!*	m.tip_a = 'A'
*!*	m.tip_b = 'B'
*************


IF m.Detalle='CD'

	IF m.SIContado='S'
	
	TEXT TO cmdSQL noshow
	SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a inner join vt_clientes b 
			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 WHERE 
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 union 
			 SELECT  a.fecha, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , 
			 ?m.tip_b AS tipo, 
			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
			 vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  
		  	 and a.IdEmpresa = ?oApp.Empresa
	  	  	 union all
	  	  	 SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, 
			 ?m.tip_b AS tipo, 'Contado' AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a, vt_clientes b 
			 WHERE  
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 			 and a.numero_Ref is null
	ENDTEXT			 			 
	ELSE
TEXT TO cmdSQL noshow
	SELECT  a.fecha, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a inner join vt_clientes b 
			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 WHERE 
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 union 
			 SELECT  a.fecha, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , 
			 ?m.tip_b AS tipo, 
			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
			 vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  
		  	 and a.IdEmpresa = ?oApp.Empresa

	ENDTEXT
	ENDIF





ELSE
	IF m.SIContado='S'
	
	TEXT TO cmdSQL noshow
	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda
			 FROM vt_factura a, vt_clientes b 
			 WHERE  
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 union all 
			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, a.TotalValores importe , 
			 ?m.tip_b AS tipo, 
			 ' ' AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a, vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
	  	  and a.IdEmpresa = ?oApp.Empresa
	  	  union all
	  	  	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, 'Contado' AS referencia,
			 a.IdMoneda 
			 FROM vt_factura a, vt_clientes b 
			 WHERE 
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 			 and a.numero_Ref is null

	ENDTEXT
	ELSE
		TEXT TO cmdSQL noshow
	SELECT  a.fecha,IdPago=0, 
			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, 
			 TotalFactura AS importe, 
			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,
			 a.IdMoneda
			 FROM vt_factura a, vt_clientes b 
			 WHERE  
			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 and a.IdEmpresa = ?oApp.Empresa
			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)
			 union all 
			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, 
			 a.tip_reci AS idcomprobante, 
			 a.num_recibo AS numero, a.TotalValores importe , 
			 ?m.tip_b AS tipo, 
			 ' ' AS referencia,
			 a.IdMoneda 
			 FROM vt_pagos a, vt_clientes d 
			 WHERE 
			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
			 and a.fecha between ?m.dfecha  and ?m.hfecha 
			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) 
	  	  and a.IdEmpresa = ?oApp.Empresa
	ENDTEXT
	
	ENDIF
	
ENDIF



	
 = sql(cmdSQL, 'vt_restadocuenta')
 SELECT vt_restadocuenta

 
 
*!*	 	TEXT TO cmdSQL noshow
*!*				 SELECT  a.IdPago, a.fecha, a.idcliente, d.razsocial, 
*!*				 a.tip_reci AS idcomprobante, 
*!*				 a.num_recibo AS numero, b.importe_pag importe , 
*!*				 ?m.tip_b AS tipo, 
*!*				 'Fact. '  +STR(b.numero) AS referencia,
*!*				 a.IdMoneda 
*!*				 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  
*!*				 vt_clientes d 
*!*				 WHERE 
*!*				 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa
*!*				 and a.fecha between ?m.dfecha  and ?m.hfecha 
*!*				 AND a.idcliente = ?m.idcliente 
*!*		  	  and a.IdEmpresa = ?oApp.Empresa

*!*		ENDTEXT
* = sql(cmdSQL, 'cDetalle')	


 SELECT  nvl(f.Importe,0)-NVL(c.Importe,0) anterior, fecha, ;
		  r.idcliente,razsocial, idcomprobante,  numero, ;
		  tipo, referencia,;
		  0 as DEC ,;
		  r.importe total, ;
	      r.IdMoneda;
		  from vt_restadocuenta r ;
		  LEFT JOIN vfact f ON r.IdMoneda=f.IdMoneda AND r.idcliente= f.idcliente;
		  LEFT JOIN vPago c ON r.IdMoneda=c.IdMoneda  AND r.idcliente= c.idcliente; 	
  	ORDER BY r.IdCliente,r.IdMoneda, fecha INTO CURSOR xDetalle

SELECT xDetalle

*!*	SELECT cDetalle
*!*	INDEX on IdPago TAG Idpago


*!*	SELECT xDetalle
*!*	SET RELATION TO idPago INTO  cDetalle 				 

ENDPROC
               =���    $   $                         ��   %   '      �  �   O          �  U  
  �  � U  SETEO� ��  � � %�C�� ���) � T�� ���� � G(� DATOS�	 M(� ��, �& 		SELECT sum(TotalFactura) AS importe,�" � 		 a.IdMoneda, a.Idcliente  � � 		 FROM vt_factura a�F �@ 		 WHERE  (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �( �" 		 and a.IdEmpresa = ?oApp.Empresa�  � 		 and a.fecha < ?m.dfecha�Z �T 		 and exists(Select IdFactura from vt_forma_pago f where f.IdFactura = a.IdFactura)�+ �% 		 group by a.IdMoneda, a.Idcliente  � �  � � ��C � � vfact� �� T�� �C� � � ��� T�� �� ��	 M(� ��) �# 	SELECT sum(b.importe_pag) importe,�  � 	a.IdMoneda, a.IdCliente  �' �! 	FROM vt_pagos a, vt_det_pagos b �! � 	WHERE a.idpago = b.idpago � � 	and a.fecha < ?m.dfecha �A �; 	AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �& �  	and a.IdEmpresa = ?oApp.Empresa�( �" 	group by a.IdMoneda,a.IdCliente  � �  � � ��C � � vpago� �� T��	 �� A�� T��
 �� B�� %��� � CD��9� %��� � S���	 M(� �� � 	SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda �5 �/ 			 FROM vt_factura a inner join vt_clientes b �D �> 			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa� �
 			 WHERE �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa� �
 			 union �5 �/ 			 SELECT  a.fecha, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �Q �K 			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , � � 			 ?m.tip_b AS tipo, �U �O 			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,� � 			 a.IdMoneda �M �G 			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  � � 			 vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �E �? 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  �+ �% 		  	 and a.IdEmpresa = ?oApp.Empresa� � 	  	  	 union all� � 	  	  	 SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �Q �K 			 ISNULL(a.exenta,0) + ISNULL(a.gravada,0) + ISNULL(a.iva,0) AS importe, �4 �. 			 ?m.tip_b AS tipo, 'Contado' AS referencia,� � 			 a.IdMoneda �+ �% 			 FROM vt_factura a, vt_clientes b � � 			 WHERE  �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�_ �Y 			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)�& �  			 			 and a.numero_Ref is null� � �5�	 M(� �� � 	SELECT  a.fecha, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda �5 �/ 			 FROM vt_factura a inner join vt_clientes b �D �> 			 on a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa� �
 			 WHERE �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�[ �U 			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)� �
 			 union �5 �/ 			 SELECT  a.fecha, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �Q �K 			 a.num_recibo AS numero, ISNULL(b.importe_pag,a.TotalValores) importe , � � 			 ?m.tip_b AS tipo, �U �O 			 'Fact. '  +LTRIM(STR(b.numero)) +'-' + LTRIM(STR(b.Cuota))   AS referencia,� � 			 a.IdMoneda �M �G 			 FROM vt_pagos a left join vt_det_pagos b on a.idpago = b.idpago ,  � � 			 vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �E �? 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null)  �+ �% 		  	 and a.IdEmpresa = ?oApp.Empresa� �  � � � ��� %��� � S���	 M(� ��! � 	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda�+ �% 			 FROM vt_factura a, vt_clientes b � � 			 WHERE  �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa� � 			 union all �? �9 			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �; �5 			 a.num_recibo AS numero, a.TotalValores importe , � � 			 ?m.tip_b AS tipo, � � 			 ' ' AS referencia,� � 			 a.IdMoneda �) �# 			 FROM vt_pagos a, vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �D �> 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� � 	  	  union all�' �! 	  	  	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �4 �. 			 ?m.tip_a AS tipo, 'Contado' AS referencia,� � 			 a.IdMoneda �+ �% 			 FROM vt_factura a, vt_clientes b � �
 			 WHERE �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�_ �Y 			 and not exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)�& �  			 			 and a.numero_Ref is null� �  � � ���	 M(� ��! � 	SELECT  a.fecha,IdPago=0, �@ �: 			 a.idcliente, b.razsocial, a.idcomprobante,  a.numero, �# � 			 TotalFactura AS importe, �J �D 			 ?m.tip_a AS tipo, dbo.vt_Descripcion(a.IdFactura) AS referencia,� � 			 a.IdMoneda�+ �% 			 FROM vt_factura a, vt_clientes b � � 			 WHERE  �@ �: 			 (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �E �? 			 and a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �) �# 			 and a.IdEmpresa = ?oApp.Empresa�[ �U 			 and exists(Select IdFactura from vt_forma_pago f where a.IdFactura = f.IdFactura)� � 			 union all �? �9 			 SELECT  a.fecha, a.IdPago, a.idcliente, d.razsocial, �' �! 			 a.tip_reci AS idcomprobante, �; �5 			 a.num_recibo AS numero, a.TotalValores importe , � � 			 ?m.tip_b AS tipo, � � 			 ' ' AS referencia,� � 			 a.IdMoneda �) �# 			 FROM vt_pagos a, vt_clientes d � �
 			 WHERE �A �; 			 a.idcliente = d.idcliente and a.IdEmpresa = d.IdEmpresa�7 �1 			 and a.fecha between ?m.dfecha  and ?m.hfecha �D �> 			 AND (a.idcliente = ?m.idcliente or  ?m.idcliente is null) �+ �% 	  	  and a.IdEmpresa = ?oApp.Empresa� � � �! ��C � � vt_restadocuenta� �� F� �o� vt_restadocuentaQ� X�� vfactQ�  �� � �� � � � �� 	�X�� vPagoQ�  �� � �� � � � �� 	��C�� � �C�� � ��Q� �� ��� � ��� ��� ��� ��� ��� ��� �Q� �� � �Q� �� � ���� � ��� � ��� ���� xDetalle� F� � U  STRSQL
 WIDCLIENTE	 IDCLIENTE DATOS CMDSQL SQL SAL_FACT VFACT IMPORTE TIP_A TIP_B DETALLE	 SICONTADO VT_RESTADOCUENTA ANTERIOR FECHA R	 RAZSOCIAL IDCOMPROBANTE NUMERO TIPO
 REFERENCIA DEC TOTAL IDMONEDA F VPAGO C XDETALLE BeforeOpenTables,     �� InitA     ��1 q 3 � � A � � �!�a���a A br� � �q�a�a A b�QB� �1�QQAq�Qq�QQ��qQ�q�AQ�Qq��aA � � �1�QQAq��Qq�QQ��qQ�a A A � A� 1�A�Qq�A�q���Q�qA�Qq1AQ�Qq��aa A � � 1�A�Qq��A�q���Q�qA�A B B q �1r 9                       &         A   $$      )   $                                                                              