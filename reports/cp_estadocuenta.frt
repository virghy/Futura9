  )�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Enviar a OneNote 2013
OUTPUT=nul:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
      ?  )  winspool  Enviar a OneNote 2013  nul:                       �Enviar a OneNote 2013           � /    �4d   ,  ,  A4                                                            ����                DINU" �   ���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         �   SMTJ     �                                                                                                                                                                                       Arial      idmoneda      debe      :iif(tipo = 'B', nvl(total,0), 1000000000.00-1000000000.00)      0      haber      6iif(tipo='A',nvl(total,0),1000000000.00-1000000000.00)      0      saldo      haber - debe      XESTADO.anterior      dec      IIF(Idmoneda='GS',0,2)      0      Arial      Arial      Arial      Arial      Arial      Arial      !"Estado de Cuenta de Proveedores"             Arial      empresa             Arial      ""Desde " ,m.dfecha," al ",m.hfecha             Arial      
"Per�odo:"      Arial      "Referencia"      Arial      "Debe"      Arial      "Haber"      Arial      	"Fecha
"      Arial      "Cpbte."      Arial      "Nro.Compra"      Arial      "Saldo"      Arial      "Proveedor:"      Arial      $alltrim(idproveedor) + " - " + razon             Arial      "Saldo Anterior"             Arial      saldo      "9,999,999,999.99"             Arial      dec > 0      saldo      "999,999,999,999"      Arial      dec <= 0      idmoneda             Arial      fecha      Arial      -alltrim(idcomprobante), " ", facturaproveedor      Arial      numero      Arial      -iif(tipo='A','Factura ','Pago ') + referencia      Arial      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "999,999,999.99"      Arial      dec > 0      haber      "@Z 999,999,999,999"      Arial      dec <= 0      haber      "999,999,999.99"      Arial      dec > 0      saldo      "999,999,999,999"      Arial      dec=0      saldo      "9,999,999.99"      Arial      dec>0      debe      "999,999,999,999"             Arial      dec = 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      haber      "999,999,999,999"             Arial      dec = 0      saldo      "@Z 999,999,999,999.99"             Arial      dec > 0      saldo      "999,999,999,999.99"      Arial      dec>0      saldo      "999,999,999,999"      Arial      dec=0      "Saldos del Periodo"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      `Top = 156
Left = 89
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     YPROCEDURE Init
LOCAL strsql
SET DATABASE TO DATOS
strsql='SELECT sum(Total) AS importe ' +;
		'FROM cp_factura a ' + ;
		 'WHERE  a.idproveedor = ?m.idproveedor ' + ;
		 ' and a.fecha <= ?m.dfecha  and a.IdEmpresa=?oApp.Empresa and a.IdCuentaPago is null' 
		 
=sql(strsql, 'anterior')

m.sal_fact = nvl(anterior.importe,0)

strsql =  'SELECT sum(Isnull(b.importe_pag,0)) as importe  '+;
		 'FROM cp_pagos_base a, cp_pagosdet_base b '+;
		 'WHERE a.idpago = b.idpago '+;
		 ' and a.fecha <= ?m.dfecha '+;
		 ' AND a.idproveedor = ?m.idproveedor and a.IdEmpresa=?oApp.Empresa'
=sql(strsql, 'vpago')
m.sal_pago = vpago.importe
IF USED('anterior')
	USE IN 'anterior'
ENDIF
IF USED('vpago')
	USE IN 'vpago'
ENDIF
m.tip_a = 'A'
m.tip_b = 'B'
m.tip_pag = 'PA'

*Faltaba el filtro de empresa
*VG 30-01-06

TEXT TO strsql  noshow
SELECT  a.fecha, 
	  a.idproveedor, b.razon, a.idcomprobante,  a.idfactura numero,a.facturaproveedor, 
	   Total AS importe, 
	   ?m.tip_a AS tipo, referencia AS referencia,a.idmoneda,0 as valorizado 
	   FROM cp_factura a, cp_proveedor b WHERE  a.idproveedor = ?m.idproveedor 
	   and a.idproveedor = b.idproveedor and a.IdEmpresa=b.IdEmpresa and b.IdEmpresa=?oApp.Empresa
	   and convert(varchar(12),a.fecha,103) between ?m.dfecha  and ?m.hfecha 
	   Union
SELECT  a.fecha, 
	  a.idproveedor, b.razon, a.idcomprobante,  a.idfactura numero,a.facturaproveedor, 
	   Total AS importe, 
	   ?m.tip_b AS tipo, ISNULL('CH '+ LEFT(NroCheque,9),'Efectivo') AS referencia,a.idmoneda,0 as valorizado 
	   FROM cp_factura a inner join  cp_proveedor b 
	   ON a.idproveedor = b.idproveedor and a.IdEmpresa=b.IdEmpresa 
	   inner join cp_Condicion cp on a.IdCondicion = cp.IdCondicion and a.IdEmpresa=cp.IdEmpresa
	   WHERE  a.idproveedor = ?m.idproveedor 
	   and b.IdEmpresa=?oApp.Empresa
	   and convert(varchar(12),a.fecha,103) between ?m.dfecha  and ?m.hfecha 
	   and cp.Plazo = 0	   
	   union 
SELECT  a.fecha, a.idproveedor, d.razon, 
	    ?m.tip_pag AS idcomprobante,  a.idpago AS numero,SPACE(15) as facturaproveedor, b.importe , 
	    ?m.tip_b AS tipo,  ?m.tip_pag+b.facturaproveedor AS referencia,b.idmoneda,importe_pag 
	    FROM cp_pagos_base a, cp_pagosdet_base b, cp_proveedor d 
	    WHERE a.idpago = b.idpago  
	    and a.idproveedor = d.idproveedor and a.IdEmpresa=d.IdEmpresa and d.IdEmpresa=?oApp.Empresa 
	    and convert(varchar(12),a.fecha,103) between ?m.dfecha  and ?m.hfecha 
	    AND a.idproveedor = ?m.idproveedor order by a.IdProveedor,a.IdMoneda,a.Fecha	
ENDTEXT

		 
 = sql(strsql, 'cp_restadocuenta')
SELECT cp_restadocuenta
*brow
SELECT   m.sal_fact-m.sal_pago anterior, fecha, ;
		  idproveedor,razon, idcomprobante,  numero, ;
		  facturaproveedor,tipo, referencia,; 
		  idmoneda,valorizado,importe total ;
		  from cp_restadocuenta  ;
  	ORDER BY idmoneda,fecha,tipo INTO CURSOR XESTADO
 SELECT XESTADO
*	GROUP BY  tipo,idmoneda,numero ;
*brow

*!*		GROUP BY  fecha, ;
*!*			  idproveedor,razon,idcomprobante, numero, ;
*!*			  facturaproveedor,tipo, referencia ;
				 


ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     ����    �  �                        �T   %   �      G  7   �          �  U  �
 ��  � G(� DATOS�� T�  �� SELECT sum(Total) AS importe � FROM cp_factura a �& WHERE  a.idproveedor = ?m.idproveedor �S  and a.fecha <= ?m.dfecha  and a.IdEmpresa=?oApp.Empresa and a.IdCuentaPago is null�� ��C �  � anterior� �� T�� �C� � � ���� T�  ��0 SELECT sum(Isnull(b.importe_pag,0)) as importe  �) FROM cp_pagos_base a, cp_pagosdet_base b � WHERE a.idpago = b.idpago �  and a.fecha <= ?m.dfecha �A  AND a.idproveedor = ?m.idproveedor and a.IdEmpresa=?oApp.Empresa�� ��C �  � vpago� �� T�� �� � �� %�C� anterior���B� Q�� anterior�� � %�C� vpago���i� Q�� vpago�� � T�� �� A�� T��	 �� B�� T��
 �� PA��	 M(�  �� � SELECT  a.fecha, �Z �T 	  a.idproveedor, b.razon, a.idcomprobante,  a.idfactura numero,a.facturaproveedor, � � 	   Total AS importe, �P �J 	   ?m.tip_a AS tipo, referencia AS referencia,a.idmoneda,0 as valorizado �R �L 	   FROM cp_factura a, cp_proveedor b WHERE  a.idproveedor = ?m.idproveedor �e �_ 	   and a.idproveedor = b.idproveedor and a.IdEmpresa=b.IdEmpresa and b.IdEmpresa=?oApp.Empresa�P �J 	   and convert(varchar(12),a.fecha,103) between ?m.dfecha  and ?m.hfecha � �	 	   Union� � SELECT  a.fecha, �Z �T 	  a.idproveedor, b.razon, a.idcomprobante,  a.idfactura numero,a.facturaproveedor, � � 	   Total AS importe, �q �k 	   ?m.tip_b AS tipo, ISNULL('CH '+ LEFT(NroCheque,9),'Efectivo') AS referencia,a.idmoneda,0 as valorizado �7 �1 	   FROM cp_factura a inner join  cp_proveedor b �G �A 	   ON a.idproveedor = b.idproveedor and a.IdEmpresa=b.IdEmpresa �c �] 	   inner join cp_Condicion cp on a.IdCondicion = cp.IdCondicion and a.IdEmpresa=cp.IdEmpresa�0 �* 	   WHERE  a.idproveedor = ?m.idproveedor �' �! 	   and b.IdEmpresa=?oApp.Empresa�P �J 	   and convert(varchar(12),a.fecha,103) between ?m.dfecha  and ?m.hfecha � � 	   and cp.Plazo = 0	   � �
 	   union �/ �) SELECT  a.fecha, a.idproveedor, d.razon, �g �a 	    ?m.tip_pag AS idcomprobante,  a.idpago AS numero,SPACE(15) as facturaproveedor, b.importe , �a �[ 	    ?m.tip_b AS tipo,  ?m.tip_pag+b.facturaproveedor AS referencia,b.idmoneda,importe_pag �D �> 	    FROM cp_pagos_base a, cp_pagosdet_base b, cp_proveedor d �& �  	    WHERE a.idpago = b.idpago  �g �a 	    and a.idproveedor = d.idproveedor and a.IdEmpresa=d.IdEmpresa and d.IdEmpresa=?oApp.Empresa �Q �K 	    and convert(varchar(12),a.fecha,103) between ?m.dfecha  and ?m.hfecha �X �R 	    AND a.idproveedor = ?m.idproveedor order by a.IdProveedor,a.IdMoneda,a.Fecha	� �! ��C �  � cp_restadocuenta� �� F� �� o� cp_restadocuenta��� �� �Q� �� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� �Q� ��� ��� ��� ���� XESTADO� F� � U  STRSQL DATOS SQL SAL_FACT ANTERIOR IMPORTE SAL_PAGO VPAGO TIP_A TIP_B TIP_PAG CP_RESTADOCUENTA M FECHA IDPROVEEDOR RAZON IDCOMPROBANTE NUMERO FACTURAPROVEEDOR TIPO
 REFERENCIA IDMONEDA
 VALORIZADO TOTAL XESTADO
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 q � �r�a!q!A A� A � q��!Q� q��qq1q��qAaq�A q �q ; q 1                            5   F  N  U    )   �                  