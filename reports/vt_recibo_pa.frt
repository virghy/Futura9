  2   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Arial                                                         "cRec"                                                        "cRec"                                                        "cRec"                                                        Facturas.num_recibo                                           Arial                                                         Facturas.cliente                                              Arial                                                         Facturas.ruc                                                  Arial                                                         mLetras                                                       Arial                                                         cRec.cFecha                                                   "@D"                                                          Arial                                                         !empty(cRec.cFecha)                                           	cRec.cNro                                                     Arial                                                         cRec.cBanco                                                   Arial                                                         cRec.cImporte                                                 "@Z 999,999,999,999.99"                                       Arial                                                         	cRec.fNro                                                     "@Z"                                                          Arial                                                         cRec.fFecha                                                   "@D"                                                          Arial                                                         !empty(cRec.fFecha)                                           cRec.fImporte                                                 "@Z 999,999,999,999.99"                                       Arial                                                         cRec.fImporte                                                 "@Z 999,999,999,999.99"                                       Arial                                                         day(Facturas.Fecha)                                           "99"                                                          Arial                                                          nombremes(month(Facturas.Fecha))                              Arial                                                         year(Facturas.Fecha)                                          Arial                                                         Facturas.TotalFacturas                                        "@Z 999,999,999,999.99"                                       Arial                                                         "X"                                                           Arial                                                         Facturas.IdMoneda='GS'                                        "X"                                                           Arial                                                         Facturas.IdMoneda='USD'                                       	mEfectivo                                                     "@Z 999,999,999,999.99"                                       Arial                                                         Facturas.TotalFacturas                                        "@Z 999,999,999,999.99"                                       Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                                                   
mPROCEDURE Destroy
RELEASE mLetras


ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init

TEXT TO cmdSQL NOSHOW 
	select a.idpago,a.sucursal,a.num_recibo,a.tip_reci,a.fecha,rtrim(a.idcliente)+'-'+c.razsocial cliente,
	rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador,c.Ruc,
	b.numero,b.idmoneda,b.importe,b.importe_pag,f.fecha fechafact,TotalFacturas,f.Idcomprobante,b.Cuota
	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago 
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa = d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	left join vt_factura f on b.idfactura=f.idfactura 
	where a.IdPago = ?m.IdRecibo
ENDTEXT 
=sql(cmdSQL,'Facturas')
	
*	sql('exec vt_recibo ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
	SELECT Facturas
TEXT TO cmdSQL NOSHOW 
SELECT     v.nrocheque, v.fchcheque, v.importe, b.descripcion AS Banco, tv.tipovalor,IdCobro
FROM         ts_valores_base AS v LEFT OUTER JOIN
                      bs_bancos AS b ON v.idbanco = b.idbanco LEFT OUTER JOIN
                      ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor
                      where v.IdCobro= ?m.IdRecibo

ENDTEXT
=sql(cmdSQL,'Valores')

	
PUBLIC mLetras,mEfectivo



mLetras= ALLTRIM(numeral(int(TotalFacturas)))

m.decimales=IIF(Idmoneda='GS',0,2)
IF m.decimales > 0
	mletras = "DOLARES " + mletras 
	mLetras = mLetras + ' CON ' + SUBSTR(STR(TotalFacturas - INT(TotalFacturas),3,2),2) + '/100'
ELSE 
	mletras = "GUARANIES " + mletras 
ENDIF

	


CREATE CURSOR cRec (fFecha D null,fNro N(13) null,fImporte Y null,cNro char(20) null,cBanco c(20) null,cFecha D null,cImporte Y null)


SELECT Facturas 
i=0
SCAN
	INSERT INTO cRec(fFecha,fNro,fImporte);
	values(Facturas.fechafact,Facturas.Numero,Facturas.Importe)
	i = i + 1 
	IF i>4
		LOOP 
	ENDIF
ENDSCAN 

SELECT Valores
GO TOP IN cRec 
i=0
mEfectivo=0
SCAN 
	IF tipovalor='Cheque'
		IF EOF('cRec')
			APPEND BLANK IN cRec
		endif	
		Replace cNro WITH Valores.nrocheque,;
			cBanco WITH Valores.Banco,;
			cFecha WITH Valores.fchcheque,;
			cImporte WITH Valores.Importe;
			IN cRec
		i = i + 1 
		IF i>4
			LOOP 
		ENDIF
	ENDIF
	
	IF tipovalor='Efectivo'	
		mEfectivo = mEfectivo + Valores.Importe
	ENDIF
		
ENDSCAN 

SELECT Facturas
GO TOP 
*BROWSE


SELECT cRec
*BROWSE

*SELECT cRecibo

m.nro = RECCOUNT()

DO WHILE m.nro<4
	APPEND BLANK
	m.nro = m.nro + 1
ENDDO 


			
		


ENDPROC
                
S���    :
  :
                        ��   %   	      �	  K   ;	          �  U  
  <�  � U  MLETRAS
  �  � U  SETEO�	 M(�  ��m �g 	select a.idpago,a.sucursal,a.num_recibo,a.tip_reci,a.fecha,rtrim(a.idcliente)+'-'+c.razsocial cliente,�T �N 	rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador,c.Ruc,�j �d 	b.numero,b.idmoneda,b.importe,b.importe_pag,f.fecha fechafact,TotalFacturas,f.Idcomprobante,b.Cuota�F �@ 	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago �% � 	and a.idempresa=?oApp.empresa �W �Q 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa�\ �V 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa = d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	left join vt_factura f on b.idfactura=f.idfactura �# � 	where a.IdPago = ?m.IdRecibo� � ��C �  � Facturas� �� F� �	 M(�  ��b �\ SELECT     v.nrocheque, v.fchcheque, v.importe, b.descripcion AS Banco, tv.tipovalor,IdCobro�7 �1 FROM         ts_valores_base AS v LEFT OUTER JOIN�S �M                       bs_bancos AS b ON v.idbanco = b.idbanco LEFT OUTER JOIN�P �J                       ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor�8 �2                       where v.IdCobro= ?m.IdRecibo� �  � � ��C �  � Valores� �� 7� � � T� �CCC� 8� ���# T�� �C� � GS� � � �6�� %��� � ��O� T� �� DOLARES � ��5 T� �� �  CON CC� C� 8��Z�\� /100�� �r� T� ��
 GUARANIES � �� �d h�� cRec�
 � D�� � N����� � Y�� � CHAR����� � C����� � D�� � Y�� F� � T� �� �� ~�V�6 r�� cRec�
 � � ��� � ��� � ��� � �� T� �� ��� %�� ���R� .� � � F� �	 #�	 )� T� �� �� T� �� �� ~�]� %�� � Cheque��(� %�C� cRec+����	 �	 � �; >�	 � ��� � �� ��� � �� ��� � �� ��� � �� T� �� ��� %�� ���$� .� � � %�� � Efectivo��Y� T� �� � � �� � � F� � #)� F�	 � T�� �CN�� +��� ����� � T�� ��� ��� � U  CMDSQL SQL FACTURAS MLETRAS	 MEFECTIVO NUMERAL TOTALFACTURAS	 DECIMALES IDMONEDA CREC FFECHA FNRO FIMPORTE CNRO CBANCO CFECHA CIMPORTE I	 FECHAFACT NUMERO IMPORTE VALORES	 TIPOVALOR	 NROCHEQUE BANCO	 FCHCHEQUE IN NRO Destroy,     �� BeforeOpenTablesC     �� InitX     ��1 q 4 q 2 � �A�aQq���1A �s � !q1�a A �� T21�Q� �A Es � � bA A A r � � � � q1� A �A A A �AA B r Q t � BQ QA 7                       &         M   U         p   b
  	    )   :
                                                             %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Arial                                                         "cRec"                                                        "cRec"                                                        "cRec"                                                        Facturas.num_recibo                                           Arial                                                         Facturas.cliente                                              Arial                                                         Facturas.ruc                                                  Arial                                                         mLetras                                                       Arial                                                         cRec.cFecha                                                   "@D"                                                          Arial                                                         !empty(cRec.cFecha)                                           	cRec.cNro                                                     Arial                                                         cRec.cBanco                                                   Arial                                                         cRec.cImporte                                                 "@Z 999,999,999,999.99"                                       Arial                                                         	cRec.fNro                                                     "@Z"                                                          Arial                                                         cRec.fFecha                                                   "@D"                                                          Arial                                                         !empty(cRec.fFecha)                                           cRec.fImporte                                                 "@Z 999,999,999,999.99"                                       Arial                                                         cRec.fImporte                                                 "@Z 999,999,999,999.99"                                       Arial                                                         day(Facturas.Fecha)                                           "99"                                                          Arial                                                          nombremes(month(Facturas.Fecha))                              Arial                                                         year(Facturas.Fecha)                                          Arial                                                         Facturas.TotalFacturas                                        "@Z 999,999,999,999.99"                                       Arial                                                         "X"                                                           Arial                                                         Facturas.IdMoneda='GS'                                        "X"                                                           Arial                                                         Facturas.IdMoneda='USD'                                       	mEfectivo                                                     "@Z 999,999,999,999.99"                                       Arial                                                         Facturas.TotalFacturas                                        "@Z 999,999,999,999.99"                                       Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                                                   
WPROCEDURE Init

TEXT TO cmdSQL NOSHOW 
	select a.idpago,a.sucursal,a.num_recibo,a.tip_reci,a.fecha,rtrim(a.idcliente)+'-'+c.razsocial cliente,
	rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador,c.Ruc,
	b.numero,b.idmoneda,b.importe,b.importe_pag,f.fecha fechafact,TotalFacturas,f.Idcomprobante,b.Cuota
	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago 
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa = d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	left join vt_factura f on b.idfactura=f.idfactura 
	where a.IdPago = ?m.IdRecibo
ENDTEXT 
=sql(cmdSQL,'Facturas')
	
*	sql('exec vt_recibo ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
	SELECT Facturas
TEXT TO cmdSQL NOSHOW 
SELECT     v.nrocheque, v.fchcheque, v.importe, b.descripcion AS Banco, tv.tipovalor,IdCobro
FROM         ts_valores_base AS v LEFT OUTER JOIN
                      bs_bancos AS b ON v.idbanco = b.idbanco LEFT OUTER JOIN
                      ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor
                      where v.IdCobro= ?m.IdRecibo

ENDTEXT
=sql(cmdSQL,'Valores')

	
PUBLIC mLetras,mEfectivo



mLetras= ALLTRIM(numeral(int(TotalFacturas)))

m.decimales=IIF(Idmoneda='GS',0,2)
IF m.decimales > 0
	mletras = "DOLARES " + mletras 
	mLetras = mLetras + ' CON ' + SUBSTR(STR(TotalFacturas - INT(TotalFacturas),3,2),2) + '/100'
ELSE 
	mletras = "GUARANIES " + mletras 
ENDIF

	


CREATE CURSOR cRec (fFecha D null,fNro N(13) null,fImporte Y null,cNro char(20) null,cBanco c(20) null,cFecha D null,cImporte Y null)


SELECT Facturas 
i=0
SCAN
	INSERT INTO cRec(fFecha,fNro,fImporte);
	values(Facturas.fechafact,Facturas.Numero,Facturas.Importe)
	i = i + 1 
	IF i>4
		LOOP 
	ENDIF
ENDSCAN 

SELECT Valores
GO TOP IN cRec 
i=0
mEfectivo=0
SCAN 
	IF tipovalor='Cheque'
		IF EOF('cRec')
			APPEND BLANK IN cRec
		endif	
		Replace cNro WITH Valores.nrocheque,;
			cBanco WITH Valores.Banco,;
			cFecha WITH Valores.fchcheque,;
			cImporte WITH Valores.Importe;
			IN cRec
		i = i + 1 
		IF i>4
			LOOP 
		ENDIF
	ENDIF
	
	IF tipovalor='Efectivo'	
		mEfectivo = mEfectivo + Valores.Importe
	ENDIF
		
ENDSCAN 

SELECT Facturas
GO TOP 
*BROWSE


SELECT cRec
*BROWSE

*SELECT cRecibo

m.nro = RECCOUNT()

DO WHILE m.nro<4
	APPEND BLANK
ENDDO 

			
		


ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Destroy
RELEASE mLetras


ENDPROC
                                      
<���    #
  #
                        �   %   �      �	  J   &	          �  U  �	 M(�  ��m �g 	select a.idpago,a.sucursal,a.num_recibo,a.tip_reci,a.fecha,rtrim(a.idcliente)+'-'+c.razsocial cliente,�T �N 	rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador,c.Ruc,�j �d 	b.numero,b.idmoneda,b.importe,b.importe_pag,f.fecha fechafact,TotalFacturas,f.Idcomprobante,b.Cuota�F �@ 	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago �% � 	and a.idempresa=?oApp.empresa �W �Q 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa�\ �V 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa = d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	left join vt_factura f on b.idfactura=f.idfactura �# � 	where a.IdPago = ?m.IdRecibo� � ��C �  � Facturas� �� F� �	 M(�  ��b �\ SELECT     v.nrocheque, v.fchcheque, v.importe, b.descripcion AS Banco, tv.tipovalor,IdCobro�7 �1 FROM         ts_valores_base AS v LEFT OUTER JOIN�S �M                       bs_bancos AS b ON v.idbanco = b.idbanco LEFT OUTER JOIN�P �J                       ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor�8 �2                       where v.IdCobro= ?m.IdRecibo� �  � � ��C �  � Valores� �� 7� � � T� �CCC� 8� ���# T�� �C� � GS� � � �6�� %��� � ��O� T� �� DOLARES � ��5 T� �� �  CON CC� C� 8��Z�\� /100�� �r� T� ��
 GUARANIES � �� �d h�� cRec�
 � D�� � N����� � Y�� � CHAR����� � C����� � D�� � Y�� F� � T� �� �� ~�V�6 r�� cRec�
 � � ��� � ��� � ��� � �� T� �� ��� %�� ���R� .� � � F� �	 #�	 )� T� �� �� T� �� �� ~�]� %�� � Cheque��(� %�C� cRec+����	 �	 � �; >�	 � ��� � �� ��� � �� ��� � �� ��� � �� T� �� ��� %�� ���$� .� � � %�� � Efectivo��Y� T� �� � � �� � � F� � #)� F�	 � T�� �CN�� +��� ����� � � U  CMDSQL SQL FACTURAS MLETRAS	 MEFECTIVO NUMERAL TOTALFACTURAS	 DECIMALES IDMONEDA CREC FFECHA FNRO FIMPORTE CNRO CBANCO CFECHA CIMPORTE I	 FECHAFACT NUMERO IMPORTE VALORES	 TIPOVALOR	 NROCHEQUE BANCO	 FCHCHEQUE IN NRO
  �  � U  SETEO
  <�  � U  MLETRAS Init,     �� BeforeOpenTables�    �� Destroy�    ��1 � �A�aQq���1A �s � !q1�a A �� T21�Q� �A Es � � bA A A r � � � � q1� A �A A A �AA B r Q t � BQ A 7 q 2 q 3                       �	     F   
  
  l   H   9
  L
  o    )   #
                                                                              