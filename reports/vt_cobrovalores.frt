  u                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      IdMoneda      	TipoValor      Arial      Arial      Arial      Arial      Arial      "Control de Valores de Cobros"      Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Tipo de Valor:"      Arial      +iif( isnull(m.TipoValor),"Todos",TipoValor)      Arial      "Fecha
Operac.
"      "@I"      Arial      "Fecha 
Valor"      "@I"      Arial      "Nro
Valor"      "@I"      Arial      "Emisor"      Arial      "Recibo"      Arial      	"Cliente"      Arial      	"Importe"      Arial      "Moneda ", IdMoneda      Arial      "Tipo de Valor: ",TipoValor      Arial      Fecha      Arial      	fchcheque      Arial      	nrocheque      Arial      banco      Arial       ALLTRIM(tip_Reci)," ",Num_Recibo      Arial      cliente      Arial      importe      "999,999,999,999"      Arial      "Total ", TipoValor      Arial      importe      "999,999,999,999"      Arial      "Total ",IdMoneda      Arial      importe      "999,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     |PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


If Empty(m.TipoValor )
	Store null To m.TipoValor 
endif
TEXT TO cmdSQL NOSHOW 
SELECT     p.idpago, v.idtipovalor, v.nrocheque, v.idmoneda, v.fchemision, v.fchcheque, v.importe, p.tip_reci, p.num_recibo, p.fecha, rtrim(p.idcliente)+'-'+c.razsocial cliente, 
                      tv.tipovalor, b.descripcion AS Banco
FROM         ts_valores_base AS v INNER JOIN
                      vt_pagos AS p ON v.idcobro = p.idpago INNER JOIN
                      ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor INNER JOIN
                      vt_clientes AS c ON p.idempresa = c.IdEmpresa AND p.idcliente = c.IdCliente LEFT OUTER JOIN
                      bs_bancos AS b ON v.idbanco = b.idbanco
 	WHERE    P.fecha between ?m.dfecha and ?m.hfecha 
	AND (convert(char(10),v.idtipovalor) =?m.TipoValor or ?m.TipoValor is null)
	and v.IdEmpresa=?oApp.Empresa 
	ORDER BY v.idmoneda, tv.tipovalor, p.fecha

ENDTEXT 
=sql(cmdSQL,'vt_rrecibo')
	
*	sql('exec vt_recibo ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
	SELECT vt_rrecibo
	
	

ENDPROC
     ����    �  �                        ��   %   /      �     W          �  U  
  �  � U  SETEO� %�C��  ��� � J���(��  � �	 M(� ��� �� SELECT     p.idpago, v.idtipovalor, v.nrocheque, v.idmoneda, v.fchemision, v.fchcheque, v.importe, p.tip_reci, p.num_recibo, p.fecha, rtrim(p.idcliente)+'-'+c.razsocial cliente, �@ �:                       tv.tipovalor, b.descripcion AS Banco�2 �, FROM         ts_valores_base AS v INNER JOIN�L �F                       vt_pagos AS p ON v.idcobro = p.idpago INNER JOIN�[ �U                       ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor INNER JOIN�w �q                       vt_clientes AS c ON p.idempresa = c.IdEmpresa AND p.idcliente = c.IdCliente LEFT OUTER JOIN�C �=                       bs_bancos AS b ON v.idbanco = b.idbanco�9 �3  	WHERE    P.fecha between ?m.dfecha and ?m.hfecha �R �L 	AND (convert(char(10),v.idtipovalor) =?m.TipoValor or ?m.TipoValor is null)�% � 	and v.IdEmpresa=?oApp.Empresa �1 �+ 	ORDER BY v.idmoneda, tv.tipovalor, p.fecha� �  � � ��C � �
 vt_rrecibo� �� F� � U 	 TIPOVALOR CMDSQL SQL
 VT_RRECIBO BeforeOpenTables,     �� InitA     ��1 q 2 � A � �!��q1�!Qa A �s 4                       $         ?   q      )   �                  