  0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      fecha      
num_recibo      Arial      Arial      Arial      Arial      "Control de Recibos"             Arial      empresa             Arial      sucursal             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Fecha
"      Arial      "ReciboNro."      Arial      	"Cliente"      Arial      
"Cobrador"      Arial      "FacturaNro."      Arial      	"Fecha
"      Arial      "Total"      Arial      fecha             Arial      
num_recibo             Arial      cliente             Arial      cobrador             Arial      idmoneda             Arial      importe      "999,999,999,999"             Arial      numero             Arial      	fechafact             Arial      "Total Recibo"             Arial      idmoneda             Arial      importe      "999,999,999,999"             Arial      "Total "+ dtoc(fecha)             Arial      idmoneda             Arial      importe      "999,999,999,999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total General"      Arial      importe      "999,999,999,999"             Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     3PROCEDURE Init


If Empty(m.idcobrador)
	Store null To m.idcobrador
endif
TEXT TO cmdSQL NOSHOW 
	select a.idpago,a.sucursal,a.num_recibo,a.tip_reci,a.fecha,rtrim(a.idcliente)+'-'+c.razsocial cliente,
	rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador,
	b.numero,b.idmoneda,b.importe,b.importe_pag,f.fecha fechafact
	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago 
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa = d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	left join vt_factura f on b.idfactura=f.idfactura 
	where a.fecha between ?m.dfecha and ?m.hfecha and 
	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)
	order by a.fecha,a.num_recibo
ENDTEXT 
=sql(cmdSQL,'vt_rrecibo')
	
*	sql('exec vt_recibo ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
	SELECT vt_rrecibo
	
	

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     ����    �  �                        �   %   �      =               �  U  } %�C��  ��� � J���(��  � �	 M(� ��m �g 	select a.idpago,a.sucursal,a.num_recibo,a.tip_reci,a.fecha,rtrim(a.idcliente)+'-'+c.razsocial cliente,�N �H 	rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador,�D �> 	b.numero,b.idmoneda,b.importe,b.importe_pag,f.fecha fechafact�F �@ 	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago �% � 	and a.idempresa=?oApp.empresa �W �Q 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa�\ �V 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa = d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	left join vt_factura f on b.idfactura=f.idfactura �9 �3 	where a.fecha between ?m.dfecha and ?m.hfecha and �< �6 	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)�$ � 	order by a.fecha,a.num_recibo� � ��C � �
 vt_rrecibo� �� F� � U 
 IDCOBRADOR CMDSQL SQL
 VT_RRECIBO
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � A � ��AaQq�����AA �s 5 q 1                       �           (      )   �                  