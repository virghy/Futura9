   �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Arial                                                         
idcobrador                                                    !"Control de Recibos por Cobrador"                                                                                           Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         fecha                                                                                                                       Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         sucursal                                                                                                                    Arial                                                         
"Sucursal"                                                    Arial                                                         "Total"                                                       Arial                                                         "Total General"                                               Arial                                                         "ReciboNro."                                                  Arial                                                         "Total Cobrador"                                                                                                            Arial                                                         idmoneda                                                                                                                    Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         
num_recibo                                                                                                                  Arial                                                         cobrador                                                                                                                    Arial                                                         
"Cobrador"                                                    Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         idmoneda                                                                                                                    Arial                                                         idmoneda                                                                                                                    Arial                                                         cliente                                                                                                                     Arial                                                         	"Cliente"                                                     Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                                                   �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


If Empty(m.idcobrador)
	Store null To m.idcobrador
endif
TEXT TO cmdSQL noshow
	select a.idpago,a.sucursal,a.num_recibo,a.fecha, 
	rtrim(a.idcliente)+'-'+c.razsocial cliente,
	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, 
	b.idmoneda,sum(b.importe)as importe 
	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago 
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	where a.fecha between ?m.dfecha and ?m.hfecha and 
	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)
	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,
	a.idcobrador,a.idcobrador,e.nombre,e.apellido,b.idmoneda
	order by a.idcobrador,a.fecha,a.num_recibo
ENDTEXT
	
*	sql('exec vt_recibocobrador ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
=	sql(cmdSQL,'vt_rrecibo')
	SELECT vt_rrecibo
	
	
	

ENDPROC
                                                    ���    �  �                        �   %   ;      �     c          �  U  
  �  � U  SETEO� %�C��  ��� � J���(��  � �	 M(� ��8 �2 	select a.idpago,a.sucursal,a.num_recibo,a.fecha, �2 �, 	rtrim(a.idcliente)+'-'+c.razsocial cliente,�\ �V 	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, �+ �% 	b.idmoneda,sum(b.importe)as importe �F �@ 	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago �% � 	and a.idempresa=?oApp.empresa �V �P 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa�[ �U 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	where a.fecha between ?m.dfecha and ?m.hfecha and �< �6 	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)�Q �K 	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,�? �9 	a.idcobrador,a.idcobrador,e.nombre,e.apellido,b.idmoneda�1 �+ 	order by a.idcobrador,a.fecha,a.num_recibo� � ��C � �
 vt_rrecibo� �� F� � U 
 IDCOBRADOR CMDSQL SQL
 VT_RRECIBO BeforeOpenTables,     �� InitA     ��1 q 2 � A � �!��aQa�����A �q 5                       $         ?   ~      )   �                                                                         %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Arial                                                         
idcobrador                                                    !"Control de Recibos por Cobrador"                                                                                           Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         fecha                                                                                                                       Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         sucursal                                                                                                                    Arial                                                         
"Sucursal"                                                    Arial                                                         "Total"                                                       Arial                                                         "Total General"                                               Arial                                                         "ReciboNro."                                                  Arial                                                         "Total Cobrador"                                                                                                            Arial                                                         idmoneda                                                                                                                    Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         
num_recibo                                                                                                                  Arial                                                         cobrador                                                                                                                    Arial                                                         
"Cobrador"                                                    Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         idmoneda                                                                                                                    Arial                                                         idmoneda                                                                                                                    Arial                                                         cliente                                                                                                                     Arial                                                         	"Cliente"                                                     Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                                                   �PROCEDURE Init


If Empty(m.idcobrador)
	Store null To m.idcobrador
endif
TEXT TO cmdSQL noshow
	select a.idpago,a.sucursal,a.num_recibo,a.fecha, 
	rtrim(a.idcliente)+'-'+c.razsocial cliente,
	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, 
	b.idmoneda,sum(b.importe)as importe 
	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago 
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	where a.fecha between ?m.dfecha and ?m.hfecha and 
	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)
	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,
	a.idcobrador,a.idcobrador,e.nombre,e.apellido,b.idmoneda
	order by a.idcobrador,a.fecha,a.num_recibo
ENDTEXT
	
*	sql('exec vt_recibocobrador ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
=	sql(cmdSQL,'vt_rrecibo')
	SELECT vt_rrecibo
	
	
	

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
                                                    ���    �  �                        �   %   ;      �     c          �  U  � %�C��  ��� � J���(��  � �	 M(� ��8 �2 	select a.idpago,a.sucursal,a.num_recibo,a.fecha, �2 �, 	rtrim(a.idcliente)+'-'+c.razsocial cliente,�\ �V 	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, �+ �% 	b.idmoneda,sum(b.importe)as importe �F �@ 	from vt_pagos a inner join vt_det_pagos b on a.idpago=b.idpago �% � 	and a.idempresa=?oApp.empresa �V �P 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa�[ �U 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	where a.fecha between ?m.dfecha and ?m.hfecha and �< �6 	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)�Q �K 	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,�? �9 	a.idcobrador,a.idcobrador,e.nombre,e.apellido,b.idmoneda�1 �+ 	order by a.idcobrador,a.fecha,a.num_recibo� � ��C � �
 vt_rrecibo� �� F� � U 
 IDCOBRADOR CMDSQL SQL
 VT_RRECIBO
  �  � U  SETEO Init,     �� BeforeOpenTables&    ��1 � A � �!��aQa�����A �q 6 q 1                       O        v  ~       )   �                                                                   