     @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=1
PAPERSIZE=9
COLOR=2
PUT=PrimoPort:
ORIENTATION=1
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
                                                          8    winspool  PrimoPDF  PrimoPort:                        Arial                                                         !"Planilla de Control de Cobranza"                             Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         fecha                                                                                                                       Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         (iif(isnull(m.sucursal),'Todos',sucursal)                      Arial                                                         
"Sucursal"                                                    Arial                                                         "Total"                                                       Arial                                                         "Total General"                                               Arial                                                         "ReciboNro."                                                  Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         
num_recibo                                                                                                                  Arial                                                         cobrador                                                                                                                    Arial                                                         
"Cobrador"                                                    Arial                                                         importe                                                       "999,999,999,999"                                             Arial                                                         cliente                                                                                                                     Arial                                                         	"Cliente"                                                     Arial                                                         
"Efectivo"                                                    Arial                                                         Efectivo                                                      "999,999,999,999"                                             Arial                                                         Efectivo                                                      "999,999,999"                                                 Arial                                                         "Cheque"                                                      Arial                                                         Cheque                                                        "999,999,999,999"                                             Arial                                                         Cheque                                                        "999,999,999,999"                                             Arial                                                         
"Facturas"                                                    Arial                                                         Valores                                                       Arial                                                         	"Valores"                                                     Arial                                                         Facturas                                                      Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                                                   �PROCEDURE Init


If Empty(m.idcobrador)
	Store null To m.idcobrador
ENDIF
If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF


TEXT TO cmdSQL noshow
	select a.idpago,a.sucursal,a.num_recibo,a.fecha, 
	rtrim(a.idcliente)+'-'+c.razsocial cliente,
	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, 
	SUM(case when IdTipoValor = 2 then v.importe else 0 end) as Cheque,
	SUM(case when IdTipoValor <> 2 then v.importe else 0 end) as Efectivo,
	totalValores as Importe, 
	Valores = dbo.ts_DescripcionValores(a.IdPago,'C'),
	Facturas = dbo.ts_DescripcionValores(a.IdPago,'F')
	from vt_pagos a left join ts_valores_Base v on a.IdPAgo = v.IdCobro  
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	where a.fecha between ?m.dfecha and ?m.hfecha and 
	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)
	and (a.Sucursal=?m.Sucursal or ?m.sucursal is null)
	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,
	a.idcobrador,a.idcobrador,e.nombre,e.apellido, TotalValores
	order by a.fecha,a.num_recibo
ENDTEXT
	
*	sql('exec vt_recibocobrador ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
=	sql(cmdSQL,'vt_rrecibo')
	SELECT vt_rrecibo
	
	
	

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
                        w���    ^  ^                        �v   %   �        !   �          �  U  ' %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � �	 M(� ��8 �2 	select a.idpago,a.sucursal,a.num_recibo,a.fecha, �2 �, 	rtrim(a.idcliente)+'-'+c.razsocial cliente,�\ �V 	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, �J �D 	SUM(case when IdTipoValor = 2 then v.importe else 0 end) as Cheque,�M �G 	SUM(case when IdTipoValor <> 2 then v.importe else 0 end) as Efectivo,�  � 	totalValores as Importe, �9 �3 	Valores = dbo.ts_DescripcionValores(a.IdPago,'C'),�9 �3 	Facturas = dbo.ts_DescripcionValores(a.IdPago,'F')�L �F 	from vt_pagos a left join ts_valores_Base v on a.IdPAgo = v.IdCobro  �% � 	and a.idempresa=?oApp.empresa �V �P 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa�[ �U 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	where a.fecha between ?m.dfecha and ?m.hfecha and �< �6 	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)�: �4 	and (a.Sucursal=?m.Sucursal or ?m.sucursal is null)�Q �K 	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,�B �< 	a.idcobrador,a.idcobrador,e.nombre,e.apellido, TotalValores�$ � 	order by a.fecha,a.num_recibo� � ��C � �
 vt_rrecibo� �� F� � U 
 IDCOBRADOR SUCURSAL CMDSQL SQL
 VT_RRECIBO
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � A � A � �!������Qa�����!AA �q 6 q 1                       �        �  �  *    )   ^                         �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=1
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
                                                          8    winspool  PrimoPDF  PrimoPort:                        Arial                                                         !"Planilla de Control de Cobranza"                             Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         fecha                                                                                                                       Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         (iif(isnull(m.sucursal),'Todos',sucursal)                      Arial                                                         
"Sucursal"                                                    Arial                                                         "Total"                                                       Arial                                                         "Total General"                                               Arial                                                         "ReciboNro."                                                  Arial                                                         importe                                                       "999,999,999,999"                                                                                                           Arial                                                         
num_recibo                                                                                                                  Arial                                                         cobrador                                                                                                                    Arial                                                         
"Cobrador"                                                    Arial                                                         importe                                                       "999,999,999,999"                                             Arial                                                         cliente                                                                                                                     Arial                                                         	"Cliente"                                                     Arial                                                         
"Efectivo"                                                    Arial                                                         Efectivo                                                      "999,999,999,999"                                             Arial                                                         Efectivo                                                      "999,999,999"                                                 Arial                                                         "Cheque"                                                      Arial                                                         Cheque                                                        "999,999,999,999"                                             Arial                                                         Cheque                                                        "999,999,999,999"                                             Arial                                                         
"Facturas"                                                    Arial                                                         Valores                                                       Arial                                                         	"Valores"                                                     Arial                                                         Facturas                                                      Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                                                   �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


If Empty(m.idcobrador)
	Store null To m.idcobrador
ENDIF
If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF


TEXT TO cmdSQL noshow
	select a.idpago,a.sucursal,a.num_recibo,a.fecha, 
	rtrim(a.idcliente)+'-'+c.razsocial cliente,
	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, 
	SUM(case when IdTipoValor = 2 then v.importe else 0 end) as Cheque,
	SUM(case when IdTipoValor <> 2 then v.importe else 0 end) as Efectivo,
	SUM(totalValores) as Importe, 
	Valores = dbo.ts_DescripcionValores(a.IdPago,'C'),
	Facturas = dbo.ts_DescripcionValores(a.IdPago,'F')
	from vt_pagos a left join ts_valores_Base v on a.IdPAgo = v.IdCobro  
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	where a.fecha between ?m.dfecha and ?m.hfecha and 
	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)
	and (a.Sucursal=?m.Sucursal or ?m.sucursal is null)
	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,
	a.idcobrador,a.idcobrador,e.nombre,e.apellido
	order by a.fecha,a.num_recibo
ENDTEXT
	
*	sql('exec vt_recibocobrador ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
=	sql(cmdSQL,'vt_rrecibo')
	SELECT vt_rrecibo
	
	
	

ENDPROC
                                 n���    U  U                        ��   %   �      �  !   �          �  U  
  �  � U  SETEO %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � �	 M(� ��8 �2 	select a.idpago,a.sucursal,a.num_recibo,a.fecha, �2 �, 	rtrim(a.idcliente)+'-'+c.razsocial cliente,�\ �V 	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, �J �D 	SUM(case when IdTipoValor = 2 then v.importe else 0 end) as Cheque,�M �G 	SUM(case when IdTipoValor <> 2 then v.importe else 0 end) as Efectivo,�% � 	SUM(totalValores) as Importe, �9 �3 	Valores = dbo.ts_DescripcionValores(a.IdPago,'C'),�9 �3 	Facturas = dbo.ts_DescripcionValores(a.IdPago,'F')�L �F 	from vt_pagos a left join ts_valores_Base v on a.IdPAgo = v.IdCobro  �% � 	and a.idempresa=?oApp.empresa �V �P 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa�[ �U 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �9 �3 	where a.fecha between ?m.dfecha and ?m.hfecha and �< �6 	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)�: �4 	and (a.Sucursal=?m.Sucursal or ?m.sucursal is null)�Q �K 	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,�4 �. 	a.idcobrador,a.idcobrador,e.nombre,e.apellido�$ � 	order by a.fecha,a.num_recibo� � ��C � �
 vt_rrecibo� �� F� � U 
 IDCOBRADOR SUCURSAL CMDSQL SQL
 VT_RRECIBO BeforeOpenTables,     �� InitA     ��1 q 2 � A � A � �!���Q���Qa�����AAA �q 5                       $         ?   �      )   U                            