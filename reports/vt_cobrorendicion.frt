  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      Cobrador      Arial      Arial      Arial      Arial      Arial      oApp.NombreEmpresa      Arial      $"Planilla de Rendici�n de Cobranzas"      Arial      (iif(isnull(m.sucursal),'Todos',sucursal)      Arial      
"Sucursal"      Arial      "Fecha Rendici�n:"      Arial      m.fecha      Arial      	"Fecha
"      Arial      "Recibo Nro."      Arial      	"Cliente"      Arial      
"Facturas"      Arial      
"Efectivo"      Arial      "Cheque"      Arial      "Retenciones"      Arial      "Total"      Arial      
"Comision"      Arial      "%"      Arial      
"Cobrador"      Arial      cobrador             Arial      fecha             Arial      
num_recibo             Arial      cliente             Arial      Facturas      Arial      Efectivo      "999,999,999,999"      Arial      Cheque      "999,999,999,999"      Arial      	Retencion      "999,999,999,999"      Arial      importe      "999,999,999,999"      Arial       round(importe * Comision /100,0)      "9,999,999,999"      Arial      Comision      "999"      Arial      Valores      Arial      Efectivo      "999,999,999,999"      Arial      Cheque      "999,999,999,999"      Arial      	Retencion      "999,999,999,999"      Arial      importe      "999,999,999,999"      Arial       round(importe * Comision /100,0)      "9,999,999,999"      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      "Total General"      Arial      Efectivo      "999,999,999,999"      Arial      Cheque      "999,999,999,999"      Arial      	Retencion      "999,999,999,999"      Arial      importe      "999,999,999,999"             Arial       round(importe * Comision /100,0)      "9,999,999,999"      Arial       round(importe * Comision /100,0)      "999,999,999,999"      Arial      
"Comision"      Arial      +Efectivo - round(importe * Comision /100,0)      "999,999,999,999"      Arial      "Saldo"      Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init


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
	SUM(case when IdTipoValor = 1 then v.importe else 0 end) as Efectivo,
	SUM(case when IdTipoValor = 5 then v.importe else 0 end) as Retencion,
	totalValores as Importe, 
	Valores = dbo.ts_DescripcionValores(a.IdPago,'C'),
	Facturas = dbo.ts_DescripcionValores(a.IdPago,'F'),a.Comision
	from vt_pagos a left join ts_valores_Base v on a.IdPAgo = v.IdCobro  
	and a.idempresa=?oApp.empresa 
	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa
	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa
	left join bs_personas e on d.idpersona=e.idpersona 
	where IdOrdenPago in(Select IdCaja from vt_Caja where IdEmpresa=?oApp.Empresa and datediff(dd,Fin,?m.Fecha)=0)
    and a.IdEmpresa=?oApp.Empresa and  
	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)
	and (a.Sucursal=?m.Sucursal or ?m.sucursal is null)
	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,
	a.idcobrador,a.idcobrador,e.nombre,e.apellido, TotalValores,a.Comision
	order by a.IdCobrador,a.fecha,a.num_recibo
ENDTEXT
	
*	sql('exec vt_recibocobrador ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
=	sql(cmdSQL,'vt_rrecibo')
	SELECT vt_rrecibo
	
	
	

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     S���    :  :                        Y�   %   s      �  #   �          �  U  � %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � �	 M(� ��8 �2 	select a.idpago,a.sucursal,a.num_recibo,a.fecha, �2 �, 	rtrim(a.idcliente)+'-'+c.razsocial cliente,�\ �V 	a.idcobrador,rtrim(a.idcobrador)+'-'+rtrim(e.nombre)+' '+rtrim(e.apellido) cobrador, �J �D 	SUM(case when IdTipoValor = 2 then v.importe else 0 end) as Cheque,�L �F 	SUM(case when IdTipoValor = 1 then v.importe else 0 end) as Efectivo,�M �G 	SUM(case when IdTipoValor = 5 then v.importe else 0 end) as Retencion,�  � 	totalValores as Importe, �9 �3 	Valores = dbo.ts_DescripcionValores(a.IdPago,'C'),�D �> 	Facturas = dbo.ts_DescripcionValores(a.IdPago,'F'),a.Comision�L �F 	from vt_pagos a left join ts_valores_Base v on a.IdPAgo = v.IdCobro  �% � 	and a.idempresa=?oApp.empresa �V �P 	left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa= c.idempresa�[ �U 	left join vt_cobradores d on a.idcobrador=d.idcobrador  and a.IdEmpresa= d.idempresa�: �4 	left join bs_personas e on d.idpersona=e.idpersona �u �o 	where IdOrdenPago in(Select IdCaja from vt_Caja where IdEmpresa=?oApp.Empresa and datediff(dd,Fin,?m.Fecha)=0)�- �'     and a.IdEmpresa=?oApp.Empresa and  �< �6 	(a.idcobrador=?m.idcobrador or ?m.idcobrador is null)�: �4 	and (a.Sucursal=?m.Sucursal or ?m.sucursal is null)�Q �K 	group by a.idpago,a.sucursal,a.num_recibo,a.fecha,a.idcliente,c.razsocial,�M �G 	a.idcobrador,a.idcobrador,e.nombre,e.apellido, TotalValores,a.Comision�1 �+ 	order by a.IdCobrador,a.fecha,a.num_recibo� � ��C � �
 vt_rrecibo� �� F� � U 
 IDCOBRADOR SUCURSAL CMDSQL SQL
 VT_RRECIBO
  �  � U  SETEO Init,     �� BeforeOpenTables^    ��1 � A � A � �!�����A�Qa��Q����A �q 6 q 1                       {     !   �  �  ,    )   :                  