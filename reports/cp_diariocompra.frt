  9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=1
      Arial      idmoneda      IdTipo      fecha      Arial      Arial      Arial      Arial      Arial      "Diario de Control de Compras"             Arial      empresa             Arial      *iif(empty(m.sucursal),'Todos',descripci�n)             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Cpbte."      Arial      "Nro."      Arial      "Proveedor"      Arial      	"Exentas"      Arial      
"Gravadas"      Arial      "Iva"      Arial      "Total"      Arial      	"Fecha
"      Arial      " Moneda : " +idmoneda             Arial      " Tipo: " +Tipo             Arial      fecha             Arial      idcomprobante             Arial      numero             Arial      	proveedor             Arial      exenta      "99,999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "99,999,999,999.99"      Arial      "Total "+ dtoc(fecha)             Arial      exenta      "99,999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "99,999,999,999.99"      Arial      " Total Tipo : " +Tipo             Arial      exenta      "99,999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "99,999,999,999.99"      Arial      " Total Moneda : " +idmoneda             Arial      exenta      "99,999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "99,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


If Empty(m.sucursal)
	m.sucursal=null
endif
	
	
TEXT TO cmdSQL noshow
	select a.fecha,a.idcomprobante,a.facturaproveedor numero, 
	rtrim(a.idproveedor)+'-'+c.razon proveedor,Exenta=ISNULL(a.exenta,0),Gravada=ISNULL(a.gravada,0),Iva=ISNULL(a.iva,0), 
	ISNULL(a.exenta,0)+ISNULL(a.gravada,0)+ISNULL(a.iva,0) total,a.idmoneda,a.sucursal,d.descripci�n, case when a.Tipo='C' then 'Compras' else 'Gastos' end as Tipo ,
	a.Tipo as IdTipo
	from cp_factura a 
	left join cp_proveedor c on a.idproveedor=c.idproveedor and a.IdEmpresa = c.IdEmpresa
	left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa   
	where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha) and a.Tipo in ('C','G')
	order by a.Tipo, a.IdMoneda,a.fecha, a.IdProveedor
ENDTEXT

sql(cmdSQL,'vt_rdiariocompra')
	
*	sql('exec cp_diariocompra ?oapp.empresa,?m.dFecha,?m.hFecha,?m.sucursal','vt_rdiariocompra')
	SELECT vt_rdiariocompra

ENDPROC
     v���    ]  ]                        ��   %   �           �          �  U  
  �  � U  SETEOF %�C��  ��� � T��  ���� �	 M(� ��A �; 	select a.fecha,a.idcomprobante,a.facturaproveedor numero, �} �w 	rtrim(a.idproveedor)+'-'+c.razon proveedor,Exenta=ISNULL(a.exenta,0),Gravada=ISNULL(a.gravada,0),Iva=ISNULL(a.iva,0), �� �� 	ISNULL(a.exenta,0)+ISNULL(a.gravada,0)+ISNULL(a.iva,0) total,a.idmoneda,a.sucursal,d.descripci�n, case when a.Tipo='C' then 'Compras' else 'Gastos' end as Tipo ,� � 	a.Tipo as IdTipo� � 	from cp_factura a �\ �V 	left join cp_proveedor c on a.idproveedor=c.idproveedor and a.IdEmpresa = c.IdEmpresa�U �O 	left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa   �l �f 	where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha) and a.Tipo in ('C','G')�9 �3 	order by a.Tipo, a.IdMoneda,a.fecha, a.IdProveedor� �! ��C � � vt_rdiariocompra� �� F� � U  SUCURSAL CMDSQL SQL VT_RDIARIOCOMPRA BeforeOpenTables,     �� InitA     ��1 q 2 � A � ��
q��Q��A s 2                       $         ?   �      )   ]                  