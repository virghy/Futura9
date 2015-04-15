  [                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      Cobrador      Arial      Arial      Arial      Arial      oApp.NombreEmpresa      Arial      ""Planilla de Gesti�n de Cobranzas"      Arial      (iif(isnull(m.sucursal),'Todos',sucursal)      Arial      
"Sucursal"      Arial      "Fecha:"      Arial      m.Fecha      Arial      "Cliente
"      Arial      
"Producto"      Arial      "O.P."      Arial      "Cuota"      Arial      "Plazo"      Arial      "Saldo Ant."      Arial      "Meta Dia Ant."      Arial      
"Meta Hoy"      Arial      
"Pag� Hoy"      Arial      "Boleta No."      Arial      "Saldo Dia"      Arial      "Saldo Actual"      Arial      "Cuotas Cob."      Arial      "Cuotas Pend."      Arial      
"Cobrador"      Arial      cobrador             Arial      9IdCliente,' ',alltrim(RazSocial),' ',Telefono,' ',Celular      Arial      Facturas      Arial      ImporteCuota      "999,999,999"      Arial      Cuotas      "999"      Arial      Saldo      "999,999,999"      Arial      Anterior      "999,999,999"      Arial      Actual      "999,999,999"      Arial      Pago      "999,999,999"      Arial      Actual-Pago      "999,999,999"      Arial      Saldo      "999,999,999"      Arial      CuotasPagadas      "999"      Arial      Cuotas-CuotasPagadas      "999"      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     kPROCEDURE Init



If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF


TEXT TO cmdSQL noshow
Declare @Fecha datetime
Set @Fecha ='23-01-2012'

SELECT     cl.IdCliente, cl.RazSocial, cl.Telefono, cl.Celular, cn.Descripcion,fa.IdFactura,
sum(de.Saldo) as Saldo,
Sum(case when cn.tipovencimiento='D'  AND de.Vencimiento< @Fecha then de.Saldo 
	WHEN cn.tipovencimiento='S'  AND (DATEPART(wk,de.Vencimiento)< DATEPART(wk,@Fecha) or year(de.Vencimiento) < year(@Fecha)) then de.Saldo else 0 end) as Anterior,
Sum(case when cn.tipovencimiento='D'  AND de.Vencimiento=@Fecha then de.Saldo 
	WHEN cn.tipovencimiento='S'  AND DATEPART(wk,de.Vencimiento)= DATEPART(wk,@Fecha) then de.Saldo else 0 end) as Actual,
count(*) as Cuotas,
max(de.Importe) as ImporteCuota,
Sum(case when de.Saldo=0 then 1 else 0 end) as CuotasPagadas,
Pago=0
FROM         vt_clientes AS cl INNER JOIN
                      vt_factura AS fa ON cl.IdEmpresa = fa.IdEmpresa AND cl.IdCliente = fa.IdCliente INNER JOIN
                      vt_forma_pago AS de ON fa.IdFactura = de.IdFactura INNER JOIN
                      vt_Condicion AS cn ON fa.IdEmpresa = cn.IdEmpresa AND fa.IdCondicion = cn.IdCondicion
where cn.tipovencimiento in('D','S') and 
cl.IdEmpresa=?oApp.Empresa                      
group by cl.IdCliente, cl.RazSocial, cl.Telefono, cl.Celular, cn.Descripcion,fa.IdFactura
order by cn.Descripcion,cl.IdCliente
ENDTEXT
	
*	sql('exec vt_recibocobrador ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idcobrador','vt_rrecibo')
=	sql(cmdSQL,'vt_rrecibo')
	SELECT vt_rrecibo
	
	
	

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     ���    �  �                        =�   %   5      �      ]          �  U  � %�C��  ��� � J���(��  � �	 M(� �� � Declare @Fecha datetime� � Set @Fecha ='23-01-2012'� �  �b �\ SELECT     cl.IdCliente, cl.RazSocial, cl.Telefono, cl.Celular, cn.Descripcion,fa.IdFactura,� � sum(de.Saldo) as Saldo,�U �O Sum(case when cn.tipovencimiento='D'  AND de.Vencimiento< @Fecha then de.Saldo �� �� 	WHEN cn.tipovencimiento='S'  AND (DATEPART(wk,de.Vencimiento)< DATEPART(wk,@Fecha) or year(de.Vencimiento) < year(@Fecha)) then de.Saldo else 0 end) as Anterior,�T �N Sum(case when cn.tipovencimiento='D'  AND de.Vencimiento=@Fecha then de.Saldo �} �w 	WHEN cn.tipovencimiento='S'  AND DATEPART(wk,de.Vencimiento)= DATEPART(wk,@Fecha) then de.Saldo else 0 end) as Actual,� � count(*) as Cuotas,�& �  max(de.Importe) as ImporteCuota,�C �= Sum(case when de.Saldo=0 then 1 else 0 end) as CuotasPagadas,� � Pago=0�/ �) FROM         vt_clientes AS cl INNER JOIN�v �p                       vt_factura AS fa ON cl.IdEmpresa = fa.IdEmpresa AND cl.IdCliente = fa.IdCliente INNER JOIN�Y �S                       vt_forma_pago AS de ON fa.IdFactura = de.IdFactura INNER JOIN�q �k                       vt_Condicion AS cn ON fa.IdEmpresa = cn.IdEmpresa AND fa.IdCondicion = cn.IdCondicion�/ �) where cn.tipovencimiento in('D','S') and �6 �0 cl.IdEmpresa=?oApp.Empresa                      �_ �Y group by cl.IdCliente, cl.RazSocial, cl.Telefono, cl.Celular, cn.Descripcion,fa.IdFactura�* �$ order by cn.Descripcion,cl.IdCliente� � ��C � �
 vt_rrecibo� �� F� � U  SUCURSAL CMDSQL SQL
 VT_RRECIBO
  �  � U  SETEO Init,     �� BeforeOpenTables     ��1 � A � ��a !�Q�
A��a1� �a��a��A �q 6 q 1                       1        X  `  *    )   �                  