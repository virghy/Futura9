  tp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      Usuario      IdCaja      IdMoneda      Orden      	Tingresos      Ingreso      0      tEgresos      Egresos      0      tCierre      iif(operacion='C',Importe,0)      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Detalle de Caja"      Arial      alltrim(oApp.Nombreempresa )      Arial      m.dfecha, ' al ' ,m.hfecha      Arial      
"Periodo:"      Arial      	"Resumen"      Arial      IdUsuario='XXX'      &"Cajero/a: " + IdUsuario+ " " +Usuario      Arial      IdUsuario<>'XXX'      4"Abierto por : " + IdUsuarioApertura+ " " +UsuarioAP      Arial      IdUsuario<>'XXX'      Inicio      Arial      "Inicio"      Arial      Fin      Arial      "Cierre"      Arial      "
"      Arial      "Operacion"      Arial      "Ingresos
"      Arial      "Egresos
"      Arial      "Total"      Arial      "Obs"      Arial      "Moneda: ", IdMoneda      Arial      #Operacion<>'C' or isnull(operacion)      'iif(IdMoneda="GS",Sencillo,SencilloUSD)      "999,999,999.99"      Arial      
Descuentos      "999,999,999.99"      Arial      "Dinero para Sencillo"      Arial      "Descuentos"      Arial      "1. Cobros
"      Arial      left(Orden,2)='03'      Cobros      "9,999,999,999.99"      Arial      left(Orden,2)='03'      "2. Ventas Contado
"      Arial      left(Orden,2)='03'      Contado      "9,999,999,999.99"      Arial      left(Orden,2)='03'      Credito      "9,999,999,999.99"      Arial      left(Orden,2)='03'      "3. Ventas Credito
"      Arial      left(Orden,2)='03'      Orden      Arial      /iif(Orden="01" or Orden="02",Descripcion,Forma)      Arial      iif(Ingreso=0,Cierre,Ingreso)      "9,999,999,999.99"      Arial      Egresos      "9,999,999,999.99"      Arial      Operacion<>'C'      obs      Arial      Operacion<>'C'      "Cambio Divisa
"      Arial      !CambioDivisa + (Valorizado1*-1)>0      CambioDivisa + (Valorizado1*-1)      "9,999,999,999.99"      Arial      
Valorizado      "@Z 99,999,999.99"      Arial      Credito + Contado      "9,999,999,999.99"      Arial      "Total Ventas ( 2+3)
"      Arial      "Total Ingresos ( 1+2)
"      Arial      +TIngresos + CambioDivisa + (Valorizado1*-1)      "9,999,999,999.99"      Arial      TEgresos      "9,999,999,999.99"      Arial      "Saldo en Caja
"      Arial      8(TIngresos + CambioDivisa + (Valorizado1*-1)) - TEgresos      "9,999,999,999.99"      Arial      "Declarado
"      Arial      tCierre      "9,999,999,999.99"      Arial      "Diferencia
"      "@I"      Arial      H(((TIngresos +CambioDivisa + (Valorizado1*-1)) - TEgresos) - tCierre)*-1      "9,999,999,999.99"      Arial      
datetime()             Arial      "P�g. " + str( _pageno,3 )             Arial      dataenvironment      ~Top = 75
Left = 208
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     1&PROCEDURE Init


IF EMPTY(m.Usuario)
	m.Usuario= null
ENDIF
TEXT TO cmdSQL noshow
	SELECT COUNT(*) Cantidad FROM vt_Caja WHERE IdEmpresa=?oApp.Empresa
	and Fecha=?m.Dfecha
ENDTEXT

*!*	IF sql(cmdSQL,'cCierres')>0
*!*		IF cCierres.Cantidad>1
*!*			m.ConResumen='S'
*!*		ENDIF

*!*	ENDIF




IF m.ConResumen='N'


TEXT TO cmdSQL NOSHOW 
SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, c.Sencillo,c.SencilloUSD,
	d.IdOperacion,op.Descripcion,Forma,
Importe,Operacion,d.IdMoneda,
Case Operacion when 'A' then '01 - Apertura de Caja'
			   when 'E' then '02 - Egresos'
			   when 'F' then '03 - Resumen de Valores' 
			   when 'C' then '04 - Declarado' 
			   end as Orden,
					CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END AS Ingreso, 
                      CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, 
                      CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, 
                          ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1
                            FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion 
                            WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, 
							
							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1
                            FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion 
                            WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,
                            
                            ISNULL((SELECT     SUM(ImpDesc) AS Expr1
                            FROM          vt_factura v 
                            WHERE      (IdHabilitacion = c.IdCaja)  and v.IdMoneda=d.IdMoneda),0) AS Descuentos,
                            
                            
							ISNULL((Select SUM(TotalValores) from vt_pagos where IdOrdenPago=c.IdCaja and vt_pagos.IdMoneda=d.IdMoneda),0) as Cobros,                                                        
                            Usuario = RTRIM(u.last_name) + ' ' + u.first_name,
                            UsuarioAP = RTRIM(ua.last_name) + ' ' + ua.first_name,
							ISNULL((SELECT     ISNULL(SUM(vc.Importe),0) AS Expr1
                            FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura 
                            WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS CambioDivisa,                                                       
							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1
                            FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura 
                            WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado,
							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1
                            FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura 
                            WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda<>d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado1                                                                                   
                            
FROM         vt_caja AS c left JOIN
                      vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and (importe<>0  or d.Operacion='F') left JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
                      left JOIN
                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
			left join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa
			left join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa
where c.Fecha between ?m.Dfecha  and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
and c.Estado='C'

--GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name
order by c.IdUsuario, c.Inicio,d.IdMoneda,
Case Operacion when 'A' then '01'
			   when 'E' then '02'
			    when 'F' then '03' 
			   when 'C' then '04' end

ENDTEXT

ELSE

TEXT TO cmdSQL NOSHOW 

SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, c.Sencillo,c.SencilloUSD,
	d.IdOperacion,op.Descripcion,Forma,
Importe,Operacion,d.IdMoneda,
Case Operacion when 'A' then '01 - Apertura de Caja'
			   when 'E' then '02 - Egresos'
			   when 'F' then '03 - Resumen de Valores' 
			   when 'C' then '04 - Declarado' 
			   end as Orden,
					CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END AS Ingreso, 
                      CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, 
                      CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, 
                          ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1
                            FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion 
                            WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, 
							
							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1
                            FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion 
                            WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,
                            
                            ISNULL((SELECT     SUM(ImpDesc) AS Expr1
                            FROM          vt_factura v 
                            WHERE      (IdHabilitacion = c.IdCaja)  and v.IdMoneda=d.IdMoneda),0) AS Descuentos,
                            
                            
							ISNULL((Select SUM(TotalValores) from vt_pagos where IdOrdenPago=c.IdCaja and vt_pagos.IdMoneda=d.IdMoneda),0) as Cobros,                                                        
                            Usuario = RTRIM(u.last_name) + ' ' + u.first_name,
                            UsuarioAP = RTRIM(ua.last_name) + ' ' + ua.first_name,
							ISNULL((SELECT     ISNULL(SUM(vc.Importe),0) AS Expr1
                            FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura 
                            WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS CambioDivisa,                                                       
							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1
                            FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura 
                            WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado,
							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1
                            FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura 
                            WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda<>d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado1                                                                                   
                            
FROM         vt_caja AS c left JOIN
                      vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and (importe<>0  or d.Operacion='F') left JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
                      left JOIN
                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
			left join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa
			left join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa
where c.Fecha between ?m.Dfecha  and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
and c.Estado='C'

--GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name

union


SELECT     IdCaja=99999999, IdUsuario='XXX', Fecha=null, Min(c.Inicio) as Inicio, Max(c.Fin) as Fin, Estado='', Obs='', IdUsuarioApertura='RESUMEN', Sencillo=0,SencilloUSD=0,
	d.IdOperacion,op.Descripcion,Forma,
Sum(D.Importe) as Importe,Operacion,d.IdMoneda,
Case Operacion when 'A' then '01 - Apertura de Caja'
			   when 'E' then '02 - Egresos'
			   when 'F' then '03 - Resumen de Valores' 
			   when 'C' then '04 - Declarado' 
			   end as Orden,
					SUM(CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END) AS Ingreso, 
                      SUM(CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END) AS Egresos, 
                      SUM(CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END) AS Cierre,              Contado,Credito,isnull(Descuentos,0) as Descuentos,isnull(Cobros,0) as Cobros,Usuario='',UsuarioAp='',                                                                    
 CambioDivisa=$0,Valorizado=$0,Valorizado1=$0                                                                                                             
FROM         vt_caja AS c left JOIN
                      vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and (importe<>0  or d.Operacion='F') INNER JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
                      left JOIN
                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
			inner join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa
			inner join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa
left JOIN (
SELECT   IdMoneda,
Sum(Contado) as Contado,Sum(Credito) as Credito,sum(Descuentos) as Descuentos
FROM         vt_caja AS c inner  JOIN
(	SELECT   IdHabilitacion,  IdMoneda,Sum(Case when Plazo=0 then TotalFactura else 0 end) as Contado,
	Sum(Case when Plazo>0 then TotalFactura else 0 end) as Credito,
	Sum(ImpDesc) as Descuentos
    FROM          vt_factura v inner join vt_condicion vc 
	on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion  = vc.IdCondicion 
	group by IDMoneda,IdHabilitacion
) fs on c.IdCaja = fs.IdHabilitacion 

where c.Fecha between ?m.Dfecha  and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
and c.Estado='C'
group by IdMoneda

) R ON D.IdMoneda = r.IdMoneda
left join (SELECT   IdMoneda,
Sum(Cobros) as Cobros
FROM         vt_caja AS c inner  JOIN
(Select	IdOrdenPago,IdMoneda,SUM(TotalValores) as Cobros from vt_pagos
group by IdOrdenPago,IdMoneda
) fs on c.IdCaja = fs.IdOrdenPago

where c.Fecha between ?m.Dfecha  and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
and c.Estado='C'
group by IdMoneda) p on d.IdMoneda = p.IdMoneda


where c.Fecha between ?m.Dfecha  and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
--and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
and c.Estado='C'


GROUP BY d.IdOperacion,op.Descripcion,Forma,Operacion,d.IdMoneda,Contado,Credito,Descuentos,Cobros

order by 1,2,16,17



ENDTEXT


ENDIF



sql(cmdSQL,'rlnegocio')

SELECT rlnegocio
IF RECCOUNT('rlnegocio')=1
	IF ISNULL(IdMoneda)
		replace IdMoneda WITH 'GS'
	ENDIF
		
	SCATTER MEMVAR memo
	APPEND BLANK
	GATHER MEMVAR memo
	IF IdMoneda='GS'
		replace IdMoneda WITH 'U$S'
	ELSE
		replace IdMoneda WITH 'GS'
	ENDIF
	replace Importe WITH 0,;
	Ingreso WITH 0,;
	Egresos WITH 0,;
	Cierre WITH 0,;
	Contado WITH 0
ENDIF
	
	
		
*PUBLIC NroVentas
*CALCULATE COUNT() FOR LEFT(Orden,2)='03' TO NroVentas 
*GO TOP 

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     4����    �4  �4                        U   %   c2      )4  �   �2          �  U  �1 %�C��  ��� � T��  ���� �	 M(� ��J �D 	SELECT COUNT(*) Cantidad FROM vt_Caja WHERE IdEmpresa=?oApp.Empresa� � 	and Fecha=?m.Dfecha� � %��� � N����	 M(� ��� �{ SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, c.Sencillo,c.SencilloUSD,�* �$ 	d.IdOperacion,op.Descripcion,Forma,�# � Importe,Operacion,d.IdMoneda,�: �4 Case Operacion when 'A' then '01 - Apertura de Caja'�( �" 			   when 'E' then '02 - Egresos'�4 �. 			   when 'F' then '03 - Resumen de Valores' �+ �% 			   when 'C' then '04 - Declarado' � � 			   end as Orden,�V �P 					CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END AS Ingreso, �] �W                       CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, �\ �V                       CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, �� �|                           ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �|                             WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, � � 							�o �i 							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �z                             WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,�" �                             �J �D                             ISNULL((SELECT     SUM(ImpDesc) AS Expr1�= �7                             FROM          vt_factura v �v �p                             WHERE      (IdHabilitacion = c.IdCaja)  and v.IdMoneda=d.IdMoneda),0) AS Descuentos,�" �                             �" �                             �� �� 							ISNULL((Select SUM(TotalValores) from vt_pagos where IdOrdenPago=c.IdCaja and vt_pagos.IdMoneda=d.IdMoneda),0) as Cobros,                                                        �T �N                             Usuario = RTRIM(u.last_name) + ' ' + u.first_name,�X �R                             UsuarioAP = RTRIM(ua.last_name) + ' ' + ua.first_name,�B �< 							ISNULL((SELECT     ISNULL(SUM(vc.Importe),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS CambioDivisa,                                                       �E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado,�E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda<>d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado1                                                                                   �" �                             �) �# FROM         vt_caja AS c left JOIN�r �l                       vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and (importe<>0  or d.Operacion='F') left JOIN�m �g                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�% �                       left JOIN�w �q                       usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id�e �_ 			left join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa�U �O 			left join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa�4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa�: �4 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)� � and c.Estado='C'� �  �m �g --GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name�0 �* order by c.IdUsuario, c.Inicio,d.IdMoneda,�' �! Case Operacion when 'A' then '01'� � 			   when 'E' then '02'�  � 			    when 'F' then '03' �" � 			   when 'C' then '04' end� �  � � ��0�	 M(� �� �  �� �{ SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, c.Sencillo,c.SencilloUSD,�* �$ 	d.IdOperacion,op.Descripcion,Forma,�# � Importe,Operacion,d.IdMoneda,�: �4 Case Operacion when 'A' then '01 - Apertura de Caja'�( �" 			   when 'E' then '02 - Egresos'�4 �. 			   when 'F' then '03 - Resumen de Valores' �+ �% 			   when 'C' then '04 - Declarado' � � 			   end as Orden,�V �P 					CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END AS Ingreso, �] �W                       CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, �\ �V                       CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, �� �|                           ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �|                             WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, � � 							�o �i 							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �z                             WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,�" �                             �J �D                             ISNULL((SELECT     SUM(ImpDesc) AS Expr1�= �7                             FROM          vt_factura v �v �p                             WHERE      (IdHabilitacion = c.IdCaja)  and v.IdMoneda=d.IdMoneda),0) AS Descuentos,�" �                             �" �                             �� �� 							ISNULL((Select SUM(TotalValores) from vt_pagos where IdOrdenPago=c.IdCaja and vt_pagos.IdMoneda=d.IdMoneda),0) as Cobros,                                                        �T �N                             Usuario = RTRIM(u.last_name) + ' ' + u.first_name,�X �R                             UsuarioAP = RTRIM(ua.last_name) + ' ' + ua.first_name,�B �< 							ISNULL((SELECT     ISNULL(SUM(vc.Importe),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS CambioDivisa,                                                       �E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado,�E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda<>d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado1                                                                                   �" �                             �) �# FROM         vt_caja AS c left JOIN�r �l                       vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and (importe<>0  or d.Operacion='F') left JOIN�m �g                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�% �                       left JOIN�w �q                       usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id�e �_ 			left join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa�U �O 			left join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa�4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa�: �4 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)� � and c.Estado='C'� �  �m �g --GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name� �  � � union� �  � �  �� �� SELECT     IdCaja=99999999, IdUsuario='XXX', Fecha=null, Min(c.Inicio) as Inicio, Max(c.Fin) as Fin, Estado='', Obs='', IdUsuarioApertura='RESUMEN', Sencillo=0,SencilloUSD=0,�* �$ 	d.IdOperacion,op.Descripcion,Forma,�5 �/ Sum(D.Importe) as Importe,Operacion,d.IdMoneda,�: �4 Case Operacion when 'A' then '01 - Apertura de Caja'�( �" 			   when 'E' then '02 - Egresos'�4 �. 			   when 'F' then '03 - Resumen de Valores' �+ �% 			   when 'C' then '04 - Declarado' � � 			   end as Orden,�[ �U 					SUM(CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END) AS Ingreso, �b �\                       SUM(CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END) AS Egresos, ��                      SUM(CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END) AS Cierre,              Contado,Credito,isnull(Descuentos,0) as Descuentos,isnull(Cobros,0) as Cobros,Usuario='',UsuarioAp='',                                                                    �� ��  CambioDivisa=$0,Valorizado=$0,Valorizado1=$0                                                                                                             �) �# FROM         vt_caja AS c left JOIN�s �m                       vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and (importe<>0  or d.Operacion='F') INNER JOIN�m �g                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�% �                       left JOIN�w �q                       usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id�f �` 			inner join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa�V �P 			inner join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa� � left JOIN (� � SELECT   IdMoneda,�S �M Sum(Contado) as Contado,Sum(Credito) as Credito,sum(Descuentos) as Descuentos�+ �% FROM         vt_caja AS c inner  JOIN�j �d (	SELECT   IdHabilitacion,  IdMoneda,Sum(Case when Plazo=0 then TotalFactura else 0 end) as Contado,�F �@ 	Sum(Case when Plazo>0 then TotalFactura else 0 end) as Credito,�! � 	Sum(ImpDesc) as Descuentos�@ �:     FROM          vt_factura v inner join vt_condicion vc �I �C 	on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion  = vc.IdCondicion �' �! 	group by IDMoneda,IdHabilitacion�+ �% ) fs on c.IdCaja = fs.IdHabilitacion � �  �4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa� � and c.Estado='C'� � group by IdMoneda� �  �$ � ) R ON D.IdMoneda = r.IdMoneda�# � left join (SELECT   IdMoneda,� � Sum(Cobros) as Cobros�+ �% FROM         vt_caja AS c inner  JOIN�L �F (Select	IdOrdenPago,IdMoneda,SUM(TotalValores) as Cobros from vt_pagos�# � group by IdOrdenPago,IdMoneda�' �! ) fs on c.IdCaja = fs.IdOrdenPago� �  �4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa� � and c.Estado='C'�5 �/ group by IdMoneda) p on d.IdMoneda = p.IdMoneda� �  � �  �4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa�< �6 --and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)� � and c.Estado='C'� �  � �  �h �b GROUP BY d.IdOperacion,op.Descripcion,Forma,Operacion,d.IdMoneda,Contado,Credito,Descuentos,Cobros� �  � � order by 1,2,16,17� �  � �  � �  � � � ��C � �	 rlnegocio� �� F� � %�C�	 rlnegocioN����1� %�C� ���,1� >� ��� GS�� � ^�� � _�� %�� � GS��d1� >� ��� U$S�� �{1� >� ��� GS�� �5 >� ��� �� ��� �� ��� ��	 ��� ��
 ��� �� � U  USUARIO CMDSQL
 CONRESUMEN SQL	 RLNEGOCIO IDMONEDA IMPORTE INGRESO EGRESOS CIERRE CONTADO
  �  � U  SETEO Init,     �� BeforeOpenTablesN2    ��1 � A � ��A L� �1��A��a��!�	!� ��	!��a!!�A�!�	�Q�	A	Q�	�!�!�QqQQA1�aa �q�!a A � � a �1��A��a��!�	!� ��	!��a!!�A�!�	�Q�	A	Q�	�!�!�QqQQA1�aa �a � a a A�Q��A���!�
�1�Qqaa�1��a�q�a A1aqa A1���1qa A1aQa a A1�aa a �a �a a a A C �r �� � A b Q a 1� � A UA 9 q 2                       �0     �   1  1  �    )   �4                  