  3�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      Usuario      IdCaja      IdMoneda      Orden      	Tingresos      Ingreso      0      tEgresos      Egresos      0      tCierre      iif(operacion='C',Importe,0)      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Detalle de Caja"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      &"Cajero/a: " + IdUsuario+ " " +Usuario      Arial      4"Abierto por : " + IdUsuarioApertura+ " " +UsuarioAP      Arial      Sencillo      "999,999,999.99"      Arial      Inicio      Arial      "Dinero para Sencillo"      Arial      "Inicio"      Arial      Fin      Arial      
Descuentos      "999,999,999.99"      Arial      "Cierre"      Arial      "Descuentos"      Arial      "
"      Arial      "Operacion"      Arial      "Ingresos
"      Arial      "Egresos
"      Arial      
"Creditos"      Arial      "Total"      Arial      "Obs"      Arial      "Moneda: ", IdMoneda      Arial      Operacion<>'C'      "1. Cobros
"      Arial      left(Orden,2)='03'      Cobros      "99,999,999.99"      Arial      left(Orden,2)='03'      "2. Ventas Contado
"      Arial      left(Orden,2)='03'      Contado      "99,999,999.99"      Arial      left(Orden,2)='03'      Credito      "@Z 99,999,999.99"      Arial      left(Orden,2)='03'      Credito + Contado      "@Z 99,999,999.99"      Arial      left(Orden,2)='03'      "3. Ventas Credito
"      Arial      left(Orden,2)='03'      Orden      Arial      /iif(Orden="01" or Orden="02",Descripcion,Forma)      Arial      iif(Ingreso=0,Cierre,Ingreso)      "@Z 99,999,999.99"      Arial      Egresos      "@Z 99,999,999.99"      Arial      Operacion<>'C'      obs      Arial      Operacion<>'C'      "Cambio Divisa
"      Arial      !CambioDivisa + (Valorizado1*-1)>0      CambioDivisa + (Valorizado1*-1)      "@Z 99,999,999.99"      Arial      
Valorizado      "@Z 99,999,999.99"      Arial      "Total Ingresos ( 1+2)
"      Arial      +TIngresos + CambioDivisa + (Valorizado1*-1)      "99,999,999.99"      Arial      TEgresos      "@Z 99,999,999.99"      Arial      "Saldo en Caja
"      Arial      8(TIngresos + CambioDivisa + (Valorizado1*-1)) - TEgresos      "99,999,999.99"      Arial      "Declarado
"      Arial      tCierre      "99,999,999.99"      Arial      "Diferencia
"      "@I"      Arial      \(((TIngresos + Cobros + Contado + CambioDivisa + (Valorizado1*-1)) - TEgresos) - tCierre)*-1      "99,999,999.99"      Arial      
datetime()             Arial      "P�g. " + str( _pageno,3 )             Arial      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init


IF EMPTY(m.Usuario)
	m.Usuario= null
ENDIF


TEXT TO cmdSQL NOSHOW 
SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, c.Sencillo,
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
                      vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and importe<>0 INNER JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
                      left JOIN
                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
			inner join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa
			inner join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa
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

sql(cmdSQL,'rlnegocio')

SELECT rlnegocio

ENDPROC
     ����    �  �                           %   �      0  C   �          �  U  
  �  � U  SETEO %�C��  ��� � T��  ���� �	 M(� ��s �m SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, c.Sencillo,�* �$ 	d.IdOperacion,op.Descripcion,Forma,�# � Importe,Operacion,d.IdMoneda,�: �4 Case Operacion when 'A' then '01 - Apertura de Caja'�( �" 			   when 'E' then '02 - Egresos'�4 �. 			   when 'F' then '03 - Resumen de Valores' �+ �% 			   when 'C' then '04 - Declarado' � � 			   end as Orden,�V �P 					CASE WHEN Operacion in('I','A','F') THEN d .Importe ELSE 0 END AS Ingreso, �] �W                       CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, �\ �V                       CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, �� �|                           ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �|                             WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, � � 							�o �i 							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �z                             WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,�" �                             �J �D                             ISNULL((SELECT     SUM(ImpDesc) AS Expr1�= �7                             FROM          vt_factura v �v �p                             WHERE      (IdHabilitacion = c.IdCaja)  and v.IdMoneda=d.IdMoneda),0) AS Descuentos,�" �                             �" �                             �� �� 							ISNULL((Select SUM(TotalValores) from vt_pagos where IdOrdenPago=c.IdCaja and vt_pagos.IdMoneda=d.IdMoneda),0) as Cobros,                                                        �T �N                             Usuario = RTRIM(u.last_name) + ' ' + u.first_name,�X �R                             UsuarioAP = RTRIM(ua.last_name) + ' ' + ua.first_name,�B �< 							ISNULL((SELECT     ISNULL(SUM(vc.Importe),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS CambioDivisa,                                                       �E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado,�E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda<>d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado1                                                                                   �" �                             �) �# FROM         vt_caja AS c left JOIN�] �W                       vt_CajaDet AS d ON c.IdCaja = d.IdCaja  and importe<>0 INNER JOIN�m �g                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�% �                       left JOIN�w �q                       usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id�f �` 			inner join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa�V �P 			inner join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa�4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa�: �4 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)� � and c.Estado='C'� �  �m �g --GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name�0 �* order by c.IdUsuario, c.Inicio,d.IdMoneda,�' �! Case Operacion when 'A' then '01'� � 			   when 'E' then '02'�  � 			    when 'F' then '03' �" � 			   when 'C' then '04' end� �  � � ��C � �	 rlnegocio� �� F� � U  USUARIO CMDSQL SQL	 RLNEGOCIO BeforeOpenTables,     �� InitA     ��1 q 3 � A � 1�1��A��a��!�	!� ��	!��a!!�A�!�	�Q�	A	Q�	�!���QqaaA1�aa �q�!a A �r 2                       &         A   �      )   �                  