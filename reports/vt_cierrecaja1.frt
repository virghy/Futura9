  1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      Usuario      IdCaja      IdMoneda      	Tingresos      Ingreso      0      tEgresos      Egresos      0      tCierre      iif(operacion='C',Importe,0)      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Detalle de Caja"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      &"Cajero/a: " + IdUsuario+ " " +Usuario      Arial      4"Abierto por : " + IdUsuarioApertura+ " " +UsuarioAP      Arial      Inicio      Arial      "Inicio"      Arial      Fin      Arial      "Cierre"      Arial      "
"      Arial      "Operacion"      Arial      "Ingresos
"      Arial      "Egresos
"      Arial      
"Creditos"      Arial      "Obs"      Arial      "Moneda: ", IdMoneda      Arial      Operacion<>'C'      Descripcion      Arial      Operacion<>'C'      Ingreso      "@Z 99,999,999.99"      Arial      Operacion<>'C'      Egresos      "99,999,999.99"      Arial      Operacion<>'C'      obs      Arial      Operacion<>'C'      
"Cobros
"      Arial      Cobros      "99,999,999.99"      Arial      
"Ventas
"      Arial      Contado      "99,999,999.99"      Arial      Credito      "@Z 99,999,999.99"      Arial      "Cambio Divisa
"      Arial      CambioDivisa + (Valorizado1*-1)      "99,999,999.99"      Arial      
Valorizado      "@Z 99,999,999.99"      Arial      "Total General
"      Arial      >TIngresos + Cobros + Contado + CambioDivisa + (Valorizado1*-1)      "99,999,999.99"      Arial      TEgresos      "@Z 99,999,999.99"      Arial      "Saldo Final
"      Arial      K(TIngresos + Cobros + Contado + CambioDivisa + (Valorizado1*-1)) - TEgresos      "99,999,999.99"      Arial      "Declarado
"      Arial      tCierre      "99,999,999.99"      Arial      "Diferencia
"      "@I"      Arial      \(((TIngresos + Cobros + Contado + CambioDivisa + (Valorizado1*-1)) - TEgresos) - tCierre)*-1      "99,999,999.99"      Arial      
datetime()             Arial      "P�g. " + str( _pageno,3 )             Arial      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init


IF EMPTY(m.Usuario)
	m.Usuario= null
ENDIF


TEXT TO cmdSQL NOSHOW 
SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, 
	d.IdOperacion,op.Descripcion,Forma,
Importe,Operacion,d.IdMoneda,
Case Operacion when 'A' then '01'
			   when 'E' then '02'
			   when 'C' then '03' end as Orden,
					CASE WHEN Operacion = 'I' or Operacion = 'A' THEN d .Importe ELSE 0 END AS Ingreso, 
                      CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, 
                      CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, 
                          ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1
                            FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion 
                            WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, 
							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1
                            FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion 
                            WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,
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
                            
FROM         vt_caja AS c INNER JOIN
                      vt_CajaDet AS d ON c.IdCaja = d.IdCaja INNER JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
                      left JOIN
                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
			inner join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa
			inner join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa
where c.Fecha between ?m.Dfecha  and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
and c.Estado='C'
and importe<>0
--GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name
order by c.IdUsuario, c.Inicio,d.IdMoneda,
Case Operacion when 'A' then '01'
			   when 'E' then '02'
			   when 'C' then '03' end

ENDTEXT

sql(cmdSQL,'rlnegocio')

*!*	TEXT TO CMDSQL NOSHOW
*!*	SELECT   c.IdCaja,tv.tipovalor, sum(v.importe) as importe
*!*	FROM     vt_caja AS c INNER JOIN vt_factura f on c.IdCaja = f.IdHabilitacion inner join 
*!*	                      ts_valores_base AS v ON f.IdFactura = v.IdFactura INNER JOIN
*!*	                      ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor 
*!*	                      
*!*	where c.idempresa=?oApp.Empresa and c.Fecha between ?m.dfecha and ?m.hfecha and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
*!*	and c.Estado='C'
*!*	group by c.IdCaja, tv.tipovalor
*!*	ENDTEXT
*!*	sql (cmdsql, "consulta")



SELECT rlnegocio
*!*	SELECT distinct IdCaja FROM rlnegicio INTO CURSOR cCajas NOFILTER readwrite



*!*	SELECT rlnegocio
*!*	INDEX on IdCaja TAG Nro

*!*	SELECT Consulta
*!*	INDEX on IdCaja TAG Nro


*!*	SELECT cCajas
*!*	SET RELATION TO IdCaja INTO rlnegocio ADDITIVE  
*!*	SET RELATION TO IdCaja INTO Consulta ADDITIVE  



ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    }  }                        �'   %   �      $  ;   �          �  U    %�C��  ��� � T��  ���� �	 M(� ��h �b SELECT     c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs, c.IdUsuarioApertura, �* �$ 	d.IdOperacion,op.Descripcion,Forma,�# � Importe,Operacion,d.IdMoneda,�' �! Case Operacion when 'A' then '01'� � 			   when 'E' then '02'�, �& 			   when 'C' then '03' end as Orden,�_ �Y 					CASE WHEN Operacion = 'I' or Operacion = 'A' THEN d .Importe ELSE 0 END AS Ingreso, �] �W                       CASE WHEN Operacion = 'E' THEN d .Importe ELSE 0 END AS Egresos, �\ �V                       CASE WHEN Operacion = 'C' THEN d .Importe ELSE 0 END AS Cierre, �� �|                           ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �|                             WHERE      (IdHabilitacion = c.IdCaja and v.IdMoneda=d.IdMoneda) and Plazo = 0 ),0) AS Contado, �o �i 							ISNULL((SELECT     ISNULL(SUM(ISNULL(Exenta, 0) + ISNULL(Gravada, 0) + ISNULL(Iva, 0)),0) AS Expr1�� ��                             FROM          vt_factura v inner join vt_condicion vc on v.IdEmpresa = vc.IdEmpresa and v.IdCondicion = vc.IdCondicion �� �z                             WHERE      (IdHabilitacion = c.IdCaja) and Plazo > 0 and v.IdMoneda=d.IdMoneda),0) AS Credito,�� �� 							ISNULL((Select SUM(TotalValores) from vt_pagos where IdOrdenPago=c.IdCaja and vt_pagos.IdMoneda=d.IdMoneda),0) as Cobros,                                                        �T �N                             Usuario = RTRIM(u.last_name) + ' ' + u.first_name,�X �R                             UsuarioAP = RTRIM(ua.last_name) + ' ' + ua.first_name,�B �< 							ISNULL((SELECT     ISNULL(SUM(vc.Importe),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS CambioDivisa,                                                       �E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda=d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado,�E �? 							ISNULL((SELECT     ISNULL(SUM(vc.Valorizado),0) AS Expr1�� ��                             FROM          vt_factura v inner join ts_Valores_Base vc on v.IdEmpresa = vc.IdEmpresa and v.IdFactura = vc.IdFactura �� ��                             WHERE      (IdHabilitacion = c.IdCaja and vc.IdMoneda<>d.IdMoneda) and vc.Importe<>vc.Valorizado),0) AS Valorizado1                                                                                   �" �                             �* �$ FROM         vt_caja AS c INNER JOIN�M �G                       vt_CajaDet AS d ON c.IdCaja = d.IdCaja INNER JOIN�m �g                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�% �                       left JOIN�w �q                       usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id�f �` 			inner join vt_OperacionCaja op on d.IdOperacion = op.IdOperacion and d.IdEmpresa=op.IdEmpresa�V �P 			inner join vt_tpvForma f on d.IdForma = f.IdForma and d.IdEmpresa=f.IdEmpresa�4 �. where c.Fecha between ?m.Dfecha  and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa�: �4 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)� � and c.Estado='C'� � and importe<>0�m �g --GROUP BY c.IdUsuario, c.Fecha, c.Inicio, c.Fin, c.Estado, d.Obs,  c.IdCaja, u.last_name, u.first_name�0 �* order by c.IdUsuario, c.Inicio,d.IdMoneda,�' �! Case Operacion when 'A' then '01'� � 			   when 'E' then '02'�" � 			   when 'C' then '03' end� �  � � ��C � �	 rlnegocio� �� F� � U  USUARIO CMDSQL SQL	 RLNEGOCIO
  �  � U  SETEO Init,     �� BeforeOpenTablesq    ��1 � A � ��1q�����!�	!��	�A�!�	�Q�	A	Q�	�!���QqaaA1�aA�q�!a A �� q  1 q 2                       ^     9   �  �  ]    )   }                  