  "�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Planilla de Movimiento Diario"      Arial      empresa             Arial      'iif(empty(m.sucursal),'Todos',Sucursal)      Arial      
"Sucursal"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Per�odo:"      Arial      "SS"      Arial      "S"      Arial      "A"      Arial      "B"      Arial      "C"      Arial      "PIC"      Arial      "Otros"      Arial      	"Contado"      Arial      	"Credito"      Arial      "Cheque"      Arial      	"Fecha
"      Arial      "Cpbte."      Arial      "Nro."      Arial      	"Cliente"      Arial      Fecha      "99/99"      Arial      comprobante      Arial      vt_rdiariocontrol.numero             Arial      Cliente             Arial      SS      "@Z 99999.9"      Arial      S      "@Z 9999.9"             Arial      A      "@Z 99999.9"      Arial      B      "@Z 99999.9"      Arial      C      "@Z 99999.9"      Arial      PIC      "@Z 99999.9"      Arial      Otros      "@Z 99999.9"      Arial      Contado      "@Z 999,999,999"      Arial      Credito      "@Z 999,999,999"      Arial      Cheque      "999,999,999"      Arial      Valores      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      SS      "@Z 99999.9"      Arial      S      "@Z 9999.9"             Arial      A      "@Z 99999.9"      Arial      B      "@Z 99999.9"      Arial      C      "@Z 99999.9"      Arial      PIC      "@Z 99999.9"      Arial      Otros      "@Z 99999.9"      Arial      Contado      "@Z 999,999,999"      Arial      Credito      "@Z 999,999,999"      Arial      Cheque      "@Z 999,999,999"      Arial      "Total General"      Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     ?PROCEDURE Init
If Empty(m.sucursal)
	Store null To sucursal
ENDIF

TEXT TO cmdSQL noshow

SELECT     a.Sucursal + d.Descripci�n AS Sucursal, a.Fecha, c.Abrev AS Comprobante, a.Numero, 
						RTRIM(a.IdCliente) + ' ' + b.RazSocial AS Cliente,
                        a.TotalFactura, 
                      	SUM(CASE when m.IdProducto = '01' then Cantidad else 0 end) as SS,
						SUM(CASE when m.IdProducto = '02' then Cantidad else 0 end) as S,
                      	SUM(CASE when m.IdProducto = '03' then Cantidad else 0 end) as A,
						SUM(CASE when m.IdProducto = '04' then Cantidad else 0 end) as B,                      	
						SUM(CASE when m.IdProducto = '05' then Cantidad else 0 end) as C,
						SUM(CASE when m.IdProducto = '06' then Cantidad else 0 end) as PIC,
						SUM(CASE when m.IdProducto NOT in('02','03','04','05','06','01') then  Cantidad else 0 end) as Otros,
						CASE when cond.Plazo = 0 then TotalFactura - ISNULL(val.Cheque,0) else 0 end as Contado,  
						CASE when cond.Plazo > 0 then TotalFactura else 0 end as Credito,
						CASE when cond.Plazo = 0 AND NOT val.IdFactura is null then VAL.cHEQUE else 0 end as Cheque,
						Valores = dbo.ts_DescripcionValores(a.IdFActura,'V')
FROM         
                      dbo.vt_factura a INNER JOIN
                      dbo.st_movimiento_Det m ON a.IdFactura = m.IdFactura INNER JOIN
                      dbo.st_Producto p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN
                      dbo.vt_Condicion cond ON a.IdEmpresa = cond.IdEmpresa AND a.IdCondicion = cond.IdCondicion 
                      Left join (SELECT     IdFactura, SUM(CASE WHEN idtipoValor = 2 THEN v.importe ELSE 0 END) AS Cheque 
						FROM         dbo.ts_valores_base v LEFT OUTER JOIN
						                      dbo.bs_bancos b ON v.idbanco = b.idbanco
						GROUP BY IdFactura) val
                      ON a.IdFactura = val.IdFactura LEFT OUTER JOIN
                      dbo.vt_cpbt c ON a.IdEmpresa = c.IdEmpresa AND a.IdComprobante = c.IdComprobante  LEFT OUTER JOIN
                      dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal LEFT OUTER JOIN
                      dbo.vt_clientes b ON a.IdEmpresa = b.IdEmpresa AND a.IdCliente = b.IdCliente 
                      where
	a.IdEmpresa = ?oApp.Empresa  and                     
	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha 
	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) 
	group by a.Sucursal + d.Descripci�n , a.Fecha, c.Abrev , a.Numero, RTRIM(a.IdCliente) + ' ' + b.RazSocial ,
                      a.TotalFactura, val.Cheque, cond.Plazo, val.Cheque, val.IdFActura, a.IdFActura
	ORDER BY a.sucursal, a.fecha, c.Abrev, a.numero 

ENDTEXT

	
	sql(cmdSQL,'vt_rdiariocontrol')
	SELECT vt_rdiariocontrol
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     ����    �  �                        $a   %   �      ;  /   �          �  U  F %�C��  ��� � J���(�  � �	 M(� �� �  �d �^ SELECT     a.Sucursal + d.Descripci�n AS Sucursal, a.Fecha, c.Abrev AS Comprobante, a.Numero, �> �8 						RTRIM(a.IdCliente) + ' ' + b.RazSocial AS Cliente,�. �(                         a.TotalFactura, �_ �Y                       	SUM(CASE when m.IdProducto = '01' then Cantidad else 0 end) as SS,�M �G 						SUM(CASE when m.IdProducto = '02' then Cantidad else 0 end) as S,�^ �X                       	SUM(CASE when m.IdProducto = '03' then Cantidad else 0 end) as A,�d �^ 						SUM(CASE when m.IdProducto = '04' then Cantidad else 0 end) as B,                      	�M �G 						SUM(CASE when m.IdProducto = '05' then Cantidad else 0 end) as C,�O �I 						SUM(CASE when m.IdProducto = '06' then Cantidad else 0 end) as PIC,�q �k 						SUM(CASE when m.IdProducto NOT in('02','03','04','05','06','01') then  Cantidad else 0 end) as Otros,�f �` 						CASE when cond.Plazo = 0 then TotalFactura - ISNULL(val.Cheque,0) else 0 end as Contado,  �M �G 						CASE when cond.Plazo > 0 then TotalFactura else 0 end as Credito,�h �b 						CASE when cond.Plazo = 0 AND NOT val.IdFactura is null then VAL.cHEQUE else 0 end as Cheque,�@ �: 						Valores = dbo.ts_DescripcionValores(a.IdFActura,'V')� � FROM         �7 �1                       dbo.vt_factura a INNER JOIN�[ �U                       dbo.st_movimiento_Det m ON a.IdFactura = m.IdFactura INNER JOIN�u �o                       dbo.st_Producto p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN�w �q                       dbo.vt_Condicion cond ON a.IdEmpresa = cond.IdEmpresa AND a.IdCondicion = cond.IdCondicion �� �z                       Left join (SELECT     IdFactura, SUM(CASE WHEN idtipoValor = 2 THEN v.importe ELSE 0 END) AS Cheque �> �8 						FROM         dbo.ts_valores_base v LEFT OUTER JOIN�J �D 						                      dbo.bs_bancos b ON v.idbanco = b.idbanco�# � 						GROUP BY IdFactura) val�J �D                       ON a.IdFactura = val.IdFactura LEFT OUTER JOIN�} �w                       dbo.vt_cpbt c ON a.IdEmpresa = c.IdEmpresa AND a.IdComprobante = c.IdComprobante  LEFT OUTER JOIN�s �m                       dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal LEFT OUTER JOIN�i �c                       dbo.vt_clientes b ON a.IdEmpresa = b.IdEmpresa AND a.IdCliente = b.IdCliente �! �                       where�< �6 	a.IdEmpresa = ?oApp.Empresa  and                     �/ �) 	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha �= �7 	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) �r �l 	group by a.Sucursal + d.Descripci�n , a.Fecha, c.Abrev , a.Numero, RTRIM(a.IdCliente) + ' ' + b.RazSocial ,�j �d                       a.TotalFactura, val.Cheque, cond.Plazo, val.Cheque, val.IdFActura, a.IdFActura�7 �1 	ORDER BY a.sucursal, a.fecha, c.Abrev, a.numero � �  � �" ��C � � vt_rdiariocontrol� �� F� � U  SUCURSAL CMDSQL SQL VT_RDIARIOCONTROL
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � A � a A�����A��a��1q�Qq��1��1����!�qa A #q 2 q 1                            -   ,  4  1    )   �                  