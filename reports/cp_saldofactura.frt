  6   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=1
COLOR=2
                         Arial                                                         idproveedor                                                   empresa                                                                                                                     Arial                                                         "Saldos a Pagar por Proveedor"                                Arial                                                         "  Proveedor"                                                 Arial                                                         cp_rvencefactu.facturaproveedor                                                                                             Arial                                                         "En Fecha :"                                                  Arial                                                         SaldoGS                                                       "@Z 9,999,999,999"                                             cp_rvencefactu.saldo                                         Arial                                                         "Saldo GS"                                                   Arial                                                         "Comprobante"                                                 Arial                                                         $cp_rvencefactu.idproveedor," ",razon                                                                                        Arial                                                         "Fecha Factura"                                              Arial                                                         ttod(cp_rvencefactu.fecha)                                    "@D"                                                          Arial                                                         m.Fecha                                                       "@D"                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         "Total Proveedor"                                             Arial                                                         "Total General "                                                                                                            Arial                                                         SaldoUSD                                                      "@Z 9,999,999,999.99"                                          cp_rvencefactu.saldo                                         Arial                                                         "Saldo U$S"                                                  Arial                                                         SaldoUSD                                                      "@Z 999,999,999,999.99"                                       Arial                                                         SaldoGS                                                       "@Z 999,999,999,999"                                          Arial                                                         SaldoUSD                                                      "@Z 999,999,999,999.99"                                       Arial                                                         SaldoGS                                                       "@Z 999,999,999,999"                                          Arial                                                         lower(LCSELEREPO)                                             Arial                                                         Importe                                                       "@Z 9,999,999,999.99"                                          cp_rvencefactu.saldo                                         Arial                                                         	"Importe"                                                     Arial                                                         SaldoGS                                                       *iif(IdMoneda='GS',Importe-nvl(pagado,0),0)                    0                                                             SaldoUSD                                                      +iif(IdMoneda='U$S',importe-nvl(pagado,0),0)                   0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
                                                     SPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
LOCAL strsql
IF EMPTY(m.TipoCompra)
	m.TipoCompra=null
ENDIF

IF EMPTY(m.dproveedor)
	m.dproveedor=null
ENDIF
	
	
SET DATABASE TO DATOS 
TEXT TO cmdSQL noshow
SELECT     SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) ) AS Pagado,f.IdProveedor, f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total as Importe, cp_proveedor.Razon, f.IdMoneda
FROM         cp_proveedor INNER JOIN
                      cp_factura AS f ON cp_proveedor.IdEmpresa = f.IdEmpresa AND cp_proveedor.IdProveedor = f.IdProveedor 
					  LEFT OUTER JOIN (Select pd.idfactura, SUM(isnull(pd.importe,0)) Importe from 
                      cp_pagosdet_base AS pd INNER JOIN
                      cp_pagos_base AS p ON pd.idpago = p.idpago
					  where p.fecha <=?m.Fecha group by pd.idfactura) pd ON f.IdFactura = pd.idfactura 
left join (Select nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref, sum(isnull(nc.total,0) *-1) Total  from  cp_factura AS nc
group by nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref) nc  on f.IdEmpresa=nc.IdEmpresa and f.IdProveedor = nc.IdProveedor and f.FacturaProveedor = nc.FacturaProveedor_ref
INNER JOIN cp_condicion c ON f.IdEmpresa = c.idempresa AND f.IdCondicion = c.idcondicion
where  
	(CHARINDEX(f.Tipo, ?m.TipoCompra  ) > 0 or ?m.TipoCompra='T') 
	AND (f.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor or ?m.dproveedor is null)
	AND (f.Fecha <= ?m.Fecha)
	And f.idempresa= ?oApp.Empresa
	and f.FacturaProveedor_ref is null
	and c.plazo>0
	and YEAR(f.fecha)>2008
GROUP BY f.IdProveedor,f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total, cp_proveedor.Razon, f.IdMoneda
having SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) )<>f.Total
order by  f.Idproveedor, f.Fecha
ENDTEXT

=sql(cmdSQL ,'cp_rvencefactu')
SELECT cp_rvencefactu




ENDPROC
                                          d���    K  K                        P�   %   �      �  %   �          �  U  
  �  � U  SETEO� ��  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� � G(� DATOS�	 M(� ��� �� SELECT     SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) ) AS Pagado,f.IdProveedor, f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total as Importe, cp_proveedor.Razon, f.IdMoneda�* �$ FROM         cp_proveedor INNER JOIN�� �{                       cp_factura AS f ON cp_proveedor.IdEmpresa = f.IdEmpresa AND cp_proveedor.IdProveedor = f.IdProveedor �Z �T 					  LEFT OUTER JOIN (Select pd.idfactura, SUM(isnull(pd.importe,0)) Importe from �= �7                       cp_pagosdet_base AS pd INNER JOIN�F �@                       cp_pagos_base AS p ON pd.idpago = p.idpago�^ �X 					  where p.fecha <=?m.Fecha group by pd.idfactura) pd ON f.IdFactura = pd.idfactura �� �� left join (Select nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref, sum(isnull(nc.total,0) *-1) Total  from  cp_factura AS nc�� �� group by nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref) nc  on f.IdEmpresa=nc.IdEmpresa and f.IdProveedor = nc.IdProveedor and f.FacturaProveedor = nc.FacturaProveedor_ref�^ �X INNER JOIN cp_condicion c ON f.IdEmpresa = c.idempresa AND f.IdCondicion = c.idcondicion� � where  �E �? 	(CHARINDEX(f.Tipo, ?m.TipoCompra  ) > 0 or ?m.TipoCompra='T') �[ �U 	AND (f.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor or ?m.dproveedor is null)�  � 	AND (f.Fecha <= ?m.Fecha)�% � 	And f.idempresa= ?oApp.Empresa�) �# 	and f.FacturaProveedor_ref is null� � 	and c.plazo>0� � 	and YEAR(f.fecha)>2008�n �h GROUP BY f.IdProveedor,f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total, cp_proveedor.Razon, f.IdMoneda�G �A having SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) )<>f.Total�& �  order by  f.Idproveedor, f.Fecha� � ��C � � cp_rvencefactu� �� F� � U  STRSQL
 TIPOCOMPRA
 DPROVEEDOR DATOS CMDSQL SQL CP_RVENCEFACTU BeforeOpenTables,     �� InitA     ��1 q 3 q � A � A � � ����a����� Q�Q�A��qaA �q 5                       &         A   H      )   K                                            ORIENTATION=0
PAPERSIZE=1
                                  Arial                                                         idproveedor                                                   empresa                                                                                                                     Arial                                                         "Saldos a Pagar por Proveedor"                                Arial                                                         "  Proveedor"                                                 Arial                                                         cp_rvencefactu.facturaproveedor                                                                                             Arial                                                         "En Fecha :"                                                  Arial                                                         SaldoGS                                                       "@Z 9,999,999,999"                                             cp_rvencefactu.saldo                                         Arial                                                         "Saldo GS"                                                   Arial                                                         "Comprobante"                                                 Arial                                                         $cp_rvencefactu.idproveedor," ",razon                                                                                        Arial                                                         "Fecha Factura"                                              Arial                                                         ttod(cp_rvencefactu.fecha)                                    "@D"                                                          Arial                                                         m.Fecha                                                       "@D"                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         "Total Proveedor"                                             Arial                                                         "Total General "                                                                                                            Arial                                                         SaldoUSD                                                      "@Z 9,999,999,999.99"                                          cp_rvencefactu.saldo                                         Arial                                                         "Saldo U$S"                                                  Arial                                                         SaldoUSD                                                      "@Z 999,999,999,999.99"                                       Arial                                                         SaldoGS                                                       "@Z 999,999,999,999"                                          Arial                                                         SaldoUSD                                                      "@Z 999,999,999,999.99"                                       Arial                                                         SaldoGS                                                       "@Z 999,999,999,999"                                          Arial                                                         lower(LCSELEREPO)                                             Arial                                                         Importe                                                       "@Z 9,999,999,999.99"                                          cp_rvencefactu.saldo                                         Arial                                                         	"Importe"                                                     Arial                                                         SaldoGS                                                       *iif(IdMoneda='GS',Importe-nvl(pagado,0),0)                    0                                                             SaldoUSD                                                      +iif(IdMoneda='U$S',importe-nvl(pagado,0),0)                   0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
                                                     SPROCEDURE Init
LOCAL strsql
IF EMPTY(m.TipoCompra)
	m.TipoCompra=null
ENDIF

IF EMPTY(m.dproveedor)
	m.dproveedor=null
ENDIF
	
	
SET DATABASE TO DATOS 
TEXT TO cmdSQL noshow
SELECT     SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) ) AS Pagado,f.IdProveedor, f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total as Importe, cp_proveedor.Razon, f.IdMoneda
FROM         cp_proveedor INNER JOIN
                      cp_factura AS f ON cp_proveedor.IdEmpresa = f.IdEmpresa AND cp_proveedor.IdProveedor = f.IdProveedor 
					  LEFT OUTER JOIN (Select pd.idfactura, SUM(isnull(pd.importe,0)) Importe from 
                      cp_pagosdet_base AS pd INNER JOIN
                      cp_pagos_base AS p ON pd.idpago = p.idpago
					  where p.fecha <=?m.Fecha group by pd.idfactura) pd ON f.IdFactura = pd.idfactura 
left join (Select nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref, sum(isnull(nc.total,0) *-1) Total  from  cp_factura AS nc
group by nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref) nc  on f.IdEmpresa=nc.IdEmpresa and f.IdProveedor = nc.IdProveedor and f.FacturaProveedor = nc.FacturaProveedor_ref
INNER JOIN cp_condicion c ON f.IdEmpresa = c.idempresa AND f.IdCondicion = c.idcondicion
where  
	(CHARINDEX(f.Tipo, ?m.TipoCompra  ) > 0 or ?m.TipoCompra='T') 
	AND (f.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor or ?m.dproveedor is null)
	AND (f.Fecha <= ?m.Fecha)
	And f.idempresa= ?oApp.Empresa
	and f.FacturaProveedor_ref is null
	and c.plazo>0
	and YEAR(f.fecha)>2008
GROUP BY f.IdProveedor,f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total, cp_proveedor.Razon, f.IdMoneda
having SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) )<>f.Total
order by  f.Idproveedor, f.Fecha
ENDTEXT

=sql(cmdSQL ,'cp_rvencefactu')
SELECT cp_rvencefactu




ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                          d���    K  K                        P�   %   �      �  %   �          �  U  � ��  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� � G(� DATOS�	 M(� ��� �� SELECT     SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) ) AS Pagado,f.IdProveedor, f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total as Importe, cp_proveedor.Razon, f.IdMoneda�* �$ FROM         cp_proveedor INNER JOIN�� �{                       cp_factura AS f ON cp_proveedor.IdEmpresa = f.IdEmpresa AND cp_proveedor.IdProveedor = f.IdProveedor �Z �T 					  LEFT OUTER JOIN (Select pd.idfactura, SUM(isnull(pd.importe,0)) Importe from �= �7                       cp_pagosdet_base AS pd INNER JOIN�F �@                       cp_pagos_base AS p ON pd.idpago = p.idpago�^ �X 					  where p.fecha <=?m.Fecha group by pd.idfactura) pd ON f.IdFactura = pd.idfactura �� �� left join (Select nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref, sum(isnull(nc.total,0) *-1) Total  from  cp_factura AS nc�� �� group by nc.IdEmpresa , nc.IdProveedor , nc.FacturaProveedor_ref) nc  on f.IdEmpresa=nc.IdEmpresa and f.IdProveedor = nc.IdProveedor and f.FacturaProveedor = nc.FacturaProveedor_ref�^ �X INNER JOIN cp_condicion c ON f.IdEmpresa = c.idempresa AND f.IdCondicion = c.idcondicion� � where  �E �? 	(CHARINDEX(f.Tipo, ?m.TipoCompra  ) > 0 or ?m.TipoCompra='T') �[ �U 	AND (f.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor or ?m.dproveedor is null)�  � 	AND (f.Fecha <= ?m.Fecha)�% � 	And f.idempresa= ?oApp.Empresa�) �# 	and f.FacturaProveedor_ref is null� � 	and c.plazo>0� � 	and YEAR(f.fecha)>2008�n �h GROUP BY f.IdProveedor,f.FacturaProveedor, f.Fecha, f.IdFactura, f.Total, cp_proveedor.Razon, f.IdMoneda�G �A having SUM(isnull(pd.importe,0) + (isnull(nc.total,0)) )<>f.Total�& �  order by  f.Idproveedor, f.Fecha� � ��C � � cp_rvencefactu� �� F� � U  STRSQL
 TIPOCOMPRA
 DPROVEEDOR DATOS CMDSQL SQL CP_RVENCEFACTU
  �  � U  SETEO Init,     �� BeforeOpenTablesk    ��1 q � A � A � � ����a����� Q�Q�A��qaA �q 6 q 2                            #   >  H  ,    )   K                                      