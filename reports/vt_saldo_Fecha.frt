  �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=1
COLOR=2
                         Arial                                                         	DesMoneda                                                     	IdCliente                                                     
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         Jalltrim(IdComprobante) + " - " ,transform(nrocomprob,'@L 999-999-9999999')                                                    Arial                                                         	"Factura"                                                                                                                   Arial                                                         !soloresumen                                                  "Fecha:"                                                      Arial                                                         !"Saldos de Clientes a una Fecha "                             Arial                                                         empresa                                                                                                                     Arial                                                         m.Fecha                                                       Arial                                                         )iif(empty(m.sucursal),'Todos',m.sucursal)                                                                                   Arial                                                         
"Sucursal"                                                    Arial                                                         IdCliente,razsocial                                                                                                         Arial                                                         !SoloResumen                                                  "Cliente :"                                                                                                                 Arial                                                         ttod(fecha)                                                   "@D"                                                          Arial                                                         "Fecha"                                                                                                                     Arial                                                         !soloresumen                                                  IdCliente,razsocial                                                                                                         Arial                                                         SoloResumen                                                   saldo                                                         "99,999,999,999.99"                                           Arial                                                         "Total",DesMoneda                                             Arial                                                         saldo                                                         "99,999,999,999.99"                                                                                                         Arial                                                         "Saldo"                                                       Arial                                                         saldo                                                         "99,999,999,999.99"                                           Arial                                                         "Total"                                                       Arial                                                         TotalFactura                                                  "99,999,999,999.99"                                           Arial                                                         	"Importe"                                                     Arial                                                         	DesMoneda                                                     Arial                                                         !SoloResumen                                                  Saldo                                                         TotalFactura-Pagos                                            0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               aTop = 114
Left = 162
Width = 381
Height = 301
DataSource = .NULL.
Name = "Dataenvironment"
                            vPROCEDURE BeforeOpenTables
DO seteo
ENDPROC
PROCEDURE Init
Local sSQL

*--- agar 04/10/05

IF EMPTY(m.IdCliente)
	m.IdCliente = null
ENDIF
IF EMPTY(m.Sucursal)
	m.sucursal=null
ENDIF
	
	

*!*	sSQL = "select c.IdCliente, c.RazSocial, " + ;
*!*	              "f.IdComprobante, " + ;
*!*	              "f.Numero as NroComprob, " + ;
*!*	              "f.Fecha,fp.Vencimiento, " + ;
*!*	              "mo.Descripcion as DesMoneda, " + ;
*!*	              "sum( fp.Saldo ) as Saldo " + ;             
*!*	        "from vt_Forma_Pago fp, vt_factura f, vt_Clientes c, bs_monedas mo " + ;
*!*	       "where fp.IdFactura = f.IdFactura and fp.IdEmpresa = f.IdEmpresa " + ;
*!*	          "and f.IdCliente = c.IdCliente and f.IdEmpresa = c.IdEmpresa " + ;
*!*	          "and fp.IdMoneda = mo.IdMoneda " + ;
*!*	          " and f.IdEmpresa = ?oApp.Empresa " +;       
*!*	          "and ( '" + m.Sucursal + "' = '' or f.Sucursal = '" + m.Sucursal + "' ) " + ;
*!*	          "and f.fecha between ?m.dfecha and ?m.hfecha " + ;
*!*	          "and fp.vencimiento between ?m.dvence and ?m.hvence " + ;
*!*	          "and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente) " + ;
*!*	        "group by c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero, f.Fecha, fp.Vencimiento, mo.Descripcion " + ;
*!*	        " Having sum(fp.Saldo) <> 0 " + ;
*!*	        "order by 1, 2 ,3 ,4"

*!*	SELECT     c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero AS NroComprob, f.Fecha, mo.Descripcion AS DesMoneda, 
*!*	SUM(isnull(dp.importe,0) + isnull(nc.TotalFactura * -1,0)) AS Pagos, f.TotalFactura
*!*	FROM         vt_factura AS f INNER JOIN
*!*	                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa INNER JOIN
*!*	                      bs_Monedas AS mo ON f.IdMoneda = mo.IdMoneda LEFT OUTER JOIN
*!*	                      vt_factura AS nc ON f.IdEmpresa = nc.IdEmpresa AND f.IdComprobante = nc.IdComprobante_ref AND f.Numero = nc.Numero_ref and nc.Fecha <= ?m.Fecha LEFT OUTER JOIN
*!*	                      vt_pagos AS p INNER JOIN
*!*	                      vt_det_pagos AS dp ON p.idpago = dp.idpago and p.Fecha <= ?m.Fecha ON f.IdFactura = dp.idfactura
*!*	                      inner join vt_condicion cn on f.IdEmpresa = cn.IdEmpresa and f.Idcondicion = cn.Idcondicion 
*!*	where                       
*!*	           f.IdEmpresa = ?oApp.Empresa       
*!*	          and f.fecha <=?m.Fecha
*!*	          and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  
*!*	          and f.Numero_ref is null 
*!*	          and cn.plazo>0
*!*	          and YEAR(f.Fecha)>2008
*!*			GROUP BY c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero, f.Fecha, mo.Descripcion, f.TotalFactura
*!*	         Having SUM(isnull(dp.importe,0) + isnull(nc.TotalFactura * -1,0)) <> f.TotalFactura  
*!*	        order by mo.Descripcion ,1, f.fecha

TEXT TO cmdSQL noshow


SELECT     c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero AS NroComprob, f.Fecha, mo.Descripcion AS DesMoneda, 
isnull(dp.Pagos,0) + isnull(nc.Nc * -1,0) AS Pagos, f.TotalFactura
FROM         vt_factura AS f INNER JOIN
                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa INNER JOIN
                      bs_Monedas AS mo ON f.IdMoneda = mo.IdMoneda LEFT OUTER JOIN
                      (Select IdEmpresa,idcomprobante_ref,numero_ref,Sum(TotalFactura) as NC
							from vt_factura nc1
							where nc1.Fecha <= ?m.Fecha
							and nc1.IdEmpresa=?oApp.Empresa
							and not numero_ref is null
							group by IdEmpresa,idcomprobante_ref,numero_ref) nc 
						ON f.IdEmpresa = nc.IdEmpresa AND f.IdComprobante = nc.IdComprobante_ref AND f.Numero = nc.Numero_ref LEFT OUTER JOIN
                      (Select dp1.IdFactura,sum(Importe) as Pagos
							from vt_pagos AS p INNER JOIN
												  vt_det_pagos AS dp1 ON p.idpago = dp1.idpago 
							where p.Fecha <= ?m.Fecha
							and p.IdEmpresa=?oApp.Empresa
							group by dp1.IdFactura) dp
						ON f.IdFactura = dp.idfactura
                      inner join vt_condicion cn on f.IdEmpresa = cn.IdEmpresa and f.Idcondicion = cn.Idcondicion 
where                       
           f.IdEmpresa = ?oApp.Empresa 
          and f.fecha <=?m.Fecha
          and f.Numero_ref is null 
          and cn.plazo>0
          and YEAR(f.Fecha)>2008
		and isnull(dp.Pagos,0) + isnull(nc.NC * -1,0) <> f.TotalFactura  
		and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  
		AND (?M.Sucursal is null or f.Sucursal = ?m.sucursal)
        order by mo.Descripcion ,1, f.fecha        


ENDTEXT

sql( cmdSQL, 'xxSaldoCli' )

Select xxSaldoCli



ENDPROC
       	2���    	  	                        [   %   4      �  2   \          �  U  
  �  � U  SETEO� ��  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� �	 M(� �� �  � �  �z �t SELECT     c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero AS NroComprob, f.Fecha, mo.Descripcion AS DesMoneda, �H �B isnull(dp.Pagos,0) + isnull(nc.Nc * -1,0) AS Pagos, f.TotalFactura�- �' FROM         vt_factura AS f INNER JOIN�r �l                       vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa INNER JOIN�X �R                       bs_Monedas AS mo ON f.IdMoneda = mo.IdMoneda LEFT OUTER JOIN�b �\                       (Select IdEmpresa,idcomprobante_ref,numero_ref,Sum(TotalFactura) as NC�  � 							from vt_factura nc1�( �" 							where nc1.Fecha <= ?m.Fecha�, �& 							and nc1.IdEmpresa=?oApp.Empresa�' �! 							and not numero_ref is null�A �; 							group by IdEmpresa,idcomprobante_ref,numero_ref) nc �� �{ 						ON f.IdEmpresa = nc.IdEmpresa AND f.IdComprobante = nc.IdComprobante_ref AND f.Numero = nc.Numero_ref LEFT OUTER JOIN�G �A                       (Select dp1.IdFactura,sum(Importe) as Pagos�* �$ 							from vt_pagos AS p INNER JOIN�A �; 												  vt_det_pagos AS dp1 ON p.idpago = dp1.idpago �& �  							where p.Fecha <= ?m.Fecha�* �$ 							and p.IdEmpresa=?oApp.Empresa�' �! 							group by dp1.IdFactura) dp�) �# 						ON f.IdFactura = dp.idfactura�x �r                       inner join vt_condicion cn on f.IdEmpresa = cn.IdEmpresa and f.Idcondicion = cn.Idcondicion �" � where                       �- �'            f.IdEmpresa = ?oApp.Empresa �& �            and f.fecha <=?m.Fecha�) �#           and f.Numero_ref is null � �           and cn.plazo>0�& �            and YEAR(f.Fecha)>2008�I �C 		and isnull(dp.Pagos,0) + isnull(nc.NC * -1,0) <> f.TotalFactura  �C �= 		and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  �= �7 		AND (?M.Sucursal is null or f.Sucursal = ?m.sucursal)�9 �3         order by mo.Descripcion ,1, f.fecha        � �  � �  � � ��C � �
 xxSaldoCli� �� F� � U  SSQL	 IDCLIENTE SUCURSAL CMDSQL SQL
 XXSALDOCLI BeforeOpenTables,     �� InitA     ��1 q 2 q � A � A �� a a ���!�!��qq�a�q��!�a��a�1��a a A �r 4                       $         ?   k      )   	                              %ORIENTATION=0
PAPERSIZE=1
COLOR=2
                         Arial                                                         	DesMoneda                                                     	IdCliente                                                     
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         Jalltrim(IdComprobante) + " - " ,transform(nrocomprob,'@L 999-999-9999999')                                                    Arial                                                         	"Factura"                                                                                                                   Arial                                                         !soloresumen                                                  "Fecha:"                                                      Arial                                                         !"Saldos de Clientes a una Fecha "                             Arial                                                         empresa                                                                                                                     Arial                                                         m.Fecha                                                       Arial                                                         )iif(empty(m.sucursal),'Todos',m.sucursal)                                                                                   Arial                                                         
"Sucursal"                                                    Arial                                                         IdCliente,razsocial                                                                                                         Arial                                                         !SoloResumen                                                  "Cliente :"                                                                                                                 Arial                                                         ttod(fecha)                                                   "@D"                                                          Arial                                                         "Fecha"                                                                                                                     Arial                                                         !soloresumen                                                  IdCliente,razsocial                                                                                                         Arial                                                         SoloResumen                                                   saldo                                                         "99,999,999,999.99"                                           Arial                                                         "Total",DesMoneda                                             Arial                                                         saldo                                                         "99,999,999,999.99"                                                                                                         Arial                                                         "Saldo"                                                       Arial                                                         saldo                                                         "99,999,999,999.99"                                           Arial                                                         "Total"                                                       Arial                                                         TotalFactura                                                  "99,999,999,999.99"                                           Arial                                                         	"Importe"                                                     Arial                                                         	DesMoneda                                                     Arial                                                         !SoloResumen                                                  Saldo                                                         TotalFactura-Pagos                                            0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               aTop = 114
Left = 162
Width = 381
Height = 301
DataSource = .NULL.
Name = "Dataenvironment"
                            vPROCEDURE Init
Local sSQL

*--- agar 04/10/05

IF EMPTY(m.IdCliente)
	m.IdCliente = null
ENDIF
IF EMPTY(m.Sucursal)
	m.sucursal=null
ENDIF
	
	

*!*	sSQL = "select c.IdCliente, c.RazSocial, " + ;
*!*	              "f.IdComprobante, " + ;
*!*	              "f.Numero as NroComprob, " + ;
*!*	              "f.Fecha,fp.Vencimiento, " + ;
*!*	              "mo.Descripcion as DesMoneda, " + ;
*!*	              "sum( fp.Saldo ) as Saldo " + ;             
*!*	        "from vt_Forma_Pago fp, vt_factura f, vt_Clientes c, bs_monedas mo " + ;
*!*	       "where fp.IdFactura = f.IdFactura and fp.IdEmpresa = f.IdEmpresa " + ;
*!*	          "and f.IdCliente = c.IdCliente and f.IdEmpresa = c.IdEmpresa " + ;
*!*	          "and fp.IdMoneda = mo.IdMoneda " + ;
*!*	          " and f.IdEmpresa = ?oApp.Empresa " +;       
*!*	          "and ( '" + m.Sucursal + "' = '' or f.Sucursal = '" + m.Sucursal + "' ) " + ;
*!*	          "and f.fecha between ?m.dfecha and ?m.hfecha " + ;
*!*	          "and fp.vencimiento between ?m.dvence and ?m.hvence " + ;
*!*	          "and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente) " + ;
*!*	        "group by c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero, f.Fecha, fp.Vencimiento, mo.Descripcion " + ;
*!*	        " Having sum(fp.Saldo) <> 0 " + ;
*!*	        "order by 1, 2 ,3 ,4"

*!*	SELECT     c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero AS NroComprob, f.Fecha, mo.Descripcion AS DesMoneda, 
*!*	SUM(isnull(dp.importe,0) + isnull(nc.TotalFactura * -1,0)) AS Pagos, f.TotalFactura
*!*	FROM         vt_factura AS f INNER JOIN
*!*	                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa INNER JOIN
*!*	                      bs_Monedas AS mo ON f.IdMoneda = mo.IdMoneda LEFT OUTER JOIN
*!*	                      vt_factura AS nc ON f.IdEmpresa = nc.IdEmpresa AND f.IdComprobante = nc.IdComprobante_ref AND f.Numero = nc.Numero_ref and nc.Fecha <= ?m.Fecha LEFT OUTER JOIN
*!*	                      vt_pagos AS p INNER JOIN
*!*	                      vt_det_pagos AS dp ON p.idpago = dp.idpago and p.Fecha <= ?m.Fecha ON f.IdFactura = dp.idfactura
*!*	                      inner join vt_condicion cn on f.IdEmpresa = cn.IdEmpresa and f.Idcondicion = cn.Idcondicion 
*!*	where                       
*!*	           f.IdEmpresa = ?oApp.Empresa       
*!*	          and f.fecha <=?m.Fecha
*!*	          and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  
*!*	          and f.Numero_ref is null 
*!*	          and cn.plazo>0
*!*	          and YEAR(f.Fecha)>2008
*!*			GROUP BY c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero, f.Fecha, mo.Descripcion, f.TotalFactura
*!*	         Having SUM(isnull(dp.importe,0) + isnull(nc.TotalFactura * -1,0)) <> f.TotalFactura  
*!*	        order by mo.Descripcion ,1, f.fecha

TEXT TO cmdSQL noshow


SELECT     c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero AS NroComprob, f.Fecha, mo.Descripcion AS DesMoneda, 
isnull(dp.Pagos,0) + isnull(nc.Nc * -1,0) AS Pagos, f.TotalFactura
FROM         vt_factura AS f INNER JOIN
                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa INNER JOIN
                      bs_Monedas AS mo ON f.IdMoneda = mo.IdMoneda LEFT OUTER JOIN
                      (Select IdEmpresa,idcomprobante_ref,numero_ref,Sum(TotalFactura) as NC
							from vt_factura nc1
							where nc1.Fecha <= ?m.Fecha
							and nc1.IdEmpresa=?oApp.Empresa
							and not numero_ref is null
							group by IdEmpresa,idcomprobante_ref,numero_ref) nc 
						ON f.IdEmpresa = nc.IdEmpresa AND f.IdComprobante = nc.IdComprobante_ref AND f.Numero = nc.Numero_ref LEFT OUTER JOIN
                      (Select dp1.IdFactura,sum(Importe) as Pagos
							from vt_pagos AS p INNER JOIN
												  vt_det_pagos AS dp1 ON p.idpago = dp1.idpago 
							where p.Fecha <= ?m.Fecha
							and p.IdEmpresa=?oApp.Empresa
							group by dp1.IdFactura) dp
						ON f.IdFactura = dp.idfactura
                      inner join vt_condicion cn on f.IdEmpresa = cn.IdEmpresa and f.Idcondicion = cn.Idcondicion 
where                       
           f.IdEmpresa = ?oApp.Empresa 
          and f.fecha <=?m.Fecha
          and f.Numero_ref is null 
          and cn.plazo>0
          and YEAR(f.Fecha)>2008
		and isnull(dp.Pagos,0) + isnull(nc.NC * -1,0) <> f.TotalFactura  
		and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  
		AND (?M.Sucursal is null or f.Sucursal = ?m.sucursal)
        order by mo.Descripcion ,1, f.fecha        


ENDTEXT

sql( cmdSQL, 'xxSaldoCli' )

Select xxSaldoCli



ENDPROC
PROCEDURE BeforeOpenTables
DO seteo
ENDPROC
       	2���    	  	                        [   %   4      �  2   \          �  U  � ��  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� �	 M(� �� �  � �  �z �t SELECT     c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero AS NroComprob, f.Fecha, mo.Descripcion AS DesMoneda, �H �B isnull(dp.Pagos,0) + isnull(nc.Nc * -1,0) AS Pagos, f.TotalFactura�- �' FROM         vt_factura AS f INNER JOIN�r �l                       vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa INNER JOIN�X �R                       bs_Monedas AS mo ON f.IdMoneda = mo.IdMoneda LEFT OUTER JOIN�b �\                       (Select IdEmpresa,idcomprobante_ref,numero_ref,Sum(TotalFactura) as NC�  � 							from vt_factura nc1�( �" 							where nc1.Fecha <= ?m.Fecha�, �& 							and nc1.IdEmpresa=?oApp.Empresa�' �! 							and not numero_ref is null�A �; 							group by IdEmpresa,idcomprobante_ref,numero_ref) nc �� �{ 						ON f.IdEmpresa = nc.IdEmpresa AND f.IdComprobante = nc.IdComprobante_ref AND f.Numero = nc.Numero_ref LEFT OUTER JOIN�G �A                       (Select dp1.IdFactura,sum(Importe) as Pagos�* �$ 							from vt_pagos AS p INNER JOIN�A �; 												  vt_det_pagos AS dp1 ON p.idpago = dp1.idpago �& �  							where p.Fecha <= ?m.Fecha�* �$ 							and p.IdEmpresa=?oApp.Empresa�' �! 							group by dp1.IdFactura) dp�) �# 						ON f.IdFactura = dp.idfactura�x �r                       inner join vt_condicion cn on f.IdEmpresa = cn.IdEmpresa and f.Idcondicion = cn.Idcondicion �" � where                       �- �'            f.IdEmpresa = ?oApp.Empresa �& �            and f.fecha <=?m.Fecha�) �#           and f.Numero_ref is null � �           and cn.plazo>0�& �            and YEAR(f.Fecha)>2008�I �C 		and isnull(dp.Pagos,0) + isnull(nc.NC * -1,0) <> f.TotalFactura  �C �= 		and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  �= �7 		AND (?M.Sucursal is null or f.Sucursal = ?m.sucursal)�9 �3         order by mo.Descripcion ,1, f.fecha        � �  � �  � � ��C � �
 xxSaldoCli� �� F� � U  SSQL	 IDCLIENTE SUCURSAL CMDSQL SQL
 XXSALDOCLI
  �  � U  SETEO Init,     �� BeforeOpenTables    ��1 q � A � A �� a a ���!�!��qq�a�q��!�a��a�1��a a A �r 5 q 1                       <     0   c  k  b    )   	                        