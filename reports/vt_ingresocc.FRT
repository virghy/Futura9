  n   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?ORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2970
PAPERWIDTH=2100
                                                               Arial                                                         IdCentro                                                      Origen                                                        %"Cuadro de Ingresos y Egresos por CC"                         Arial                                                         empresa                                                                                                                     Arial                                                         
"Sucursal"                                                    Arial                                                         .iif( empty(m.sucursal), 'Todos', m.sucursal  )                                                                              Arial                                                         
"Per�odo:"                                                    Arial                                                         m.dfecha,"  al ",m.hfecha                                     Arial                                                         	"Factura"                                                     "@I"                                                          Arial                                                         
"Concepto"                                                    Arial                                                         "Cpbte."                                                      Arial                                                         "Cliente/Proveedor"                                           Arial                                                         	"Importe"                                                     Arial                                                         	"Fecha
"                                                     Arial                                                         "Centro:" , IdCentro,Centro                                   Arial                                                         IdProducto,Producto                                           Arial                                                         cp_rcompraconcepto.fecha                                                                                                    Arial                                                         Comprobante                                                   Arial                                                         Numero                                                        Arial                                                         Nombre                                                        Arial                                                         Importe                                                       "999,999,999,999.99"                                          Arial                                                         Importe*Factor                                                "999,999,999,999.99"                                          Arial                                                         "Total Centro"                                                Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Importe*Factor                                                "999,999,999,999.99"                                          Arial                                                         "Total General"                                               Arial                                                         #iif(Origen='1V','Ventas','Compras')                           Arial                                                         Importe                                                       "999,999,999,999.99"                                          Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 95
Left = 13
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
DataSource = .NULL.
Name = "Dataenvironment"
                                                 �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init


IF EMPTY(m.sucursal)
	m.sucursal = null
ENDIF	

TEXT TO cmdSQL noshow
		Declare @Empresa char(3),@dFecha datetime, @hFecha datetime, @Sucursal char(3), @Centro char(10)


		Select @Empresa=?oApp.Empresa, @dFecha=?m.dFecha , @hFecha=?m.hFecha 

		SELECT d.Centro1 IdCentro,cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto, c.Sucursal + s.Descripci�n AS Sucursal,c.IdComprobante +'-' + vt_cpbt.Descripcion as Comprobante, 
		dbo.vt_FormatNroFactura(c.Numero) Numero, 
		convert(varchar(10),c.Fecha,103) Fecha, isnull(c.RazonSocial,isnull(p.RazSocial,p.Nombre)) as Nombre, 
							   Origen = '1V',
							  d.Importe * c.Cotizacion Importe,
							  Factor  =1
		FROM         vt_factura AS c left JOIN
					st_movimiento_Det d on c.IdFactura = d.IdFactura
					left join vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente left JOIN
		                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal left JOIN
		                      vt_cpbt ON c.IdEmpresa = vt_cpbt.IdEmpresa AND c.IdComprobante = vt_cpbt.IdComprobante
		                      left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto
							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro
		WHERE c.idempresa =@Empresa
		        and   c.fecha between @dFecha and @hFecha
		        and isnull(c.Anulado,0) = 0
				and (c.Sucursal = @Sucursal or @Sucursal is null)
			 and c.TotalFactura<>0
		union
			SELECT   d.Centro1 IdCentro, cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto,  c.Sucursal + s.Descripci�n AS Sucursal, 
				c.IdComprobante+'-'+cp.Comprobante Comprobante, 	
				case when isnumeric(c.FacturaProveedor)=1 then dbo.vt_FormatNroFactura(c.FacturaProveedor) else c.FacturaProveedor end Numero,
				convert(varchar(10),c.Fecha,103) Fecha, p.Razon AS Nombre, 
				Origen = '2C',
				 d.Importe * c.Cotizacion Importe,
				 Factor  =-1
			FROM         cp_factura AS c 
			left join st_movimiento_Det d on c.IdFactura = d.IdCompra
			INNER JOIN cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN
			                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal
			                      left join cp_comprobante cp on c.IdEmpresa=cp.idempresa and c.IdComprobante= cp.idcomprob
				left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto
							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro                      
			      WHERE c.idempresa = @Empresa
			        and   c.fecha between  @dFecha and @hFecha
					and (c.Sucursal = @Sucursal or @Sucursal is null)	
					and c.Tipo='C'
			 ORDER BY IdCentro, Origen, Fecha,IdProducto 
ENDTEXT

 = sql(cmdSQL,'cp_rcompraconcepto')
 SELECT cp_rcompraconcepto
 
ENDPROC
                       <���    #  #                        �J   %   6      �  6   ^          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� �	 M(� ��h �b 		Declare @Empresa char(3),@dFecha datetime, @hFecha datetime, @Sucursal char(3), @Centro char(10)� �  � �  �M �G 		Select @Empresa=?oApp.Empresa, @dFecha=?m.dFecha , @hFecha=?m.hFecha � �  �� �� 		SELECT d.Centro1 IdCentro,cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto, c.Sucursal + s.Descripci�n AS Sucursal,c.IdComprobante +'-' + vt_cpbt.Descripcion as Comprobante, �2 �, 		dbo.vt_FormatNroFactura(c.Numero) Numero, �n �h 		convert(varchar(10),c.Fecha,103) Fecha, isnull(c.RazonSocial,isnull(p.RazSocial,p.Nombre)) as Nombre, � � 							   Origen = '1V',�0 �* 							  d.Importe * c.Cotizacion Importe,� � 							  Factor  =1�. �( 		FROM         vt_factura AS c left JOIN�; �5 					st_movimiento_Det d on c.IdFactura = d.IdFactura�j �d 					left join vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente left JOIN�n �h 		                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal left JOIN�t �n 		                      vt_cpbt ON c.IdEmpresa = vt_cpbt.IdEmpresa AND c.IdComprobante = vt_cpbt.IdComprobante�u �o 		                      left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto�[ �U 							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro�# � 		WHERE c.idempresa =@Empresa�9 �3 		        and   c.fecha between @dFecha and @hFecha�+ �% 		        and isnull(c.Anulado,0) = 0�; �5 				and (c.Sucursal = @Sucursal or @Sucursal is null)� � 			 and c.TotalFactura<>0� � 		union�� �� 			SELECT   d.Centro1 IdCentro, cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto,  c.Sucursal + s.Descripci�n AS Sucursal, �; �5 				c.IdComprobante+'-'+cp.Comprobante Comprobante, 	�� �� 				case when isnumeric(c.FacturaProveedor)=1 then dbo.vt_FormatNroFactura(c.FacturaProveedor) else c.FacturaProveedor end Numero,�E �? 				convert(varchar(10),c.Fecha,103) Fecha, p.Razon AS Nombre, � � 				Origen = '2C',�, �& 				 d.Importe * c.Cotizacion Importe,� � 				 Factor  =-1�& �  			FROM         cp_factura AS c �B �< 			left join st_movimiento_Det d on c.IdFactura = d.IdCompra�o �i 			INNER JOIN cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN�e �_ 			                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal�x �r 			                      left join cp_comprobante cp on c.IdEmpresa=cp.idempresa and c.IdComprobante= cp.idcomprob�a �[ 				left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto�q �k 							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro                      �+ �% 			      WHERE c.idempresa = @Empresa�; �5 			        and   c.fecha between  @dFecha and @hFecha�= �7 					and (c.Sucursal = @Sucursal or @Sucursal is null)	� � 					and c.Tipo='C'�6 �0 			 ORDER BY IdCentro, Origen, Fecha,IdProducto � �# ��C � � cp_rcompraconcepto� �� F� � U  SUCURSAL CMDSQL SQL CP_RCOMPRACONCEPTO BeforeOpenTables,     �� InitA     ��1 q 3 � A � �a a �a !!�������AQ�1����� ���Q��aa!�Q�����aA 2q 2                       &         A   �      )   #                                                                                    ?ORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2970
PAPERWIDTH=2100
                                                               Arial                                                         IdCentro                                                      Origen                                                        %"Cuadro de Ingresos y Egresos por CC"                         Arial                                                         empresa                                                                                                                     Arial                                                         
"Sucursal"                                                    Arial                                                         .iif( empty(m.sucursal), 'Todos', m.sucursal  )                                                                              Arial                                                         
"Per�odo:"                                                    Arial                                                         m.dfecha,"  al ",m.hfecha                                     Arial                                                         	"Factura"                                                     "@I"                                                          Arial                                                         
"Concepto"                                                    Arial                                                         "Cpbte."                                                      Arial                                                         "Cliente/Proveedor"                                           Arial                                                         	"Importe"                                                     Arial                                                         	"Fecha
"                                                     Arial                                                         "Centro:" , IdCentro,Centro                                   Arial                                                         IdProducto,Producto                                           Arial                                                         cp_rcompraconcepto.fecha                                                                                                    Arial                                                         Comprobante                                                   Arial                                                         Numero                                                        Arial                                                         Nombre                                                        Arial                                                         Importe                                                       "999,999,999,999.99"                                          Arial                                                         Importe*Factor                                                "999,999,999,999.99"                                          Arial                                                         "Total Centro"                                                Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Importe*Factor                                                "999,999,999,999.99"                                          Arial                                                         "Total General"                                               Arial                                                         #iif(Origen='1V','Ventas','Compras')                           Arial                                                         Importe                                                       "999,999,999,999.99"                                          Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 95
Left = 13
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
DataSource = .NULL.
Name = "Dataenvironment"
                                                 �PROCEDURE Init


IF EMPTY(m.sucursal)
	m.sucursal = null
ENDIF	

TEXT TO cmdSQL noshow
		Declare @Empresa char(3),@dFecha datetime, @hFecha datetime, @Sucursal char(3), @Centro char(10)


		Select @Empresa=?oApp.Empresa, @dFecha=?m.dFecha , @hFecha=?m.hFecha 

		SELECT d.Centro1 IdCentro,cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto, c.Sucursal + s.Descripci�n AS Sucursal,c.IdComprobante +'-' + vt_cpbt.Descripcion as Comprobante, 
		dbo.vt_FormatNroFactura(c.Numero) Numero, 
		convert(varchar(10),c.Fecha,103) Fecha, isnull(c.RazonSocial,isnull(p.RazSocial,p.Nombre)) as Nombre, 
							   Origen = '1V',
							  d.Importe * c.Cotizacion Importe,
							  Factor  =1
		FROM         vt_factura AS c left JOIN
					st_movimiento_Det d on c.IdFactura = d.IdFactura
					left join vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente left JOIN
		                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal left JOIN
		                      vt_cpbt ON c.IdEmpresa = vt_cpbt.IdEmpresa AND c.IdComprobante = vt_cpbt.IdComprobante
		                      left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto
							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro
		WHERE c.idempresa =@Empresa
		        and   c.fecha between @dFecha and @hFecha
		        and isnull(c.Anulado,0) = 0
				and (c.Sucursal = @Sucursal or @Sucursal is null)
			 and c.TotalFactura<>0
		union
			SELECT   d.Centro1 IdCentro, cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto,  c.Sucursal + s.Descripci�n AS Sucursal, 
				c.IdComprobante+'-'+cp.Comprobante Comprobante, 	
				case when isnumeric(c.FacturaProveedor)=1 then dbo.vt_FormatNroFactura(c.FacturaProveedor) else c.FacturaProveedor end Numero,
				convert(varchar(10),c.Fecha,103) Fecha, p.Razon AS Nombre, 
				Origen = '2C',
				 d.Importe * c.Cotizacion Importe,
				 Factor  =-1
			FROM         cp_factura AS c 
			left join st_movimiento_Det d on c.IdFactura = d.IdCompra
			INNER JOIN cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN
			                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal
			                      left join cp_comprobante cp on c.IdEmpresa=cp.idempresa and c.IdComprobante= cp.idcomprob
				left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto
							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro                      
			      WHERE c.idempresa = @Empresa
			        and   c.fecha between  @dFecha and @hFecha
					and (c.Sucursal = @Sucursal or @Sucursal is null)	
					and c.Tipo='C'
			 ORDER BY IdCentro, Origen, Fecha,IdProducto 
ENDTEXT

 = sql(cmdSQL,'cp_rcompraconcepto')
 SELECT cp_rcompraconcepto
 
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                       <���    #  #                        �J   %   6      �  6   ^          �  U  � %�C��  ��� � T��  ���� �	 M(� ��h �b 		Declare @Empresa char(3),@dFecha datetime, @hFecha datetime, @Sucursal char(3), @Centro char(10)� �  � �  �M �G 		Select @Empresa=?oApp.Empresa, @dFecha=?m.dFecha , @hFecha=?m.hFecha � �  �� �� 		SELECT d.Centro1 IdCentro,cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto, c.Sucursal + s.Descripci�n AS Sucursal,c.IdComprobante +'-' + vt_cpbt.Descripcion as Comprobante, �2 �, 		dbo.vt_FormatNroFactura(c.Numero) Numero, �n �h 		convert(varchar(10),c.Fecha,103) Fecha, isnull(c.RazonSocial,isnull(p.RazSocial,p.Nombre)) as Nombre, � � 							   Origen = '1V',�0 �* 							  d.Importe * c.Cotizacion Importe,� � 							  Factor  =1�. �( 		FROM         vt_factura AS c left JOIN�; �5 					st_movimiento_Det d on c.IdFactura = d.IdFactura�j �d 					left join vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente left JOIN�n �h 		                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal left JOIN�t �n 		                      vt_cpbt ON c.IdEmpresa = vt_cpbt.IdEmpresa AND c.IdComprobante = vt_cpbt.IdComprobante�u �o 		                      left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto�[ �U 							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro�# � 		WHERE c.idempresa =@Empresa�9 �3 		        and   c.fecha between @dFecha and @hFecha�+ �% 		        and isnull(c.Anulado,0) = 0�; �5 				and (c.Sucursal = @Sucursal or @Sucursal is null)� � 			 and c.TotalFactura<>0� � 		union�� �� 			SELECT   d.Centro1 IdCentro, cn.descripci�n Centro, d.IdProducto, pr.Descripcion Producto,  c.Sucursal + s.Descripci�n AS Sucursal, �; �5 				c.IdComprobante+'-'+cp.Comprobante Comprobante, 	�� �� 				case when isnumeric(c.FacturaProveedor)=1 then dbo.vt_FormatNroFactura(c.FacturaProveedor) else c.FacturaProveedor end Numero,�E �? 				convert(varchar(10),c.Fecha,103) Fecha, p.Razon AS Nombre, � � 				Origen = '2C',�, �& 				 d.Importe * c.Cotizacion Importe,� � 				 Factor  =-1�& �  			FROM         cp_factura AS c �B �< 			left join st_movimiento_Det d on c.IdFactura = d.IdCompra�o �i 			INNER JOIN cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN�e �_ 			                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal�x �r 			                      left join cp_comprobante cp on c.IdEmpresa=cp.idempresa and c.IdComprobante= cp.idcomprob�a �[ 				left join st_Producto pr on d.IdEmpresa = pr.IdEmpresa and d.IdProducto = pr.IdProducto�q �k 							  left join centros cn on d.IdEmpresa = cn.idempresa and d.Centro1 = cn.centro                      �+ �% 			      WHERE c.idempresa = @Empresa�; �5 			        and   c.fecha between  @dFecha and @hFecha�= �7 					and (c.Sucursal = @Sucursal or @Sucursal is null)	� � 					and c.Tipo='C'�6 �0 			 ORDER BY IdCentro, Origen, Fecha,IdProducto � �# ��C � � cp_rcompraconcepto� �� F� � U  SUCURSAL CMDSQL SQL CP_RCOMPRACONCEPTO
  �  � U  SETEO Init,     �� BeforeOpenTables!    ��1 � A � �a a �a !!�������AQ�1����� ���Q��aa!�Q�����aA 2q 3 q 2                       j     4   �  �  :    )   #                                                                              