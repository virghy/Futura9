  f   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=1
COLOR=2
00 J110 series
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
YRESOLUTION=300
COLLATE=1
                                                                  G  /  winspool  HP Deskjet 1000 J110 series  USB001                                                                         Arial                                                         idmoneda                                                      vt_rdiariocontrol.fecha                                       "Diario de Control de Ventas"                                                                                               Arial                                                         empresa                                                                                                                     Arial                                                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                                                  Arial                                                         
"Sucursal"                                                    Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         "Cpbte."                                                      Arial                                                         "Nro."                                                        Arial                                                         "Pedido"                                                      Arial                                                         	"Cliente"                                                     Arial                                                         	"Exentas"                                                     Arial                                                         
"Gravadas"                                                    Arial                                                         "Iva"                                                         Arial                                                         "Total"                                                       Arial                                                         	"Fecha
"                                                     Arial                                                         " Moneda : " +idmoneda                                                                                                      Arial                                                         ttod(vt_rdiariocontrol.fecha)                                 Arial                                                         vt_rdiariocontrol.idcomprobante                                                                                             Arial                                                         vt_rdiariocontrol.numero                                      Arial                                                         vt_rdiariocontrol.nropedido                                                                                                 Arial                                                         Brtrim(vt_rdiariocontrol.idcliente)+'-'+vt_rdiariocontrol.razsocial                                                            Arial                                                         vt_rdiariocontrol.exenta                                      "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.gravada                                     "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.iva                                         "999,999,999.99"                                              Arial                                                         Nvt_rdiariocontrol.exenta +  vt_rdiariocontrol.gravada +  vt_rdiariocontrol.iva                                                "99,999,999,999.99"                                           Arial                                                         '"Total "+ dtoc(vt_rdiariocontrol.fecha)                                                                                     Arial                                                         vt_rdiariocontrol.exenta                                      "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.gravada                                     "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.iva                                         "999,999,999.99"                                              Arial                                                         Nvt_rdiariocontrol.exenta +  vt_rdiariocontrol.gravada +  vt_rdiariocontrol.iva                                                "99,999,999,999.99"                                           Arial                                                         " Total Moneda : " +idmoneda                                                                                                Arial                                                         vt_rdiariocontrol.exenta                                      "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.gravada                                     "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.iva                                         "999,999,999.99"                                              Arial                                                         Nvt_rdiariocontrol.exenta +  vt_rdiariocontrol.gravada +  vt_rdiariocontrol.iva                                                "99,999,999,999.99"                                           Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
*!*	LOCAL strsql
*!*	SET DATABASE TO DATOS 
*!*	strsql = 'SELECT a.sucursal, a.fecha, a.idcomprobante, '+;
*!*			 ' a.numero, a.idcliente, a.exenta, '+;
*!*			 ' a.gravada, a.iva, b.razsocial, '+;
*!*			 ' c.descripcion, d.descripci�n, a.idmoneda '+;
*!*			 'FROM vt_factura a, vt_clientes b , vt_cpbt c, sucursal d '+;
*!*			 'WHERE a.idcliente = b.idcliente '+;
*!*			 '  AND a.idcomprobante = c.idcomprobante '+;
*!*			 '  AND a.sucursal = d.sucursal '+;
*!*			 '  AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha '+;
*!*			 '  AND a.sucursal = ?m.sucursal and a.anulado=0 '+;
*!*			 'ORDER BY a.sucursal, a.fecha, a.idcomprobante, '+;
*!*			 ' a.numero '

*!*	= sql(strsql,'vt_rdiariocontrol')
*!*	SELECT vt_rdiariocontrol

If Empty(m.sucursal)
	Store null To sucursal
ENDIF

TEXT TO cmdSQL noshow

SELECT     a.Sucursal, a.Fecha, a.IdComprobante, a.Numero, a.IdCliente, 
	ISNULL(a.Exenta,0) as Exenta, ISNULL(a.Gravada,0) as Gravada, ISNULL(a.Iva,0) as Iva, ISNULL(a.RazonSocial,b.RazSocial) RazSocial, c.Descripcion, d.Descripci�n, a.IdMoneda, 
                      e.NroPedido
FROM         dbo.vt_factura a LEFT OUTER JOIN
                      dbo.VT_Pedido e ON a.IdPedido = e.IdPedido AND a.IdEmpresa = e.IdEmpresa LEFT OUTER JOIN
                      dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal LEFT OUTER JOIN
                      dbo.vt_cpbt c ON a.IdEmpresa = c.IdEmpresa AND a.IdComprobante = c.IdComprobante LEFT OUTER JOIN
                      dbo.vt_clientes b ON a.IdEmpresa = b.IdEmpresa AND a.IdCliente = b.IdCliente
where
	a.IdEmpresa = ?oApp.Empresa  and                     
	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha 
	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) 
	ORDER BY a.IdMoneda, a.sucursal, a.fecha, a.idcomprobante, 
	a.numero 

ENDTEXT

	
	sql(cmdSQL,'vt_rdiariocontrol')
	SELECT vt_rdiariocontrol
ENDPROC
                                                              ����    �  �                        �1   %          `     (          �  U  
  �  � U  SETEO� %�C��  ��� � J���(�  � �	 M(� �� �  �N �H SELECT     a.Sucursal, a.Fecha, a.IdComprobante, a.Numero, a.IdCliente, �� �� 	ISNULL(a.Exenta,0) as Exenta, ISNULL(a.Gravada,0) as Gravada, ISNULL(a.Iva,0) as Iva, ISNULL(a.RazonSocial,b.RazSocial) RazSocial, c.Descripcion, d.Descripci�n, a.IdMoneda, �' �!                       e.NroPedido�3 �- FROM         dbo.vt_factura a LEFT OUTER JOIN�t �n                       dbo.VT_Pedido e ON a.IdPedido = e.IdPedido AND a.IdEmpresa = e.IdEmpresa LEFT OUTER JOIN�s �m                       dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal LEFT OUTER JOIN�| �v                       dbo.vt_cpbt c ON a.IdEmpresa = c.IdEmpresa AND a.IdComprobante = c.IdComprobante LEFT OUTER JOIN�h �b                       dbo.vt_clientes b ON a.IdEmpresa = b.IdEmpresa AND a.IdCliente = b.IdCliente� � where�< �6 	a.IdEmpresa = ?oApp.Empresa  and                     �/ �) 	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha �= �7 	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) �B �< 	ORDER BY a.IdMoneda, a.sucursal, a.fecha, a.idcomprobante, � �
 	a.numero � �  � �" ��C � � vt_rdiariocontrol� �� F� � U  SUCURSAL CMDSQL SQL VT_RDIARIOCONTROL BeforeOpenTables,     �� InitA     ��1 q 2  � A � a �Aq1A1��� ���!a A #q 1                       $         ?   �      )   �                                                              �DRIVER=winspool
DEVICE=HP Deskjet 1000 J110 series
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
YRESOLUTION=300
COLLATE=1
                                                                  G  /  winspool  HP Deskjet 1000 J110 series  USB001                                                                         Arial                                                         idmoneda                                                      vt_rdiariocontrol.fecha                                       "Diario de Control de Ventas"                                                                                               Arial                                                         empresa                                                                                                                     Arial                                                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                                                  Arial                                                         
"Sucursal"                                                    Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         "Cpbte."                                                      Arial                                                         "Nro."                                                        Arial                                                         "Pedido"                                                      Arial                                                         	"Cliente"                                                     Arial                                                         	"Exentas"                                                     Arial                                                         
"Gravadas"                                                    Arial                                                         "Iva"                                                         Arial                                                         "Total"                                                       Arial                                                         	"Fecha
"                                                     Arial                                                         " Moneda : " +idmoneda                                                                                                      Arial                                                         ttod(vt_rdiariocontrol.fecha)                                 Arial                                                         vt_rdiariocontrol.idcomprobante                                                                                             Arial                                                         vt_rdiariocontrol.numero                                      Arial                                                         vt_rdiariocontrol.nropedido                                                                                                 Arial                                                         Brtrim(vt_rdiariocontrol.idcliente)+'-'+vt_rdiariocontrol.razsocial                                                            Arial                                                         vt_rdiariocontrol.exenta                                      "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.gravada                                     "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.iva                                         "999,999,999.99"                                              Arial                                                         Nvt_rdiariocontrol.exenta +  vt_rdiariocontrol.gravada +  vt_rdiariocontrol.iva                                                "99,999,999,999.99"                                           Arial                                                         '"Total "+ dtoc(vt_rdiariocontrol.fecha)                                                                                     Arial                                                         vt_rdiariocontrol.exenta                                      "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.gravada                                     "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.iva                                         "999,999,999.99"                                              Arial                                                         Nvt_rdiariocontrol.exenta +  vt_rdiariocontrol.gravada +  vt_rdiariocontrol.iva                                                "99,999,999,999.99"                                           Arial                                                         " Total Moneda : " +idmoneda                                                                                                Arial                                                         vt_rdiariocontrol.exenta                                      "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.gravada                                     "99,999,999,999.99"                                           Arial                                                         vt_rdiariocontrol.iva                                         "999,999,999.99"                                              Arial                                                         Nvt_rdiariocontrol.exenta +  vt_rdiariocontrol.gravada +  vt_rdiariocontrol.iva                                                "99,999,999,999.99"                                           Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
                     �PROCEDURE Init
*!*	LOCAL strsql
*!*	SET DATABASE TO DATOS 
*!*	strsql = 'SELECT a.sucursal, a.fecha, a.idcomprobante, '+;
*!*			 ' a.numero, a.idcliente, a.exenta, '+;
*!*			 ' a.gravada, a.iva, b.razsocial, '+;
*!*			 ' c.descripcion, d.descripci�n, a.idmoneda '+;
*!*			 'FROM vt_factura a, vt_clientes b , vt_cpbt c, sucursal d '+;
*!*			 'WHERE a.idcliente = b.idcliente '+;
*!*			 '  AND a.idcomprobante = c.idcomprobante '+;
*!*			 '  AND a.sucursal = d.sucursal '+;
*!*			 '  AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha '+;
*!*			 '  AND a.sucursal = ?m.sucursal and a.anulado=0 '+;
*!*			 'ORDER BY a.sucursal, a.fecha, a.idcomprobante, '+;
*!*			 ' a.numero '

*!*	= sql(strsql,'vt_rdiariocontrol')
*!*	SELECT vt_rdiariocontrol

If Empty(m.sucursal)
	Store null To sucursal
ENDIF

TEXT TO cmdSQL noshow

SELECT     a.Sucursal, a.Fecha, a.IdComprobante, a.Numero, a.IdCliente, 
	ISNULL(a.Exenta,0) as Exenta, ISNULL(a.Gravada,0) as Gravada, ISNULL(a.Iva,0) as Iva, ISNULL(a.RazonSocial,b.RazSocial) RazSocial, c.Descripcion, d.Descripci�n, a.IdMoneda, 
                      e.NroPedido
FROM         dbo.vt_factura a LEFT OUTER JOIN
                      dbo.VT_Pedido e ON a.IdPedido = e.IdPedido AND a.IdEmpresa = e.IdEmpresa LEFT OUTER JOIN
                      dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal LEFT OUTER JOIN
                      dbo.vt_cpbt c ON a.IdEmpresa = c.IdEmpresa AND a.IdComprobante = c.IdComprobante LEFT OUTER JOIN
                      dbo.vt_clientes b ON a.IdEmpresa = b.IdEmpresa AND a.IdCliente = b.IdCliente
where
	a.IdEmpresa = ?oApp.Empresa  and                     
	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha 
	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) 
	ORDER BY a.IdMoneda, a.sucursal, a.fecha, a.idcomprobante, 
	a.numero 

ENDTEXT

	
	sql(cmdSQL,'vt_rdiariocontrol')
	SELECT vt_rdiariocontrol
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
                                                              ����    �  �                        �1   %          `     (          �  U  � %�C��  ��� � J���(�  � �	 M(� �� �  �N �H SELECT     a.Sucursal, a.Fecha, a.IdComprobante, a.Numero, a.IdCliente, �� �� 	ISNULL(a.Exenta,0) as Exenta, ISNULL(a.Gravada,0) as Gravada, ISNULL(a.Iva,0) as Iva, ISNULL(a.RazonSocial,b.RazSocial) RazSocial, c.Descripcion, d.Descripci�n, a.IdMoneda, �' �!                       e.NroPedido�3 �- FROM         dbo.vt_factura a LEFT OUTER JOIN�t �n                       dbo.VT_Pedido e ON a.IdPedido = e.IdPedido AND a.IdEmpresa = e.IdEmpresa LEFT OUTER JOIN�s �m                       dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal LEFT OUTER JOIN�| �v                       dbo.vt_cpbt c ON a.IdEmpresa = c.IdEmpresa AND a.IdComprobante = c.IdComprobante LEFT OUTER JOIN�h �b                       dbo.vt_clientes b ON a.IdEmpresa = b.IdEmpresa AND a.IdCliente = b.IdCliente� � where�< �6 	a.IdEmpresa = ?oApp.Empresa  and                     �/ �) 	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha �= �7 	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) �B �< 	ORDER BY a.IdMoneda, a.sucursal, a.fecha, a.idcomprobante, � �
 	a.numero � �  � �" ��C � � vt_rdiariocontrol� �� F� � U  SUCURSAL CMDSQL SQL VT_RDIARIOCONTROL
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1  � A � a �Aq1A1��� ���!a A #q 2 q 1                       �        �  �  /    )   �                                                        