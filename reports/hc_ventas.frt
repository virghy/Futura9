  .�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP Deskjet 1000 J110 series
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=5
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
COLLATE=1
      G  /  winspool  HP Deskjet 1000 J110 series  USB001                       tHP Deskjet 1000 J110 series     � �C�   �4d   ,  ,  A4                                                           ����GIS4            DINU" `� �BO                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       `  SMTJ     P{ 9 D 5 0 9 E A 4 - F 7 B D - 4 9 c b - B B 3 4 - 4 5 6 0 8 A 5 D 7 A 0 0 }   InputBin 7 RESDLL UniresDLL DocumentNUp 1 JobPageOrder Reverse DeviceLanguage PCLmS ColorMode Off Collate ON PaperSize LETTER Orientation PORTRAIT Resolution 600x300 MediaType 0.1004.4770_0_600x300                                                                V4DM                         Arial      VT_rfactura.centrocosto      VT_rfactura.producto      
totalventa      round(VT_rfactura.itotal,0)      0      totalcabeza      VT_rfactura.cantidad      0      ptotalNetoventa      VT_rfactura.pneto      0      pesototalbruto      VT_rfactura.pbruto      0      cotizacion_centro      VT_rfactura.cotizacion      0      total_centro      VT_rfactura.itotal      0      
imptotales      VT_rfactura.itotal      0      cantidad_por_prod      VT_rfactura.cantidad      0      total_kpor_prod      VT_rfactura.pbruto      0      total_kl_neto_prod      VT_rfactura.pneto      0      imp_total_prod      VT_rfactura.itotal      0      imp_total_prod_us      VT_rfactura.itotal_us      0      cotizacion_prod      VT_rfactura.cotizacion      0      ultcotizacion      VT_rfactura.cotizacion      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "PLANILLA DE VENTAS"             Arial      empresa             Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Per�odo:"      Arial      VT_rfactura.descproducto             Arial      "Categor�a:"      Arial      "Centro de Costo:"      Arial      Centro             Arial      	"Cliente"      Arial      
"Cantidad"      Arial      " Peso 
Bruto"      Arial      " Peso Prom 
       Bruto"      Arial      " Peso Total 
Neto de Vta"      Arial      " Peso Prom 
Neto de Vta"      Arial      "    Kls.
Gancho"      Arial      "       %
Rend-Frig."      Arial      "Destare
kg. Cab."      Arial      "%"      Arial      "Importe X Cab."      Arial      "Ventas Totales"      Arial      	"Fecha
"      Arial      " Pre. Kls.
Gancho"      Arial      
"Cotizac."             Arial      "Importe X Kilos"      Arial      "Gs."      Arial      "Gs."      Arial      "Gs."      Arial      "U$s."      Arial      "U$s."      Arial      "U$s."      Arial             	razsocial             Arial      VT_rfactura.fecha             Arial      VT_rfactura.cantidad      "99,999"             Arial      VT_rfactura.pbruto      	"999,999"             Arial      (VT_rfactura.pbruto/ VT_rfactura.cantidad      
"9,999.99"             Arial      VT_rfactura.pneto      	"999,999"             Arial      0round(VT_rfactura.pneto/ VT_rfactura.cantidad,0)      "99,999"             Arial      VT_rfactura.pgancho      	"999,999"             Arial      Miif(VT_rfactura.pgancho>0,round(VT_rfactura.itotal/ VT_rfactura.pgancho,0),0)      	"999,999"      Arial      VT_rfactura.rendimiento      "999.99"             Arial      VT_rfactura.destare      "99999"             Arial      VT_rfactura.porcentaje      "99.9"             Arial      VT_rfactura.precio_kg_gs      "99,999"      Arial      VT_rfactura.precio_kg_us      "99.99"      Arial      VT_rfactura.precio_cab      "9,999,999"      Arial      VT_rfactura.precio_cab_us      "9,999"      Arial      VT_rfactura.itotal      "9,999,999,999"      Arial      vt_rfactura.itotal_US      "999,999,999"      Arial      VT_rfactura.cotizacion      "99,999"             Arial             "Totales Producto"      Arial      VT_rfactura.cantidad      "99,999"             Arial      VT_rfactura.pbruto      "99,999,999"             Arial      "total_kpor_prod/ cantidad_por_prod      "9,999"             Arial      VT_rfactura.pneto      "999,999,999"             Arial      %total_kl_neto_prod/ cantidad_por_prod      	"999,999"             Arial      Wround(( total_kpor_prod/ cantidad_por_prod)-( total_kl_neto_prod/ cantidad_por_prod),0)      "999"             Arial      7round(100 - (total_kl_neto_prod*100/total_kpor_prod),1)      "99.9"             Arial      +round(imp_total_prod/ total_kl_neto_prod,0)      "99,999"      Arial      .round(imp_total_prod_us/ total_kl_neto_prod,2)      "99.99"      Arial      !imp_total_prod/ cantidad_por_prod      "9,999,999"      Arial      -round(imp_total_prod_us/ cantidad_por_prod,2)      "9,999"      Arial      VT_rfactura.itotal      "99,999,999,999"      Arial      	ITOTAL_US      "99,999,999"      Arial      cotizacion_prod      "99,999"             Arial      VT_rfactura.descproducto             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 68
Left = 31
Width = 643
Height = 351
InitialSelectedAlias = "VT_rfactura"
DataSource = .NULL.
Name = "Dataenvironment"
     	vPROCEDURE Init
IF EMPTY(m.Centro)
	m.Centro=null
ENDIF
	

TEXT TO cmdSQL noshow

SELECT     d.IdProducto AS Producto, d.Cantidad, d.Precio, d.PNeto, d.PBruto, 
					d.ITotal * CASE WHEN F.IDMONEDA= 'GS' THEN 1 ELSE F.COTIZACION END AS ITOTAL,
					d.ITotal / CASE WHEN F.IDMONEDA <> 'GS' THEN 1 ELSE F.COTIZACION END AS ITOTAL_US, 
					d.IPromedio, d.TipoMov, d.PgAncho, ROUND((d.PBruto - d.PNeto) 
                      / d.Cantidad, 0) AS destare, 
                      ROUND(d.PgAncho / d.PNeto * 100, 1) AS rendimiento, 
                      ROUND(d.ITotal / d.PNeto * CASE WHEN F.IDMONEDA= 'GS' THEN 1 ELSE F.COTIZACION END , 0) AS precio_kg_gs, 
                      ROUND(d.ITotal / d.Cantidad * CASE WHEN F.IDMONEDA= 'GS' THEN 1 ELSE F.COTIZACION END, 0) AS precio_cab, 
                      f.CentroCosto, f.IdCliente, f.Fecha, f.Descuento, f.Vence, f.Exenta, f.Cotizacion, f.IdMoneda, f.Imagro, 
                      f.Senacsa, f.Guia, f.Comision, f.TipoVenta, 
                      ROUND(d.ITotal / CASE WHEN F.IDMONEDA<> 'GS' THEN 1 ELSE F.COTIZACION END, 1) AS imp_total_us, 
                      ROUND(d.ITotal / d.Cantidad / CASE WHEN F.IDMONEDA<> 'GS' THEN 1 ELSE F.COTIZACION END, 0) AS precio_cab_us, 
                      ROUND(d.ITotal / d.PNeto / CASE WHEN F.IDMONEDA<> 'GS' THEN 1 ELSE F.COTIZACION END, 2) AS precio_kg_us, 
                      ROUND((d.PBruto / d.Cantidad - d.PNeto / d.Cantidad) 
                      / (d.PBruto / d.Cantidad) * 100, 1) AS porcentaje, p.Descripcion AS descproducto, cl.RazSocial, dbo.centros.descripci�n AS Centro
FROM         dbo.centros RIGHT OUTER JOIN
                      dbo.vt_factura f INNER JOIN
                      dbo.st_movimiento_Det d ON f.IdFactura = d.IdFactura ON dbo.centros.idempresa = f.IdEmpresa AND 
                      dbo.centros.centro = f.CentroCosto RIGHT OUTER JOIN
                      dbo.st_Producto p ON d.IdProducto = p.IdProducto AND d.IdEmpresa = p.IdEmpresa RIGHT OUTER JOIN
                      dbo.vt_clientes cl ON f.IdEmpresa = cl.IdEmpresa AND f.IdCliente = cl.IdCliente
where f.idempresa = ?oApp.empresa
   AND f.fecha BETWEEN ?dfecha AND ?hfecha
   AND (f.centrocosto = ?centro or ?centro is null)
   AND f.tipo = 'H'
 ORDER BY f.centrocosto, d.Idproducto,
  f.fecha

ENDTEXT

sql(cmdsql,'vt_rFactura')


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo
ENDPROC
     
����    �
  �
                        |�   %   �	      O
  '   
          �  U  	 %�C��  ��� � T��  ���� �	 M(� �� �  �T �N SELECT     d.IdProducto AS Producto, d.Cantidad, d.Precio, d.PNeto, d.PBruto, �X �R 					d.ITotal * CASE WHEN F.IDMONEDA= 'GS' THEN 1 ELSE F.COTIZACION END AS ITOTAL,�^ �X 					d.ITotal / CASE WHEN F.IDMONEDA <> 'GS' THEN 1 ELSE F.COTIZACION END AS ITOTAL_US, �I �C 					d.IPromedio, d.TipoMov, d.PgAncho, ROUND((d.PBruto - d.PNeto) �9 �3                       / d.Cantidad, 0) AS destare, �P �J                       ROUND(d.PgAncho / d.PNeto * 100, 1) AS rendimiento, �� �                       ROUND(d.ITotal / d.PNeto * CASE WHEN F.IDMONEDA= 'GS' THEN 1 ELSE F.COTIZACION END , 0) AS precio_kg_gs, �� �                       ROUND(d.ITotal / d.Cantidad * CASE WHEN F.IDMONEDA= 'GS' THEN 1 ELSE F.COTIZACION END, 0) AS precio_cab, �� �                       f.CentroCosto, f.IdCliente, f.Fecha, f.Descuento, f.Vence, f.Exenta, f.Cotizacion, f.IdMoneda, f.Imagro, �H �B                       f.Senacsa, f.Guia, f.Comision, f.TipoVenta, �{ �u                       ROUND(d.ITotal / CASE WHEN F.IDMONEDA<> 'GS' THEN 1 ELSE F.COTIZACION END, 1) AS imp_total_us, �� ��                       ROUND(d.ITotal / d.Cantidad / CASE WHEN F.IDMONEDA<> 'GS' THEN 1 ELSE F.COTIZACION END, 0) AS precio_cab_us, �� �                       ROUND(d.ITotal / d.PNeto / CASE WHEN F.IDMONEDA<> 'GS' THEN 1 ELSE F.COTIZACION END, 2) AS precio_kg_us, �Q �K                       ROUND((d.PBruto / d.Cantidad - d.PNeto / d.Cantidad) �� ��                       / (d.PBruto / d.Cantidad) * 100, 1) AS porcentaje, p.Descripcion AS descproducto, cl.RazSocial, dbo.centros.descripci�n AS Centro�/ �) FROM         dbo.centros RIGHT OUTER JOIN�7 �1                       dbo.vt_factura f INNER JOIN�| �v                       dbo.st_movimiento_Det d ON f.IdFactura = d.IdFactura ON dbo.centros.idempresa = f.IdEmpresa AND �O �I                       dbo.centros.centro = f.CentroCosto RIGHT OUTER JOIN�{ �u                       dbo.st_Producto p ON d.IdProducto = p.IdProducto AND d.IdEmpresa = p.IdEmpresa RIGHT OUTER JOIN�k �e                       dbo.vt_clientes cl ON f.IdEmpresa = cl.IdEmpresa AND f.IdCliente = cl.IdCliente�' �! where f.idempresa = ?oApp.empresa�0 �*    AND f.fecha BETWEEN ?dfecha AND ?hfecha�9 �3    AND (f.centrocosto = ?centro or ?centro is null)� �    AND f.tipo = 'H'�, �&  ORDER BY f.centrocosto, d.Idproducto,� �	   f.fecha� �  � � ��C � � vt_rFactura� �� U  CENTRO CMDSQL SQL
  �  � U  SETEO Init,     �� BeforeOpenTables�	    ��1 � A � a A����QQQ���Q�	�q����q���� a A �4 q 1                       <	     %   c	  k	  +    )   �
                  