  J<                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      ch_rfactura_prod.centrocosto      	Categoria      ultimocotizacion      ch_rfactura_prod.cotizacion      0      totalcompra      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      0      totalcabezags      �((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal)/ ch_rfactura_prod.cantidad      0      totalCabezaProd      ch_rfactura_prod.cantidad      0      totalCabeza      ch_rfactura_prod.cantidad      0      TotalKgProd      ch_rfactura_prod.pbruto      0      totalkg      ch_rfactura_prod.pbruto      0      imp_total_prod      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      0      imp_total_centro      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      0      imp_totales      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      0      total_sin_flete      ch_rfactura_prod.itotal      0      totalAnimal      ch_rfactura_prod.cantidad      0      totales_sin_fletes      ch_rfactura_prod.itotal      0      filtrar      5iif(isnull(m.centro) and isnull(m.producto), .t.,.f.)      .t.      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      !"PLANILLA DE COMPRA DE HACIENDA "             Arial      empresa             Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Per�odo:"      Arial      	Categoria             Arial      "Categoria:"      Arial      Centro             Arial      "Cento de Costo:"      Arial      "TOTAL X CAB."      Arial      "Total X KG"      Arial      "IMPORTE TOTAL"      Arial      "Cotiz."      Arial      "CANT."      Arial      "CATEGORIA"      Arial      	"FECHA
"      Arial      "PROVEEDOR"      Arial      
"KG TOTAL"      Arial      " PROM."      Arial      "    GS. X KG."      Arial      "TOTAL X CAB"      Arial      
"TOTALGS."      Arial      "FLETES"      Arial      "COMISION."      Arial      "Gs."      Arial      "Gs."      Arial      "Gs."      Arial      "U$S."      Arial      "U$S."      Arial      "U$S."      Arial      ch_rfactura_prod.cantidad      	"999,999"             Arial      ch_rfactura_prod.nomproducto             Arial      ch_rfactura_prod.fecha             Arial      niif(ch_rfactura_prod.idproveedor='0000', ch_rfactura_prod.referencia,ch_rfactura_prod.idproveedor + " "+razon)             Arial      ch_rfactura_prod.pbruto      	"999,999"             Arial      ch_rfactura_prod.pesopromedio      "99,999"             Arial      ch_rfactura_prod.precio_x_kg      	"999,999"             Arial      ch_rfactura_prod.precio_x_cab      "999,999,999"             Arial      ch_rfactura_prod.itotal      "999,999,999,999"             Arial      Mvc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "999,999,999"      Arial      Pvc_flete.total_comision/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "99,999,999"      Arial      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      "9,999,999,999"             Arial      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)/ ch_rfactura_prod.cotizacion      "999,999,999"             Arial      �((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal)/ ch_rfactura_prod.cantidad      "999,999,999"             Arial      �((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal)/ ch_rfactura_prod.cantidad/ ch_rfactura_prod.cotizacion      	"999,999"             Arial      �round((round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0))/ch_rfactura_prod.pbruto,0)      	"999,999"             Arial      �round((round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0))/ch_rfactura_prod.pbruto,0)/ ch_rfactura_prod.cotizacion      "9.9999"             Arial      ch_rfactura_prod.cotizacion      "9,999"             Arial      ch_rfactura_prod.cantidad      	"999,999"             Arial      ch_rfactura_prod.pbruto      "99,999,999"             Arial      totalkg/ totalcabeza      "99,999"             Arial      total_sin_flete/totalkg      	"999,999"             Arial      total_sin_flete/ totalcabeza      "999,999,999"             Arial      ch_rfactura_prod.itotal      "999,999,999,999"             Arial      Mvc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "999,999,999"             Arial      Pvc_flete.total_comision/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "999,999,999,999"      Arial      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      "9,999,999,999"             Arial      "imp_total_centro/ ultimocotizacion      "999,999,999"             Arial      3(imp_total_prod/ totalcabezaProd)/ ultimocotizacion      	"999,999"             Arial      (imp_total_prod/ totalKgProd)      	"999,999"             Arial      ((totalcompra/ totalkg)/ ultimocotizacion      "9.9999"             Arial      ch_rfactura_prod.cotizacion      "9,999"             Arial      #"Total ",ch_rfactura_prod.Categoria             Arial      !(imp_total_prod/ totalcabezaProd)      "999,999,999"             Arial      "Totales Centro de Costo"      Arial      ch_rfactura_prod.cantidad      	"999,999"             Arial      ch_rfactura_prod.pbruto      "99,999,999"             Arial      totalkg/ totalcabeza      "99,999"             Arial      total_sin_flete/totalkg      	"999,999"             Arial      total_sin_flete/ totalcabeza      "999,999,999"             Arial      ch_rfactura_prod.itotal      "999,999,999,999"             Arial      Mvc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "999,999,999"             Arial      Pvc_flete.total_comision/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "999,999,999,999"      Arial      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      "9,999,999,999"             Arial      "imp_total_centro/ ultimocotizacion      "999,999,999"             Arial      (imp_total_centro/ totalcabeza)      "999,999,999"             Arial      1(imp_total_centro/ totalcabeza)/ ultimocotizacion      	"999,999"             Arial      (imp_total_centro/ totalKg)      	"999,999"             Arial      ((totalcompra/ totalkg)/ ultimocotizacion      "9.9999"             Arial      ch_rfactura_prod.cotizacion      "9,999"             Arial      Centro             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Totales General"             Arial      filtrar      vtotal_compra.total_producto      	"999,999"             Arial      filtrar      ch_rfactura_prod.pbruto      "99,999,999"             Arial      filtrar      =round(vtotal_compra.total_kg /vtotal_compra.total_producto,0)      	"999,999"             Arial      filtrar      <round(vtotal_compra.total_importe/ vtotal_compra.total_kg,0)      "999,999,999"             Arial      filtrar      totales_sin_fletes/ totalanimal      "999,999,999"             Arial      filtrar      ch_rfactura_prod.itotal      "9999,999,999,999"             Arial      filtrar      vc_flete.total_flete      "9,999,999,999"             Arial      filtrar      Pvc_flete.total_comision/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad      "999,999,999,999"      Arial      filtrar      �round((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal,0)      "999,999,999,999"      Arial      filtrar      imp_totales/ ultimocotizacion      "999,999,999,999"             Arial      filtrar      �((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal)/ vtotal_compra.total_producto      "999,999,999,999"             Arial      filtrar      Fround((totalcompra/ vtotal_compra.total_producto)/ ultimocotizacion,0)      	"999,999"             Arial      filtrar      �round(((vc_flete.total_flete/ vtotal_compra.total_producto* ch_rfactura_prod.cantidad)+ ch_rfactura_prod.comision+ ch_rfactura_prod.itotal)/  vtotal_compra.total_kg,0)      "999,999,999,999"             Arial      filtrar      7(totalcompra/ vtotal_compra.total_kg)/ ultimocotizacion      "9.9999"             Arial      filtrar      ch_rfactura_prod.cotizacion      	"999,999"             Arial      filtrar      dataenvironment      �Top = 110
Left = 229
Width = 536
Height = 375
InitialSelectedAlias = "ch_rfactura_prod"
DataSource = .NULL.
Name = "Dataenvironment"
     OPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
PUBLIC m.dCentro,m.hCentro

m.dCentro=m.Centro
m.hCentro=m.centro

IF EMPTY(m.centro)
	m.centro=null
ENDIF
	
IF EMPTY(m.producto)
	m.producto=null
ENDIF
	
TEXT TO cmdSQL noshow
SELECT     TOP 100 PERCENT dbo.cp_factura.IdEmpresa, dbo.cp_factura.IdFactura, dbo.cp_factura.CentroCosto, dbo.cp_factura.FacturaProveedor, 
                      dbo.cp_factura.IdProveedor, dbo.cp_factura.IdCondicion, dbo.cp_factura.Fecha, dbo.cp_factura.Sucursal, dbo.cp_factura.Exenta, 
                      dbo.cp_factura.IdCuenta, dbo.cp_factura.Cotizacion, dbo.cp_factura.OtroGasto, Fa_detfactu.Cantidad, RTRIM(dbo.st_Producto.Descripcion) 
                      + ' ' + Fa_detfactu.Clase AS nomproducto, Fa_detfactu.PNeto, Fa_detfactu.PBruto, Fa_detfactu.ITotal, Fa_detfactu.IdProducto AS Producto, 
                      ROUND(Fa_detfactu.PBruto / Fa_detfactu.Cantidad, 0) AS pesopromedio, ROUND(Fa_detfactu.ITotal / Fa_detfactu.PBruto, 0) AS precio_x_kg, 
                      ROUND(Fa_detfactu.ITotal / Fa_detfactu.Cantidad, 0) AS precio_x_cab, Fa_detfactu.Comision, dbo.cp_factura.Referencia, dbo.cp_proveedor.Razon, 
                      dbo.centros.descripci�n AS Centro,RTRIM(dbo.st_Producto.Descripcion) as Categoria
FROM         dbo.centros RIGHT OUTER JOIN
                      dbo.st_movimiento_Det Fa_detfactu INNER JOIN
                      dbo.cp_factura ON Fa_detfactu.IdCompra = dbo.cp_factura.IdFactura ON dbo.centros.idempresa = dbo.cp_factura.IdEmpresa AND 
                      dbo.centros.centro = dbo.cp_factura.CentroCosto LEFT OUTER JOIN
                      dbo.st_Producto ON Fa_detfactu.IdProducto = dbo.st_Producto.IdProducto AND Fa_detfactu.IdEmpresa = dbo.st_Producto.IdEmpresa LEFT OUTER JOIN
                      dbo.cp_proveedor ON dbo.cp_factura.IdEmpresa = dbo.cp_proveedor.IdEmpresa AND 
                      dbo.cp_factura.IdProveedor = dbo.cp_proveedor.IdProveedor
WHERE     (dbo.cp_factura.Tipo = 'H')
 AND Cp_factura.idempresa = ?oApp.Empresa
   AND Cp_factura.fecha BETWEEN ?m.dfecha AND ?m.hfecha
   AND (Cp_factura.centrocosto = ?m.centro or ?m.Centro is null)
   and (Fa_detfactu.IdProducto = ?m.producto or ?m.producto is null)
ORDER BY dbo.cp_factura.CentroCosto, Categoria,dbo.cp_factura.Fecha
ENDTEXT

=sql(cmdSQL,'ch_rfactura_prod')


TEXT TO cmdSQL noshow
	SELECT 
	  SUM(Fa_detfactu.cantidad) AS total_producto,
	  SUM(Fa_detfactu.pbruto) AS total_kg,
	  SUM(Fa_detfactu.itotal) AS total_importe
	 FROM  cp_factura INNER JOIN st_movimiento_det fa_detfactu 
	   ON  Cp_factura.idfactura = Fa_detfactu.idCompra
	 WHERE Fa_detfactu.idempresa = ?oApp.empresa
	   AND Cp_factura.tipo = 'H'
	   AND Cp_factura.fecha BETWEEN ?m.dfecha AND ?m.hfecha
	   AND Cp_factura.idempresa = ?oapp.empresa
	   AND (Cp_factura.centrocosto = ?m.centro or ?m.Centro is null)
ENDTEXT
=sql(cmdSQL,'vTotal_Compra')


TEXT TO cmdSQL noshow
DECLARE @Cuenta1 char(9)
DECLARE @Cuenta2 char(9)

SELECT @Cuenta1 = convert(char(9),dbo.LeerConstante(?oApp.Empresa,'HC_FLETE_COMPRA')),
		@Cuenta2 = convert(char(9),dbo.LeerConstante(?oApp.Empresa,'HC_COMISION_COMPRA'))
		
SELECT 
  ISNULL(SUM(CASE WHEN CUENTA = @Cuenta1 THEN Detalle_base.debe ELSE 0 END),0) AS total_flete,
  ISNULL(SUM(CASE WHEN CUENTA = @Cuenta2 THEN Detalle_base.debe ELSE 0 END),0) AS total_Comision
 FROM  cn_asientos asientos_base INNER JOIN cn_detalle detalle_base 
   ON  Asientos_base.IdAsiento = Detalle_base.IdAsiento
 WHERE Asientos_base.idempresa = ?oApp.empresa
   AND Asientos_base.fecha BETWEEN ?m.dfecha AND ?m.hfecha
   AND (Detalle_base.cuenta = @Cuenta1
    or Detalle_base.cuenta = @Cuenta2)
   AND Detalle_base.idempresa = ?oapp.empresa
   AND (Detalle_base.centro = ?m.centro or ?m.Centro is null)
ENDTEXT


=sql(cmdSQL,'vc_flete')

SELECT ch_rfactura_prod
 


ENDPROC
     ����    �  �                        R   %   �      q  G   �          �  U  
  �  � U  SETEO3 7��  �� � T��  ��� �� T�� ��� �� %�C�� ���O � T�� ���� � %�C�� ���q � T�� ���� �	 M(� ��� �� SELECT     TOP 100 PERCENT dbo.cp_factura.IdEmpresa, dbo.cp_factura.IdFactura, dbo.cp_factura.CentroCosto, dbo.cp_factura.FacturaProveedor, �� ��                       dbo.cp_factura.IdProveedor, dbo.cp_factura.IdCondicion, dbo.cp_factura.Fecha, dbo.cp_factura.Sucursal, dbo.cp_factura.Exenta, �� ��                       dbo.cp_factura.IdCuenta, dbo.cp_factura.Cotizacion, dbo.cp_factura.OtroGasto, Fa_detfactu.Cantidad, RTRIM(dbo.st_Producto.Descripcion) �� ��                       + ' ' + Fa_detfactu.Clase AS nomproducto, Fa_detfactu.PNeto, Fa_detfactu.PBruto, Fa_detfactu.ITotal, Fa_detfactu.IdProducto AS Producto, �� ��                       ROUND(Fa_detfactu.PBruto / Fa_detfactu.Cantidad, 0) AS pesopromedio, ROUND(Fa_detfactu.ITotal / Fa_detfactu.PBruto, 0) AS precio_x_kg, �� ��                       ROUND(Fa_detfactu.ITotal / Fa_detfactu.Cantidad, 0) AS precio_x_cab, Fa_detfactu.Comision, dbo.cp_factura.Referencia, dbo.cp_proveedor.Razon, �m �g                       dbo.centros.descripci�n AS Centro,RTRIM(dbo.st_Producto.Descripcion) as Categoria�/ �) FROM         dbo.centros RIGHT OUTER JOIN�H �B                       dbo.st_movimiento_Det Fa_detfactu INNER JOIN�� ��                       dbo.cp_factura ON Fa_detfactu.IdCompra = dbo.cp_factura.IdFactura ON dbo.centros.idempresa = dbo.cp_factura.IdEmpresa AND �[ �U                       dbo.centros.centro = dbo.cp_factura.CentroCosto LEFT OUTER JOIN�� ��                       dbo.st_Producto ON Fa_detfactu.IdProducto = dbo.st_Producto.IdProducto AND Fa_detfactu.IdEmpresa = dbo.st_Producto.IdEmpresa LEFT OUTER JOIN�j �d                       dbo.cp_proveedor ON dbo.cp_factura.IdEmpresa = dbo.cp_proveedor.IdEmpresa AND �U �O                       dbo.cp_factura.IdProveedor = dbo.cp_proveedor.IdProveedor�+ �% WHERE     (dbo.cp_factura.Tipo = 'H')�/ �)  AND Cp_factura.idempresa = ?oApp.Empresa�= �7    AND Cp_factura.fecha BETWEEN ?m.dfecha AND ?m.hfecha�F �@    AND (Cp_factura.centrocosto = ?m.centro or ?m.Centro is null)�J �D    and (Fa_detfactu.IdProducto = ?m.producto or ?m.producto is null)�I �C ORDER BY dbo.cp_factura.CentroCosto, Categoria,dbo.cp_factura.Fecha� �! ��C � � ch_rfactura_prod� ��	 M(� �� � 	SELECT �5 �/ 	  SUM(Fa_detfactu.cantidad) AS total_producto,�- �' 	  SUM(Fa_detfactu.pbruto) AS total_kg,�1 �+ 	  SUM(Fa_detfactu.itotal) AS total_importe�B �< 	 FROM  cp_factura INNER JOIN st_movimiento_det fa_detfactu �9 �3 	   ON  Cp_factura.idfactura = Fa_detfactu.idCompra�3 �- 	 WHERE Fa_detfactu.idempresa = ?oApp.empresa�# � 	   AND Cp_factura.tipo = 'H'�> �8 	   AND Cp_factura.fecha BETWEEN ?m.dfecha AND ?m.hfecha�2 �, 	   AND Cp_factura.idempresa = ?oapp.empresa�G �A 	   AND (Cp_factura.centrocosto = ?m.centro or ?m.Centro is null)� � ��C � � vTotal_Compra� ��	 M(� �� � DECLARE @Cuenta1 char(9)� � DECLARE @Cuenta2 char(9)� �  �\ �V SELECT @Cuenta1 = convert(char(9),dbo.LeerConstante(?oApp.Empresa,'HC_FLETE_COMPRA')),�Y �S 		@Cuenta2 = convert(char(9),dbo.LeerConstante(?oApp.Empresa,'HC_COMISION_COMPRA'))� � 		� � SELECT �d �^   ISNULL(SUM(CASE WHEN CUENTA = @Cuenta1 THEN Detalle_base.debe ELSE 0 END),0) AS total_flete,�f �`   ISNULL(SUM(CASE WHEN CUENTA = @Cuenta2 THEN Detalle_base.debe ELSE 0 END),0) AS total_Comision�J �D  FROM  cn_asientos asientos_base INNER JOIN cn_detalle detalle_base �= �7    ON  Asientos_base.IdAsiento = Detalle_base.IdAsiento�4 �.  WHERE Asientos_base.idempresa = ?oApp.empresa�@ �:    AND Asientos_base.fecha BETWEEN ?m.dfecha AND ?m.hfecha�, �&    AND (Detalle_base.cuenta = @Cuenta1�, �&     or Detalle_base.cuenta = @Cuenta2)�3 �-    AND Detalle_base.idempresa = ?oapp.empresa�C �=    AND (Detalle_base.centro = ?m.centro or ?m.Centro is null)� � ��C � � vc_flete� �� F� � U  DCENTRO HCENTRO CENTRO PRODUCTO CMDSQL SQL CH_RFACTURA_PROD BeforeOpenTables,     �� InitA     ��1 q 3 � � A � A � !	�	1
Q
1
�
���a	��
�Q���a��A � � Q�!�11�!qA �� ��a ��� � Aa��A��11A �r 4                       &         A   D      )   �                  