  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=1
      Arial      IdMoneda      cp_rcompraproveedor.idproveedor      Arial      Arial      Arial      Arial      Arial       "Compras y Gastos por Proveedor"             Arial      Empresa             Arial      -iif(empty(m.sucursal),'Todos','Casa Central')             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Factura
Proveedor"      "@I"      Arial      "Proveedor"      Arial      "Cpbte."      Arial      "Nro."      Arial      "Referencia"      Arial      "Total"      Arial      	"Fecha
"      Arial      "Moneda: ", IdMoneda      Arial      cp_rcompraproveedor.idproveedor             Arial      cp_rcompraproveedor.razon             Arial      cp_rcompraproveedor.fecha             Arial      cp_rcompraproveedor.idcomprob      "@!"             Arial      	IdFactura             Arial      
referencia             Arial      $cp_rcompraproveedor.facturaproveedor             Arial      Total      "9,999,999,999.99"      : cp_rcompraproveedor.exenta +  cp_rcompraproveedor.gravada      Arial      "Total Prov."      Arial      Total      "99,999,999,999,999.99"      Arial      "Total Moneda: ", IdMoneda      Arial      Total      "99,999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 161
Left = -102
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraproveedor"
DataSource = .NULL.
Name = "Dataenvironment"
     pPROCEDURE Init
LOCAL strsql

=SETNull(@m.idproveedor)
=SETNull(@m.sucursal)


TEXT TO cmdSQL noshow

SELECT c.idfactura, a.idcomprob,a.comprobante des_comp, c.facturaproveedor, 
	c.idproveedor, c.fecha, Referencia, 
	c.sucursal, c.Total , c.idmoneda, b.razon 
	FROM cp_comprobante a, cp_proveedor b, cp_factura c 
	WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.idEmpresa = b.IdEmpresa 
	AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa 
	AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha  
	and (c.idproveedor = ?m.idproveedor or ?m.idproveedor is null)
	AND (c.sucursal = ?m.sucursal or ?m.sucursal  is null)
	ORDER BY c.IdMoneda, c.idproveedor, c.fecha, c.idfactura,c.idcomprobante 
ENDTEXT
	
 =sql(cmdSQL ,'cp_rcompraproveedor')
 SELECT cp_rcompraproveedor

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     \���    C  C                        v~   %   �      �     �          �  U   ��  � ��C�� � �� ��C�� � ��	 M(� �� �  �R �L SELECT c.idfactura, a.idcomprob,a.comprobante des_comp, c.facturaproveedor, �+ �% 	c.idproveedor, c.fecha, Referencia, �1 �+ 	c.sucursal, c.Total , c.idmoneda, b.razon �; �5 	FROM cp_comprobante a, cp_proveedor b, cp_factura c �h �b 	WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.idEmpresa = b.IdEmpresa �H �B 	AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa �4 �. 	AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha  �E �? 	and (c.idproveedor = ?m.idproveedor or ?m.idproveedor is null)�= �7 	AND (c.sucursal = ?m.sucursal or ?m.sucursal  is null)�P �J 	ORDER BY c.IdMoneda, c.idproveedor, c.fecha, c.idfactura,c.idcomprobante � �$ ��C � � cp_rcompraproveedor� �� F� � U  STRSQL SETNULL IDPROVEEDOR SUCURSAL CMDSQL SQL CP_RCOMPRAPROVEEDOR
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 q � a !����AQ�A Bq 3 q 2                       4        [  e      )   C                  