  ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      	"cVentas"      
"cCompras"      "cTesor"      tFactura      cVentas.TotalFactura      0      tCompra      cCompras.Total      0      tBanco      cTesor.Debito-cTesor.Credito      0      tDebito      cTesor.Debito      0      tCredito      cTesor.Credito      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Hoja de Proyecto"      Arial      empresa             Arial      Nro      Arial      "Nro:"      Arial      Nombre      Arial      IdCliente,RazSocial      Arial      
"Cliente:"      Arial      	"Nombre:"      Arial      "Fecha Inicio
"      Arial      Inicio      Arial      Descripcion      Arial      "Presupuesto"      Arial      Presupuesto      "999,999,999,999"      Arial      "Facturaciones:"      Arial      "Nro Factura"      Arial      "Referencia"      Arial      "Importe sin IVA"      Arial      cVentas.totalFactura      "999,999,999,999"      Arial      cVentas.Fecha      Arial      $cVentas.IdComprobante,cVentas.Numero      Arial      #cVentas.Idcliente,cVentas.RazSocial      Arial      	"Totales"      Arial      cVentas.totalFactura      "999,999,999,999"      Arial      "Compras y Gastos:"      Arial      "Nro Factura"      Arial      "Referencia"      Arial      "Importe sin IVA"      Arial      cCompras.total      "999,999,999,999"      Arial      cCompras.Fecha      Arial      cCompras.FacturaProveedor      Arial      7cCompras.IdProveedor,cCompras.Razon,cCompras.Referencia      Arial      cCompras.total      "999,999,999,999"      Arial      	"Totales"      Arial      "Tesorer�a:"      Arial      "Cuenta"      Arial      "Referencia"      Arial      "Debito"      Arial      	"Credito"      Arial      cTesor.Fecha      Arial      cTesor.NroCuenta,cTesor.Nombre      Arial      "cTesor.referencia,cTesor.Nrocheque      Arial      cTesor.Debito      "999,999,999,999"      Arial      cTesor.credito      "999,999,999,999"      Arial      cTesor.Debito      "999,999,999,999"      Arial      cTesor.credito      "999,999,999,999"      Arial      	"Totales"      Arial      cTesor.Debito- cTesor.Credito      "999,999,999,999"      Arial      "Saldo"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Resumen General"      Arial      Presupuesto      "999,999,999,999"      Arial      "Presupuestado:"      Arial      "Diferencia"      Arial      "% Dif."      Arial      "Facturado:"      Arial      tFactura + tCredito      "999,999,999,999.99"      Arial      tCompra+ tDebito      "999,999,999,999.99"      Arial      '(tFactura+ tCredito)-(tCompra+ tDebito)      "999,999,999,999.99"      Arial      Piif(tCompra+tDebito>0,(mton(tFactura+tCredito)/mton(tCompra+tDebito)*100)-100,0)      "999.99"      Arial      
"Gastado:"      Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     	PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
TEXT TO cmdSQL
	SELECT     p.Nro, p.Fecha, p.IdCliente, p.Cliente, p.Nombre, p.Descripcion, p.Inicio, p.Fin, p.Estado, p.Localidad, p.Presupuesto, c.RazSocial
	FROM         pr_Proyecto AS p INNER JOIN
	                      vt_clientes AS c ON p.IdEmpresa = c.IdEmpresa AND p.IdCliente = c.IdCliente
					where p.IdEmpresa=?oApp.Empresa
					and Nro = ?m.NroProyecto                                            
ENDTEXT

=sql(cmdSQL,'cProy')
SELECT cProy 

TEXT TO cmdSQL NOSHOW 
SELECT     f.FacturaProveedor, p.Razon, f.IdProveedor, f.Fecha,f.Iva, 
Referencia=RTRIM(ISNULL(f.Referencia,'')) + case when IdMoneda<>'GS' then IdMoneda +' '+ Convert(char(10),f.Total - ISNULL(IVA,0)) else '' end , Total=((f.Total - ISNULL(IVA,0))*Cotizacion),NroProyecto
FROM         cp_factura AS f INNER JOIN
                      cp_proveedor AS p ON f.IdEmpresa = p.IdEmpresa AND f.IdProveedor = p.IdProveedor
                      					where f.IdEmpresa=?oApp.Empresa
					and NroProyecto= ?m.NroProyecto
					order by f.Fecha,f.FacturaProveedor    

ENDTEXT

=sql(cmdSQL,'cCompras')

TEXT TO cmdSQL NOSHOW 
SELECT     f.IdComprobante, f.Numero, f.Iva, f.IdCliente, c.RazSocial, f.Fecha, f.IdMoneda, TotalFactura=(f.TotalFactura - ISNULL(IVA,0)), f.NroProyecto
FROM         vt_factura AS f INNER JOIN
                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente
                      where f.IdEmpresa=?oApp.Empresa
					and NroProyecto= ?m.NroProyecto   
					order by f.Fecha,f.Numero

ENDTEXT

=sql(cmdSQL,'cVentas')

TEXT TO cmdSQL NOSHOW 
SELECT     ISNULL(d.FechaDiferida, d.fecha) AS Fecha, c.nrocuenta, c.nombre, c.idmoneda, d.iddeposito, d.idoperacion, LEFT(o.descripcion, 4) AS operacion, 
                      d.idconcepto, RTRIM(ISNULL(d.depositante, '')) + '-' + RTRIM(ISNULL(d.referencia, '')) AS Referencia, dt.nrocheque, d.nroasiento, d.nrooperacion, 
                      Credito=$0,Debito=dt.importe,
NroProyecto
FROM         ts_detdepos_base AS dt INNER JOIN
                      ts_operacion AS o INNER JOIN
                      ts_depositos_base AS d ON o.idoperacion = d.idoperacion INNER JOIN
                      ts_cuentas AS c ON c.idcuenta = d.idcuenta ON dt.iddeposito = d.iddeposito
				where d.IdEmpresa=?oApp.Empresa
				and NroProyecto= ?m.NroProyecto   
union
SELECT     ISNULL(d.FechaDiferida, d.fecha) AS Fecha, c.nrocuenta, c.nombre, c.idmoneda, d.iddeposito, d.idoperacion, LEFT(o.descripcion, 4) AS operacion, 
                      d.idconcepto, RTRIM(ISNULL(d.depositante, '')) + '-' + RTRIM(ISNULL(d.referencia, '')) AS Referencia, dt.nrocheque, d.nroasiento, d.nrooperacion, 
                      Credito=dt.importe,Debito=$0,
NroProyecto
FROM         ts_detdepos_base AS dt INNER JOIN
                      ts_operacion AS o INNER JOIN
                      ts_depositos_base AS d ON o.idoperacion = d.idoperacion INNER JOIN
                      ts_cuentas AS c ON c.idcuenta = d.idcuenta_Ent ON dt.iddeposito = d.iddeposito
             	where d.IdEmpresa=?oApp.Empresa
				and NroProyecto= ?m.NroProyecto  
ORDER BY 1, d.idoperacion, d.nrooperacion
ENDTEXT

=sql(cmdSQL,'cTesor')

SELECT cCompras
INDEX on NroProyecto TAG Nro

SELECT cVentas
INDEX on NroProyecto TAG Nro

SELECT cTesor
INDEX on NroProyecto TAG Nro

SELECT cProy 
SET RELATION TO Nro INTO cVentas ADDITIVE  
SET RELATION TO Nro INTO cCompras ADDITIVE  
SET RELATION TO Nro INTO cTesor ADDITIVE  



ENDPROC
     ����    �  �                        I_   %   �      �  E             �  U  
  �  � U  SETEOS M(�  �� �� 	SELECT     p.Nro, p.Fecha, p.IdCliente, p.Cliente, p.Nombre, p.Descripcion, p.Inicio, p.Fin, p.Estado, p.Localidad, p.Presupuesto, c.RazSocial�/ �) 	FROM         pr_Proyecto AS p INNER JOIN�h �b 	                      vt_clientes AS c ON p.IdEmpresa = c.IdEmpresa AND p.IdCliente = c.IdCliente�* �$ 					where p.IdEmpresa=?oApp.Empresa�O �I 					and Nro = ?m.NroProyecto                                            � � ��C �  � cProy� �� F� �	 M(�  ��L �F SELECT     f.FacturaProveedor, p.Razon, f.IdProveedor, f.Fecha,f.Iva, �� �� Referencia=RTRIM(ISNULL(f.Referencia,'')) + case when IdMoneda<>'GS' then IdMoneda +' '+ Convert(char(10),f.Total - ISNULL(IVA,0)) else '' end , Total=((f.Total - ISNULL(IVA,0))*Cotizacion),NroProyecto�- �' FROM         cp_factura AS f INNER JOIN�l �f                       cp_proveedor AS p ON f.IdEmpresa = p.IdEmpresa AND f.IdProveedor = p.IdProveedor�@ �:                       					where f.IdEmpresa=?oApp.Empresa�* �$ 					and NroProyecto= ?m.NroProyecto�2 �, 					order by f.Fecha,f.FacturaProveedor    � �  � � ��C �  � cCompras� ��	 M(�  ��� �� SELECT     f.IdComprobante, f.Numero, f.Iva, f.IdCliente, c.RazSocial, f.Fecha, f.IdMoneda, TotalFactura=(f.TotalFactura - ISNULL(IVA,0)), f.NroProyecto�- �' FROM         vt_factura AS f INNER JOIN�g �a                       vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente�; �5                       where f.IdEmpresa=?oApp.Empresa�- �' 					and NroProyecto= ?m.NroProyecto   �$ � 					order by f.Fecha,f.Numero� �  � � ��C �  � cVentas� ��	 M(�  ��� �� SELECT     ISNULL(d.FechaDiferida, d.fecha) AS Fecha, c.nrocuenta, c.nombre, c.idmoneda, d.iddeposito, d.idoperacion, LEFT(o.descripcion, 4) AS operacion, �� ��                       d.idconcepto, RTRIM(ISNULL(d.depositante, '')) + '-' + RTRIM(ISNULL(d.referencia, '')) AS Referencia, dt.nrocheque, d.nroasiento, d.nrooperacion, �9 �3                       Credito=$0,Debito=dt.importe,� � NroProyecto�4 �. FROM         ts_detdepos_base AS dt INNER JOIN�8 �2                       ts_operacion AS o INNER JOIN�^ �X                       ts_depositos_base AS d ON o.idoperacion = d.idoperacion INNER JOIN�f �`                       ts_cuentas AS c ON c.idcuenta = d.idcuenta ON dt.iddeposito = d.iddeposito�) �# 				where d.IdEmpresa=?oApp.Empresa�, �& 				and NroProyecto= ?m.NroProyecto   � � union�� �� SELECT     ISNULL(d.FechaDiferida, d.fecha) AS Fecha, c.nrocuenta, c.nombre, c.idmoneda, d.iddeposito, d.idoperacion, LEFT(o.descripcion, 4) AS operacion, �� ��                       d.idconcepto, RTRIM(ISNULL(d.depositante, '')) + '-' + RTRIM(ISNULL(d.referencia, '')) AS Referencia, dt.nrocheque, d.nroasiento, d.nrooperacion, �9 �3                       Credito=dt.importe,Debito=$0,� � NroProyecto�4 �. FROM         ts_detdepos_base AS dt INNER JOIN�8 �2                       ts_operacion AS o INNER JOIN�^ �X                       ts_depositos_base AS d ON o.idoperacion = d.idoperacion INNER JOIN�j �d                       ts_cuentas AS c ON c.idcuenta = d.idcuenta_Ent ON dt.iddeposito = d.iddeposito�3 �-              	where d.IdEmpresa=?oApp.Empresa�+ �% 				and NroProyecto= ?m.NroProyecto  �/ �) ORDER BY 1, d.idoperacion, d.nrooperacion� � ��C �  � cTesor� �� F� � & �� ��� � F� � & �� ��� � F� � & �� ��� � F� � G-(�� ��� � G-(�� ��� � G-(�� ��� � U  CMDSQL SQL CPROY CCOMPRAS NROPROYECTO NRO CVENTAS CTESOR BeforeOpenTables,     �� InitA     ��1 q 3 � Q	����A bq � �����!a A �� �	�q��Aa A �� 
�
�A��a��� 
�
�A���1��A rr � r � r � r 4                       &         A   �      )   �                  