                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=1
COLOR=1
      Arial      Arial      Arial      Arial      Arial      Arial      "Orden de Pago"             Arial      oApp.nombreempresa      Arial      nroorden             Arial      "Nro."      Arial      fecha             Arial      	"Fecha
"      Arial      cp_rordenpago.idproveedor      Arial      Nombre      Arial      "Proveedor"      Arial      	"Factura"      Arial      "Cuota"      Arial      	"Importe"      Arial      "Moneda"      Arial      "Valorizado"      Arial      "Referencia"      Arial      cp_rordenpago.facturaproveedor             Arial      cp_rordenpago.cuota      "999"             Arial      cp_rordenpago.importe      "999,999,999,999.99"             Arial      cp_rordenpago.idmoneda             Arial      cp_rordenpago.valorizado      "999,999,999,999.99"             Arial      cp_rordenpago.Referencia      "999,999,999,999.99"      Arial      "Recib� conforme:"      Arial      "Verificado por:"      Arial      "Autorizado por:"      Arial      "Firma:"      Arial      "Firma:"      Arial      "Firma:"      Arial      	NroCuenta      Arial      "Nro.Cuenta:"      Arial      	NroCheque      Arial      "Nro.Cheque:"      Arial      "Fecha:"      Arial      FechaCheque      "@D"      Arial      "Fecha:"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Detalle      Arial      cp_rordenpago.valorizado      "999,999,999,999.99"      Arial      Total=0      Total      "999,999,999,999.99"      Arial      Total>0      	"Detalle"      Arial      !empty(Detalle)      "Total /Orden de Pago"      Arial      dataenvironment      �Top = 178
Left = -11
Width = 759
Height = 448
InitialSelectedAlias = "cp_rordenpago"
DataSource = .NULL.
Name = "Dataenvironment"
     )PROCEDURE Init
DO seteo
TEXT TO cmdSQL NOSHOW 
SELECT     TOP 100 PERCENT a.nroorden, a.fecha, a.idproveedor, b.facturaproveedor, b.cuota, b.importe, b.importe * b.cotizacion AS valorizado, b.idmoneda, 
                      a.nombre, a.Detalle, a.Importe AS Total, RTRIM(c.nrocuenta) + ' ' + c.nombre AS NroCuenta, a.nrocheque, ISNULL(d.FechaDiferida, d.fecha) 
                      AS FechaCheque, f.Referencia
FROM         dbo.cp_ordenpago a LEFT OUTER JOIN
                      dbo.ts_depositos_base d ON a.idempresa = d.idempresa AND a.nroorden = d.nroorden LEFT OUTER JOIN
                      dbo.ts_cuentas c ON a.idcuenta = c.idcuenta and a.IdEmpresa = c.IdEmpresa LEFT OUTER JOIN
                      dbo.cp_orddet_pago b ON a.idordenpago = b.nroorden LEFT JOIN 
                      cp_factura f on b.IdProveedor = f.IdProveedor and b.FacturaProveedor=f.FacturaProveedor 
WHERE     (a.nroorden = ?m.NroOrden) and a.IdEmpresa = ?oApp.Empresa
ORDER BY a.nroorden
ENDTEXT


sql(cmdSQL,'cp_rordenpago')
SELECT cp_rordenpago

ENDPROC
     ����    �  �                        ��   %   c      �     q          �  U   �  �	 M(� ��� �� SELECT     TOP 100 PERCENT a.nroorden, a.fecha, a.idproveedor, b.facturaproveedor, b.cuota, b.importe, b.importe * b.cotizacion AS valorizado, b.idmoneda, �� ��                       a.nombre, a.Detalle, a.Importe AS Total, RTRIM(c.nrocuenta) + ' ' + c.nombre AS NroCuenta, a.nrocheque, ISNULL(d.FechaDiferida, d.fecha) �8 �2                       AS FechaCheque, f.Referencia�5 �/ FROM         dbo.cp_ordenpago a LEFT OUTER JOIN�| �v                       dbo.ts_depositos_base d ON a.idempresa = d.idempresa AND a.nroorden = d.nroorden LEFT OUTER JOIN�u �o                       dbo.ts_cuentas c ON a.idcuenta = c.idcuenta and a.IdEmpresa = c.IdEmpresa LEFT OUTER JOIN�Y �S                       dbo.cp_orddet_pago b ON a.idordenpago = b.nroorden LEFT JOIN �t �n                       cp_factura f on b.IdProveedor = f.IdProveedor and b.FacturaProveedor=f.FacturaProveedor �J �D WHERE     (a.nroorden = ?m.NroOrden) and a.IdEmpresa = ?oApp.Empresa� � ORDER BY a.nroorden� � ��C � � cp_rordenpago� �� F� � U  SETEO CMDSQL SQL CP_RORDENPAGO Init,     ��1 q � 
Q
�Q�Q�A��A �q 2                             )   �                  