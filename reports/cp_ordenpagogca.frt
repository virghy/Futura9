                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Arial      Arial      Arial      Arial      Arial      Arial      "Orden de Pago"             Arial      oApp.nombreempresa      Arial      nroorden             Arial      "Nro."      Arial      fecha             Arial      	"Fecha
"      Arial      cp_rordenpago.idproveedor      Arial      Nombre      Arial      "Proveedor"      Arial      "Factura Proveedor"      Arial      "Cuota"      Arial      	"Importe"      Arial      "Moneda"      Arial      "Valorizado"      Arial      cp_rordenpago.facturaproveedor             Arial      cp_rordenpago.cuota      "999"             Arial      cp_rordenpago.importe      "999,999,999,999.99"             Arial      cp_rordenpago.idmoneda             Arial      cp_rordenpago.valorizado      "999,999,999,999.99"             Arial      Detalle      Arial      cp_rordenpago.valorizado      "999,999,999,999.99"      Arial      Total=0      Total      "999,999,999,999.99"      Arial      Total>0      	"Detalle"      Arial      !empty(Detalle)      "Total /Orden de Pago"      Arial      "Recib� conforme:"      Arial      "Confeccionado por:"      Arial      "Autorizado por:"      Arial      Usuario      Arial      "Firma:"      Arial      "Firma:"      Arial      "Firma:"      Arial      	NroCuenta      Arial      "Nro.Cuenta:"      Arial      	NroCheque      Arial      "Nro.Cheque:"      Arial      "Fecha:"      Arial      FechaCheque      "@D"      Arial      "Fecha:"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 178
Left = -11
Width = 759
Height = 448
InitialSelectedAlias = "cp_rordenpago"
DataSource = .NULL.
Name = "Dataenvironment"
     IPROCEDURE Init
DO seteo
TEXT TO cmdSQL NOSHOW 
SELECT     a.nroorden, a.fecha, a.idproveedor, b.facturaproveedor, b.cuota, b.importe, b.importe * b.cotizacion AS valorizado, b.idmoneda, 
                      a.nombre, a.Detalle, a.Importe AS Total, RTRIM(c.nrocuenta) + ' ' + c.nombre AS NroCuenta, a.nrocheque, ISNULL(d.FechaDiferida, d.fecha) 
                      AS FechaCheque, RTRIM(first_name) + ' ' + last_name Usuario
FROM         dbo.cp_ordenpago a LEFT OUTER JOIN
                      dbo.ts_depositos_base d ON a.idempresa = d.idempresa AND a.nroorden = d.nroorden LEFT OUTER JOIN
                      dbo.ts_cuentas c ON a.idcuenta = c.idcuenta and a.IdEmpresa = c.IdEmpresa LEFT OUTER JOIN
                      dbo.cp_orddet_pago b ON a.idordenpago = b.nroorden
                      left join 
                      usuarios u ON a.Audit_Usuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
WHERE     (a.nroorden = ?m.NroOrden) and a.IdEmpresa = ?oApp.Empresa
ORDER BY a.nroorden
ENDTEXT


sql(cmdSQL,'cp_rordenpago')
SELECT cp_rordenpago

ENDPROC
     ���                              ȸ   %   �      �     �          �  U  4 �  �	 M(� ��� �� SELECT     a.nroorden, a.fecha, a.idproveedor, b.facturaproveedor, b.cuota, b.importe, b.importe * b.cotizacion AS valorizado, b.idmoneda, �� ��                       a.nombre, a.Detalle, a.Importe AS Total, RTRIM(c.nrocuenta) + ' ' + c.nombre AS NroCuenta, a.nrocheque, ISNULL(d.FechaDiferida, d.fecha) �W �Q                       AS FechaCheque, RTRIM(first_name) + ' ' + last_name Usuario�5 �/ FROM         dbo.cp_ordenpago a LEFT OUTER JOIN�| �v                       dbo.ts_depositos_base d ON a.idempresa = d.idempresa AND a.nroorden = d.nroorden LEFT OUTER JOIN�u �o                       dbo.ts_cuentas c ON a.idcuenta = c.idcuenta and a.IdEmpresa = c.IdEmpresa LEFT OUTER JOIN�N �H                       dbo.cp_orddet_pago b ON a.idordenpago = b.nroorden�& �                        left join �n �h                       usuarios u ON a.Audit_Usuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�J �D WHERE     (a.nroorden = ?m.NroOrden) and a.IdEmpresa = ?oApp.Empresa� � ORDER BY a.nroorden� � ��C � � cp_rordenpago� �� F� � U  SETEO CMDSQL SQL CP_RORDENPAGO Init,     ��1 q � 	Q
qQ�Q�a���A �q 2                       >      )                     