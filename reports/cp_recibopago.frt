  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      IdMoneda      Arial      Arial      Arial      Arial      Arial      "Resumen de Control de Pagos"      Arial      empresa      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Per�odo :"      Arial      ,iif(isnull(m.Idproveedor),'Todos',Proveedor)      Arial      "Proveedor"      Arial      	"  Fecha"      Arial      "  Proveedor"      Arial      "Recibo Nro."      Arial      
"Moneda
"      Arial      "Importe
"      Arial      "Moneda "  + IdMoneda      Arial      	proveedor             Arial      	nrorecibo             Arial      idmoneda             Arial      importe_pag      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      fecha             Arial      "Total : "+IdMoneda      Arial      importe_pag      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      
datetime()             Arial      L'P�g. '+TRANS(_PAGENO) + IIF(SYS(2040)="1", "", " de " + TRANS(_PAGETOTAL))       Arial      dataenvironment      �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF

If Empty(m.idproveedor)
	Store null To m.idproveedor
ENDIF

TEXT TO cmdSQL noshow

SELECT     a.fecha, a.nrocomprobante AS nrorecibo, RTRIM(a.idproveedor) + '-' + c.Razon AS proveedor, 
		SUM(b.importe_pag) AS importe_pag, a.idmoneda
FROM         cp_pagos_base AS a INNER JOIN
                      cp_pagosdet_base AS b ON a.idpago = b.idpago LEFT OUTER JOIN
                      cp_proveedor AS c ON a.idproveedor = c.IdProveedor AND a.idempresa = c.IdEmpresa
	WHERE  a.idempresa=?oApp.Empresa and (a.fecha BETWEEN ?m.dfecha and ?m.hfecha ) 
	and (a.sucursal=?m.sucursal or ?m.sucursal is null) 
	and (a.idproveedor = ?m.idproveedor or ?m.idproveedor is null)
GROUP BY a.fecha, a.nrocomprobante, RTRIM(a.idproveedor) + '-' + c.Razon, a.idmoneda
ORDER BY a.idmoneda, a.fecha, proveedor

ENDTEXT



sql(cmdSQL,'rrecibopago')
SELECT rrecibopago


ENDPROC
     ����    �  �                        �   %   �      H               �  U  
  �  � U  SETEOv %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � �	 M(� �� �  �l �f SELECT     a.fecha, a.nrocomprobante AS nrorecibo, RTRIM(a.idproveedor) + '-' + c.Razon AS proveedor, �5 �/ 		SUM(b.importe_pag) AS importe_pag, a.idmoneda�0 �* FROM         cp_pagos_base AS a INNER JOIN�X �R                       cp_pagosdet_base AS b ON a.idpago = b.idpago LEFT OUTER JOIN�l �f                       cp_proveedor AS c ON a.idproveedor = c.IdProveedor AND a.idempresa = c.IdEmpresa�W �Q 	WHERE  a.idempresa=?oApp.Empresa and (a.fecha BETWEEN ?m.dfecha and ?m.hfecha ) �; �5 	and (a.sucursal=?m.sucursal or ?m.sucursal is null) �E �? 	and (a.idproveedor = ?m.idproveedor or ?m.idproveedor is null)�Z �T GROUP BY a.fecha, a.nrocomprobante, RTRIM(a.idproveedor) + '-' + c.Razon, a.idmoneda�- �' ORDER BY a.idmoneda, a.fecha, proveedor� �  � � ��C � � rrecibopago� �� F� � U  SUCURSAL IDPROVEEDOR CMDSQL SQL RRECIBOPAGO BeforeOpenTables,     �� InitA     ��1 q 3 � A � A � a �Q��q�Q��a A �q 3                       &         A   �      )   �                  