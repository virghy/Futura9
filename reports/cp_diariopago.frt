  h                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=TOSHIBA e-STUDIO451c PS3
OUTPUT=192.168.0.17
ORIENTATION=0
PAPERSIZE=1
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
DUPLEX=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      J  ,  winspool  TOSHIBA e-STUDIO451c PS3  192.168.0.17                       �TOSHIBA e-STUDIO451c PS3         � S��  �
od   ,  ,  Letter                                                                            PRIV�0                                                                                       '''  '        @                                  \K hC                             x�d�      �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @  SMTJ     0T O S H I B A   e - S T U D I O 4 5 1 c   P S 3   Resolution 600dpi PageSize Letter PageRegion  InputSlot Auto MediaType Plain Duplex None Collate True OutputBin Bin2 Stapling Off HolePunch Off PrintMode Normal DINDigit1 0 DINDigit2 0 DINDigit3 0 DINDigit4 0 DINDigit5 0 DeptCode False DCDigit1 0 DCDigit2 0 DCDigit3 0 DCDigit4 0 DCDigit5 0 ColorResType ColorLowGeneral DistinguishThinLines True BlackOverPrint True PureBlackGray BlackGrayAuto TonerSave False BlankPage False Smoothing True                                                                            Arial      IdMoneda      IdPago      Arial      Arial      Arial      Arial      Arial      Arial      "Diario de Control de Pagos"      Arial      empresa      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Per�odo :"      Arial      ,iif(isnull(m.Idproveedor),'Todos',Proveedor)      Arial      "Proveedor :"      Arial      	"  Fecha"      Arial      "  Proveedor"      Arial      "Recibo Nro."      Arial      
"Moneda
"      Arial      	"Cuota
"      Arial      "Importe
"      Arial      "Factura
"      Arial      "Moneda "  + IdMoneda      Arial      	proveedor             Arial      	nrorecibo             Arial      idmoneda             Arial      fecha             Arial      cuota      "99"             Arial      importe_pag      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      FacturaProveedor      "99"      Arial      importe_pag      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      "Total : "+IdMoneda      Arial      importe_pag      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()      Arial      dataenvironment      �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
     rPROCEDURE Init
If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF

If Empty(m.IdProveedor)
	Store null To m.IdProveedor
ENDIF

TEXT TO cmdSQL noshow

	SELECT a.fecha,a.IdPago,a.nrocomprobante nrorecibo,b.facturaproveedor, 
	rtrim(a.idproveedor)+'-'+c.razon proveedor, b.cuota,b.idmoneda, b.cotizacion,
	b.importe_pag 
	FROM cp_pagos_base a inner join cp_pagosdet_base b on a.idpago=b.idpago 
	left join  cp_proveedor c on a.idproveedor=c.idproveedor and a.idempresa=c.idempresa 
	WHERE  a.idempresa=?oApp.Empresa and (a.fecha BETWEEN ?m.dfecha and ?m.hfecha ) 
	and (a.sucursal=?m.sucursal or ?m.sucursal is null) 
	and (a.IdProveedor = ?m.IdProveedor or ?m.IdProveedor is null)
	ORDER BY b.idmoneda, a.fecha, a.idproveedor,a.IdPago 

ENDTEXT



sql(cmdSQL,'rrecibopago')
SELECT rrecibopago


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     :���    !  !                        ��   %   n      �     �          �  U  � %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � �	 M(� �� �  �N �H 	SELECT a.fecha,a.IdPago,a.nrocomprobante nrorecibo,b.facturaproveedor, �T �N 	rtrim(a.idproveedor)+'-'+c.razon proveedor, b.cuota,b.idmoneda, b.cotizacion,� � 	b.importe_pag �O �I 	FROM cp_pagos_base a inner join cp_pagosdet_base b on a.idpago=b.idpago �\ �V 	left join  cp_proveedor c on a.idproveedor=c.idproveedor and a.idempresa=c.idempresa �W �Q 	WHERE  a.idempresa=?oApp.Empresa and (a.fecha BETWEEN ?m.dfecha and ?m.hfecha ) �; �5 	and (a.sucursal=?m.sucursal or ?m.sucursal is null) �E �? 	and (a.IdProveedor = ?m.IdProveedor or ?m.IdProveedor is null)�< �6 	ORDER BY b.idmoneda, a.fecha, a.idproveedor,a.IdPago � �  � � ��C � � rrecibopago� �� F� � U  SUCURSAL IDPROVEEDOR CMDSQL SQL RRECIBOPAGO
  �  � U  SETEO Init,     �� BeforeOpenTablesY    ��1 � A � A � a �AQ��q�Q�a A �q 4 q 2                       6        ]  g      )   !                  