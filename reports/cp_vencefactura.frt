  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
od   ,  ,  Letter                                                                            PRIV�0                                                                                       '''  '        @                                  \K hC                             x�d�      �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @  SMTJ     0T O S H I B A   e - S T U D I O 4 5 1 c   P S 3   Resolution 600dpi PageSize Letter PageRegion  InputSlot Auto MediaType Plain Duplex None Collate True OutputBin Bin2 Stapling Off HolePunch Off PrintMode Normal DINDigit1 0 DINDigit2 0 DINDigit3 0 DINDigit4 0 DINDigit5 0 DeptCode False DCDigit1 0 DCDigit2 0 DCDigit3 0 DCDigit4 0 DCDigit5 0 ColorResType ColorLowGeneral DistinguishThinLines True BlackOverPrint True PureBlackGray BlackGrayAuto TonerSave False BlankPage False Smoothing True                                                                            Arial      	idcartera      idproveedor      Arial      Arial      Arial      Arial      Arial      $"Vencimientos a Pagar por Proveedor"             Arial      empresa             Arial      &dtoc(m.dvence)+ " al " +dtoc(m.hvence)             Arial      "Per�odo :"      Arial      "  Proveedor"      Arial      "Vencimiento"      Arial      "Fecha a Pagar"      Arial      "Fecha Compra
"      Arial      "Comprobante"      Arial      	"Cuota
"      Arial      
"Moneda
"      Arial      	"Saldo
"      Arial      "Valorizado
"      Arial      $cp_rvencefactu.idcartera," ",cartera      Arial      $cp_rvencefactu.idproveedor," ",razon             Arial      cp_rvencefactu.vencimiento      "@D"      Arial      cp_rvencefactu.fec_acobrar      "@D"      Arial      cp_rvencefactu.fecha      "@D"      Arial      cp_rvencefactu.facturaproveedor             Arial      cp_rvencefactu.cuota      "99"             Arial      idmoneda             Arial       cp_rvencefactu.saldo      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      " cp_rvencefactu.saldo * Cotizacion      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      8"Total Proveedor: "+cp_rvencefactu.idproveedor," ",razon             Arial      !cp_rvencefactu.saldo * Cotizacion      "999,999,999,999.99"             Arial      @"Total Cartera Proveedor: "+cp_rvencefactu.idcartera," ",cartera      Arial      !cp_rvencefactu.saldo * Cotizacion      "999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total General "             Arial      !cp_rvencefactu.saldo * Cotizacion      "999,999,999,999.99"             Arial      dataenvironment      �Top = 94
Left = 16
Width = 555
Height = 285
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
     KPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
If Empty(m.cartera)
	Store null To m.cartera
ENDIF

IF EMPTY(m.hproveedor)
	m.hproveedor='ZZZZ'
ENDIF

*LOCAL strsql
SET DATABASE TO DATOS 
*!*		strsql ='SELECT a.nrocomprob, '+;
*!*		'  a.facturaproveedor, a.cuota, a.sucursal, '+;
*!*		'  a.idproveedor, a.fecha, a.vencimiento, b.idcartera,c.cartera, '+;
*!*		'  a.fec_acobrar, a.importe, a.saldo, '+;
*!*		'  a.idmoneda, a.pendiente, b.razon, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion'+;
*!*		' FROM cp_forma_pago a, cp_proveedor b, cp_cartera c '+; 
*!*		' WHERE  a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa ' + ;
*!*		'   AND a.saldo > 0  and (b.idcartera=?m.cartera and c.idcartera=?m.cartera or ?m.cartera is null) and b.idempresa=c.idempresa'+;
*!*		IIF(!EMPTY(m.dproveedor),'   AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor ','')+;
*!*		IIF(!EMPTY(m.dvence),'   AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence ','')+;
*!*		' ORDER BY a.idproveedor, a.idmoneda, a.vencimiento, a.facturaproveedor, b.idcartera'

TEXT TO cmdSQL noshow
	SELECT a.nrocomprob, 
	a.facturaproveedor, a.cuota, a.sucursal, 
	a.idproveedor, a.fecha, a.vencimiento, b.idcartera,c.cartera, 
	a.fec_acobrar, a.importe, a.saldo, 
	a.idmoneda, a.pendiente, b.razon, 
	case when ?m.TipoCotizacion='F' then  f.Cotizacion else DBO.fnGetCotizacion(a.IdMoneda,GetDate()) end as Cotizacion
	FROM cp_forma_pago a inner join cp_proveedor b 
	on a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa 
	left join cp_cartera c on b.idcartera=c.idcartera and b.IdEmpresa = c.IdEmpresa
	inner join cp_factura f on a.nrocomprob = f.IdFactura  
	WHERE  a.IdEmpresa = ?oApp.Empresa
	AND a.saldo > 0  
	and (b.idcartera=?m.cartera or ?m.cartera is null) 
	AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor
	AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence 
	ORDER BY b.idcartera, a.idproveedor, a.idmoneda, a.vencimiento, a.facturaproveedor

ENDTEXT

=sql(cmdSQL,'cp_rvencefactu')
SELECT cp_rvencefactu


ENDPROC
     ����    �  �                        <   %   �      4      �          �  U  
  �  � U  SETEON %�C��  ��� � J���(��  � � %�C�� ���F � T�� �� ZZZZ�� � G(� DATOS�	 M(� �� � 	SELECT a.nrocomprob, �0 �* 	a.facturaproveedor, a.cuota, a.sucursal, �E �? 	a.idproveedor, a.fecha, a.vencimiento, b.idcartera,c.cartera, �* �$ 	a.fec_acobrar, a.importe, a.saldo, �) �# 	a.idmoneda, a.pendiente, b.razon, �z �t 	case when ?m.TipoCotizacion='F' then  f.Cotizacion else DBO.fnGetCotizacion(a.IdMoneda,GetDate()) end as Cotizacion�6 �0 	FROM cp_forma_pago a inner join cp_proveedor b �E �? 	on a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa �V �P 	left join cp_cartera c on b.idcartera=c.idcartera and b.IdEmpresa = c.IdEmpresa�> �8 	inner join cp_factura f on a.nrocomprob = f.IdFactura  �) �# 	WHERE  a.IdEmpresa = ?oApp.Empresa� � 	AND a.saldo > 0  �: �4 	and (b.idcartera=?m.cartera or ?m.cartera is null) �@ �: 	AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor�9 �3 	AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence �Y �S 	ORDER BY b.idcartera, a.idproveedor, a.idmoneda, a.vencimiento, a.facturaproveedor� �  � � ��C � � cp_rvencefactu� �� F� � U  CARTERA
 HPROVEEDOR DATOS CMDSQL SQL CP_RVENCEFACTU BeforeOpenTables,     �� InitA     ��1 q 3 � A 1A � � �Q���aQa������a A �q 3                       &         A   @      )   �                  