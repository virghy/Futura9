  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\192.168.0.195\HP Deskjet 1000 J110 series
OUTPUT=USB002
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      W  ?  winspool  \\192.168.0.195\HP Deskjet 1000 J110 series  USB002                       	H\\192.168.0.195\HP Deskjet 100   � �C�  �
od   ,  ,  Carta                                                                          DINU"  \PFj�                                 {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      SMTJ     H P   D e s k j e t   1 0 0 0   J 1 1 0   s e r i e s   InputBin 7 RESDLL UniresDLL Locale Spanish_Paraguay hpPrinterProtocol pcl3gui hpImagingDll hpfime50.dll Orientation PORTRAIT ColorMode 24bpp HPGrayScale 0 HPTextThreshold 24 HPGraphicThreshold 22 hpSupportsREST 1 hpDPIInfo 0 HPHideQualitySettings 0 Resolution 300x300dpi Halftone HT_PATSIZE_DEFAULT HPMechOffset 60 HPRlt 1 HPPagesToPrint 4_AllPages PaperSize A4 HPMaxDpi 0_disabled MediaType 0_-2_300x300                                                                    P  LPPH   Q�`Q�`                                      X  ,  X  ,     �  �  �  }  K   K   <   <   <   <          '           '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Fri Aug 12 14:20:40:107 2011                                                                    w�`K   K                w�`      Arial      mes      IdMoneda      Arial      Arial      Arial      Arial      Arial      Arial      &"Recordatorio de Vencimientos Mensual"             Arial      empresa             Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Per�odo :"      Arial      "Vencimiento"      Arial      "Fecha "      Arial      "  Proveedor"      Arial      "Comprobante"      Arial      	"Cuota
"      Arial      
"Moneda
"      Arial      	"Saldo
"      Arial      "Valorizado
"      Arial      mes             Arial      vencimiento             Arial      fecha             Arial      	proveedor             Arial      facturaproveedor             Arial      cuota      "99"             Arial      idmoneda             Arial      saldo      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      saldo * Cotizacion      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      "Total Moneda: "+IdMoneda             Arial      saldo      "999,999,999,999.99"             Arial      saldo * Cotizacion      "999,999,999,999.99"             Arial      "Total mes de: "+mes             Arial      saldo * Cotizacion      "999,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total General "             Arial      saldo * Cotizacion      "999,999,999,999.99"             Arial      dataenvironment      �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
     DPROCEDURE Init
If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF

TEXT TO cmdSQL noshow

	SELECT DATENAME(month,a.vencimiento) mes, a.fecha, a.nrocomprob,a.facturaproveedor, a.cuota,
	 a.sucursal,a.vencimiento, a.fec_acobrar, a.importe, a.saldo, 
	a.idmoneda, a.pendiente, rtrim(a.idproveedor)+'-'+b.razon proveedor, 
	DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion 
	FROM cp_forma_pago a, cp_proveedor b 
	WHERE  a.idempresa=?oApp.empresa and a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa  
	AND a.saldo > 0 and (a.vencimiento BETWEEN ?m.dfecha and ?m.hfecha ) 
	and (a.sucursal=?m.sucursal or ?m.sucursal is null)
	ORDER BY a.idmoneda,a.vencimiento,a.idproveedor

ENDTEXT

sql(cmdSQL,'rrecordatoriopag')
SELECT rrecordatoriopag

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���                              ס   %   W      �               �  U  � %�C��  ��� � J���(��  � �	 M(� �� �  �c �] 	SELECT DATENAME(month,a.vencimiento) mes, a.fecha, a.nrocomprob,a.facturaproveedor, a.cuota,�E �? 	 a.sucursal,a.vencimiento, a.fec_acobrar, a.importe, a.saldo, �L �F 	a.idmoneda, a.pendiente, rtrim(a.idproveedor)+'-'+b.razon proveedor, �? �9 	DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion �, �& 	FROM cp_forma_pago a, cp_proveedor b �h �b 	WHERE  a.idempresa=?oApp.empresa and a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa  �L �F 	AND a.saldo > 0 and (a.vencimiento BETWEEN ?m.dfecha and ?m.hfecha ) �: �4 	and (a.sucursal=?m.sucursal or ?m.sucursal is null)�6 �0 	ORDER BY a.idmoneda,a.vencimiento,a.idproveedor� �  � �! ��C � � rrecordatoriopag� �� F� � U  SUCURSAL CMDSQL SQL RRECORDATORIOPAG
  �  � U  SETEO Init,     �� BeforeOpenTablesB    ��1 � A � a 1Q������aa A q 3 q 2                               /  9      )                     