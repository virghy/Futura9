  L                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Enviar a OneNote 2007
OUTPUT=Send To Microsoft OneNote Port:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=120
COLOR=1
YRESOLUTION=144
      Z  )  winspool  Enviar a OneNote 2007  Send To Microsoft OneNote Port:                       	XEnviar a OneNote 2007 J110 ser   � � /        d   x   �    Letter                                                                                wpno               �          �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          8  SMTJ     (H P   D e s k j e t   1 0 0 0   J 1 1 0   s e r i e s   InputBin 7 RESDLL UniresDLL Locale Spanish_Paraguay hpPrinterProtocol pcl3gui hpImagingDll hpfime51.dll Orientation PORTRAIT ColorMode 24bpp HPGrayScale 0 hpSupportsREST 1 HPTextThreshold 24 HPGraphicThreshold 22 Halftone HT_PATSIZE_DEFAULT HPMechOffset 60 hpDPIInfo 0 Resolution 600x300dpi HPRlt 1 HPPagesToPrint 4_AllPages PaperSize A4 MediaType 0.1004_0_600x300 HPHideQualitySettings 0 HPMaxDpi 0_disabled hpSpeedMech 3                                                                          H  LPPH   Q�`Q�`                              X  X  X  ,     �  �  �  }  K   K   <   <   <   <          '           '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Fri Apr 20 19:44:38:476 2012                                                                    w�`K   K                w�`      Arial      vt_rresumensaldos.idmoneda      	idcliente      Arial      Arial      Arial      Arial      Arial       "An�lisis de Cartera por Deudor"             Arial      empresa      Arial      vt_rresumensaldos.descripci�n             Arial      "Sucursal:"      Arial      1"Desde " + dtoc(m.dvence)+ " al " +dtoc(m.hvence)             Arial      "Vencimientos:"      Arial      "Vto."      Arial      "Fch. a Cobrar"      Arial      "Cuota"      Arial      	"Importe"      Arial      "Saldo"      Arial      "Nota"      Arial      	"Cliente"             Arial      	"Fecha
"      Arial      "Cpbte."      Arial      "Moneda: ", IdMoneda      Arial      �alltrim(idCliente) + " - " + Alltrim(razSocial), ", "+Alltrim(direccion),' ('+alltrim(NomContacto)+') ',' Tel. ' +alltrim(telefono)      Arial      Notas      Arial      fecha      "@D"      Arial      idcomprobante,numero             Arial      vt_rresumensaldos.vencimiento      "@D"      Arial      vt_rresumensaldos.fec_acobrar      "@D"      Arial      vt_rresumensaldos.cuota             Arial      vt_rresumensaldos.importe      "@Z 999,999,999.99"             Arial      vt_rresumensaldos.saldo      "@Z 999,999,999.99"             Arial      vt_rresumensaldos.idmoneda             Arial      ;"Total Cliente : " + alltrim(idCliente) + " - " + razSocial             Arial      vt_rresumensaldos.saldo      "@Z 999,999,999,999.99"             Arial      vt_rresumensaldos.idmoneda             Arial      -"Total Moneda : " +vt_rresumensaldos.idmoneda             Arial      vt_rresumensaldos.saldo      "@Z 999,999,999,999.99"             Arial      vt_rresumensaldos.idmoneda             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 78
Left = 169
Width = 555
Height = 347
InitialSelectedAlias = "vt_rresumensaldos"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
LOCAL strsql
DO SETEO

SET DATABASE TO datos

If Empty(m.idcliente)
	m.idcliente = null
ENDIF

IF EMPTY(m.Sucursal)
	m.Sucursal = null
ENDIF
	
	

strsql ='select b.idcliente, b.fecha, b.sucursal, b.idcomprobante, b.idfactura, '+;
		'b.numero, a.vencimiento, a.cuota, a.importe, a.saldo, a.fec_acobrar, '+;
		'a.idmoneda, c.razsocial, d.descripci�n,b.Notas,NomContacto,Telefono,Direccion '+;
		'from vt_factura b, vt_forma_pago a, vt_clientes c, sucursal d '+;
		'where a.idfactura = b.idfactura and a.IdEmpresa = ?oApp.Empresa '+;
		'and b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa '+;
		'and b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa '+;
		'and (b.idcliente = ?m.idcliente or ?m.idcliente is null)'+;
		'and (b.sucursal = ?m.sucursal or ?m.sucursal is null)'+;
		'and a.vencimiento BETWEEN ?m.dvence and ?m.hvence '+;
		'and a.saldo <> 0 ORDER BY a.idmoneda,B.IDCLIENTE,B.FECHA,B.NUMERO  '
=sql(strsql ,'vt_rresumensaldos')
SELECT vt_rresumensaldos

ENDPROC
     p���    W  W                        {   %   �           �          �  U  q ��  � � � G(� datos� %�C�� ���: � T�� ���� � %�C�� ���\ � T�� ���� ��T�  ��G select b.idcliente, b.fecha, b.sucursal, b.idcomprobante, b.idfactura, �E b.numero, a.vencimiento, a.cuota, a.importe, a.saldo, a.fec_acobrar, �N a.idmoneda, c.razsocial, d.descripci�n,b.Notas,NomContacto,Telefono,Direccion �> from vt_factura b, vt_forma_pago a, vt_clientes c, sucursal d �@ where a.idfactura = b.idfactura and a.IdEmpresa = ?oApp.Empresa �: and b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa �< and b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa �8 and (b.idcliente = ?m.idcliente or ?m.idcliente is null)�5 and (b.sucursal = ?m.sucursal or ?m.sucursal is null)�2 and a.vencimiento BETWEEN ?m.dvence and ?m.hvence �C and a.saldo <> 0 ORDER BY a.idmoneda,B.IDCLIENTE,B.FECHA,B.NUMERO  ��" ��C �  � vt_rresumensaldos� �� F� � U  STRSQL SETEO DATOS	 IDCLIENTE SUCURSAL SQL VT_RRESUMENSALDOS Init,     ��1 q q � � A � A ^.!q 2                       �      )   W                  