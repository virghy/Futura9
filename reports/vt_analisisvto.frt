  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=1
      Arial      vt_rresumensaldos.idmoneda      Arial      Arial      Arial      Arial      Arial      %"An�lisis de Cartera por Vencimiento"             Arial      empresa             Arial      vt_rresumensaldos.descripci�n             Arial      "Sucursal:"      Arial      1"Desde " + dtoc(m.dvence)+ " al " +dtoc(m.hvence)             Arial      "Vencimientos:"      Arial      "Vto."      Arial      	"Cliente"             Arial      	"Fecha
"      Arial      "Cpbte."      Arial      "Cuota"      Arial      	"Importe"      Arial      "Saldo"      Arial      '"Moneda : " +vt_rresumensaldos.idmoneda      Arial      vt_rresumensaldos.vencimiento      Arial      &alltrim(idCliente) + " - " + razSocial             Arial      ttod(fecha)      "@D"      Arial      alltrim(idcomprobante),numero      Arial      vt_rresumensaldos.cuota             Arial      vt_rresumensaldos.importe      "@Z 999,999,999.99"             Arial      vt_rresumensaldos.saldo      "@Z 999,999,999,999.99"      Arial      vt_rresumensaldos.idmoneda             Arial      -"Total Moneda : " +vt_rresumensaldos.idmoneda             Arial      vt_rresumensaldos.saldo      "@Z 999,999,999,999.99"      Arial      vt_rresumensaldos.idmoneda             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 367
Left = 235
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rresumensaldos"
DataSource = .NULL.
Name = "Dataenvironment"
     -PROCEDURE Init
LOCAL strsql
SET DATABASE TO datos
DO SETEO


strsql ='select b.idcliente, b.fecha, b.sucursal, b.idcomprobante, b.idfactura, '+;
		'b.numero, a.vencimiento, a.cuota, a.importe, a.saldo, a.fec_acobrar, '+;
		'a.idmoneda, c.razsocial, d.descripci�n '+;
		'from vt_factura b, vt_forma_pago a, vt_clientes c, sucursal d '+;
		'where a.idfactura = b.idfactura and a.IdEmpresa = ?oApp.Empresa '+;
		'and b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa '+;
		'and b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa '+;
		'and b.sucursal = ?m.sucursal '+;
		'and a.vencimiento BETWEEN ?m.dvence and ?m.hvence '+;
		'and a.saldo > 0 ' +;
		' order by a.IdMoneda,a.vencimiento, b.idcliente, b.idcomprobante '
=sql(strsql ,'vt_rresumensaldos')
SELECT vt_rresumensaldos
ENDPROC
     ����    �  �                        /{   %   "      @     0          �  U  � ��  � G(� datos� � �|T�  ��G select b.idcliente, b.fecha, b.sucursal, b.idcomprobante, b.idfactura, �E b.numero, a.vencimiento, a.cuota, a.importe, a.saldo, a.fec_acobrar, �' a.idmoneda, c.razsocial, d.descripci�n �> from vt_factura b, vt_forma_pago a, vt_clientes c, sucursal d �@ where a.idfactura = b.idfactura and a.IdEmpresa = ?oApp.Empresa �: and b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa �< and b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa � and b.sucursal = ?m.sucursal �2 and a.vencimiento BETWEEN ?m.dvence and ?m.hvence � and a.saldo > 0 �A  order by a.IdMoneda,a.vencimiento, b.idcliente, b.idcomprobante ��" ��C �  � vt_rresumensaldos� �� F� � U  STRSQL DATOS SETEO SQL VT_RRESUMENSALDOS Init,     ��1 q � q �'!q 1                       "      )   �                  