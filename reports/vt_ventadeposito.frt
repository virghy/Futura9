  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      sucursal      Arial      Arial      Arial      Arial      "Ventas por Sucursales"             Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Comprob."      Arial      	"Cliente"      Arial      "Total"      Arial      	"Fecha
"      Arial      ?"Sucursal : "+   vt_rventadeposito.sucursal + '  '+ descripci�n             Arial      vt_rVentaDeposito.fecha             Arial      numero             Arial      vt_rVentaDeposito.idcliente             Arial      	razsocial             Arial      TotalFactura      "999,999,999,999.99"      Arial      idcomprobante             Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total Sucursal"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      �Top = 142
Left = -61
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rventadeposito"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
SET DATABASE TO DATOS 
strsql = 'select a.idfactura, a.numero, a.idcomprobante, a.fecha, '+;
'a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  '+;
'c.razsocial,b.abrev, d.descripci�n,a.TotalFactura '+;
'from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d '+;
'where a.idcomprobante = b.idcomprobante AND a.idempresa=b.IdEmpresa '+;
'and a.idcliente = c.idcliente and  a.idempresa=c.IdEmpresa '+;
'and a.sucursal = d.sucursal and a.idempresa=d.IdEmpresa '+;
' and a.IdEmpresa = ?oApp.Empresa '+;
'AND a.fecha >= ?m.dfecha '+;
'AND a.fecha <= ?m.hfecha '+;
IIF(!EMPTY(m.sucursal ),'and a.sucursal = ?m.sucursal ','')+;
' order by a.sucursal,a.fecha, a.idcliente, a.numero '

*IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')+;


= sql(strsql,'vt_rVentaDeposito')
SELECT vt_rVentaDeposito

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        E�   %   :      r     b          �  U  � G(� DATOS��T� ��8 select a.idfactura, a.numero, a.idcomprobante, a.fecha, �A a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  �2 c.razsocial,b.abrev, d.descripci�n,a.TotalFactura �8 from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d �D where a.idcomprobante = b.idcomprobante AND a.idempresa=b.IdEmpresa �; and a.idcliente = c.idcliente and  a.idempresa=c.IdEmpresa �8 and a.sucursal = d.sucursal and a.idempresa=d.IdEmpresa �!  and a.IdEmpresa = ?oApp.Empresa � AND a.fecha >= ?m.dfecha � AND a.fecha <= ?m.hfecha CC�� �
�# � and a.sucursal = ?m.sucursal � �  6�4  order by a.sucursal,a.fecha, a.idcliente, a.numero ��" ��C � � vt_rVentaDeposito� �� F� � U  DATOS STRSQL SUCURSAL SQL VT_RVENTADEPOSITO
  �  � U  SETEO Init,     �� BeforeOpenTables%    ��1 � �(%q 3 q 2                       X          �      )   �                  