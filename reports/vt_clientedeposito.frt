  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=1
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
      W    winspool  PrimoPDF  PrimoPort:                  B001                                tPrimoPDF t 1000 J110 series     � �S�  �4d   X  X  A4                                                                                PRIV�0                                                                                       '''  '        � �                                  P4 (�                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       �   SMTJ     �                                                                                                                                                                                            V4DM                         Arial      IdMoneda      sucursal      	idcliente      Arial      Arial      Arial      Arial      Arial      "Ventas por Cliente y Sucursal"             Arial      empresa             Arial      *iif(empty(m.sucursal),'Todos',descripci�n)             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Cliente"      Arial      	"Fecha
"      Arial      "Cpbte."      Arial      "Nro."      Arial      "Exenta"             Arial      	"Gravada"      Arial      "IVA"      Arial      "Total"      Arial      IdMoneda      Arial      "Moneda"      Arial      descripci�n             Arial      
"Sucursal"      Arial      +IdCliente,' ',vt_rclientedeposito.razsocial      Arial      vt_rclientedeposito.fecha             Arial      idcomprobante             Arial      numero      "@B"      Arial      exenta      "999,999,999.99"      Arial      Gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "999,999,999,999.99"      Arial      V"Total ("+  alltrim(vt_rclientedeposito.idcliente)+") "+ vt_rclientedeposito.razsocial      Arial      exenta      "999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "999,999,999,999.99"      Arial      $"Total ("+  alltrim(descripci�n)+")"      Arial      exenta      "999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "999,999,999,999.99"      Arial      !"Total ("+  alltrim(IdMoneda)+")"      Arial      exenta      "999,999,999.99"      Arial      gravada      "99,999,999,999.99"      Arial      iva      "999,999,999.99"      Arial      total      "999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 157
Left = -2
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rclientedeposito"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
LOCAL strsql
SET DATABASE TO datos

IF EMPTY(m.Idcliente)
	m.Idcliente = null
ENDIF
	

strsql = 'select b.idcliente, b.fecha, b.sucursal, b.idcomprobante, b.idfactura, '+;
		'b.numero, c.razsocial, d.descripci�n, '+;
		'isnull(b.exenta,0) exenta, '+;
		'isnull(b.gravada,0) gravada, '+;
		'isnull(b.iva,0) iva, b.ImpDesc,'+;
		'b.TotalFactura total, b.IdMoneda '+;
		'from vt_factura b, vt_clientes c, sucursal d '+;
		'where  b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa '+;
		'and b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa '+;
		'and (b.idcliente = ?m.idcliente or ?m.idcliente is null)'+;
		'and b.IdEmpresa = ?oApp.Empresa ' +;
		'and b.sucursal = ?m.sucursal '+;
		'and b.fecha BETWEEN ?m.dfecha and ?m.hfecha '+;
		'order by IdMoneda,b.Sucursal,b.idcliente, b.idcomprobante '

=sql(strsql ,'vt_rclientedeposito')
SELECT vt_rclientedeposito
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     9���                                )�   %   �      �     �          �  U   ��  � G(� datos� %�C�� ���3 � T�� ���� ��T�  ��G select b.idcliente, b.fecha, b.sucursal, b.idcomprobante, b.idfactura, �& b.numero, c.razsocial, d.descripci�n, � isnull(b.exenta,0) exenta, � isnull(b.gravada,0) gravada, � isnull(b.iva,0) iva, b.ImpDesc,�! b.TotalFactura total, b.IdMoneda �- from vt_factura b, vt_clientes c, sucursal d �= where  b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa �< and b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa �8 and (b.idcliente = ?m.idcliente or ?m.idcliente is null)�  and b.IdEmpresa = ?oApp.Empresa � and b.sucursal = ?m.sucursal �, and b.fecha BETWEEN ?m.dfecha and ?m.hfecha �: order by IdMoneda,b.Sucursal,b.idcliente, b.idcomprobante ��$ ��C �  � vt_rclientedeposito� �� F� � U  STRSQL DATOS	 IDCLIENTE SQL VT_RCLIENTEDEPOSITO
  �  � U  SETEO Init,     �� BeforeOpenTablesp    ��1 q � � A � q*Bq 2 q 2                       �        �  �      )                      