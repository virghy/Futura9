  Y                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=1
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      W    winspool  PrimoPDF  PrimoPort:                  1)  USB006                          	HPrimoPDF .0.195\HP Deskjet 100  � �S�  �
od   ,  ,  Letter                                                                            PRIV�0                                                                                       '''  '          �                                  \K �	                             �{��     � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         tocol pcl3gui hpImagingDll hpfime50.dll Orientation PORTRAIT ColorMode 24bpp HPGrayScale 0 HPTextThreshold 24 HPGraphicThreshold 22 hpSupportsREST 1 hpDPIInfo 0 HPHideQualitySettings 0 Resolution 300x300dpi Halftone HT_PATSIZE_DEFAULT HPMechOffset 60 HPRlt 1 HPPagesToPrint 4_AllPages PaperSize A4 HPMaxDpi 0_disabled MediaType 0_-2_300x300                                                                    P  LPPH   Q�`Q�`                                      X  ,  X  ,     �  �  �  }  K   K   <   <   <   <          '           '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Mon Aug 01 09:33:00:535 2011                                                                    w�`K   K                w�`      Arial      IdMoneda      vt_rventacliente.idcliente      Arial      Arial      Arial      Arial      Arial      "Ventas por Cliente"             Arial      empresa             Arial      *iif(empty(m.sucursal),'Todos',descripci�n)             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Cliente"      Arial      "Cpbte."      Arial      "Nro."      Arial      "Total"      Arial      	"Fecha
"      Arial      "Moneda: ",IdMoneda      Arial      vt_rventacliente.idcliente             Arial      vt_rventacliente.razsocial      Arial      vt_rventacliente.fecha             Arial      vt_rventacliente.abrev             Arial      numero      Arial      TotalFactura      "9,999,999,999,999.99"      Arial      P"Total ("+  alltrim(vt_rventacliente.idcliente)+") "+ vt_rventacliente.razsocial             Arial      TotalFactura      "999,999,999,999,999.99"      Arial      "Total ("+  IdMoneda+") "      Arial      TotalFactura      "999,999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 187
Left = -65
Width = 793
Height = 439
InitialSelectedAlias = "vt_rventacliente"
DataSource = .NULL.
Name = "Dataenvironment"
     MPROCEDURE BeforeOpenTables
DO seteo

strsql = 'select a.idfactura, a.numero, a.idcomprobante, a.fecha, '+;
'a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  '+;
'c.razsocial,b.abrev, d.descripci�n,a.TotalFactura,a.IdMoneda '+;
'from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d '+;
'where a.idcomprobante = b.idcomprobante and a.IdEmpresa = b.IdEmpresa  and a.idempresa=?oapp.empresa '+;
'and a.idcliente = c.idcliente and a.IdEmpresa = c.IdEmpresa '+;
'and a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa '+;
'AND a.fecha >= ?m.dfecha '+;
'AND a.fecha <= ?m.hfecha '+;
IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')+;
IIF(!EMPTY(m.idcliente ),'and a.sucursal = ?m.sucursal  ',''+'Order by a.IdMoneda,a.idcliente ')

= sql(strsql,'vt_rventacliente')
SELECT vt_rventacliente
ENDPROC
     ����    �  �                        e4   %   K      q     e          �  U  � �  ��T� ��8 select a.idfactura, a.numero, a.idcomprobante, a.fecha, �A a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  �= c.razsocial,b.abrev, d.descripci�n,a.TotalFactura,a.IdMoneda �8 from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d �e where a.idcomprobante = b.idcomprobante and a.IdEmpresa = b.IdEmpresa  and a.idempresa=?oapp.empresa �< and a.idcliente = c.idcliente and a.IdEmpresa = c.IdEmpresa �: and a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa � AND a.fecha >= ?m.dfecha � AND a.fecha <= ?m.hfecha CC�� �
�& �   and a.idcliente = ?m.idcliente � �  6CC�� �
�$ � and a.sucursal = ?m.sucursal  �' �  �  Order by a.IdMoneda,a.idcliente 6��! ��C � � vt_rventacliente� �� F� � U  SETEO STRSQL	 IDCLIENTE SQL VT_RVENTACLIENTE BeforeOpenTables,     ��1 q �+q 1                       B      )   �                  