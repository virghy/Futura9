     @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
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
                                                          8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S�  �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             D�M      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Arial                                                         vt_rventacliente.idcliente                                    <(nvl(exenta,0) +  nvl(gravada,0)+nvl(iva,0))- nvl(impdesc,0)                                                                  "9,999,999,999,999.99"                                                                                                      Arial                                                         "Ventas por Cliente"                                                                                                        Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         vt_rventacliente.fecha                                                                                                      Arial                                                         vt_rventacliente.idcliente                                                                                                  Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         	"Cliente"                                                     Arial                                                         "Nro."                                                        Arial                                                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                                                  Arial                                                         vt_rventacliente.razsocial                                                                                                  Arial                                                         numero                                                                                                                      Arial                                                         
"Sucursal"                                                    Arial                                                         <(nvl(exenta,0) +  nvl(gravada,0)+nvl(iva,0))- nvl(impdesc,0)                                                                  "999,999,999,999,999.99"                                                                                                    Arial                                                         <(nvl(exenta,0) +  nvl(gravada,0)+nvl(iva,0))- nvl(impdesc,0)                                                                  "99,999,999,999,999.99"                                                                                                     Arial                                                         "Total"                                                       Arial                                                         "Total General"                                               Arial                                                         "Cpbte."                                                      Arial                                                         vt_rventacliente.abrev                                                                                                      Arial                                                         P"Total ("+  alltrim(vt_rventacliente.idcliente)+") "+ vt_rventacliente.razsocial                                                                                                            Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 187
Left = -65
Width = 793
Height = 439
InitialSelectedAlias = "vt_rventacliente"
DataSource = .NULL.
Name = "Dataenvironment"
                                                 *PROCEDURE BeforeOpenTables
DO seteo

strsql = 'select a.idfactura, a.numero, a.idcomprobante, a.fecha, '+;
'a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  '+;
'c.razsocial,b.abrev, d.descripci�n '+;
'from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d '+;
'where a.idcomprobante = b.idcomprobante and a.IdEmpresa = b.IdEmpresa  and a.idempresa=?oapp.empresa '+;
'and a.idcliente = c.idcliente and a.IdEmpresa = c.IdEmpresa '+;
'and a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa '+;
'AND a.fecha >= ?m.dfecha '+;
'AND a.fecha <= ?m.hfecha '+;
IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')+;
IIF(!EMPTY(m.idcliente ),'and a.sucursal = ?m.sucursal  ',''+'Order by a.idcliente ')

= sql(strsql,'vt_rventacliente')
SELECT vt_rventacliente

ENDPROC
                   ����    �  �                        ��   %   &      L     @          �  U  � �  ��T� ��8 select a.idfactura, a.numero, a.idcomprobante, a.fecha, �A a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  �# c.razsocial,b.abrev, d.descripci�n �8 from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d �e where a.idcomprobante = b.idcomprobante and a.IdEmpresa = b.IdEmpresa  and a.idempresa=?oapp.empresa �< and a.idcliente = c.idcliente and a.IdEmpresa = c.IdEmpresa �: and a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa � AND a.fecha >= ?m.dfecha � AND a.fecha <= ?m.hfecha CC�� �
�& �   and a.idcliente = ?m.idcliente � �  6CC�� �
�$ � and a.sucursal = ?m.sucursal  � �  � Order by a.idcliente 6��! ��C � � vt_rventacliente� �� F� � U  SETEO STRSQL	 IDCLIENTE SQL VT_RVENTACLIENTE BeforeOpenTables,     ��1 q <)q 2                             )   �                                  �DRIVER=winspool
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
                                                          8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S�  �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             D�M      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Arial                                                         vt_rventacliente.idcliente                                    <(nvl(exenta,0) +  nvl(gravada,0)+nvl(iva,0))- nvl(impdesc,0)                                                                  "9,999,999,999,999.99"                                                                                                      Arial                                                         "Ventas por Cliente"                                                                                                        Arial                                                         empresa                                                                                                                     Arial                                                         "Fecha"                                                      Arial                                                         
"Per�odo:"                                                    Arial                                                         &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                                      Arial                                                         vt_rventacliente.fecha                                                                                                      Arial                                                         vt_rventacliente.idcliente                                                                                                  Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         	"Cliente"                                                     Arial                                                         "Nro."                                                        Arial                                                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                                                  Arial                                                         vt_rventacliente.razsocial                                                                                                  Arial                                                         numero                                                                                                                      Arial                                                         
"Sucursal"                                                    Arial                                                         <(nvl(exenta,0) +  nvl(gravada,0)+nvl(iva,0))- nvl(impdesc,0)                                                                  "999,999,999,999,999.99"                                                                                                    Arial                                                         <(nvl(exenta,0) +  nvl(gravada,0)+nvl(iva,0))- nvl(impdesc,0)                                                                  "99,999,999,999,999.99"                                                                                                     Arial                                                         "Total"                                                       Arial                                                         "Total General"                                               Arial                                                         "Cpbte."                                                      Arial                                                         vt_rventacliente.abrev                                                                                                      Arial                                                         P"Total ("+  alltrim(vt_rventacliente.idcliente)+") "+ vt_rventacliente.razsocial                                                                                                            Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 187
Left = -65
Width = 793
Height = 439
InitialSelectedAlias = "vt_rventacliente"
DataSource = .NULL.
Name = "Dataenvironment"
                                                 8PROCEDURE BeforeOpenTables
SET DATABASE TO DATOS 

strsql = 'select a.idfactura, a.numero, a.idcomprobante, a.fecha, '+;
'a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  '+;
'c.razsocial,b.abrev, d.descripci�n '+;
'from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d '+;
'where a.idcomprobante = b.idcomprobante and a.IdEmpresa = b.IdEmpresa  and a.idempresa=?oapp.empresa '+;
'and a.idcliente = c.idcliente and a.IdEmpresa = c.IdEmpresa '+;
'and a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa '+;
'AND a.fecha >= ?m.dfecha '+;
'AND a.fecha <= ?m.hfecha '+;
IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')+;
IIF(!EMPTY(m.idcliente ),'and a.sucursal = ?m.sucursal  ',''+'Order by a.idcliente ')

= sql(strsql,'vt_rventacliente')
SELECT vt_rventacliente

ENDPROC
     ����    �  �                        �   %   -      S     G          �  U  � G(� DATOS��T� ��8 select a.idfactura, a.numero, a.idcomprobante, a.fecha, �A a.exenta, a.gravada, a.iva, a.impdesc, a.sucursal, a.idcliente,  �# c.razsocial,b.abrev, d.descripci�n �8 from vt_factura a, vt_cpbt b, vt_clientes c, sucursal d �e where a.idcomprobante = b.idcomprobante and a.IdEmpresa = b.IdEmpresa  and a.idempresa=?oapp.empresa �< and a.idcliente = c.idcliente and a.IdEmpresa = c.IdEmpresa �: and a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa � AND a.fecha >= ?m.dfecha � AND a.fecha <= ?m.hfecha CC�� �
�& �   and a.idcliente = ?m.idcliente � �  6CC�� �
�$ � and a.sucursal = ?m.sucursal  � �  � Order by a.idcliente 6��! ��C � � vt_rventacliente� �� F� � U  DATOS STRSQL	 IDCLIENTE SQL VT_RVENTACLIENTE BeforeOpenTables,     ��1 � <)q 2                       -      )   �                     