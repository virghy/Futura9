  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=1
PAPERSIZE=5
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=3
COLLATE=1
      8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S�  �
od   ,  ,  Letter                                                                            PRIV�0                                                                                       '''  '          �                                  \K hC                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial      Arial      Arial      Arial      Arial      'Resumen de Gastos'      Arial      empresa      Arial      
"Per�odo:"      Arial      m.dfecha,  ' al ', m.hfecha      Arial      "Concepto
"      Arial      	"Monto
"      Arial      "Documento
"      Arial      "Observacion
"      Arial      concepto      Arial      Total      "9,999,999,999"      Arial      FacturaProveedor      Arial      
referencia      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Total      "9,999,999,999"      Arial      "Total Egreso: 
"      Arial      dataenvironment      �Top = -9
Left = 506
Width = 385
Height = 419
InitialSelectedAlias = "cuentas_y_presupuestos"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
DO SETEO
TEXT TO CMDSQL NOSHOW
SELECT cp_factura.IdEmpresa, cn_conceptos.concepto, cp_factura.referencia, cp_factura.FacturaProveedor, cp_factura.Total
FROM   cp_factura INNER JOIN 
					cn_conceptos ON cp_factura.IdEmpresa = cn_conceptos.idempresa AND cp_factura.IdConcepto = cn_conceptos.idconcepto
where cp_factura.idempresa=?oApp.Empresa and cp_factura.fecha between ?m.dfecha and ?m.hfecha and (cp_factura.tipo = 'V' or cp_factura.tipo = 'G')
ENDTEXT
sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
     ����    �  �                        *�   %   :      ^     H          �  U  � �  �	 M(� ��~ �x SELECT cp_factura.IdEmpresa, cn_conceptos.concepto, cp_factura.referencia, cp_factura.FacturaProveedor, cp_factura.Total�# � FROM   cp_factura INNER JOIN �| �v 					cn_conceptos ON cp_factura.IdEmpresa = cn_conceptos.idempresa AND cp_factura.IdConcepto = cn_conceptos.idconcepto�� �� where cp_factura.idempresa=?oApp.Empresa and cp_factura.fecha between ?m.dfecha and ?m.hfecha and (cp_factura.tipo = 'V' or cp_factura.tipo = 'G')� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA Init,     ��1 q � �1��	A �q 1                             )   �                  