  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      IdMoneda      cp_rcompraconcepto.idconcepto      Arial      Arial      Arial      Arial      Arial      "Compras y Gastos por Concepto"             Arial      empresa             Arial      
"Sucursal"      Arial      .iif( empty(m.sucursal), 'Todos', m.sucursal  )             Arial      
"Per�odo:"      Arial      m.dfecha,"  al ",m.hfecha      Arial      "Factura
Proveedor"      "@I"      Arial      
"Concepto"      Arial      "Cpbte."      Arial      "Nro."      Arial      "Proveedor"      Arial      "Total"      Arial      	"Fecha
"      Arial      "Moneda:" , IdMoneda      Arial      cp_rcompraconcepto.idconcepto             Arial      cp_rcompraconcepto.concepto             Arial      cp_rcompraconcepto.fecha             Arial      cp_rcompraconcepto.idcomprob             Arial      cp_rcompraconcepto.idfactura             Arial      #cp_rcompraconcepto.facturaproveedor             Arial      =alltrim(IdProveedor),' ' ,cp_rcompraconcepto.razon,Referencia      Arial      Total      "999,999,999,999.99"      Arial      V"Total ("+  alltrim(cp_rcompraconcepto.idconcepto)+") "+   cp_rcompraconcepto.concepto             Arial      Total      "99,999,999,999,999.99"      Arial      Total      "999,999,999,999.99"      Arial      "Total Moneda"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 95
Left = 13
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
LOCAL strsql, dvfecha, hvfecha, vproveedor,vsucursal 
IF EMPTY(m.idproveedor)
	m.idProveedor = null
ENDIF

IF EMPTY(m.IdConcepto)
	m.IdConcepto = null
ENDIF

IF EMPTY(m.sucursal )
	m.sucursal = null
ENDIF	

TEXT TO cmdSQL noshow
SELECT c.idfactura, a.idcomprob, c.facturaproveedor, c.idproveedor, c.fecha,
	c.sucursal, ISNULL(c.exenta,0) as exenta, ISNULL(c.gravada,0) as gravada, 
	isnull(c.iva,0)as iva, c.idmoneda, b.razon, d.idconcepto, 
	d.concepto,c.Referencia,c.Total 
	FROM cp_comprobante a, cp_proveedor b, cp_factura c 
	LEFT OUTER JOIN cn_conceptos d 
	ON  c.idempresa=d.idempresa and c.idconcepto = d.idconcepto   
	WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.IdEmpresa = b.IdEmpresa
	AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa
	AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha
	AND (c.idproveedor = ?m.idproveedor  or ?m.idProveedor is null)
	AND (c.idconcepto = ?m.idconcepto or ?m.idconcepto is null)
	AND (c.sucursal = ?m.sucursal or ?m.sucursal  is null)
	ORDER BY d.idconcepto, c.fecha,   c.idcomprobante, c.idfactura  
ENDTEXT

 = sql(cmdSQL,'cp_rcompraconcepto')
 SELECT cp_rcompraconcepto
 
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        V�   %         v      6          �  U  T ��  � � � � � %�C�� ���5 � T�� ���� � %�C�� ���W � T�� ���� � %�C�� ���y � T�� ���� �	 M(� ��R �L SELECT c.idfactura, a.idcomprob, c.facturaproveedor, c.idproveedor, c.fecha,�Q �K 	c.sucursal, ISNULL(c.exenta,0) as exenta, ISNULL(c.gravada,0) as gravada, �A �; 	isnull(c.iva,0)as iva, c.idmoneda, b.razon, d.idconcepto, �' �! 	d.concepto,c.Referencia,c.Total �; �5 	FROM cp_comprobante a, cp_proveedor b, cp_factura c �& �  	LEFT OUTER JOIN cn_conceptos d �E �? 	ON  c.idempresa=d.idempresa and c.idconcepto = d.idconcepto   �g �a 	WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.IdEmpresa = b.IdEmpresa�G �A 	AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa�2 �, 	AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha�F �@ 	AND (c.idproveedor = ?m.idproveedor  or ?m.idProveedor is null)�B �< 	AND (c.idconcepto = ?m.idconcepto or ?m.idconcepto is null)�= �7 	AND (c.sucursal = ?m.sucursal or ?m.sucursal  is null)�G �A 	ORDER BY d.idconcepto, c.fecha,   c.idcomprobante, c.idfactura  � �# ��C � � cp_rcompraconcepto�	 �� F�
 � U  STRSQL DVFECHA HVFECHA
 VPROVEEDOR	 VSUCURSAL IDPROVEEDOR
 IDCONCEPTO SUCURSAL CMDSQL SQL CP_RCOMPRACONCEPTO
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 q� A � A � A � !q�aQqq!a!�qA 2q 3 q 2                       �        �  �  $    )   �                  