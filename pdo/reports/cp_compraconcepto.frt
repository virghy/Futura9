   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                cp_rcompraconcepto.idconcepto                                   Kcp_rcompraconcepto.gravada+cp_rcompraconcepto.exenta+cp_rcompraconcepto.iva                                                     Arial                          "999,999,999,999.99"           "Compras y Gastos por Concepto"                                                                Arial                          empresa                                                       Arial                          Arial                          "Fecha"                       Arial                          
"Per�odo:"                     0"desde "+dtoc(m.dfecha) +"  al " +dtoc(m.hfecha)                                               Arial                          cp_rcompraconcepto.fecha                                      Arial                          cp_rcompraconcepto.idproveedor                                                                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          "Proveedor"                    Arial                          "Nro."                         .iif( empty(m.sucursal), 'Todos', m.sucursal  )                                                 Arial                          cp_rcompraconcepto.razon                                      Arial                          cp_rcompraconcepto.idfactura                                                                   Arial                          Arial                          
"Sucursal"                     Kcp_rcompraconcepto.exenta+cp_rcompraconcepto.gravada+cp_rcompraconcepto.iva                                                     Arial                          "99,999,999,999,999.99"        Kcp_rcompraconcepto.gravada+cp_rcompraconcepto.exenta+cp_rcompraconcepto.iva                                                     Arial                          "999,999,999,999.99"           Arial                          "Total"                        Arial                          "Total General"                Arial                          "Cpbte."                       cp_rcompraconcepto.idcomprob                                                                   Arial                          V"Total ("+  alltrim(cp_rcompraconcepto.idconcepto)+") "+   cp_rcompraconcepto.concepto                                          Arial                          Arial                          
"Concepto"                     cp_rcompraconcepto.idconcepto                                                                  Arial                          cp_rcompraconcepto.concepto                                                                    Arial                          "@I"                           Arial                          "FacturaProveedor"            #cp_rcompraconcepto.facturaproveedor                                                            Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                wLeft = 13
Top = 95
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
Name = "Dataenvironment"
          �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
LOCAL strsql, dvfecha, hvfecha, vproveedor,vsucursal 
IF EMPTY(m.idproveedor)
	m.idProveedor = null
ENDIF

IF EMPTY(m.IdConcepto)
	m.IdConcepto = null
ENDIF
	

TEXT TO cmdSQL noshow
SELECT c.idfactura, a.idcomprob, c.facturaproveedor, c.idproveedor, c.fecha,
	c.sucursal, ISNULL(c.exenta,0) as exenta, ISNULL(c.gravada,0) as gravada, 
	isnull(c.iva,0)as iva, c.idmoneda, b.razon, d.idconcepto, 
	d.concepto 
	FROM cp_comprobante a, cp_proveedor b, cp_factura c 
	LEFT OUTER JOIN cn_conceptos d 
	ON  c.idempresa=d.idempresa and c.idconcepto = d.idconcepto   
	WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.IdEmpresa = b.IdEmpresa
	AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa
	AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha
	AND (c.idproveedor = ?m.idproveedor  or ?m.idProveedor is null)
	AND (c.idconcepto = ?m.idconcepto or ?m.idconcepto is null)
	AND c.sucursal = ?m.sucursal  
	ORDER BY d.idconcepto, c.fecha,   c.idcomprobante, c.idfactura  
ENDTEXT

 = sql(cmdSQL,'cp_rcompraconcepto')
 SELECT cp_rcompraconcepto
 
ENDPROC
               ����    p  p                        QA   %   �           �          �  U  
  �  � U  SETEO ��  � � � � � %�C�� ���5 � T�� ���� � %�C�� ���W � T�� ���� �	 M(� ��R �L SELECT c.idfactura, a.idcomprob, c.facturaproveedor, c.idproveedor, c.fecha,�Q �K 	c.sucursal, ISNULL(c.exenta,0) as exenta, ISNULL(c.gravada,0) as gravada, �A �; 	isnull(c.iva,0)as iva, c.idmoneda, b.razon, d.idconcepto, � � 	d.concepto �; �5 	FROM cp_comprobante a, cp_proveedor b, cp_factura c �& �  	LEFT OUTER JOIN cn_conceptos d �E �? 	ON  c.idempresa=d.idempresa and c.idconcepto = d.idconcepto   �g �a 	WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.IdEmpresa = b.IdEmpresa�G �A 	AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa�2 �, 	AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha�F �@ 	AND (c.idproveedor = ?m.idproveedor  or ?m.idProveedor is null)�B �< 	AND (c.idconcepto = ?m.idconcepto or ?m.idconcepto is null)�% � 	AND c.sucursal = ?m.sucursal  �G �A 	ORDER BY d.idconcepto, c.fecha,   c.idcomprobante, c.idfactura  � �# ��C � � cp_rcompraconcepto� �� F�	 � U
  STRSQL DVFECHA HVFECHA
 VPROVEEDOR	 VSUCURSAL IDPROVEEDOR
 IDCONCEPTO CMDSQL SQL CP_RCOMPRACONCEPTO BeforeOpenTables,     �� InitA     ��1 q 3 q� A � A � !!�aQqq!a!QqA 2q 2                       &         A   �      )   p                  