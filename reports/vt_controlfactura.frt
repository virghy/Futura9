                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Arial      idmoneda       vt_rcontrolfactura.idcomprobante      Arial      Arial      Arial      Arial       "Control de Emisi�n de Facturas"             Arial      empresa             Arial      descripci�n             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Nro."      Arial      "Cond."      Arial      	"Cliente"      Arial      	"Exentas"      Arial      
"Gravadas"      Arial      "Iva"      Arial      "Total"      Arial      	"Fecha
"      Arial      *"Moneda : "+   vt_rcontrolfactura.idmoneda             Arial      2"Comprobante : "+   vt_rcontrolfactura.descripcion             Arial      vt_rcontrolfactura.fecha             Arial      vt_rcontrolfactura.numero             Arial      IdCondicion      Arial      9vt_rcontrolfactura.idcliente,vt_rcontrolfactura.razsocial      Arial      vt_rcontrolfactura.exenta      "999,999,999.99"      Arial      vt_rcontrolfactura.gravada      "999,999,999.99"      Arial      vt_rcontrolfactura.iva      "999,999,999.99"      Arial      Pvt_rcontrolfactura.exenta +  vt_rcontrolfactura.gravada + vt_rcontrolfactura.iva      "9,999,999,999.99"      Arial      vt_rcontrolfactura.exenta      "99,999,999,999.99"      Arial      vt_rcontrolfactura.gravada      "99,999,999,999.99"      Arial      vt_rcontrolfactura.iva      "99,999,999,999.99"      Arial      Qvt_rcontrolfactura.exenta +  vt_rcontrolfactura.gravada +  vt_rcontrolfactura.iva      "999,999,999,999.99"      Arial      "Total /Comprobante"      Arial      vt_rcontrolfactura.exenta      "99,999,999,999.99"      Arial      vt_rcontrolfactura.gravada      "99,999,999,999.99"      Arial      vt_rcontrolfactura.iva      "99,999,999,999.99"      Arial      Qvt_rcontrolfactura.exenta +  vt_rcontrolfactura.gravada +  vt_rcontrolfactura.iva      "999,999,999,999.99"      Arial      0"Total Moneda : "+   vt_rcontrolfactura.idmoneda             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      vt_rcontrolfactura.exenta      "999,999,999,999.99"      Arial      vt_rcontrolfactura.gravada      "999,999,999,999.99"      Arial      vt_rcontrolfactura.iva      "999,999,999.99"      Arial      Qvt_rcontrolfactura.exenta +  vt_rcontrolfactura.gravada +  vt_rcontrolfactura.iva      "999,999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      �Top = 159
Left = 242
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "restado_cuenta"
DataSource = .NULL.
Name = "Dataenvironment"
     6PROCEDURE BeforeOpenTables
DO seteo
	


LOCAL strsql
SET DATABASE TO DATOS 
SET DATE BRITISH 

IF EMPTY(m.Sucursal)
	m.Sucursal = null
ENDIF
	


 strsql = 'SELECT a.sucursal, a.fecha, a.idcomprobante, '+;
'  a.numero, idfactura, a.idcliente, ISNULL(a.exenta,0) as Exenta, '+;
'  ISNULL(a.gravada,0) as Gravada, ISNULL(a.iva,0) as Iva, a.idmoneda, b.razsocial, '+;
'  c.descripcion, d.descripci�n,a.IdCondicion '+;
' FROM vt_factura a, vt_clientes b, vt_cpbt c, sucursal d,vt_Condicion con '+;
' WHERE a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa'+;
'   AND a.idcomprobante = c.idcomprobante and a.IdEmpresa = c.IdEmpresa'+;
'   AND a.IdCondicion = con.Idcondicion and a.IdEmpresa = con.IdEmpresa'+;
'   AND a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa'+;
'   AND a.IdEmpresa = ?oApp.Empresa' +;
'   AND a.fecha >= ?m.dfecha '+;
'   AND a.fecha <= ?m.hfecha '+;
'   AND (a.Sucursal = ?m.Sucursal or ?m.Sucursal is null) ' + ;
IIF(!EMPTY(m.idcomprob),'   AND a.idcomprobante = ?m.idcomprob ','')+;
IIF(!INLIST(m.IdCondicion,'XX','X1') ,'   AND a.IdCondicion = ?m.IdCondicion ','')+;
IIF(m.IdCondicion='X1' ,'   AND con.Plazo >0 ','')+;
' order by a.idmoneda, a.Sucursal, a.idcomprobante, a.fecha, a.Numero '

=sql(strsql, 'vt_rcontrolfactura')
SELECT vt_rcontrolfactura

ENDPROC
     ����    �  �                        Nm   %         B     (          �  U  � �  � �� � G(� DATOS� G� BRITISH� %�C�� ���I � T�� ���� �
T� ��- SELECT a.sucursal, a.fecha, a.idcomprobante, �B   a.numero, idfactura, a.idcliente, ISNULL(a.exenta,0) as Exenta, �S   ISNULL(a.gravada,0) as Gravada, ISNULL(a.iva,0) as Iva, a.idmoneda, b.razsocial, �-   c.descripcion, d.descripci�n,a.IdCondicion �J  FROM vt_factura a, vt_clientes b, vt_cpbt c, sucursal d,vt_Condicion con �>  WHERE a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�F    AND a.idcomprobante = c.idcomprobante and a.IdEmpresa = c.IdEmpresa�F    AND a.IdCondicion = con.Idcondicion and a.IdEmpresa = con.IdEmpresa�<    AND a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa�"    AND a.IdEmpresa = ?oApp.Empresa�    AND a.fecha >= ?m.dfecha �    AND a.fecha <= ?m.hfecha �9    AND (a.Sucursal = ?m.Sucursal or ?m.Sucursal is null) CC�� �
�, �&    AND a.idcomprobante = ?m.idcomprob � �  6CC�� � XX� X1�
�, �&    AND a.IdCondicion = ?m.IdCondicion � �  6C�� � X1� �    AND con.Plazo >0 � �  6�E  order by a.idmoneda, a.Sucursal, a.idcomprobante, a.fecha, a.Numero ��# ��C � � vt_rcontrolfactura� �� F� � U	  SETEO STRSQL DATOS BRITISH SUCURSAL	 IDCOMPROB IDCONDICION SQL VT_RCONTROLFACTURA BeforeOpenTables,     ��1 q t � � � A 0�@2q 2                       +      )   �                  