  g                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Arial      Sucursal      idmoneda      Idcondicion      Arial      Arial      Arial      Arial      +"Control Emisi�n de Facturas por Condici�n"      Arial      empresa             Arial      descripci�n             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Nro."      Arial      	"Cliente"      Arial      "Total"      Arial      	"Fecha
"      Arial      &"Sucursal : "+   Sucursal, Descripci�n      Arial      *"Moneda : "+   vt_rcontrolfactura.idmoneda             Arial      ("Condici�n : "+   IdCondicion, Condicion      Arial      vt_rcontrolfactura.fecha      Arial      (IdComprobante, vt_rcontrolfactura.numero      Arial      9vt_rcontrolfactura.idcliente,vt_rcontrolfactura.razsocial      Arial      TotalFactura      "9,999,999,999.99"      Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total /Condicion"      Arial      TotalFactura      "999,999,999,999.99"      Arial      0"Total Moneda : "+   vt_rcontrolfactura.idmoneda      Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total Sucursal : "+   Sucursal      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      �Top = 159
Left = 242
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "restado_cuenta"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo
	


LOCAL strsql
SET DATABASE TO DATOS 
SET DATE BRITISH 

IF EMPTY(m.Sucursal)
	m.Sucursal = null
ENDIF
	


 strsql = 'SELECT a.sucursal, a.fecha, a.idcomprobante, '+;
'  a.numero, idfactura, a.idcliente, ISNULL(a.exenta,0) as Exenta, '+;
'  ISNULL(a.gravada,0) as Gravada, ISNULL(a.iva,0) as Iva, a.TotalFactura, a.idmoneda, b.razsocial, '+;
'  c.descripcion, d.descripci�n,a.IdCondicion, con.Descripcion Condicion '+;
' FROM vt_factura a, vt_clientes b, vt_cpbt c, sucursal d,vt_Condicion con '+;
' WHERE a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa'+;
'   AND a.idcomprobante = c.idcomprobante and a.IdEmpresa = c.IdEmpresa'+;
'   AND a.IdCondicion = con.Idcondicion and a.IdEmpresa = con.IdEmpresa'+;
'   AND a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa'+;
'   AND a.IdEmpresa = ?oApp.Empresa' +;
'   AND a.fecha >= ?m.dfecha '+;
'   AND a.fecha <= ?m.hfecha '+;
'   AND (a.Sucursal = ?m.Sucursal or ?m.Sucursal is null) ' + ;
IIF(!INLIST(m.IdCondicion,'XX','X1') ,'   AND a.IdCondicion = ?m.IdCondicion ','')+;
IIF(m.IdCondicion='X1' ,'   AND con.Plazo >0 ','')+;
' order by a.idmoneda, a.Sucursal, a.IdCondicion,  a.fecha, a.Numero '

=sql(strsql, 'vt_rcontrolfactura')
SELECT vt_rcontrolfactura

ENDPROC
     ����    m  m                        s�   %   �      $     
          �  U  q �  � �� � G(� DATOS� G� BRITISH� %�C�� ���I � T�� ���� ��T� ��- SELECT a.sucursal, a.fecha, a.idcomprobante, �B   a.numero, idfactura, a.idcliente, ISNULL(a.exenta,0) as Exenta, �c   ISNULL(a.gravada,0) as Gravada, ISNULL(a.iva,0) as Iva, a.TotalFactura, a.idmoneda, b.razsocial, �H   c.descripcion, d.descripci�n,a.IdCondicion, con.Descripcion Condicion �J  FROM vt_factura a, vt_clientes b, vt_cpbt c, sucursal d,vt_Condicion con �>  WHERE a.idcliente = b.idcliente and a.IdEmpresa = b.IdEmpresa�F    AND a.idcomprobante = c.idcomprobante and a.IdEmpresa = c.IdEmpresa�F    AND a.IdCondicion = con.Idcondicion and a.IdEmpresa = con.IdEmpresa�<    AND a.sucursal = d.sucursal and a.IdEmpresa = d.IdEmpresa�"    AND a.IdEmpresa = ?oApp.Empresa�    AND a.fecha >= ?m.dfecha �    AND a.fecha <= ?m.hfecha �9    AND (a.Sucursal = ?m.Sucursal or ?m.Sucursal is null) CC�� � XX� X1�
�, �&    AND a.IdCondicion = ?m.IdCondicion � �  6C�� � X1� �    AND con.Plazo >0 � �  6�D  order by a.idmoneda, a.Sucursal, a.IdCondicion,  a.fecha, a.Numero ��# ��C � � vt_rcontrolfactura� �� F� � U  SETEO STRSQL DATOS BRITISH SUCURSAL IDCONDICION SQL VT_RCONTROLFACTURA BeforeOpenTables,     ��1 q t � � � A  q?2q 2                             )   m                  