                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      IdMoneda      idproveedor      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial       "Vencimientos a Pagar Proveedor"      Arial      empresa             Arial      &iif(isnull(m.moneda),'Todos',m.Moneda)      Arial      &dtoc(m.dvence)+ " al " +dtoc(m.hvence)             Arial      
"Moneda :"      Arial      "Per�odo :"      Arial      "  Proveedor"      Arial      "Vencimiento"      Arial      "Fecha a Pagar"      Arial      "Fecha Compra
"      Arial      "Comprobante"      Arial      	"Cuota
"      Arial      "Importe
"      Arial      	"Saldo
"      Arial      "Moneda: " + idmoneda      Arial      $cp_rvencefactu.idproveedor," ",razon             Arial      cp_rvencefactu.vencimiento      "@D"             Arial      cp_rvencefactu.fec_acobrar      "@D"             Arial      cp_rvencefactu.fecha      "@D"             Arial      cp_rvencefactu.facturaproveedor             Arial      cp_rvencefactu.cuota      "99"             Arial      Importe      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      saldo      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      8"Total Proveedor: "+cp_rvencefactu.idproveedor," ",razon             Arial      saldo      "999,999,999,999.99"      Arial      "Total " + idmoneda      Arial      saldo      "999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
LOCAL strsql
IF EMPTY(m.Moneda)
	m.Moneda = null
ENDIF
	
SET DATABASE TO DATOS 
	strsql ='SELECT a.nrocomprob, '+;
	'  a.facturaproveedor, a.cuota, a.sucursal, '+;
	'  a.idproveedor, a.fecha, a.vencimiento, '+;
	'  a.fec_acobrar, a.importe, a.saldo, '+;
	'  a.idmoneda, a.pendiente, b.razon, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion'+;
	' FROM cp_forma_pago a, cp_proveedor b '+; 
	' WHERE  a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa ' + ;
	'   AND a.saldo > 0 '+;
	IIF(!EMPTY(m.dproveedor),'   AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor ','')+;
	IIF(!EMPTY(m.dvence),'   AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence ','')+;
	IIF(!isnull(m.moneda),'   AND a.IdMoneda=?m.Moneda ','')+;
	' ORDER BY a.idmoneda, a.idproveedor, a.vencimiento, a.facturaproveedor'
=sql(strsql,'cp_rvencefactu')
SELECT cp_rvencefactu


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     b���    I  I                        ��   %   �      �     �          �  U  + ��  � %�C�� ���% � T�� ���� � G(� DATOS��T�  �� SELECT a.nrocomprob, �+   a.facturaproveedor, a.cuota, a.sucursal, �)   a.idproveedor, a.fecha, a.vencimiento, �%   a.fec_acobrar, a.importe, a.saldo, �[   a.idmoneda, a.pendiente, b.razon, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion�&  FROM cp_forma_pago a, cp_proveedor b �c  WHERE  a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa �    AND a.saldo > 0 CC�� �
�C �=    AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor � �  6CC�� �
�; �5    AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence � �  6CC�� �
�" �    AND a.IdMoneda=?m.Moneda � �  6�F  ORDER BY a.idmoneda, a.idproveedor, a.vencimiento, a.facturaproveedor�� ��C �  � cp_rvencefactu� �� F� � U  STRSQL MONEDA DATOS
 DPROVEEDOR DVENCE SQL CP_RVENCEFACTU
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 q � A � �,�q 4 q 2                       �     
   �  �      )   I                  