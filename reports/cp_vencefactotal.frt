  1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                       �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$         Arial      cp_vencefactotal.idmoneda      Arial      Arial      Arial      Arial      Arial      empresa             Arial      ""Vencimientos a Pagar por Monedas"             Arial      m.dvence," al " ,m.hvence             Arial      "Per�odo :"      Arial      
"  Moneda"      Arial      "Proveedor"      Arial      "Importe
"      Arial      	"Saldo
"      Arial      "Valorizado
"      Arial      
"Comprob."      Arial      "Vence"      Arial      	"Cuota
"      Arial      "Moneda  : " +idmoneda             Arial      idproveedor +" "+ razon             Arial      !cp_vencefactotal.facturaproveedor             Arial      cp_vencefactotal.vencimiento             Arial      cp_vencefactotal.cuota             Arial      cp_vencefactotal.importe      "999,999,999,999.99"             Arial      cp_vencefactotal.saldo      "999,999,999,999.99"             Arial      "cp_vencefactotal.saldo *cotizacion      "999,999,999,999.99"             Arial      "Total Moneda : "+idmoneda             Arial      cp_vencefactotal.importe      "999,999,999,999.99"             Arial      cp_vencefactotal.saldo      "999,999,999,999.99"             Arial      #cp_vencefactotal.saldo * cotizacion      "999,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total General "             Arial      #cp_vencefactotal.saldo * cotizacion      "999,999,999,999.99"             Arial      dataenvironment      vLeft = 37
Top = 150
Width = 552
Height = 288
InitialSelectedAlias = "cp_vencefactotal"
Name = "Dataenvironment"
     APROCEDURE Init
LOCAL strsql
SET DATABASE TO DATOS 

	strsql ='SELECT a.nrocomprob, '+;
	'  a.facturaproveedor, a.cuota, a.sucursal, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion, '+;
	'  a.idproveedor, a.fecha, a.vencimiento, '+;
	'  a.fec_acobrar, a.importe, a.saldo, '+;
	'  a.idmoneda, a.pendiente, b.razon '+;
	' FROM cp_forma_pago a inner join cp_proveedor b '+; 
	' on a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa ' +;
	'   AND a.saldo > 0 '+;
	IIF(!EMPTY(m.dproveedor),'   AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor ',' ')+;
	IIF(!EMPTY(m.dvence),'   AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence ',' ')+;
	' ORDER BY a.idmoneda, a.vencimiento, a.facturaproveedor'
=sql(strsql,'cp_vencefactotal')
SELECT cp_vencefactotal


ENDPROC
     ����    �  �                        5�   %   <      X     J          �  U  � ��  � G(� DATOS��T�  �� SELECT a.nrocomprob, �d   a.facturaproveedor, a.cuota, a.sucursal, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion, �)   a.idproveedor, a.fecha, a.vencimiento, �%   a.fec_acobrar, a.importe, a.saldo, �#   a.idmoneda, a.pendiente, b.razon �0  FROM cp_forma_pago a inner join cp_proveedor b �_  on a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa �    AND a.saldo > 0 CC�� �
�C �=    AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor � �  6CC�� �
�; �5    AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence � �  6�7  ORDER BY a.idmoneda, a.vencimiento, a.facturaproveedor��! ��C �  � cp_vencefactotal� �� F� � U  STRSQL DATOS
 DPROVEEDOR DVENCE SQL CP_VENCEFACTOTAL Init,     ��1 q � ,)q 3                       6      )   �                  