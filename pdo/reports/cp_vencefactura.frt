   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                 idproveedor                    empresa                                                       Arial                          $"Vencimientos a Pagar por Proveedor"                                                           Arial                          Arial                          "  Proveedor"                  cp_rvencefactu.facturaproveedor                                                                Arial                          Arial                          "Per�odo :"                     cp_rvencefactu.saldo           cp_rvencefactu.saldo          Arial                          "9,999,999,999.99"             Arial                          "Saldo"                       Arial                          "Comprobante"                  Arial                          "Fecha a Pagar"                Arial                          "Vencimiento"                  $cp_rvencefactu.idproveedor," ",razon                                                           Arial                          Arial                          "Cuota"                       Arial                          "Fecha Compra"                cp_rvencefactu.cuota                                          Arial                          "99"                           cp_rvencefactu.vencimiento                                                                     Arial                          "@D"                           cp_rvencefactu.fec_acobrar                                                                     Arial                          "@D"                           cp_rvencefactu.fecha                                          Arial                          "@D"                           &dtoc(m.dvence)+ " al " +dtoc(m.hvence)                                                         Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          8"Total Proveedor: "+cp_rvencefactu.idproveedor," ",razon                                       Arial                          idmoneda                                                      Arial                          Arial                          	"Moneda"                      " cp_rvencefactu.saldo * Cotizacion                               cp_rvencefactu.saldo          Arial                          "9,999,999,999.99"             Arial                          "Valorizado"                  !cp_rvencefactu.saldo * Cotizacion                                                              Arial                          "999,999,999,999.99"           "Total General "                                              Arial                          !cp_rvencefactu.saldo * Cotizacion                                                              Arial                          "999,999,999,999.99"           Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                sLeft = 16
Top = 94
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
Name = "Dataenvironment"
              sPROCEDURE Init
LOCAL strsql
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
	' ORDER BY a.idproveedor, a.idmoneda, a.vencimiento, a.facturaproveedor'
=sql(strsql,'cp_rvencefactu')
SELECT cp_rvencefactu


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        �j   %   S      �  	   {          �  U  � ��  � G(� DATOS��T�  �� SELECT a.nrocomprob, �+   a.facturaproveedor, a.cuota, a.sucursal, �)   a.idproveedor, a.fecha, a.vencimiento, �%   a.fec_acobrar, a.importe, a.saldo, �[   a.idmoneda, a.pendiente, b.razon, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion�&  FROM cp_forma_pago a, cp_proveedor b �c  WHERE  a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa �    AND a.saldo > 0 CC�� �
�C �=    AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor � �  6CC�� �
�; �5    AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence � �  6�F  ORDER BY a.idproveedor, a.idmoneda, a.vencimiento, a.facturaproveedor�� ��C �  � cp_rvencefactu� �� F� � U  STRSQL DATOS
 DPROVEEDOR DVENCE SQL CP_RVENCEFACTU
  �  � U  SETEO Init,     �� BeforeOpenTables>    ��1 q � �)�q 4 q 2                       7        ^  h      )   �                  