  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                       \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$              Arial      idproveedor      Arial      Arial      Arial      Arial      Arial      "Vencimientos a Pagar"             Arial      	"empresa"             Arial      &dtoc(m.dvence)+ " al " +dtoc(m.hvence)             Arial      "Per�odo :"      Arial      "  Proveedor"      Arial      "Vencimiento"      Arial      "Fecha a Pagar"      Arial      "Fecha Compra
"      Arial      "Comprobante"      Arial      	"Cuota
"      Arial      
"Moneda
"      Arial      	"Saldo
"      Arial      "Valorizado
"      Arial      $cp_rvencefactu.idproveedor," ",razon             Arial      cp_rvencefactu.vencimiento             Arial      cp_rvencefactu.fec_acobrar             Arial      cp_rvencefactu.fecha             Arial      cp_rvencefactu.facturaproveedor             Arial      cp_rvencefactu.cuota      "99"             Arial      idmoneda             Arial       cp_rvencefactu.saldo      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      " cp_rvencefactu.saldo * Cotizacion      "9,999,999,999.99"       cp_rvencefactu.saldo      Arial      8"Total Proveedor: "+cp_rvencefactu.idproveedor," ",razon             Arial      cp_rvencefactu.saldo      "999,999,999,999.99"             Arial      !cp_rvencefactu.saldo * Cotizacion      "999,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total General "             Arial      !cp_rvencefactu.saldo * Cotizacion      "999,999,999,999.99"             Arial      dataenvironment      �Top = 94
Left = 16
Width = 555
Height = 285
InitialSelectedAlias = "cp_rvencefactu"
DataSource = .NULL.
Name = "Dataenvironment"
     =PROCEDURE Init
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
	' ORDER BY a.idmoneda, a.facturaproveedor, '+;
	'  a.vencimiento '
=sql(strsql,'cp_rvencefactu')
SELECT cp_rvencefactu


ENDPROC
     ����    �  �                        Pu   %   6      R     D          �  U  � ��  � G(� DATOS��T�  �� SELECT a.nrocomprob, �+   a.facturaproveedor, a.cuota, a.sucursal, �)   a.idproveedor, a.fecha, a.vencimiento, �%   a.fec_acobrar, a.importe, a.saldo, �[   a.idmoneda, a.pendiente, b.razon, DBO.fnGetCotizacion(a.IdMoneda,GetDate()) as Cotizacion�&  FROM cp_forma_pago a, cp_proveedor b �c  WHERE  a.idproveedor = b.idproveedor and a.idempresa= b.IdEmpresa and b.IdEmpresa = ?oApp.Empresa �    AND a.saldo > 0 CC�� �
�C �=    AND a.idproveedor BETWEEN ?m.dproveedor AND ?m.hproveedor � �  6CC�� �
�; �5    AND a.vencimiento BETWEEN ?m.dvence AND ?m.hvence � �  6�*  ORDER BY a.idmoneda, a.facturaproveedor, �   a.vencimiento �� ��C �  � cp_rvencefactu� �� F� � U  STRSQL DATOS
 DPROVEEDOR DVENCE SQL CP_RVENCEFACTU Init,     ��1 q � )�q 3                       2      )   �                  