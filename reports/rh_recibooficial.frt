   *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Brother Laser Leg Type1 Class Driver
OUTPUT=192.168.0.109
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
DUPLEX=1
YRESOLUTION=300
COLLATE=0
      W  8  winspool  Brother Laser Leg Type1 Class Driver  192.168.0.109                       Brother Laser Leg Type1 Class   � �C�   �4d   ,  ,   A4                                                         
  ����GIS4            DINU" d 	.�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         SMTJ     �{ 3 9 4 3 1 9 A 6 - 7 F F 5 - 4 B 4 1 - A B 7 A - 6 6 7 B F 2 E F A E 5 3 }   InputBin AutoSelect RESDLL UniresDLL MediaType Plain PaperSize LETTER Orientation PORTRAIT Duplex NONE ColorMode Mono Resolution Option2 PagePrintSetting General Collate OFF JobNUpAllDocumentsContiguously 1 JobPresentationDirection RightBottom PageBrPoster Normal PageReverseImage Off PageBrWatermark None JobTonerSaveMode Off JobReprint Off JobBrSleepTime PrinterDefault                                                          V4DM                         Courier New      rsueldos.idempleado      primer      0      0      numero      1      0      par      1      0      salto      3iif( par = int( numero/2) and  par =  numero/2,1,0)      0      Arial      Courier New      Arial      Arial      Arial      0rsueldos.nroliquidacion,"-", rsueldos.idliquidet      Arial      "Liquidaci�n de Salarios"             Arial      "Empleador:" ,cempresa.Razon      Arial      cConfig.Nro      Arial      "Nro. Patronal"      Arial      rsueldos.idempleado             Arial      &rsueldos.apellido," " ,rsueldos.nombre             Arial      #"Apellido y Nombre del Trabajador:"      Arial      "Periodo de Pago:",descripcion      Arial      c'Mes de '+letrames(rsueldos.mes)+iif(rsueldos.a�o>=2000,' del ',' de ')+alltrim(str( rsueldos.a�o))             Arial      rsueldos.sueldo_bas      "999,999,999"             Arial      "Sueldo/Jornal"      Arial      
"Concepto"      Arial      
"Unidades"      Arial      	"Haberes"      Arial      "Deducciones"      Arial      "Documento"      Arial      rsueldos.concepto             Arial      rsueldos.documento      "@Z"             Arial      rsueldos.cantidad      "99.99"             Arial      )iif(rsueldos.idtipo='I',rsueldos.monto,0)      "@Z 999,999,999"             Arial      )iif(rsueldos.idtipo='I',0,rsueldos.monto)      "@Z 999,999,999"             Arial      rsueldos.obs             Arial      rsueldos.th      "999,999,999"             Arial      rsueldos.td      "999,999,999"             Arial      ,"Recib� conforme de la empresa, original e "      Arial      *"importe neto de la presente liquidaci�n."      Arial      "Firma del Empleado:"      Arial      rsueldos.tn      "99,999,999,999"             Arial      "Saldo a cobrar"      Arial      dataenvironment      �Top = 53
Left = 222
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
DataSource = .NULL.
Name = "Dataenvironment"
     EPROCEDURE Init

=sql("Select convert(char(20),dbo.LeerConstante(?oApp.Empresa,'RH_NROPATRONAL')) as NRO", 'cConfig')
=sql("Select Raz�n as Razon from empresa where IdEmpresa=?oApp.Empresa", 'cEmpresa')


DO seteo
IF EMPTY(m.centropago)
	m.centropago= null
ENDIF

TEXT TO cmdSQL noshow
	SELECT rh_liquidacion.nroliquidacion, rh_liquidacion.descripcion, rh_liquidacion.fecha, 
	rh_liquidacion.mes, rh_liquidacion.a�o, rh_liquidacion.confirmado, rh_liquidacion.idfrecuencia, 
	rh_liquidet.idempleado, rh_liquidet.thi, rh_liquidet.thn, rh_liquidet.th, rh_liquidet.td, 
	rh_liquidet.tn, rh_empleado.nombre, rh_empleado.apellido, 
	rh_empleado.ocupacion, rh_empleado.sueldo_bas, rh_empleado.centro_pag, rh_liquida_conceptos.idtipo, 
	rh_liquida_conceptos.monto, rh_liquida_conceptos.cantidad, rh_liquida_conceptos.documento, rh_liquida_conceptos.nro, 
	rh_conceptos.concepto, rh_liquida_conceptos.idconcepto, rh_liquidet.idliquidet, rh_liquidet.obs 
	FROM rh_empleado, rh_liquida_conceptos, rh_conceptos, 
	rh_liquidacion INNER JOIN rh_liquidet ON rh_liquidacion.idliquidacion = rh_liquidet.idliquidacion 
	WHERE rh_liquidet.idempleado = rh_empleado.idempleado AND rh_liquidet.IdEmpresa = rh_empleado.IdEmpresa 
	AND rh_liquidet.idliquidet = rh_liquida_conceptos.idliquidet AND rh_liquida_conceptos.idconcepto = rh_conceptos.idconcepto AND rh_liquida_conceptos.IdEmpresa = rh_conceptos.IdEmpresa 
	AND (rh_liquidacion.idempresa = ?oapp.empresa
	AND (rh_empleado.centro_pag = ?m.centropago or ?m.CentroPago is null) AND rh_liquidacion.fecha = ?m.fecha AND 
	rh_liquidet.idempleado BETWEEN  ?m.dlegajo and ?m.hlegajo) 
	ORDER BY rh_liquidet.idempleado, rh_empleado.apellido, rh_empleado.nombre, rh_liquida_conceptos.idconcepto 
ENDTEXT

sql(cmdSQL,"rSueldos")
SELECT rSueldos


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     B���    )  )                        �P   %   n      �     �          �  U  �h ��C�Q Select convert(char(20),dbo.LeerConstante(?oApp.Empresa,'RH_NROPATRONAL')) as NRO� cConfig�  ��X ��C�@ Select Raz�n as Razon from empresa where IdEmpresa=?oApp.Empresa� cEmpresa�  �� � � %�C�� ���� � T�� ���� �	 M(� ��_ �Y 	SELECT rh_liquidacion.nroliquidacion, rh_liquidacion.descripcion, rh_liquidacion.fecha, �g �a 	rh_liquidacion.mes, rh_liquidacion.a�o, rh_liquidacion.confirmado, rh_liquidacion.idfrecuencia, �a �[ 	rh_liquidet.idempleado, rh_liquidet.thi, rh_liquidet.thn, rh_liquidet.th, rh_liquidet.td, �A �; 	rh_liquidet.tn, rh_empleado.nombre, rh_empleado.apellido, �k �e 	rh_empleado.ocupacion, rh_empleado.sueldo_bas, rh_empleado.centro_pag, rh_liquida_conceptos.idtipo, �| �v 	rh_liquida_conceptos.monto, rh_liquida_conceptos.cantidad, rh_liquida_conceptos.documento, rh_liquida_conceptos.nro, �g �a 	rh_conceptos.concepto, rh_liquida_conceptos.idconcepto, rh_liquidet.idliquidet, rh_liquidet.obs �= �7 	FROM rh_empleado, rh_liquida_conceptos, rh_conceptos, �i �c 	rh_liquidacion INNER JOIN rh_liquidet ON rh_liquidacion.idliquidacion = rh_liquidet.idliquidacion �o �i 	WHERE rh_liquidet.idempleado = rh_empleado.idempleado AND rh_liquidet.IdEmpresa = rh_empleado.IdEmpresa �� �� 	AND rh_liquidet.idliquidet = rh_liquida_conceptos.idliquidet AND rh_liquida_conceptos.idconcepto = rh_conceptos.idconcepto AND rh_liquida_conceptos.IdEmpresa = rh_conceptos.IdEmpresa �4 �. 	AND (rh_liquidacion.idempresa = ?oapp.empresa�u �o 	AND (rh_empleado.centro_pag = ?m.centropago or ?m.CentroPago is null) AND rh_liquidacion.fecha = ?m.fecha AND �B �< 	rh_liquidet.idempleado BETWEEN  ?m.dlegajo and ?m.hlegajo) �r �l 	ORDER BY rh_liquidet.idempleado, rh_empleado.apellido, rh_empleado.nombre, rh_liquida_conceptos.idconcepto � � ��C � � rSueldos�  �� F� � U  SQL SETEO
 CENTROPAGO CMDSQL RSUELDOS
  �  � U  SETEO Init,     �� BeforeOpenTablesY    ��1 ��s � A � �q��q����AQ!!A �q 4 q 2                       	        0  :  #    )   )                  