  6   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
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
TTOPTION=3
COLLATE=1
                 T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �Detectando autom�ticamente HP    � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         $   �$               $   �$   k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                             Arial                          r  S  winspool  Detectando autom�ticamente HP DeskJet 840C/841C/842C/843C en AR  \\AR\HPDeskJe                                  �DRIVER=winspool
DEVICE=Detectando autom�ticamente HP DeskJet 840C/841C/842C/843C en AR
OUTPUT=\\AR\HPDeskJe
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=3
COLLATE=1
                    )"Planilla de costos de producci�n Verano"                                                      Arial                          empresa                                                       Arial                          "Cuenta"                       Arial                          "Descripci�n"                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          "Total"                        "@J"                                                          Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          "Costo/Cabeza"                 "@J"                                                          Arial                          /round(cn_rcostos.saldo / m.cabeza ,m.decimales)                 "999,999.999"                                                 Arial                          "Costo/Kilo"                   "@J"                                                          Arial                          m.kilo>1                       :round((cn_rcostos.saldo / m.cabeza ) / m.kilo,m.decimales)      "999,999.999"                                                 Arial                          m.kilo>1                       (round(cn_rcostos.saldo * 100  / total,2)                        "999.99"                                                      Arial                          "Centro Costo:"                Arial                          cn_rcostos.cuenta                                             Arial                          'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)                                                        Arial                          	"Periodo"                      Arial                          cn_rcostos.descripci�n                                        Arial                          "%"                            "@J"                                                          Arial                          m.kilo                                                        Arial                          m.kilo>1                       m.cabeza                                                      Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          .round(cn_rcostos.saldo / m.cabeza,m.decimales)                  "999,999.999"                                                 Arial                          9round((cn_rcostos.saldo / m.cabeza) / m.kilo,m.decimales)       "999,999.999"                                                 Arial                          m.kilo>1                       100                            "999.99"                                                      Arial                          "Total"                        "@J"                                                          Arial                          Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)                        Arial                          m.totalcabeza                                                 Arial                          "Promedio General de Cabezas:"                                  Arial                          Biif(m.tipoganado = 'C', 'Promedio de Cabezas','Terneros Marcados')                                                              Arial                          Kiif(m.tipoganado = 'C', 'Produccion Kilo Promedio','Kilo Promedio Destete')                                                     Arial                          m.kilo>1                       6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')                                         Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                �Top = 66
Left = 20
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
                              �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
DO SETEO
PUBLIC total, decimales
IF m.tipomoneda = 'L'
     m.decimales = 0
ELSE
     m.decimales = 3
ENDIF
TEXT TO cmdSQL noshow

SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, 
	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo
	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento 
	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio
	WHERE asientos.idempresa = ?oApp.Empresa
	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND 
	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) 
	AND (detalle.centro = ?m.Centro or ?m.Centro is null)
	and MONTH(asientos.fecha) in(1,2,11,12)
	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n
ENDTEXT

=sql(cmdSQL,'cn_rcostos')
SELECT cn_rcostos
=sql("Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro",'vcentrocosto')
SUM Saldo TO Total
ENDPROC
PROCEDURE Destroy
RELEASE total, m.decimales

ENDPROC
                                ����    �  �                        ��   %   �      K               �  U  
  �  � U  SETEO& �  � 7� � � %��� � L��5 � T�� �� �� �L � T�� ���� �	 M(� �� �  �F �@ SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, �x �r 	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo�p �j 	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento �� �� 	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio�/ �) 	WHERE asientos.idempresa = ?oApp.Empresa�B �< 	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND �; �5 	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) �< �6 	AND (detalle.centro = ?m.Centro or ?m.Centro is null)�. �( 	and MONTH(asientos.fecha) in(1,2,11,12)�C �= 	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� �� K(� �� �� U  SETEO TOTAL	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO  <�  �� � U  TOTAL	 DECIMALES BeforeOpenTables,     �� InitA     �� Destroy�    ��1 q 3 q � A� � � A � a a��	�!���1A �q � 2 � 2                       &         A   ~        �  �       )   �                                                 �DRIVER=winspool
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
TTOPTION=3
COLLATE=1
                 T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                             Arial                          )"Planilla de costos de producci�n Verano"                                                      Arial                          empresa                                                       Arial                          "Cuenta"                       Arial                          "Descripci�n"                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          "Total"                        "@J"                                                          Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          "Costo/Cabeza"                 "@J"                                                          Arial                          /round(cn_rcostos.saldo / m.cabeza ,m.decimales)                 "999,999.999"                                                 Arial                          "Costo/Kilo"                   "@J"                                                          Arial                          m.kilo>1                       :round((cn_rcostos.saldo / m.cabeza ) / m.kilo,m.decimales)      "999,999.999"                                                 Arial                          m.kilo>1                       (round(cn_rcostos.saldo * 100  / total,2)                        "999.99"                                                      Arial                          "Centro Costo:"                Arial                          cn_rcostos.cuenta                                             Arial                          'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)                                                        Arial                          	"Periodo"                      Arial                          cn_rcostos.descripci�n                                        Arial                          "%"                            "@J"                                                          Arial                          m.kilo                                                        Arial                          m.kilo>1                       m.cabeza                                                      Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          .round(cn_rcostos.saldo / m.cabeza,m.decimales)                  "999,999.999"                                                 Arial                          9round((cn_rcostos.saldo / m.cabeza) / m.kilo,m.decimales)       "999,999.999"                                                 Arial                          m.kilo>1                       100                            "999.99"                                                      Arial                          "Total"                        "@J"                                                          Arial                          Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)                        Arial                          m.totalcabeza                                                 Arial                          "Promedio General de Cabezas:"                                  Arial                          Biif(m.tipoganado = 'C', 'Promedio de Cabezas','Terneros Marcados')                                                              Arial                          Kiif(m.tipoganado = 'C', 'Produccion Kilo Promedio','Kilo Promedio Destete')                                                     Arial                          m.kilo>1                       6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')                                         Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                �Top = 66
Left = 20
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
                              �PROCEDURE Destroy
RELEASE total, m.decimales

ENDPROC
PROCEDURE Init
DO SETEO
PUBLIC total, decimales
IF m.tipomoneda = 'L'
     m.decimales = 0
ELSE
     m.decimales = 3
ENDIF
TEXT TO cmdSQL noshow

SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, 
	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo
	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento 
	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio
	WHERE asientos.idempresa = ?oApp.Empresa
	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND 
	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) 
	AND (detalle.centro = ?m.Centro or ?m.Centro is null)
	and MONTH(asientos.fecha) in(1,2,11,12)
	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n
ENDTEXT

=sql(cmdSQL,'cn_rcostos')
SELECT cn_rcostos
=sql("Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro",'vcentrocosto')
SUM Saldo TO Total
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                ����    �  �                        ��   %   �      K               �  U    <�  �� � U  TOTAL	 DECIMALES& �  � 7� � � %��� � L��5 � T�� �� �� �L � T�� ���� �	 M(� �� �  �F �@ SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, �x �r 	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo�p �j 	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento �� �� 	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio�/ �) 	WHERE asientos.idempresa = ?oApp.Empresa�B �< 	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND �; �5 	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) �< �6 	AND (detalle.centro = ?m.Centro or ?m.Centro is null)�. �( 	and MONTH(asientos.fecha) in(1,2,11,12)�C �= 	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� �� K(� �� �� U  SETEO TOTAL	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO
  �  � U  SETEO Destroy,     �� InitR     �� BeforeOpenTables�    ��1 � 3 q � A� � � A � a a��	�!���1A �q � 2 q 2                       /         J   �        �  �       )   �                                           