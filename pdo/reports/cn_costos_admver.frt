  n   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
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
                    r  S  winspool  Detectando autom�ticamente HP DeskJet 840C/841C/842C/843C en AR  \\AR\HPDeskJe                                 �Detectando autom�ticamente HP    � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         $   �$               $   �$                                     Arial                          *"Gastos Administrativos/Producci�n Verano"                                                     Arial                          empresa                                                       Arial                          "Cuenta"                       Arial                          "Descripci�n"                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          "Total"                        "@J"                                                          Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          "Costo/Cabeza"                 "@J"                                                          Arial                          ;round(cn_rcostos.saldo / m.cabeza * porcentaje,m.decimales)                                      "999,999.999"                                                 Arial                          Oround((cn_rcostos.saldo * 100 / (m.kilo * m.totalcabeza)) / m.totalCostoKilo,2)                  "999.99"                                                      Arial                          vcentrocosto.descripci�n       Arial                          "Centro Costo:"                Arial                          cn_rcostos.cuenta                                             Arial                          'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)                                                        Arial                          	"Periodo"                      Arial                          cn_rcostos.descripci�n                                        Arial                          "%"                            "@J"                                                          Arial                          m.cabeza                                                      Arial                          "Promedio de Cabezas:"         Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          ;round(cn_rcostos.saldo * porcentaje / m.cabeza,m.decimales)                                      "999,999.999"                                                 Arial                          100                            "999.99"                                                      Arial                          "Total"                        "@J"                                                          Arial                          "Prorrateo"                    "@J"                                                          Arial                          0round(cn_rcostos.saldo * porcentaje,m.decimales)                "999,999,999,999.99"                                          Arial                          /round(cn_rcostos.saldo *porcentaje,m.decimales)                 "999,999,999,999.99"                                          Arial                          Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)                        Arial                          round(porcentaje * 100,0)      "999 %"                                                       Arial                          vcentroref.descripci�n                                        Arial                          "Centro de Referencia:"        Arial                          "Costo/Kilo"                   "@J"                                                          Arial                          m.kilo>1                       Cround((cn_rcostos.saldo / m.cabeza *porcentaje)/m.kilo,m.decimales)                              "999,999.999"                                                 Arial                          m.kilo>1                       Cround((cn_rcostos.saldo / m.cabeza *porcentaje)/m.kilo,m.decimales)                              "999,999.999"                                                 Arial                          m.kilo>1                       m.kilo                                                        Arial                          m.kilo>1                       "Producci�n Kilo Promedio:"                                                                    Arial                          m.kilo>1                       m.totalcabeza                                                 Arial                          "Promedio General de Cabezas:"                                  Arial                          %round(cn_rcostos.saldo *100/ total,2)                           "999.99"                                                      Arial                          "%"                            "@J"                                                          Arial                          100                            "999.99"                                                      Arial                          6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')                                         Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                �Top = 54
Left = 90
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
                              xPROCEDURE Init
PUBLIC total, porcentaje, m.totalcostokilo, decimales
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
=sql("Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.CentroRef",'vcentroref')



SUM saldo TO total 
porcentaje = ROUND(m.cabeza / totalcabeza, 2)
m.totalcostokilo = total / (m.kilo * totalcabeza)

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                T���    ;  ;                        ��   %   �      �     �          �  U  � 7�  � �� � � %��� � L��8 � T�� �� �� �O � T�� ���� �	 M(� ��F �@ SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, �x �r 	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo�p �j 	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento �� �� 	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio�/ �) 	WHERE asientos.idempresa = ?oApp.Empresa�B �< 	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND �; �5 	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) �< �6 	AND (detalle.centro = ?m.Centro or ?m.Centro is null)�. �( 	and MONTH(asientos.fecha) in(1,2,11,12)�C �= 	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� ��q ��C�W Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.CentroRef�
 vcentroref� �� K(�  �� �� T� �C��	 �
 �T�� T�� ��  �� �
 �� U  TOTAL
 PORCENTAJE TOTALCOSTOKILO	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO CABEZA TOTALCABEZA KILO
  �  � U  SETEO Init,     �� BeforeOpenTablesk    ��1 QA� � � A � a��	�!���1A �q � ��3 q 2                       <        c  m  #    )   ;                                              �DRIVER=winspool
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
                    r  S  winspool  Detectando autom�ticamente HP DeskJet 840C/841C/842C/843C en AR  \\AR\HPDeskJe                                 �Detectando autom�ticamente HP    � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         $   �$               $   �$                                     Arial                          *"Gastos Administrativos/Producci�n Verano"                                                     Arial                          empresa                                                       Arial                          "Cuenta"                       Arial                          "Descripci�n"                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          "Total"                        "@J"                                                          Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          "Costo/Cabeza"                 "@J"                                                          Arial                          ;round(cn_rcostos.saldo / m.cabeza * porcentaje,m.decimales)                                      "999,999.999"                                                 Arial                          Oround((cn_rcostos.saldo * 100 / (m.kilo * m.totalcabeza)) / m.totalCostoKilo,2)                  "999.99"                                                      Arial                          vcentrocosto.descripci�n       Arial                          "Centro Costo:"                Arial                          cn_rcostos.cuenta                                             Arial                          'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)                                                        Arial                          	"Periodo"                      Arial                          cn_rcostos.descripci�n                                        Arial                          "%"                            "@J"                                                          Arial                          m.cabeza                                                      Arial                          "Promedio de Cabezas:"         Arial                          cn_rcostos.saldo               "999,999,999,999.99"                                          Arial                          ;round(cn_rcostos.saldo * porcentaje / m.cabeza,m.decimales)                                      "999,999.999"                                                 Arial                          100                            "999.99"                                                      Arial                          "Total"                        "@J"                                                          Arial                          "Prorrateo"                    "@J"                                                          Arial                          0round(cn_rcostos.saldo * porcentaje,m.decimales)                "999,999,999,999.99"                                          Arial                          /round(cn_rcostos.saldo *porcentaje,m.decimales)                 "999,999,999,999.99"                                          Arial                          Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)                        Arial                          round(porcentaje * 100,0)      "999 %"                                                       Arial                          vcentroref.descripci�n                                        Arial                          "Centro de Referencia:"        Arial                          "Costo/Kilo"                   "@J"                                                          Arial                          m.kilo>1                       Cround((cn_rcostos.saldo / m.cabeza *porcentaje)/m.kilo,m.decimales)                              "999,999.999"                                                 Arial                          m.kilo>1                       Cround((cn_rcostos.saldo / m.cabeza *porcentaje)/m.kilo,m.decimales)                              "999,999.999"                                                 Arial                          m.kilo>1                       m.kilo                                                        Arial                          m.kilo>1                       "Producci�n Kilo Promedio:"                                                                    Arial                          m.kilo>1                       m.totalcabeza                                                 Arial                          "Promedio General de Cabezas:"                                  Arial                          %round(cn_rcostos.saldo *100/ total,2)                           "999.99"                                                      Arial                          "%"                            "@J"                                                          Arial                          100                            "999.99"                                                      Arial                          6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')                                         Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                �Top = 54
Left = 90
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
                              xPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
PUBLIC total, porcentaje, m.totalcostokilo, decimales
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
=sql("Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.CentroRef",'vcentroref')



SUM saldo TO total 
porcentaje = ROUND(m.cabeza / totalcabeza, 2)
m.totalcostokilo = total / (m.kilo * totalcabeza)

ENDPROC
                T���    ;  ;                        ��   %   �      �     �          �  U  
  �  � U  SETEO� 7�  � �� � � %��� � L��8 � T�� �� �� �O � T�� ���� �	 M(� ��F �@ SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, �x �r 	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo�p �j 	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento �� �� 	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio�/ �) 	WHERE asientos.idempresa = ?oApp.Empresa�B �< 	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND �; �5 	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) �< �6 	AND (detalle.centro = ?m.Centro or ?m.Centro is null)�. �( 	and MONTH(asientos.fecha) in(1,2,11,12)�C �= 	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� ��q ��C�W Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.CentroRef�
 vcentroref� �� K(�  �� �� T� �C��	 �
 �T�� T�� ��  �� �
 �� U  TOTAL
 PORCENTAJE TOTALCOSTOKILO	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO CABEZA TOTALCABEZA KILO BeforeOpenTables,     �� InitA     ��1 q 3 QA� � � A � a��	�!���1A �q � ��2                       &         A   m      )   ;                                        