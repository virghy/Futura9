  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      )"Planilla de costos de producci�n Verano"             Arial      empresa             Arial      "Promedio General de Cabezas:"      Arial      m.totalcabeza             Arial      'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)             Arial      	"Periodo"      Arial      Biif(m.tipoganado = 'C', 'Promedio de Cabezas','Terneros Marcados')             Arial      m.cabeza             Arial      Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)      Arial      "Centro Costo:"      Arial      Kiif(m.tipoganado = 'C', 'Produccion Kilo Promedio','Kilo Promedio Destete')             Arial      m.kilo>1      m.kilo             Arial      m.kilo>1      6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "%"      "@J"             Arial      "Total"      "@J"             Arial      "Costo/Cabeza"      "@J"             Arial      "Costo/Kilo"      "@J"             Arial      m.kilo>1      cn_rcostos.cuenta             Arial      cn_rcostos.descripci�n             Arial      cn_rcostos.saldo      "999,999,999,999.99"             Arial      /round(cn_rcostos.saldo / m.cabeza ,m.decimales)      "999,999.999"             Arial      :round((cn_rcostos.saldo / m.cabeza ) / m.kilo,m.decimales)      "999,999.999"             Arial      m.kilo>1      (round(cn_rcostos.saldo * 100  / total,2)      "999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total"      "@J"             Arial      cn_rcostos.saldo      "999,999,999,999.99"             Arial      .round(cn_rcostos.saldo / m.cabeza,m.decimales)      "999,999.999"             Arial      9round((cn_rcostos.saldo / m.cabeza) / m.kilo,m.decimales)      "999,999.999"             Arial      m.kilo>1      100      "999.99"             Arial      dataenvironment      �Top = 66
Left = 20
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
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
     ����    �  �                        ��   %   �      K               �  U    <�  �� � U  TOTAL	 DECIMALES& �  � 7� � � %��� � L��5 � T�� �� �� �L � T�� ���� �	 M(� �� �  �F �@ SELECT     detalle.cuenta, detalle.centro, cuentas.descripci�n, �x �r 	SUM(ROUND((detalle.debe - detalle.haber) / CASE WHEN ?m.tipomoneda = 'L' THEN 1 ELSE cotizacion END, 0)) AS saldo�p �j 	FROM         cn_asientos asientos left join cn_Detalle detalle on asientos.idasiento = detalle.idasiento �� �� 	left join cn_cuentas cuentas on detalle.cuenta = cuentas.cuenta and detalle.IdEmpresa = cuentas.IdEmpresa and detalle.Ejercicio = cuentas.Ejercicio�/ �) 	WHERE asientos.idempresa = ?oApp.Empresa�B �< 	AND ( asientos.fecha BETWEEN ?m.dFecha and  ?m.hFecha) AND �; �5 	(detalle.cuenta BETWEEN ?m.dCuenta and  ?m.hCuenta) �< �6 	AND (detalle.centro = ?m.Centro or ?m.Centro is null)�. �( 	and MONTH(asientos.fecha) in(1,2,11,12)�C �= 	GROUP BY detalle.cuenta, detalle.centro, cuentas.descripci�n� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� �� K(� �� �� U  SETEO TOTAL	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO
  �  � U  SETEO Destroy,     �� InitR     �� BeforeOpenTables�    ��1 � 3 q � A� � � A � a a��	�!���1A �q � 2 q 2                       /         J   �        �  �       )   �                  