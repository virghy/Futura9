  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      kilo1      m.kilo/m.cabeza      m.kilo/m.cabeza      Arial      Arial      Arial      Arial      Arial      ""Planilla de costos de producci�n"             Arial      empresa             Arial      "Promedio General de Cabezas:"      Arial      m.totalcabeza             Arial      'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)             Arial      	"Periodo"      Arial      Biif(m.tipoganado = 'C', 'Promedio de Cabezas','Terneros Marcados')             Arial      m.cabeza             Arial      Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)             Arial      "Centro Costo:"      Arial      Kiif(m.tipoganado = 'C', 'Produccion Kilo Promedio','Kilo Promedio Destete')             Arial      m.kilo>1      m.kilo1      
"@B 99999"      Arial      m.kilo>1      6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      "Total Kg. producidos:"      Arial      m.kilo      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "%"      "@J"             Arial      "Total"      "@J"             Arial      "Costo/Cabeza"      "@J"             Arial      "Costo/Kilo"      "@J"             Arial      m.kilo>1      cn_rcostos.cuenta             Arial      cn_rcostos.descripci�n      Arial      cn_rcostos.saldo      "999,999,999,999.99"             Arial      /round(cn_rcostos.saldo / m.cabeza ,m.decimales)      "999,999.999"             Arial      ;round((cn_rcostos.saldo / m.cabeza ) / m.kilo1,m.decimales)      "999,999.999"      Arial      m.kilo>1      (round(cn_rcostos.saldo * 100  / total,2)      "999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total"      "@J"             Arial      cn_rcostos.saldo      "999,999,999,999.99"             Arial      .round(cn_rcostos.saldo / m.cabeza,m.decimales)      "999,999.999"      Arial      :round((cn_rcostos.saldo / m.cabeza) / m.kilo1,m.decimales)      "999,999.999"      Arial      m.kilo>1      100      "999.99"             Arial      dataenvironment      �Top = 66
Left = 20
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
DO SETEO
PUBLIC total, decimales
IF m.tipomoneda = 'L'
     m.decimales = 0
ELSE
     m.decimales = 3
ENDIF
TEXT TO cmdSQL noshow
	SELECT * 
	FROM dbo.cn_saldoCuenta(?oApp.Empresa,?m.dcuenta,?m.hcuenta,?m.centro,?m.dfecha,?m.hFecha,?m.TipoMoneda)
ENDTEXT

=sql(cmdSQL,'cn_rcostos')
SELECT cn_rcostos
=sql("Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro",'vcentrocosto')
SUM Saldo TO Total

ENDPROC
PROCEDURE Destroy
RELEASE  total, decimales
ENDPROC
     ����    �  �                        U�   %         \     6          �  U  ~ �  � 7� � � %��� � L��5 � T�� �� �� �L � T�� ���� �	 M(� �� �
 	SELECT * �o �i 	FROM dbo.cn_saldoCuenta(?oApp.Empresa,?m.dcuenta,?m.hcuenta,?m.centro,?m.dfecha,?m.hFecha,?m.TipoMoneda)� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� �� K(� �� �� U  SETEO TOTAL	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO  <�  � � U  TOTAL	 DECIMALES Init,     �� Destroy�    ��1 q � A� � � A � �A �q � 3 � 1                       �        �        )   �                  