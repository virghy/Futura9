  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      kilo1      m.kilo/m.cabeza      m.kilo/m.cabeza      Arial      Arial      Arial      Arial      Arial      #"Gastos Administrativos/Producci�n"             Arial      empresa             Arial      6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      m.totalcabeza      "@B 99,999,999"      Arial      "Promedio General de Cabezas:"      Arial      'dtoc(m.dfecha) + " - " + dtoc(m.hfecha)             Arial      	"Periodo"      Arial      m.cabeza      "@B 99,999,999"      Arial      "Promedio de Cabezas:"      Arial      vcentrocosto.descripci�n             Arial      "Centro Costo:"      Arial      m.kilo1      "@B 99,999,999"      Arial      m.kilo>1      "Producci�n Kilo Promedio:"             Arial      m.kilo>1      vcentroref.descripci�n      Arial      m.kilo      "@B 99,999,999"      Arial      "Centro de Referencia:"      Arial      "Total Kg. producidos:"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "%"      "@J"             Arial      "Costo/Kilo"      "@J"             Arial      m.kilo>1      "%"      "@J"             Arial      "Total"      "@J"             Arial      "Prorrateo"      "@J"             Arial      "Costo/Cabeza"      "@J"             Arial      round(porcentaje * 100,0)      "999 %"             Arial      Iiif(empty(m.centro),"Todos",m.centro + " - " +  vcentrocosto.descripci�n)      Arial      Dround((cn_rcostos.saldo / m.cabeza *porcentaje)/m.kilo1,m.decimales)      "999,999.999"      Arial      m.kilo>1      cn_rcostos.cuenta             Arial      cn_rcostos.descripci�n             Arial      cn_rcostos.saldo      "99,999,999,999.99"      Arial      0round(cn_rcostos.saldo * porcentaje,m.decimales)      "99,999,999,999.99"      Arial      ;round(cn_rcostos.saldo / m.cabeza * porcentaje,m.decimales)      "99,999,999.999"      Arial      %round(cn_rcostos.saldo *100/ total,2)      "999.99"             Arial      Pround((cn_rcostos.saldo * 100 / (m.kilo1 * m.totalcabeza)) / m.totalCostoKilo,2)      "999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Dround((cn_rcostos.saldo / m.cabeza *porcentaje)/m.kilo1,m.decimales)      "999,999.999"      Arial      m.kilo>1      "Total"      "@J"             Arial      cn_rcostos.saldo      "99,999,999,999.99"      Arial      /round(cn_rcostos.saldo *porcentaje,m.decimales)      "99,999,999,999.99"      Arial      ;round(cn_rcostos.saldo * porcentaje / m.cabeza,m.decimales)      "99,999,999.999"      Arial      100      "999.99"             Arial      100      "999.99"             Arial      dataenvironment      �Top = 54
Left = 90
Width = 792
Height = 419
InitialSelectedAlias = "cn_rcostos"
DataSource = .NULL.
Name = "Dataenvironment"
     PPROCEDURE Destroy
RELEASE  total, porcentaje, m.totalcostokilo, decimales
ENDPROC
PROCEDURE Init
PUBLIC total, porcentaje, m.totalcostokilo, decimales
SET REPORTBEHAVIOR 80
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
=sql("Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.CentroRef",'vcentroref')



SUM saldo TO total 
porcentaje = ROUND(m.cabeza / totalcabeza, 2)
m.totalcostokilo = total / m.kilo
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���    �  �                        ��   %   -      �     f          �  U    <�  � �� � � U  TOTAL
 PORCENTAJE TOTALCOSTOKILO	 DECIMALES) 7�  � �� � �
 G���P�� %��� � L��B � T�� �� �� �Y � T�� ���� �	 M(� �� �
 	SELECT * �o �i 	FROM dbo.cn_saldoCuenta(?oApp.Empresa,?m.dcuenta,?m.hcuenta,?m.centro,?m.dfecha,?m.hFecha,?m.TipoMoneda)� � ��C � �
 cn_rcostos� �� F� �p ��C�T Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.Centro� vcentrocosto� ��q ��C�W Select Descripci�n from centros where IdEmpresa=?oApp.Empresa and Centro = ?m.CentroRef�
 vcentroref� �� K(�  �� �� T� �C��	 �
 �T�� T�� ��  �� �� U  TOTAL
 PORCENTAJE TOTALCOSTOKILO	 DECIMALES
 TIPOMONEDA CMDSQL SQL
 CN_RCOSTOS SALDO CABEZA TOTALCABEZA KILO
  �  � U  SETEO Destroy,     �� Initv     �� BeforeOpenTables    ��1 Q2 Q� A� � � A � �A �q � �Q2 q 2                       J         e           ;  E      )   �                  