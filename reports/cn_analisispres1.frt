  8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Pres      znvl(p1,0)+nvl(p2,0)+nvl(p3,0)+nvl(p4,0)+nvl(p5,0)+nvl(p6,0)+nvl(p7,0)+nvl(p8,0)+nvl(p9,0)+nvl(p10,0)+nvl(p11,0)+nvl(p12,0)      0      Saldo      �nvl(mes1,0)+nvl(mes2,0)+nvl(mes3,0)+nvl(mes4,0)+nvl(mes5,0)+nvl(mes6,0)+nvl(mes7,0)+nvl(mes8,0)+nvl(mes9,0)+nvl(mes10,0)+nvl(mes11,0)+nvl(mes12,0)      0      Arial      Arial      Arial      Arial      Arial      ,"Analisis de Presupuesto: ", cCuentas.Titulo      Arial      	m.empresa             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      6'Moneda: ',iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      "Centro Costo"      Arial      .iif(m.centro='%', 'Todos',centros.Descripci�n)      Arial      "Presupuestado"      Arial      "Ejecutado"      Arial      "%"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      Pres      "9,999,999,999.99"      Arial      DEC >0      Pres      "999,999,999,999"      Arial      dec=0      saldo      "999,999,999,999"             Arial      dec=0      saldo      "9,999,999,999.99"             Arial      DEC >0      iif(pres>0,saldo/pres*100,0)      
"99999.99"      Arial      replicate(' ',nivel*5)+ cuenta             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 62
Left = 100
Width = 520
Height = 200
InitialSelectedAlias = "rcuentas"
DataSource = .NULL.
Name = "Dataenvironment"
     'PROCEDURE Destroy
RELEASE M.dec 
ENDPROC
PROCEDURE Init
PUBLIC m.dec
IF m.tipoMoneda = 'L'
      m.decimales = 0
ELSE
      m.decimales = 0
ENDIF



TEXT TO cmdSQL
	SELECT cuentas.cuenta, IdCuenta=z.Cuenta,factor, titulo 
	FROM cn_cuentas cuentas, cn_analisisdet z, cn_analisis a 
	WHERE a.idanalisis = z.idanalisis 
	AND cuentas.cuenta like LTRIM(RTRIM(reverse(LTRIM(STR(CONVERT(INT,(reverse(z.cuenta))))))))+'%' 
	AND a.idanalisis = ?m.idanalisisPres 
	and a.IdEmpresa=?oApp.Empresa
	and cuentas.IdEmpresa=?oApp.Empresa
	and cuentas.Ejercicio = ?oApp.Ejercicio

ENDTEXT
sql(cmdSQL,'cCuentas')


IF m.tipomoneda = 'L'
     m.decimales = 0
     m.dec = 0
ELSE
     m.decimales = 2
     m.dec = 2
ENDIF

	

IF EMPTY(m.centro)
     m.centro = '%'
ELSE
     = sql('Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro','Centros')
ENDIF

= sql("exec cn_Presupuesto_Mensual ?oApp.empresa, ?oApp.Ejercicio, '','999',?m.dfecha,?m.hfecha, ?m.centro, ?m.TipoMoneda",'Saldos1')
SELECT saldos1
SET FILTER TO nivel <= m.nivel &&AND BETWEEN(LEFT(cuenta,10),m.dcuenta,m.hcuenta)
GOTO TOP
*SUM mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12 TO m.totalGeneral
m.totalGeneral= mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12

SELECT s.* FROM Saldos1 s INNER JOIN cCuentas c;
ON s.Cuenta = c.Cuenta;
where nivel <= m.nivel;
INTO CURSOR Saldos
SELECT Saldos




GOTO TOP
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    k  k                        �P   %   q        ,   �          �  U   	 <��  � U  DEC]	 7��  � %��� � L��, � T�� �� �� �C � T�� �� �� � M(� �? �9 	SELECT cuentas.cuenta, IdCuenta=z.Cuenta,factor, titulo �@ �: 	FROM cn_cuentas cuentas, cn_analisisdet z, cn_analisis a �) �# 	WHERE a.idanalisis = z.idanalisis �g �a 	AND cuentas.cuenta like LTRIM(RTRIM(reverse(LTRIM(STR(CONVERT(INT,(reverse(z.cuenta))))))))+'%' �, �& 	AND a.idanalisis = ?m.idanalisisPres �$ � 	and a.IdEmpresa=?oApp.Empresa�* �$ 	and cuentas.IdEmpresa=?oApp.Empresa�. �( 	and cuentas.Ejercicio = ?oApp.Ejercicio� �  � � ��C � � cCuentas� �� %��� � L��[� T�� �� �� T��  �� �� ��� T�� ���� T��  ���� � %�C�� ����� T�� �� %�� ��m ��C�V Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centros� �� �� ��C�r exec cn_Presupuesto_Mensual ?oApp.empresa, ?oApp.Ejercicio, '','999',?m.dfecha,?m.hfecha, ?m.centro, ?m.TipoMoneda� Saldos1� �� F� � G(�� �� �� #)�; T�� ��	 �
 � � � � � � � � � � ��N o� Saldos1Q� ��� cCuentasQ�  �� � �� �� ���� �� ���� Saldos� F� � #)� U  DEC
 TIPOMONEDA	 DECIMALES CMDSQL SQL CENTRO SALDOS1 NIVEL TOTALGENERAL MES1 MES2 MES3 MES4 MES5 MES6 MES7 MES8 MES9 MES10 MES11 MES12 S CCUENTAS C CUENTA SALDOS
  �  � U  SETEO Destroy,     �� InitA     �� BeforeOpenTables\    ��1 � 2 � A� � � A � ��q�A��a A �C� � � � � A � �A �q Q ��q U 2 q 2                       !         <   �     *       >    )   k                  