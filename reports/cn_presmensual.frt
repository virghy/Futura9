  P                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      	inmediato      "cuenta"+alltrim(str(m.nivel))      0      suma1      �saldos.mes1+ saldos.mes2+ saldos.mes3+ saldos.mes4+ saldos.mes5+ saldos.mes6+ saldos.mes7+ saldos.mes8+ saldos.mes9+ saldos.mes10+ saldos.mes11+ saldos.mes12      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      9"Cuadro Demostrativo de Ejecucion Presupuestaria Mensual"      Arial      	m.empresa             Arial      )"Rango: " + m.dcuenta + ' al ' +m.hcuenta             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      Aiif(m.centro='%',"Todos",m.centro + " - " +  centros.descripci�n)      Arial      6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      "Centro Costo:"      Arial      ,"Nivel de Cuentas: " + alltrim(str(m.nivel))             Arial      "Cuenta"      Arial      "Enero"             Arial      	"Febrero"             Arial      "Marzo"             Arial      "Abril"             Arial      "Mayo"             Arial      "Junio"             Arial      "Julio"             Arial      "Agosto"             Arial      "Setiembre"             Arial      	"Octubre"             Arial      "Noviembre"             Arial      "Diciembre"             Arial      "Total"             Arial      "%"             Arial      "Presupuesto
"      Arial      	1 <= m.ni      
nivel=m.ni      replicate(' ',nivel*3)+ cuenta      Arial      between(nivel,1, m.nivel )      "E"      Arial      nivel <= m.ni      saldos.mes1      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes2      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes3      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes4      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes5      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes6      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes7      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes8      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes9      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes10      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes11      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes12      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      Rmes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12      "999,999,999,999"      Arial      between(nivel,1, m.nivel )      "P"      Arial      nivel <= m.ni      p1      "9,999,999,999"      Arial      nivel <= m.ni      p2      "9,999,999,999"      Arial      nivel <= m.ni      p3      "9,999,999,999"      Arial      nivel<= m.ni      p4      "9,999,999,999"      Arial      nivel<= m.ni      p5      "9,999,999,999"      Arial      nivel<= m.ni      p6      "9,999,999,999"      Arial      nivel<= m.ni      p7      "9,999,999,999"      Arial      nivel<= m.ni      p8      "9,999,999,999"      Arial      nivel<= m.ni      p9      "9,999,999,999"      Arial      nivel<= m.ni      p10      "9,999,999,999"      Arial      nivel<= m.ni      p11      "9,999,999,999"      Arial      nivel<= m.ni      p12      "9,999,999,999"      Arial      nivel<= m.ni      &p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12      "999,999,999,999"      Arial      nivel<= m.ni      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      `Top = 77
Left = -17
Width = 792
Height = 419
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init

IF m.tipoMoneda = 'L'
      m.decimales = 0
ELSE
      m.decimales = 0
ENDIF



*!*   set filter to oapp.empresa=idempresa


IF m.tipomoneda = 'L'
     m.decimales = 0
     m.dec = 0
ELSE
     m.decimales = 2
     m.dec = 2
ENDIF
IF EMPTY(m.hcuenta)
	m.hcuenta='99999999'
ENDIF
	

IF EMPTY(m.centro)
     m.centro = '%'
ELSE
     = sql('Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro','Centros')
ENDIF

= sql('exec cn_Presupuesto_Mensual ?oApp.empresa, ?oApp.Ejercicio, ?m.dCuenta,?m.hCuenta,?m.dfecha,?m.hfecha, ?m.centro, ?m.TipoMoneda','Saldos')
SELECT saldos
SET FILTER TO nivel <= m.nivel &&AND BETWEEN(LEFT(cuenta,10),m.dcuenta,m.hcuenta)
GOTO TOP
*SUM mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12 TO m.totalGeneral
m.totalGeneral= mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12


GOTO TOP


ENDPROC
     
���    �  �                         9   %   4      �     \          �  U  
  �  � U  SETEOU %���  � L��# � T�� �� �� �: � T�� �� �� � %���  � L��p � T�� �� �� T�� �� �� �� � T�� ���� T�� ���� � %�C�� ���� � T�� �� 99999999�� � %�C�� ���� � T�� �� %�� �\�m ��C�V Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centros� �� �� ��C� exec cn_Presupuesto_Mensual ?oApp.empresa, ?oApp.Ejercicio, ?m.dCuenta,?m.hCuenta,?m.dfecha,?m.hfecha, ?m.centro, ?m.TipoMoneda� Saldos� �� F� � G(�� �� �� #)�; T�� ��	 �
 � � � � � � � � � � �� #)� U 
 TIPOMONEDA	 DECIMALES DEC HCUENTA CENTRO SQL SALDOS NIVEL TOTALGENERAL MES1 MES2 MES3 MES4 MES5 MES6 MES7 MES8 MES9 MES10 MES11 MES12 BeforeOpenTables,     �� InitA     ��1 q 3 B� � � A G� � � � � A qA � �A R	q Q �S 3                       &         A   �      )   �                  