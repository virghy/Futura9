  D                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Pres      znvl(p1,0)+nvl(p2,0)+nvl(p3,0)+nvl(p4,0)+nvl(p5,0)+nvl(p6,0)+nvl(p7,0)+nvl(p8,0)+nvl(p9,0)+nvl(p10,0)+nvl(p11,0)+nvl(p12,0)      0      Saldo      �nvl(mes1,0)+nvl(mes2,0)+nvl(mes3,0)+nvl(mes4,0)+nvl(mes5,0)+nvl(mes6,0)+nvl(mes7,0)+nvl(mes8,0)+nvl(mes9,0)+nvl(mes10,0)+nvl(mes11,0)+nvl(mes12,0)      0      Dif      
Pres-Saldo      0      Arial      Arial      Arial      Arial      Arial      Arial      %"Control de Ejecuci�n Presupuestaria"      Arial      	m.empresa             Arial      6'Moneda: ',iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "Centro Costo"      Arial      .iif(m.centro='%', 'Todos',centros.Descripci�n)      Arial      "Presup. 
Total"      "@I"      Arial      "Presup.
Acumulado
"      "@I"      Arial      "Ejecutado
Acumulado"      "@I"      Arial      "Saldo
Acumulado
"      "@I"      Arial      "% Saldo
Acum"      "@I"      Arial      "% Def/Sup
Acum"      "@I"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      year(hfecha)      "9999"      Arial      nombremes(month(hfecha))      Arial      year(hfecha)      "9999"      Arial      nombremes(month(hfecha))      Arial       left(nombremes(month(hfecha)),3)      Arial      year(hfecha)      Arial      replicate(' ',nivel*2)+ cuenta      Arial      PT      "999,999,999,999"             Arial      dec=0      Pres      "999,999,999,999"      Arial      dec=0      Pres      "9,999,999,999.99"      Arial      DEC >0      saldo      "9,999,999,999.99"             Arial      DEC >0      saldo      "999,999,999,999"      Arial      dec=0      dif      "999,999,999,999"      Arial      dec=0      dif      "9,999,999,999.99"      Arial      DEC >0      iif(pres>0,(dif/pres*100),0)      
"99999.99"      Arial      iif(PT>0,(Saldo/PT*100),0)      "999999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 62
Left = 100
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "rcuentas"
DataSource = .NULL.
Name = "Dataenvironment"
     8PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
PUBLIC m.dec
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
PROCEDURE Destroy
RELEASE M.dec 
ENDPROC
     O���    6  6                        
�   %   R      �  !   �          �  U  
  �  � U  SETEO^	 7��  � %��� � L��, � T�� �� �� �C � T�� �� �� � %��� � L��y � T�� �� �� T��  �� �� �� � T�� ���� T��  ���� � %�C�� ���� � T�� �� 99999999�� � %�C�� ���� � T�� �� %�� �e�m ��C�V Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centros� �� �� ��C� exec cn_Presupuesto_Mensual ?oApp.empresa, ?oApp.Ejercicio, ?m.dCuenta,?m.hCuenta,?m.dfecha,?m.hfecha, ?m.centro, ?m.TipoMoneda� Saldos� �� F� � G(�� �� �� #)�; T�� ��	 �
 � � � � � � � � � � �� #)� U  DEC
 TIPOMONEDA	 DECIMALES HCUENTA CENTRO SQL SALDOS NIVEL TOTALGENERAL MES1 MES2 MES3 MES4 MES5 MES6 MES7 MES8 MES9 MES10 MES11 MES12 	 <��  � U  DEC BeforeOpenTables,     �� InitA     �� Destroy=    ��1 q 3 � A� � � A G� � � � � A qA � �A R	q Q �S 2 � 1                       &         A             -  .    )   6                  