  r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Estado      ,iif(INLIST(substr(cuenta,1,1),'1','5'),1,-1)      0      Saldo1      (Saldo + ( Debe - Haber))       0      
SaldoLetra      (Saldo + Debe - Haber) * Estado      0      Arial      Arial      Arial      Arial      Arial      "Balancete de Verificaci�n"             Arial      	m.empresa             Arial      6'Moneda: ',iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      
"Sucursal"      Arial      )iif(m.sucursal='%', 'Todos',suc.sucursal)             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "Centro Costo"      Arial      (iif(m.centro='%', 'Todos',centro.centro)             Arial      "Saldo Anterior"      Arial      "Debito"      Arial      	"Credito"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "Saldo Actual"      Arial      Dreplicate(' ',nivel*5)+ iif(m.ImpresionCuenta = 'N',cuenta, cuenta1)             Arial      
abs(Saldo)      "999,999,999,999"             Arial      dec=0      
abs(Saldo)      "99,999,999.99"             Arial      DEC >0      $iif(Saldo<0,'C',IIF(SALDO>0,'D',''))             Arial      Debe      "999,999,999,999"             Arial      dec=0      Debe      "999,999,999.99"             Arial      DEC >0      Haber      "999,999,999,999"             Arial      dec=0      Haber      "999,999,999.99"             Arial      DEC >0      ABS(Saldo + Debe - Haber)      "999,999,999,999"             Arial      dec=0      ABS(Saldo + ( Debe - Haber))      "99,999,999.99"             Arial      DEC >0      Hiif((Saldo + Debe - Haber)<0,'C', iif((saldo + debe - haber) >0,'D',''))             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "RESULTADO DEL PERIODO"             Arial      saldo      "999,999,999,999"             Arial      nivel = 6 .and. dec = 0      {iif( left( cuenta,1 ) = "5" .AND. nivel = 1, (Debe-haber)*-1 , iif( left( cuenta,1) = "4" .and. nivel = 1, (haber-debe),0))      "999,999,999,999,999.99"             Arial      "RESULTADO DEL EJERCICIO"             Arial      �iif( left( cuenta,1 ) = "5" .AND. nivel = 1, (Saldo + Debe-haber)*-1 , iif( left( cuenta,1) = "4" .and. nivel = 1, ((Saldo *-1) + haber-debe),0))      "999,999,999,999,999.99"             Arial      dataenvironment      �Top = 62
Left = 100
Width = 520
Height = 200
InitialSelectedAlias = "rcuentas"
DataSource = .NULL.
Name = "Dataenvironment"
     xPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
PUBLIC m.dec
IF m.tipomoneda = 'L'
     m.decimales = 0
     m.dec = 0
ELSE
     m.decimales = 2
     m.dec = 2
ENDIF
IF EMPTY(m.sucursal)
     m.sucursal = '%'
ELSE
     = sql('Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal','Suc')
ENDIF
IF EMPTY(m.dCuenta)
	m.dCuenta='0'
ENDIF

IF EMPTY(m.hCuenta)
	m.hCuenta='9'
ENDIF
	

IF EMPTY(m.centro)
     m.centro = '%'
ELSE
     = sql('Select descripci�n centro from centros where idempresa = ?oApp.Empresa and centro = ?m.centro','Centro')
ENDIF

= sql('exec cn_balancete ?oApp.empresa,?oApp.Ejercicio, ?m.dfecha,?m.hfecha, ?m.sucursal, ?m.centro, ?m.TipoMoneda','Saldos')

SELECT saldos

SET FILTER TO nivel <= m.nivel AND BETWEEN(LEFT(cuenta,10),m.dcuenta,m.hCuenta)
GOTO TOP

ENDPROC
     '���                              A   %   M      �      u          �  U  
  �  � U  SETEO�	 7��  � %��� � L��; � T�� �� �� T��  �� �� �a � T�� ���� T��  ���� � %�C�� ���� � T�� �� %�� ��w ��C�d Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal� Suc� �� � %�C�� ���*� T�� �� 0�� � %�C�� ���O� T�� �� 9�� � %�C�� ���t� T�� �� %�� ���s ��C�] Select descripci�n centro from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centro� �� �� ��C�k exec cn_balancete ?oApp.empresa,?oApp.Ejercicio, ?m.dfecha,?m.hfecha, ?m.sucursal, ?m.centro, ?m.TipoMoneda� Saldos� �� F� �) G(��	 ��	 � CC�
 �
=�� �� �	�� #)� U  DEC
 TIPOMONEDA	 DECIMALES SUCURSAL SQL DCUENTA HCUENTA CENTRO SALDOS NIVEL CUENTA BeforeOpenTables,     �� InitA     ��1 q 3 � A� � � � � A � qA A A � 1A r �Q 2                       &         A   m      )                     