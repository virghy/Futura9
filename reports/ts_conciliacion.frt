  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=1
COLOR=2
      Arial      restado_cuenta.idcuenta      saldo      <nvl(restado_cuenta.debito,0) - nvl(restado_cuenta.credito,0)      >nvl(restado_cuenta2.debito,0) - nvl(restado_cuenta2.credito,0)      saldoB      .restado_cuenta.debito - restado_cuenta.credito      m.SaldoBanco      Arial      Arial      Arial      Arial      Arial      Arial      "Conciliaci�n Bancaria"             Arial      empresa             Arial      restado_cuenta.idcuenta             Arial      Inrocuenta + '/  ' +  rtrim(nombre)+'    '+idmoneda+'           ' +  banco             Arial      	"Cuenta:"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Saldo Banco"      Arial      
"Transac."      Arial      	"Detalle"      Arial      	"Cr�dito"      Arial      "D�bito"      Arial      	"Fecha
"      Arial      "Doc."      Arial      "Nro."      Arial      "Saldo"      Arial      "Saldo Anterior"             Arial      >nvl(restado_cuenta2.debito,0) -nvl( restado_cuenta2.credito,0)      "999,999,999,999.99"             Arial      m.saldoBanco      "999,999,999,999.99"             Arial      restado_cuenta.fecha      Arial      restado_cuenta.operacion             Arial      restado_cuenta.nrocheque             Arial      restado_cuenta.debito > 0      restado_cuenta.iddeposito             Arial      restado_cuenta.referencia             Arial      restado_cuenta.debito      "999,999,999,999.99"      Arial      restado_cuenta.credito      "999,999,999,999.99"      Arial      saldo      "@Z 999,999,999,999"      Arial      saldoB      "@Z 999,999,999,999"             Arial      "Saldos del Periodo"             Arial      restado_cuenta.debito      "999,999,999,999.99"      Arial      restado_cuenta.credito      "@Z 999,999,999,999.99"             Arial      saldo      "@Z 999,999,999,999.99"             Arial      saldoB      "@Z 999,999,999,999.99"             Arial      "Saldos del Ejercicio"             Arial      restado_cuenta1.debito      "@Z 999,999,999,999.99"             Arial      restado_cuenta1.credito      "@Z 999,999,999,999.99"             Arial      0restado_cuenta1.debito - restado_cuenta1.credito      "999,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 194
Left = 301
Width = 520
Height = 273
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "restado_cuenta"
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
sql('exec ts_rEstado_Cuenta ?dFecha, ?hFecha, ?Cuenta','rEstado_Cuenta')
SELECT rEstado_Cuenta

ENDPROC
     X���    ?  ?                        ��   %   �       �      �           �  U  
  �  � U  SETEOX N ��C�0 exec ts_rEstado_Cuenta ?dFecha, ?hFecha, ?Cuenta� rEstado_Cuenta�  �� F� � U  SQL RESTADO_CUENTA BeforeOpenTables,     �� InitA     ��1 q 3 �q 2                       &         A   �       )   ?                  