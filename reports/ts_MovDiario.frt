  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=1
COLOR=2
      Arial      restado_cuenta.idcuenta      IDEstado      saldo      .restado_cuenta.debito - restado_cuenta.credito      ?nvl(rEstado_Cuenta2.Debito,0) -  nvl(rEstado_Cuenta2.Credito,0)      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Movimiento Diario"      Arial      empresa             Arial      restado_cuenta.idcuenta             Arial      Cnrocuenta + '/  ' + rtrim(nombre)+'     '+idmoneda+'     ' +  banco             Arial      	"Cuenta:"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Fechas
"      Arial      	"Detalle"      Arial      	"Cr�dito"      Arial      "D�bito"      Arial      
"Nro.Doc."      Arial      "Beneficiario"      Arial      "Saldo"      Arial      	"Banco
"      Arial      "Emision
"      Arial      "Saldo Anterior"             Arial      >nvl(rEstado_Cuenta2.Debito,0) - nvl(rEstado_Cuenta2.Credito,0)      "999,999,999,999"             Arial      Decimales=0      >nvl(rEstado_Cuenta2.Debito,0) - nvl(rEstado_Cuenta2.Credito,0)      "999,999,999,999.99"             Arial      Decimales>0      estado_cheque      Arial      "Estado"      Arial      $nvl(restado_cuenta.fechaBanco,Fecha)      Arial      restado_cuenta.fecha1      Arial      *nvl(restado_cuenta.nrocheque,nroOperacion)      Arial      restado_cuenta.depositante      Arial      restado_cuenta.referencia             Arial       restado_cuenta.debito      "@Z 9,999,999,999.99"       restado_cuenta.debito      Arial      Decimales>0      restado_cuenta.credito      "@Z 9,999,999,999.99"      Arial      Decimales>0      restado_cuenta.credito      "@Z 9,999,999,999"      Arial      Decimales=0      saldo      "@Z 9,999,999,999.99"             Arial      Decimales>0      saldo      "999,999,999,999"      Arial      Decimales=0       restado_cuenta.debito      "@Z 9,999,999,999"       restado_cuenta.debito      Arial      Decimales=0      restado_cuenta.debito      "999,999,999,999"      Arial      Decimales=0      restado_cuenta.debito      "9,999,999,999.99"      Arial      Decimales>0      restado_cuenta.credito      "999,999,999,999"      Arial      Decimales=0      restado_cuenta.credito      "9,999,999,999.99"      Arial      Decimales>0      saldo      "9,999,999,999.99"             Arial      Decimales>0      saldo      "999,999,999,999"      Arial      Decimales=0      "Saldos del Periodo"             Arial      restado_cuenta.debito      "@Z 9,999,999,999.99"             Arial      Decimales>0      restado_cuenta.debito      "999,999,999,999"      Arial      Decimales=0      restado_cuenta.credito      "@Z 9,999,999,999.99"             Arial      Decimales>0      restado_cuenta.credito      "999,999,999,999"      Arial      Decimales=0      saldo      "9,999,999,999.99"             Arial      Decimales>0      saldo      "999,999,999,999"      Arial      Decimales=0      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      RPVersion(LCSELEREPO)      Arial      dataenvironment      Top = 122
Left = 282
Width = 520
Height = 273
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
sql('exec ts_rMovDiario ?dFecha, ?hFecha, ?Cuenta','rEstado_Cuenta')
SELECT rEstado_Cuenta
ENDPROC
     T���    ;  ;                        ��   %   �       �      �           �  U  
  �  � U  SETEOT J ��C�, exec ts_rMovDiario ?dFecha, ?hFecha, ?Cuenta� rEstado_Cuenta�  �� F� � U  SQL RESTADO_CUENTA BeforeOpenTables,     �� InitA     ��1 q 3 �q 1                       &         A   �       )   ;                  