  @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      restado_cuenta.idcuenta      saldo      .restado_cuenta.debito - restado_cuenta.credito      ?nvl(rEstado_Cuenta2.Debito,0) -  nvl(rEstado_Cuenta2.Credito,0)      Arial      Arial      Arial      Arial      Arial      Arial      "Estado de Cuenta"             Arial      oApp.NombreEmpresa      Arial      restado_cuenta.idcuenta             Arial      Cnrocuenta + '/  ' + rtrim(nombre)+'     '+idmoneda+'     ' +  banco             Arial      	"Cuenta:"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Transac."      Arial      	"Asiento"      Arial      	"Detalle"      Arial      	"Cr�dito"      Arial      "D�bito"      Arial      	"Fecha
"      Arial      "Doc."      Arial      
"Nro.Doc."      Arial      "Saldo"      Arial      "Saldo Anterior"             Arial      >nvl(rEstado_Cuenta2.Debito,0) - nvl(rEstado_Cuenta2.Credito,0)      "999,999,999,999"      Arial      Decimales=0      >nvl(rEstado_Cuenta2.Debito,0) - nvl(rEstado_Cuenta2.Credito,0)      "999,999,999,999.99"             Arial      Decimales>0      restado_cuenta.fecha      "@D"      Arial      !alltrim(restado_cuenta.operacion)      Arial      restado_cuenta.nrocheque      Arial      restado_cuenta.nrooperacion      "99999"      Arial      restado_cuenta.nroasiento      Arial      restado_cuenta.referencia             Arial       restado_cuenta.debito      "@Z 9,999,999,999.99"       restado_cuenta.debito      Arial      Decimales>0       restado_cuenta.debito      "@Z 9,999,999,999"       restado_cuenta.debito      Arial      Decimales=0      restado_cuenta.credito      "@Z 9,999,999,999.99"      Arial      Decimales>0      restado_cuenta.credito      "@Z 9,999,999,999"      Arial      Decimales=0      saldo      "@Z 9,999,999,999.99"             Arial      Decimales>0      saldo      "999,999,999,999"      Arial      Decimales=0      "Saldos del Periodo"             Arial      restado_cuenta.debito      "@Z 9,999,999,999.99"             Arial      Decimales>0      restado_cuenta.debito      "999,999,999,999"      Arial      Decimales=0      restado_cuenta.credito      "@Z 9,999,999,999.99"             Arial      Decimales>0      restado_cuenta.credito      "999,999,999,999"      Arial      Decimales=0      saldo      "9,999,999,999.99"             Arial      Decimales>0      saldo      "999,999,999,999"      Arial      Decimales=0      "Saldos del Ejercicio"             Arial      rEstado_Cuenta1.Debito      "@Z 9,999,999,999.99"             Arial      Decimales>0      rEstado_Cuenta1.Debito      "999,999,999,999"      Arial      Decimales=0      rEstado_Cuenta1.Credito      "999,999,999,999"      Arial      Decimales=0      rEstado_Cuenta1.Credito      "@Z 9,999,999,999.99"             Arial      Decimales>0      0rEstado_Cuenta1.Debito - rEstado_Cuenta1.Credito      "9,999,999,999.99"             Arial      Decimales>0      0rEstado_Cuenta1.Debito - rEstado_Cuenta1.Credito      "999,999,999,999"      Arial      Decimales=0      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      Top = 122
Left = 282
Width = 520
Height = 273
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE Init
sql('exec ts_rEstado_Cuenta ?dFecha, ?hFecha, ?Cuenta','rEstado_Cuenta')
SELECT rEstado_Cuenta
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     X���    ?  ?                        ��   %   �       �      �           �  U  X N ��C�0 exec ts_rEstado_Cuenta ?dFecha, ?hFecha, ?Cuenta� rEstado_Cuenta�  �� F� � U  SQL RESTADO_CUENTA
  �  � U  SETEO Init,     �� BeforeOpenTables�     ��1 �q 2 q 2                       o         �   �       )   ?                  