  B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      IdMoneda + NroCuenta      Arial      Arial      Arial      Arial      Arial      Arial      )"Control de emisi�n de Cheques por Fecha"      Arial      oApp.Nombreempresa      Arial      "Per�odo Pago:"      Arial      .dtoc(m.dfechaPago)+ " al " +dtoc(m.hfechaPago)      Arial      "Per�odo Emision:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Fecha
"      Arial      "Pago
"      Arial      "Cheque"      Arial      "Beneficiario"      Arial      	"Importe"      Arial      "Estado"      Arial      "Nro Asiento"      Arial      "Orden Pago"      Arial      "Emision
"      Arial      	"Cuenta:"      Arial      rcheques.idcuenta             Arial      @rcheques.nrocuenta + '/  ' +  rcheques.nombre +   rcheques.banco             Arial      *nvl(rcheques.fechaDiferida,rcheques.fecha)      Arial      rcheques.fecha             Arial      rcheques.nrocheque             Arial      rcheques.depositante,Referencia      Arial      rcheques.importe      "999,999,999,999.99"      Arial      rcheques.estado_cheque             Arial      rcheques.nroasiento      "@Z"             Arial      rcheques.nroorden      "@Z"             Arial      "Total"             Arial      rcheques.importe      "999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 95
Left = 302
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
*sql("exec ts_rCheques2 ?dFecha, ?hFecha","rCheques")

IF EMPTY(m.EstadoCheque)
	m.EstadoCheque=null
ENDIF

IF EMPTY(m.cuenta)
	m.Cuenta=null
ENDIF
            	

TEXT TO cmdSQL noshow
	SELECT Ts_depositos.iddeposito, 
	Ts_depositos.idcuenta,  
	Ts_depositos.fecha, 
	Ts_depositos.fechaDiferida, 
	Ts_depositos.idconcepto, 
	Ts_depositos.referencia,  
	Ts_detdepos.tipovalor, 
	Ts_detdepos.nrocheque, 
	Ts_detdepos.importe,  
	Ts_detdepos.iddetdepos, 
	Ts_detdepos.iddeposito,  
	Ts_depositos.Depositante as Depositante, 
	Ts_depositos.Referencia as Referencia, 
	Ts_depositos.idoperacion,  
	Ts_detdepos.idestado, 
	Ts_estado_cheque.estado_cheque,  
	Ts_depositos.nroasiento, 
	Ts_depositos.nroorden,  
	Ts_depositos.idcuenta AS cuenta,  
	Ts_cuentas.nrocuenta, Ts_cuentas.nombre, Bs_bancos.Descripcion as Banco,
	Ts_cuentas.IdMoneda
	FROM ts_estado_cheque, 
	ts_depositos_base as ts_Depositos, 
	ts_detdepos_base as ts_detdepos,  
	ts_cuentas, 
	bs_bancos 
	WHERE Ts_depositos.iddeposito = Ts_detdepos.iddeposito   
	AND Ts_detdepos.idestado = Ts_estado_cheque.idestado   
	AND ( Ts_depositos.idoperacion = 2
	AND Ts_depositos.fecha BETWEEN ?m.dfecha AND ?m.hfecha
	AND ISNULL(Ts_depositos.fechaDiferida,Ts_depositos.fecha) BETWEEN ?m.dfechaPago AND ?m.hfechaPago
	and (Ts_detdepos.idestado=?m.EstadoCheque or ?m.EstadoCheque is null)
	and (ts_depositos.IdCuenta = ?m.cuenta or ?m.cuenta is null)
	AND Ts_depositos.idcuenta = ts_cuentas.idcuenta   
	AND Ts_cuentas.idbanco = bs_bancos.idbanco)
	and Ts_depositos.IdEmpresa=?oApp.Empresa
	order by IdMoneda,Ts_depositos.idcuenta, ISNULL(Ts_depositos.fechaDiferida,Ts_depositos.fecha)

ENDTEXT

=sql(cmdsql,"rCheques")


SELECT rcheques


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     P���    7  7                        �   %   N      �  4   v          �  U  � %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� �	 M(� ��' �! 	SELECT Ts_depositos.iddeposito, � � 	Ts_depositos.idcuenta,  � � 	Ts_depositos.fecha, �# � 	Ts_depositos.fechaDiferida, �  � 	Ts_depositos.idconcepto, �! � 	Ts_depositos.referencia,  � � 	Ts_detdepos.tipovalor, � � 	Ts_detdepos.nrocheque, � � 	Ts_detdepos.importe,  � � 	Ts_detdepos.iddetdepos, �  � 	Ts_detdepos.iddeposito,  �0 �* 	Ts_depositos.Depositante as Depositante, �. �( 	Ts_depositos.Referencia as Referencia, �" � 	Ts_depositos.idoperacion,  � � 	Ts_detdepos.idestado, �( �" 	Ts_estado_cheque.estado_cheque,  �  � 	Ts_depositos.nroasiento, � � 	Ts_depositos.nroorden,  �) �# 	Ts_depositos.idcuenta AS cuenta,  �O �I 	Ts_cuentas.nrocuenta, Ts_cuentas.nombre, Bs_bancos.Descripcion as Banco,� � 	Ts_cuentas.IdMoneda� � 	FROM ts_estado_cheque, �* �$ 	ts_depositos_base as ts_Depositos, �) �# 	ts_detdepos_base as ts_detdepos,  � � 	ts_cuentas, � � 	bs_bancos �@ �: 	WHERE Ts_depositos.iddeposito = Ts_detdepos.iddeposito   �> �8 	AND Ts_detdepos.idestado = Ts_estado_cheque.idestado   �) �# 	AND ( Ts_depositos.idoperacion = 2�= �7 	AND Ts_depositos.fecha BETWEEN ?m.dfecha AND ?m.hfecha�h �b 	AND ISNULL(Ts_depositos.fechaDiferida,Ts_depositos.fecha) BETWEEN ?m.dfechaPago AND ?m.hfechaPago�L �F 	and (Ts_detdepos.idestado=?m.EstadoCheque or ?m.EstadoCheque is null)�C �= 	and (ts_depositos.IdCuenta = ?m.cuenta or ?m.cuenta is null)�9 �3 	AND Ts_depositos.idcuenta = ts_cuentas.idcuenta   �2 �, 	AND Ts_cuentas.idbanco = bs_bancos.idbanco)�/ �) 	and Ts_depositos.IdEmpresa=?oApp.Empresa�e �_ 	order by IdMoneda,Ts_depositos.idcuenta, ISNULL(Ts_depositos.fechaDiferida,Ts_depositos.fecha)� �  � � ��C � � rCheques� �� F� � U  ESTADOCHEQUE CUENTA CMDSQL SQL RCHEQUES
  �  � U  SETEO Init,     �� BeforeOpenTables9    ��1 � A � A � q��1�����!���������1�����1�!�Qa A �s 4 q 2                       �     2       =    )   7                  