  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Fax en uit-senad (desde UIT13)
OUTPUT=TS001
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=1
PRINTQUALITY=200
YRESOLUTION=200
      I  2  winspool  Fax en uit-senad (desde UIT13)  TS001                       �Fax en uit-senad (desde UIT13)   � `&   �
od    �   �    Carta                                                                                 Dfax                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Arial      n�mero      trdebe      debe      nvl(cu_anterior.deanterior,0)      trhaber      haber      nvl(cu_anterior.haanterior,0)      Arial      Arial      Arial      Arial      Arial      "Libro Diario"             Arial      empresa             Arial      
"Cuenta
"      Arial      "Debe"      Arial      "Haber"      Arial      "Transporte"             Arial      trdebe      "@Z 999,999,999,999"             Arial      dec <= 0      trdebe      "@Z 999,999,999.99"             Arial      dec > 0      trhaber      "@Z 999,999,999,999"             Arial      dec <= 0      trhaber      "@Z 999,999,999.99"             Arial      dec > 0      nro_Asiento      "99999"             Arial      fecha             Arial      "Fecha:"      Arial      nota             Arial      "Nota:"      Arial      cuenta             Arial      descripci�n             Arial      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      trdebe      "@Z 999,999,999,999"             Arial      dec <= 0      trdebe      "@Z 999,999,999.99"             Arial      dec > 0      trhaber      "@Z 999,999,999,999"             Arial      dec <= 0      trhaber      "@Z 999,999,999.99"             Arial      dec > 0      "Transporte"             Arial      dataenvironment      LLeft = 113
Top = 203
Width = 452
Height = 184
Name = "Dataenvironment"
     7PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
cmdsql = 'SELECT sum(d.debe) as deanterior, ' + ;
		'	sum(d.haber) as haanterior ' + ;
		'FROM  cn_asientos a INNER JOIN CN_detalle d ' + ;
		'ON  a.idasiento = d.idasiento ' +; 
		' WHERE a.idempresa = ?oApp.Empresa and a.Ejercicio = ?oApp.Ejercicio ' + ;
		' AND a.fecha BETWEEN ?oApp.InicioEjercicio AND ?m.dfecha - 1 '
		
= sql(cmdsql,'cu_anterior')
cmdsql = 'SELECT a.n�mero, a.nro_asiento,' + ;
'a.fecha, d.cuenta, d.debe,' + ;
'd.haber, d.detalle, d.centro,' + ;
'd.idconcepto, d.documento,' + ;
'c.descripci�n, a.descripci�n AS nota,0 AS dec ' + ;
' FROM  cn_asientos a INNER JOIN CN_detalle d ' + ;
' ON  a.idasiento = d.idasiento ' +;
' LEFT JOIN cn_cuentas c ' + ;
' ON  d.cuenta = c.cuenta AND d.ejercicio = c.Ejercicio and d.IdEmpresa = c.IdEmpresa ' + ;
' WHERE a.idempresa = ?oApp.Empresa and a.Ejercicio = ?oApp.Ejercicio ' + ;
' AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha ' + ;
' ORDER BY a.nro_asiento, a.fecha, IdDetalle'
= sql(cmdsql,'lDiario')
SELECT ldiario

ENDPROC
     ����    �  �                        ~�   %   �      +  	             �  U  
  �  � U  SETEO�+T�  ��" SELECT sum(d.debe) as deanterior, � 	sum(d.haber) as haanterior �, FROM  cn_asientos a INNER JOIN CN_detalle d � ON  a.idasiento = d.idasiento �E  WHERE a.idempresa = ?oApp.Empresa and a.Ejercicio = ?oApp.Ejercicio �=  AND a.fecha BETWEEN ?oApp.InicioEjercicio AND ?m.dfecha - 1 �� ��C �  � cu_anterior� ��-T�  �� SELECT a.n�mero, a.nro_asiento,� a.fecha, d.cuenta, d.debe,� d.haber, d.detalle, d.centro,� d.idconcepto, d.documento,�. c.descripci�n, a.descripci�n AS nota,0 AS dec �-  FROM  cn_asientos a INNER JOIN CN_detalle d �  ON  a.idasiento = d.idasiento �  LEFT JOIN cn_cuentas c �U  ON  d.cuenta = c.cuenta AND d.ejercicio = c.Ejercicio and d.IdEmpresa = c.IdEmpresa �E  WHERE a.idempresa = ?oApp.Empresa and a.Ejercicio = ?oApp.Ejercicio �-  AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha �+  ORDER BY a.nro_asiento, a.fecha, IdDetalle�� ��C �  � lDiario� �� F� � U  CMDSQL SQL LDIARIO BeforeOpenTables,     �� InitA     ��1 q 3 ���"�q 2                       &         A   ,      )   �                  