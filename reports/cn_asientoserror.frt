  I                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "'Lista de Asientos no Balanceados'      Arial      empresa             Arial      "N�mero"      Arial      "Fecha"      Arial      "Ejercicio"      Arial      	"Detalle"      Arial      "Debe"      Arial      "Haber"      Arial      n�mero      "99999"      Arial      fecha      Arial      	ejercicio      Arial      descripci�n      Arial      debe      "@Z 999,999,999,999"      Arial      haber      "@Z 999,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "rasientos"
DataSource = .NULL.
Name = "Dataenvironment"
     rPROCEDURE Init
PUBLIC dec
dec = 0

TEXT TO cmdSQL noshow
	SELECT     a.ejercicio, a.n�mero, a.fecha, a.descripci�n, SUM(d.debe) AS Debe, SUM(d.haber) AS Haber
	FROM         cn_asientos AS a INNER JOIN
	                      cn_detalle AS d ON a.idempresa = d.idempresa AND a.idasiento = d.idasiento
	WHERE     (a.idempresa = ?oApp.Empresa)
	GROUP BY a.ejercicio, a.n�mero, a.fecha, a.descripci�n
	HAVING      (SUM(d.debe) <> SUM(d.haber))
ENDTEXT



IF sql(cmdSQL,'rAsientos') > 0
     SELECT rasientos
ENDIF

ENDPROC
PROCEDURE Destroy
RELEASE dec
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo
ENDPROC
     C���    *  *                        �G   %   b      �     �          �  U  � 7�  � T�  �� ��	 M(� ��k �e 	SELECT     a.ejercicio, a.n�mero, a.fecha, a.descripci�n, SUM(d.debe) AS Debe, SUM(d.haber) AS Haber�/ �) 	FROM         cn_asientos AS a INNER JOIN�g �a 	                      cn_detalle AS d ON a.idempresa = d.idempresa AND a.idasiento = d.idasiento�. �( 	WHERE     (a.idempresa = ?oApp.Empresa)�= �7 	GROUP BY a.ejercicio, a.n�mero, a.fecha, a.descripci�n�0 �* 	HAVING      (SUM(d.debe) <> SUM(d.haber))� �" %�C � �	 rAsientos� � ���� F� � � U  DEC CMDSQL SQL	 RASIENTOS
  <�  � U  DEC
  �  � U  SETEO Init,     �� Destroy:    �� BeforeOpenTablesM    ��1 q � � ��q��A $q A 3 q 2 q 1                               -  8        _  g      )   *                  