  B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Courier New      rh_rhistorico.idempleado      Arial      Arial      Arial      Courier New      Arial      !"Hist�rico de Sueldos y Jornales"             Arial      empresa             Arial      m.dfecha, ' al ' , m.hfecha             Arial      
"Periodo:"             Arial      7iif(empty(m.centropago),'todos',  rh_rHistorico.centro)             Arial      "Centro Pago:"             Arial      	"Haberes"      Arial      "Legajo"      Arial      "Nombre"      Arial      "Fecha"      Arial      	"Periodo"      Arial      "Imponible"      Arial      "No Imponible"      Arial      "Total"      Arial      "Deducciones"      Arial      "Neto"      Arial      rh_rHistorico.idempleado             Arial      !SoloResumen      :alltrim(rh_rHistorico.apellido) +" "+ rh_rHistorico.nombre             Arial      !SoloResumen      rh_rHistorico.fecha      Arial      )rh_rHistorico.mes, '/', rh_rHistorico.a�o             Arial      rh_rHistorico.thi      "999,999,999"             Arial      rh_rHistorico.thn      "999,999,999"             Arial      rh_rHistorico.th      "999,999,999"             Arial      rh_rHistorico.td      "999,999,999"             Arial      rh_rHistorico.tn      "999,999,999"             Arial      rh_rHistorico.idempleado             Arial      SoloResumen      :alltrim(rh_rHistorico.apellido) +" "+ rh_rHistorico.nombre             Arial      SoloResumen      rh_rHistorico.thi      "999,999,999"             Arial      rh_rHistorico.thn      "999,999,999"             Arial      rh_rHistorico.th      "999,999,999"             Arial      rh_rHistorico.td      "999,999,999"             Arial      rh_rHistorico.tn      "999,999,999"             Arial      "Total Empleado"             Arial      !SoloResumen      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      rh_rHistorico.thi      "999,999,999"             Arial      rh_rHistorico.thn      "999,999,999"             Arial      rh_rHistorico.th      "999,999,999"             Arial      rh_rHistorico.td      "999,999,999"             Arial      rh_rHistorico.tn      "999,999,999"             Arial      "Total Empresa"      Arial      dataenvironment      �Top = 53
Left = 222
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.centropago)
	m.centropago = null
ENDIF
IF EMPTY(m.idempleado )
	m.idempleado = null
ENDIF
	 

TEXT TO cmdSQL noshow
SELECT     l.nroliquidacion, l.descripcion, l.fecha, l.mes, l.a�o, l.confirmado, l.idfrecuencia, ld.idempleado, ld.thi, ld.thn, ld.th, ld.td, ld.tn, e.nombre, e.apellido, 
                      e.ocupacion, e.sueldo_bas, e.centro_pag, ld.idliquidet, centro=c.descripci�n
FROM         rh_empleado AS e LEFT OUTER JOIN
                      centros AS c ON e.idempresa = c.idempresa AND e.centro_pag = c.centro RIGHT OUTER JOIN
                      rh_liquidacion AS l INNER JOIN
                      rh_liquidet AS ld ON l.idliquidacion = ld.idliquidacion ON e.idempresa = ld.IdEmpresa AND e.idempleado = ld.idempleado
WHERE l.idempresa = ?oApp.Empresa
	   AND  (e.idempleado = ?m.idempleado or ?m.idempleado is null)
	   AND  l.fecha BETWEEN ?m.dfecha AND ?m.hfecha 
	   AND  (c.centro = ?m.centropago or ?m.centropago  is null)
	 ORDER BY ld.idempleado, l.fecha
 
ENDTEXT

sql(cmdsql,'rh_rhistorico')
SELECT rh_rhistorico

ENDPROC
     _���    F  F                        �S   %   �      �     �          �  U  
  �  � U  SETEO %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� �	 M(� ��� �� SELECT     l.nroliquidacion, l.descripcion, l.fecha, l.mes, l.a�o, l.confirmado, l.idfrecuencia, ld.idempleado, ld.thi, ld.thn, ld.th, ld.td, ld.tn, e.nombre, e.apellido, �h �b                       e.ocupacion, e.sueldo_bas, e.centro_pag, ld.idliquidet, centro=c.descripci�n�3 �- FROM         rh_empleado AS e LEFT OUTER JOIN�r �l                       centros AS c ON e.idempresa = c.idempresa AND e.centro_pag = c.centro RIGHT OUTER JOIN�: �4                       rh_liquidacion AS l INNER JOIN�� ��                       rh_liquidet AS ld ON l.idliquidacion = ld.idliquidacion ON e.idempresa = ld.IdEmpresa AND e.idempleado = ld.idempleado�' �! WHERE l.idempresa = ?oApp.Empresa�F �@ 	   AND  (e.idempleado = ?m.idempleado or ?m.idempleado is null)�7 �1 	   AND  l.fecha BETWEEN ?m.dfecha AND ?m.hfecha �C �= 	   AND  (c.centro = ?m.centropago or ?m.centropago  is null)�' �! 	 ORDER BY ld.idempleado, l.fecha� �  � � ��C � � rh_rhistorico� �� F� � U 
 CENTROPAGO
 IDEMPLEADO CMDSQL SQL RH_RHISTORICO BeforeOpenTables,     �� InitA     ��1 q 3 � A � A � �1!�!	qaq1qq A �q 2                       &         A   v      )   F                  