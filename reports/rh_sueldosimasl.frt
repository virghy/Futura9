   �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Courier New      Seccion      total      0      0      totalgen      0      0      Arial      Arial      Arial      Courier New      Arial      Arial      ""Planilla Simplificada de Sueldos"             Arial      empresa             Arial      "Fecha:"             Arial      m.fecha             Arial      
"Periodo:"             Arial      <letrames(rSuelsiml.mes)+' de '+ alltrim(Str( rSuelsiml.a�o))             Arial      "Frecuencia:"             Arial      
frecuencia             Arial      
"Prestamo"      Arial      "Anticipo
Sem. 1"      Arial      "Anticipo
Sem. 2"      Arial      "Anticipo
Sem. 3"      Arial      "Anticipo
Sem. 4"      Arial      "Neto"      Arial      "Legajo"      Arial      "Nombre"      Arial      "Cargo/Ocup."      Arial      "Basico"      Arial      	"Haberes"      Arial      "Bonificacion"      Arial      "IPS"      Arial      "Otros Desc."      Arial      Seccion      Arial      Sueldo      "999"      Arial      Empleado      Arial      rSuelSiml.ocupacion             Arial      Sueldo      "999,999,999"      Arial      'rSuelsiml.th - Bonificacion - Aguinaldo      "999,999,999"             Arial      rSuelsiml.Bonificacion      "999,999,999"             Arial      rSuelsiml.IPS      "999,999,999"             Arial      Prestamo      "999,999,999"      Arial      Ant1      "999,999,999"      Arial      Ant2      "999,999,999"      Arial      Ant3      "999,999,999"      Arial      Ant4      "999,999,999"      Arial      9rSuelsiml.td - IPS - Ant1 - Ant2 - Ant3 - Ant4 - Prestamo      "999,999,999"      Arial      rSuelsiml.tn      "999,999,999"             Arial      total      "999"             Arial      'rSuelsiml.th - Bonificacion - Aguinaldo      "999,999,999"             Arial      rSuelsiml.Bonificacion      "999,999,999"             Arial      rSuelsiml.IPS      "999,999,999"             Arial      Prestamo      "999,999,999"      Arial      Ant1      "999,999,999"      Arial      Ant2      "999,999,999"      Arial      Ant3      "999,999,999"      Arial      Ant4      "999,999,999"      Arial      9rSuelsiml.td - IPS - Ant1 - Ant2 - Ant3 - Ant4 - Prestamo      "999,999,999"      Arial      rSuelsiml.tn      "999,999,999"             Arial      "Total Seccion"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      totalgen      "999"             Arial      'rSuelsiml.th - Bonificacion - Aguinaldo      "999,999,999"             Arial      rSuelsiml.Bonificacion      "999,999,999"             Arial      rSuelsiml.IPS      "999,999,999"             Arial      Prestamo      "999,999,999"      Arial      Ant1      "999,999,999"      Arial      Ant2      "999,999,999"      Arial      Ant3      "999,999,999"      Arial      Ant4      "999,999,999"      Arial      9rSuelsiml.td - IPS - Ant1 - Ant2 - Ant3 - Ant4 - Prestamo      "999,999,999"      Arial      rSuelsiml.tn      "999,999,999"             Arial      "Total de la Empresa"      Arial      dataenvironment      �Top = 178
Left = 211
Width = 520
Height = 219
Visible = .F.
TabStop = .F.
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.seccion)
	m.seccion= null
ENDIF
IF EMPTY(m.centropago )
	m.centropago = null
ENDIF

	


TEXT TO cmdSQL
SELECT     l.fecha, l.mes, l.a�o, em.idempleado, em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, 
                      c.centro + c.descripci�n AS Centro, em.ocupacion, d.sueldo, d.th, d.tn, d.td, d.idliquidet
FROM         rh_liquidet AS d INNER JOIN
                      rh_liquidacion AS l ON d.idliquidacion = l.idliquidacion INNER JOIN
                      rh_empleado AS em ON d.IdEmpresa = em.idempresa AND d.idempleado = em.idempleado INNER JOIN
                      rh_frecuencia AS fr ON l.idempresa = fr.IdEmpresa AND l.idfrecuencia = fr.idfrecliqui LEFT OUTER JOIN
                      centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN
                      rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion
	where 	 l.fecha = ?m.fecha and  l.idfrecuencia = ?m.idfrecuencia 
			and (em.SECCION = ?m.seccion or ?m.seccion is null)
			and (em.centro_pag = ?m.centropago or ?m.centropago is null)                 
	order by s.descripcion,em.idempleado

ENDTEXT

sql(cmdSQL,'rSueldo1')



SET ENGINEBEHAVIOR 70
SELECT a.*,;
sum(IIF(b.IdConcepto="100",b.Monto,0)) as IPS,; 
sum(IIF(b.IdConcepto="200",b.Monto,0)) as Bonificacion,;
sum(IIF(b.IdConcepto="204",b.Monto,0)) as Ant1,;
sum(IIF(b.IdConcepto="206",b.Monto,0)) as Ant2,;
sum(IIF(b.IdConcepto="207",b.Monto,0)) as Ant3,;
sum(IIF(b.IdConcepto="208",b.Monto,0)) as Ant4,;
sum(IIF(b.IdConcepto="205",b.Monto,0)) as Aguinaldo,;
sum(IIF(b.IdConcepto="101",b.Monto,0)) as Prestamo;
from rSueldo1 a ;
left JOIN datos!rh_liquida_conceptos b ;
ON (a.IdLiquiDet = b.IdLiquiDet AND (INLIST(b.IdConcepto ,'100','200','204','206','207','208','205','101' )));
group BY a.seccion,a.IdEmpleado;
ORDER BY a.seccion, a.idempleado ;
into cursor rsuelsiml

SELECT rsuelsiml

ENDPROC
     ����    �  �                        *l   %   �      1     �          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � M(� �� �� SELECT     l.fecha, l.mes, l.a�o, em.idempleado, em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, �v �p                       c.centro + c.descripci�n AS Centro, em.ocupacion, d.sueldo, d.th, d.tn, d.td, d.idliquidet�. �( FROM         rh_liquidet AS d INNER JOIN�_ �Y                       rh_liquidacion AS l ON d.idliquidacion = l.idliquidacion INNER JOIN�w �q                       rh_empleado AS em ON d.IdEmpresa = em.idempresa AND d.idempleado = em.idempleado INNER JOIN�� �{                       rh_frecuencia AS fr ON l.idempresa = fr.IdEmpresa AND l.idfrecuencia = fr.idfrecliqui LEFT OUTER JOIN�s �m                       centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN�f �`                       rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion�H �B 	where 	 l.fecha = ?m.fecha and  l.idfrecuencia = ?m.idfrecuencia �< �6 			and (em.SECCION = ?m.seccion or ?m.seccion is null)�V �P 			and (em.centro_pag = ?m.centropago or ?m.centropago is null)                 �+ �% 	order by s.descripcion,em.idempleado� �  � � ��C � � rSueldo1� ��
 G���F���o� rSueldo1Q� X�� datos!rh_liquida_conceptosQ�  ��� �� �9 C�� � 100� 200� 204� 206� 207� 208� 205� 101�	�� ��CC�� � 100� �� � � 6���Q� �CC�� � 200� �� � � 6���Q� �CC�� � 204� �� � � 6���Q�	 �CC�� � 206� �� � � 6���Q�
 �CC�� � 207� �� � � 6���Q� �CC�� � 208� �� � � 6���Q� �CC�� � 205� �� � � 6���Q� �CC�� � 101� �� � � 6���Q� ����  ���� �����  ���� ����	 rsuelsiml� F� � U  SECCION
 CENTROPAGO CMDSQL SQL A
 IDCONCEPTO MONTO IPS BONIFICACION ANT1 ANT2 ANT3 ANT4	 AGUINALDO PRESTAMO RSUELDO1 DATOS B
 IDLIQUIDET
 IDEMPLEADO	 RSUELSIML BeforeOpenTables,     �� InitA     ��1 q 3 � A � A � �	a��q1a��a�a A �� Or 2                       &         A         )   �                  