  &�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Courier New      seccion      total      0      0      totalgen      0      0      Arial      Arial      Arial      Courier New      Arial      Arial      ""Planilla Simplificada de Sueldos"             Arial      empresa             Arial      "Fecha:"             Arial      m.fecha             Arial      
"Periodo:"             Arial      <letrames(rSuelsiml.mes)+' de '+ alltrim(Str( rSuelsiml.a�o))             Arial      "Frecuencia:"             Arial      
frecuencia             Arial      "Neto"      Arial      "Firma"      Arial      "Legajo"      Arial      "Nombre"      Arial      "Cargo/Ocup."      Arial      "Basico"      Arial      	"Haberes"      Arial      "Extra"      Arial      "IPS"      Arial      "Otros Desc."      Arial      "Aguinaldo"      Arial      	"Viveres"      Arial      
"Adelanto"      Arial      
"Prestamo"      Arial      Seccion      Arial      Sueldo      "999"      Arial      Empleado      Arial      rSuelSiml.ocupacion             Arial      Sueldo      "999,999,999"      Arial      'rSuelsiml.th - Bonificacion - Aguinaldo      "999,999,999"             Arial      rSuelsiml.Bonificacion      "999,999,999"             Arial      rSuelsiml.IPS      "999,999,999"             Arial      2rSuelsiml.td - IPS - Viveres - Adelanto - Prestamo      "999,999,999"             Arial      	Aguinaldo      "999,999,999"             Arial      Viveres      "999,999,999"             Arial      Adelanto      "999,999,999"             Arial      Prestamo      "999,999,999"             Arial      rSuelsiml.tn      "999,999,999"             Arial      ". . . . . . . . . . . ."      Arial      total      "999"             Arial      'rSuelsiml.th - Bonificacion - Aguinaldo      "999,999,999"             Arial      rSuelsiml.Bonificacion      "999,999,999"             Arial      rSuelsiml.IPS      "999,999,999"             Arial      2rSuelsiml.td - IPS - Viveres - Adelanto - Prestamo      "999,999,999"             Arial      	Aguinaldo      "999,999,999"             Arial      Viveres      "999,999,999"             Arial      Adelanto      "999,999,999"             Arial      Prestamo      "999,999,999"             Arial      rSuelsiml.tn      "999,999,999"             Arial      "Total Seccion"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      totalgen      "999"             Arial      'rSuelsiml.th - Bonificacion - Aguinaldo      "999,999,999"             Arial      rSuelsiml.Bonificacion      "999,999,999"             Arial      rSuelsiml.IPS      "999,999,999"             Arial      2rSuelsiml.td - IPS - Viveres - Adelanto - Prestamo      "999,999,999"             Arial      	Aguinaldo      "999,999,999"             Arial      Viveres      "999,999,999"             Arial      Adelanto      "999,999,999"             Arial      Prestamo      "999,999,999"             Arial      rSuelsiml.tn      "999,999,999"             Arial      "Total de la Empresa"      Arial      dataenvironment      �Top = 178
Left = 211
Width = 520
Height = 219
Visible = .F.
TabStop = .F.
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
*!*	SELECT rh_liquidacion_base.nroliquidacion, rh_liquidacion_base.descripcion, ;
*!*	rh_liquidacion_base.fecha, rh_liquidacion_base.mes, rh_liquidacion_base.a�o, ;
*!*	rh_liquidacion_base.confirmado, rh_liquidacion_base.idfrecuencia, ;
*!*	rh_liquidet_base.idempleado, rh_liquidet_base.thi, rh_liquidet_base.thn, ;
*!*	rh_liquidet_base.th, rh_liquidet_base.td, rh_liquidet_base.tn, rh_empleado_base.nombre, ;
*!*	rh_empleado_base.apellido, rh_centro_pago.centro, rh_empleado_base.ocupacion, ;
*!*	rh_empleado_base.sueldo_bas, rh_empleado_base.centro_pag, rh_frecuencia.descripcion AS frecuencia, ;
*!*	rh_liquidet_base.IdLiquiDet;
*!*	FROM datos!rh_empleado_base, rh_frecuencia, datos!rh_centro_pago, ;
*!*	datos!rh_liquidacion_base ;
*!*	INNER JOIN datos!rh_liquidet_base;
*!*	ON rh_liquidacion_base.idliquidacion = rh_liquidet_base.idliquidacion ;
*!*	WHERE rh_liquidet_base.idempleado = rh_empleado_base.idempleado ;
*!*	AND rh_empleado_base.centro_pag = rh_centro_pago.idcentro ;
*!*	AND rh_liquidacion_base.idfrecuencia = rh_frecuencia.idfrecliqui ;
*!*	AND (rh_liquidacion_base.idempresa = oapp.empresa AND rh_empleado_base.idempresa = oapp.empresa ;
*!*	AND rh_empleado_base.centro_pag = m.centropago AND rh_liquidacion_base.fecha = m.fecha ;
*!*	AND rh_liquidacion_base.idfrecuencia = m.idfrecuencia) ;
*!*	AND rh_empleado_base.SECCION = m.seccion;
*!*	ORDER BY rh_centro_pago.centro, rh_liquidet_base.idempleado ;
*!*	INTO CURSOR rSueldo1 NOFILTER 
*!*	SELECT rSueldo1
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
sum(IIF(b.IdConcepto="202",b.Monto,0)) as Viveres,;
sum(IIF(b.IdConcepto="201",b.Monto,0)) as Adelanto,;
sum(IIF(b.IdConcepto="203",b.Monto,0)) as Aguinaldo,;
sum(IIF(b.IdConcepto="101",b.Monto,0)) as Prestamo;
from rSueldo1 a ;
left JOIN datos!rh_liquida_conceptos b ;
ON (a.IdLiquiDet = b.IdLiquiDet AND (INLIST(b.IdConcepto ,'100','101','200','201','202','203' )));
group BY a.Centro,a.IdEmpleado;
ORDER BY a.centro, a.idempleado ;
into cursor rsuelsiml

SELECT rsuelsiml

SET ENGINEBEHAVIOR 90
*ON (a.IdLiquiDet = b.IdLiquiDet AND (b.IdConcepto="253" OR b.IdConcepto="120" ));

ENDPROC
     \���    C  C                        �   %   �      �     �          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � M(� �� �� SELECT     l.fecha, l.mes, l.a�o, em.idempleado, em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, �v �p                       c.centro + c.descripci�n AS Centro, em.ocupacion, d.sueldo, d.th, d.tn, d.td, d.idliquidet�. �( FROM         rh_liquidet AS d INNER JOIN�_ �Y                       rh_liquidacion AS l ON d.idliquidacion = l.idliquidacion INNER JOIN�w �q                       rh_empleado AS em ON d.IdEmpresa = em.idempresa AND d.idempleado = em.idempleado INNER JOIN�� �{                       rh_frecuencia AS fr ON l.idempresa = fr.IdEmpresa AND l.idfrecuencia = fr.idfrecliqui LEFT OUTER JOIN�s �m                       centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN�f �`                       rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion�H �B 	where 	 l.fecha = ?m.fecha and  l.idfrecuencia = ?m.idfrecuencia �< �6 			and (em.SECCION = ?m.seccion or ?m.seccion is null)�V �P 			and (em.centro_pag = ?m.centropago or ?m.centropago is null)                 �+ �% 	order by s.descripcion,em.idempleado� �  � � ��C � � rSueldo1� ��
 G���F���o� rSueldo1Q� X�� datos!rh_liquida_conceptosQ�  ��� �� �- C�� � 100� 101� 200� 201� 202� 203�	�� ��CC�� � 100� �� � � 6���Q� �CC�� � 200� �� � � 6���Q� �CC�� � 202� �� � � 6���Q�	 �CC�� � 201� �� � � 6���Q�
 �CC�� � 203� �� � � 6���Q� �CC�� � 101� �� � � 6���Q� ���� ���� ����� ���� ����	 rsuelsiml� F� �
 G���Z�� U  SECCION
 CENTROPAGO CMDSQL SQL A
 IDCONCEPTO MONTO IPS BONIFICACION VIVERES ADELANTO	 AGUINALDO PRESTAMO RSUELDO1 DATOS B
 IDLIQUIDET CENTRO
 IDEMPLEADO	 RSUELSIML BeforeOpenTables,     �� InitA     ��1 q 3 `� A � A � �	a��q1a��a�a A �� �r � 3                       &         A   �      )   C                  