  )�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Courier New      Seccion      total      0      0      totalgen      0      0      Arial      Arial      Arial      Courier New      Arial      Arial      ""Planilla Simplificada de Sueldos"             Arial      empresa             Arial      "Fecha:"             Arial      m.fecha             Arial      
"Periodo:"             Arial      <letrames(rSuelsiml.mes)+' de '+ alltrim(Str( rSuelsiml.a�o))             Arial      "Frecuencia:"             Arial      
frecuencia             Arial      "Neto"      Arial      "Legajo"      Arial      "Nombre"      Arial      "Cargo/Ocup."      Arial      "Basico"      Arial      "Horas 
Extras"      "@I"      Arial      
"Comisi�n"      Arial      "Total"      Arial      
"Anticipo"      Arial      "Ausencias"      Arial      "IPS"      Arial      "Otros
Anticipos"      "@I"      Arial      
"Telefono"      Arial      "Productos"      Arial      "Otros Desc."      Arial      Seccion      Arial      Sueldo      "999"      Arial      Empleado      Arial      rSuelSiml.ocupacion             Arial      Sueldo      "999,999,999"      Arial      HEX      "999,999,999"      Arial      Comision      "999,999,999"      Arial      th      "999,999,999"      Arial      Adelanto      "999,999,999"      Arial      Ausencia      "999,999,999"      Arial      rSuelsiml.IPS      "999,999,999"             Arial      	Adelanto1      "999,999,999"      Arial      Telefono      "999,999,999"      Arial      Prestamo      "999,999,999"      Arial      @td - IPS - Ausencia - Adelanto - Prestamo - Adelanto1 - Telefono      "999,999,999"      Arial      rSuelsiml.tn      "999,999,999"             Arial      total      "999"             Arial      HEX      "999,999,999"      Arial      Comision      "999,999,999"      Arial      th      "999,999,999"      Arial      Adelanto      "999,999,999"             Arial      Ausencia      "999,999,999"      Arial      rSuelsiml.IPS      "999,999,999"             Arial      	Adelanto1      "999,999,999"      Arial      Telefono      "999,999,999"      Arial      Prestamo      "999,999,999"      Arial      @td - IPS - Ausencia - Adelanto - Prestamo - Adelanto1 - Telefono      "999,999,999"      Arial      rSuelsiml.tn      "999,999,999"             Arial      "Total Seccion"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      totalgen      "999"             Arial      HEX      "999,999,999"      Arial      Comision      "999,999,999"      Arial      th      "999,999,999"      Arial      Adelanto      "999,999,999"             Arial      Ausencia      "999,999,999"      Arial      rSuelsiml.IPS      "999,999,999"             Arial      	Adelanto1      "999,999,999"      Arial      Telefono      "999,999,999"      Arial      Prestamo      "999,999,999"      Arial      @td - IPS - Ausencia - Adelanto - Prestamo - Adelanto1 - Telefono      "999,999,999"      Arial      rSuelsiml.tn      "999,999,999"             Arial      "Total de la Empresa"      Arial      dataenvironment      �Top = 178
Left = 211
Width = 520
Height = 219
Visible = .F.
TabStop = .F.
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
     {PROCEDURE Init
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
                      c.centro + c.descripci�n AS Centro, em.ocupacion, d.sueldo, d.th, d.tn, d.td, pvt.*
FROM         rh_liquidet AS d INNER JOIN
                      rh_liquidacion AS l ON d.idliquidacion = l.idliquidacion INNER JOIN
                      rh_empleado AS em ON d.IdEmpresa = em.idempresa AND d.idempleado = em.idempleado INNER JOIN
                      rh_frecuencia AS fr ON l.idempresa = fr.IdEmpresa AND l.idfrecuencia = fr.idfrecliqui LEFT OUTER JOIN
                      centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN
                      rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion
                      left join 
                      (SELECT pvt.IdLiquiDet,[001] AS Basico, 
ISNULL([202],0) AS HEX,
ISNULL([203],0) AS Comision,
ISNULL([104],0) AS Adelanto,
ISNULL([109],0) AS Ausencia,
ISNULL([105],0) AS IPS,
ISNULL([103],0) AS Adelanto1,
ISNULL([108],0) AS Telefono,
ISNULL([107],0) AS Prestamo
FROM
(SELECT IdLiquiDet, IdConcepto, Monto
FROM rh_liquida_conceptos ld where ld.IdEmpresa=?oApp.Empresa ) ps
PIVOT
(
SUM (Monto)
FOR Idconcepto IN
( [001], [202],[203],[104],[109],[105],[103],[108],[107])
) AS pvt) pvt on pvt.IdLiquidet=d.IdLiquidet
	where 	 l.fecha = ?m.fecha and  l.idfrecuencia = ?m.idfrecuencia 
			and (em.SECCION = ?m.seccion or ?m.seccion is null)
			and (em.centro_pag = ?m.centropago or ?m.centropago is null)                 
	order by s.descripcion,em.idempleado

ENDTEXT

sql(cmdSQL,'rsuelsiml')



*!*	SET ENGINEBEHAVIOR 70
*!*	SELECT a.*,;
*!*	sum(IIF(b.IdConcepto="100",b.Monto,0)) as IPS,; 
*!*	sum(IIF(b.IdConcepto="200",b.Monto,0)) as Bonificacion,;
*!*	sum(IIF(b.IdConcepto="206",b.Monto,0)) as Viveres,;
*!*	sum(IIF(b.IdConcepto="207",b.Monto,0)) as Adelanto,;
*!*	sum(IIF(b.IdConcepto="205",b.Monto,0)) as Aguinaldo,;
*!*	sum(IIF(b.IdConcepto="101",b.Monto,0)) as Prestamo;
*!*	from rSueldo1 a ;
*!*	left JOIN datos!rh_liquida_conceptos b ;
*!*	ON (a.IdLiquiDet = b.IdLiquiDet AND (INLIST(b.IdConcepto ,'100','200','206','207','205','101' )));
*!*	group BY a.seccion,a.IdEmpleado;
*!*	ORDER BY a.seccion, a.idempleado ;
*!*	into cursor rsuelsiml

SELECT rsuelsiml

*SET ENGINEBEHAVIOR 90
*ON (a.IdLiquiDet = b.IdLiquiDet AND (b.IdConcepto="253" OR b.IdConcepto="120" ));

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        �|   %   �      y  0             �  U   %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � M(� �� �� SELECT     l.fecha, l.mes, l.a�o, em.idempleado, em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, �o �i                       c.centro + c.descripci�n AS Centro, em.ocupacion, d.sueldo, d.th, d.tn, d.td, pvt.*�. �( FROM         rh_liquidet AS d INNER JOIN�_ �Y                       rh_liquidacion AS l ON d.idliquidacion = l.idliquidacion INNER JOIN�w �q                       rh_empleado AS em ON d.IdEmpresa = em.idempresa AND d.idempleado = em.idempleado INNER JOIN�� �{                       rh_frecuencia AS fr ON l.idempresa = fr.IdEmpresa AND l.idfrecuencia = fr.idfrecliqui LEFT OUTER JOIN�s �m                       centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN�f �`                       rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion�& �                        left join �D �>                       (SELECT pvt.IdLiquiDet,[001] AS Basico, � � ISNULL([202],0) AS HEX,�" � ISNULL([203],0) AS Comision,�" � ISNULL([104],0) AS Adelanto,�" � ISNULL([109],0) AS Ausencia,� � ISNULL([105],0) AS IPS,�# � ISNULL([103],0) AS Adelanto1,�" � ISNULL([108],0) AS Telefono,�! � ISNULL([107],0) AS Prestamo�
 � FROM�+ �% (SELECT IdLiquiDet, IdConcepto, Monto�H �B FROM rh_liquida_conceptos ld where ld.IdEmpresa=?oApp.Empresa ) ps� � PIVOT� � (� � SUM (Monto)� � FOR Idconcepto IN�? �9 ( [001], [202],[203],[104],[109],[105],[103],[108],[107])�2 �, ) AS pvt) pvt on pvt.IdLiquidet=d.IdLiquidet�H �B 	where 	 l.fecha = ?m.fecha and  l.idfrecuencia = ?m.idfrecuencia �< �6 			and (em.SECCION = ?m.seccion or ?m.seccion is null)�V �P 			and (em.centro_pag = ?m.centropago or ?m.centropago is null)                 �+ �% 	order by s.descripcion,em.idempleado� �  � � ��C � �	 rsuelsiml� �� F� � U  SECCION
 CENTROPAGO CMDSQL SQL	 RSUELSIML
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 `� A � A � �	���q1aaA�!!!�1!� ��� q q�!��a�a A � q 6 q 2                       ?     .   f  p  ^    )   �                  