  2   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=1
PAPERSIZE=5
COLOR=2
                         Courier New                                                   seccion                                                       "Planilla General de Empleados"                               Arial                                                         empresa                                                                                                                     Arial                                                         "Legajo"                                                      Arial                                                         "Nombre"                                                      Arial                                                         "Cargo/Ocup."                                                 Arial                                                         "Basico"                                                      Arial                                                         	"Nro Doc"                                                     Arial                                                         "Frecuencia Liq"                                              Arial                                                         "IPS"                                                         Arial                                                         Seccion                                                       Arial                                                         Sueldo                                                        "999"                                                         Arial                                                         Empleado                                                      Arial                                                         rSuelSiml.ocupacion                                           Arial                                                         Sueldo                                                        "999,999,999"                                                 Arial                                                         	documento                                                     Arial                                                         
Frecuencia                                                    Arial                                                         
nro_asegur                                                    Arial                                                         total                                                         "999"                                                         Arial                                                         "Total Seccion"                                               Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         totalgen                                                      "999"                                                                                                                       Arial                                                         "Total de la Empresa"                                         Arial                                                         "Activo"                                                      Arial                                                         iif(nvl(Activo,"S"),"S","N")                                  "999,999,999"                                                 Arial                                                         Sueldo                                                        "999,999,999,999"                                             Arial                                                         Sueldo                                                        "999,999,999,999"                                             Arial                                                         total                                                         0                                                             0                                                             totalgen                                                      0                                                             0                                                             Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 178
Left = 211
Width = 520
Height = 219
Visible = .F.
TabStop = .F.
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
                                   
wPROCEDURE BeforeOpenTables
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
SELECT     em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, 
                      c.centro + c.descripci�n AS Centro, em.ocupacion, em.activo, em.documento,em.sueldo_bas Sueldo,em.nro_asegur
FROM         rh_empleado AS em left JOIN 
                      rh_frecuencia AS fr ON em.idempresa = fr.IdEmpresa AND em.frec_liqui = fr.idfrecliqui LEFT OUTER JOIN
                      centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN
                      rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion
	where em.IdEmpresa = ?oApp.Empresa and 
		(em.SECCION = ?m.seccion or ?m.seccion is null)
			and (em.centro_pag = ?m.centropago or ?m.centropago is null)
			and (case when ISNULL(em.Activo,'N') = 1 then 'A' else 'I' end = ?m.EstadoAF or ?m.EstadoAF='T')                   
	order by s.descripcion,em.idempleado

ENDTEXT

sql(cmdSQL,'rSuelsiml')

ENDPROC
      ���    f  f                        r   %   �           �          �  U  
  �  � U  SETEOJ %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � M(� �u �o SELECT     em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, �� ��                       c.centro + c.descripci�n AS Centro, em.ocupacion, em.activo, em.documento,em.sueldo_bas Sueldo,em.nro_asegur�/ �) FROM         rh_empleado AS em left JOIN �� �{                       rh_frecuencia AS fr ON em.idempresa = fr.IdEmpresa AND em.frec_liqui = fr.idfrecliqui LEFT OUTER JOIN�s �m                       centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN�f �`                       rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion�. �( 	where em.IdEmpresa = ?oApp.Empresa and �7 �1 		(em.SECCION = ?m.seccion or ?m.seccion is null)�E �? 			and (em.centro_pag = ?m.centropago or ?m.centropago is null)�| �v 			and (case when ISNULL(em.Activo,'N') = 1 then 'A' else 'I' end = ?m.EstadoAF or ?m.EstadoAF='T')                   �+ �% 	order by s.descripcion,em.idempleado� �  � � ��C � �	 rSuelsiml� �� U  SECCION
 CENTROPAGO CMDSQL SQL BeforeOpenTables,     �� InitA     ��1 q 3 `� A � A � Q��1a�qQ��a A �2                       &         A   l
      )   f                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
                         Courier New                                                   seccion                                                       "Planilla General de Empleados"                               Arial                                                         empresa                                                                                                                     Arial                                                         "Legajo"                                                      Arial                                                         "Nombre"                                                      Arial                                                         "Cargo/Ocup."                                                 Arial                                                         "Basico"                                                      Arial                                                         	"Nro Doc"                                                     Arial                                                         "Frecuencia Liq"                                              Arial                                                         "IPS"                                                         Arial                                                         Seccion                                                       Arial                                                         Sueldo                                                        "999"                                                         Arial                                                         Empleado                                                      Arial                                                         rSuelSiml.ocupacion                                           Arial                                                         Sueldo                                                        "999,999,999"                                                 Arial                                                         	documento                                                     Arial                                                         
Frecuencia                                                    Arial                                                         
nro_asegur                                                    Arial                                                         total                                                         "999"                                                         Arial                                                         "Total Seccion"                                               Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         totalgen                                                      "999"                                                                                                                       Arial                                                         "Total de la Empresa"                                         Arial                                                         "Activo"                                                      Arial                                                         iif(nvl(Activo,"S"),"S","N")                                  "999,999,999"                                                 Arial                                                         Sueldo                                                        "999,999,999,999"                                             Arial                                                         Sueldo                                                        "999,999,999,999"                                             Arial                                                         total                                                         0                                                             0                                                             totalgen                                                      0                                                             0                                                             Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 178
Left = 211
Width = 520
Height = 219
Visible = .F.
TabStop = .F.
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
                                   
wPROCEDURE Init
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
SELECT     em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, 
                      c.centro + c.descripci�n AS Centro, em.ocupacion, em.activo, em.documento,em.sueldo_bas Sueldo,em.nro_asegur
FROM         rh_empleado AS em left JOIN 
                      rh_frecuencia AS fr ON em.idempresa = fr.IdEmpresa AND em.frec_liqui = fr.idfrecliqui LEFT OUTER JOIN
                      centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN
                      rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion
	where em.IdEmpresa = ?oApp.Empresa and 
		(em.SECCION = ?m.seccion or ?m.seccion is null)
			and (em.centro_pag = ?m.centropago or ?m.centropago is null)
			and (case when ISNULL(em.Activo,'N') = 1 then 'A' else 'I' end = ?m.EstadoAF or ?m.EstadoAF='T')                   
	order by s.descripcion,em.idempleado

ENDTEXT

sql(cmdSQL,'rSuelsiml')

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
      ���    f  f                        r   %   �           �          �  U  J %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � M(� �u �o SELECT     em.idempleado +' '+ em.fNombre AS Empleado, fr.descripcion AS Frecuencia, s.descripcion AS Seccion, �� ��                       c.centro + c.descripci�n AS Centro, em.ocupacion, em.activo, em.documento,em.sueldo_bas Sueldo,em.nro_asegur�/ �) FROM         rh_empleado AS em left JOIN �� �{                       rh_frecuencia AS fr ON em.idempresa = fr.IdEmpresa AND em.frec_liqui = fr.idfrecliqui LEFT OUTER JOIN�s �m                       centros AS c ON em.idempresa = c.idempresa AND em.centro_pag = c.centro LEFT OUTER JOIN�f �`                       rh_seccion AS s ON em.idempresa = s.idempresa AND em.seccion = s.idseccion�. �( 	where em.IdEmpresa = ?oApp.Empresa and �7 �1 		(em.SECCION = ?m.seccion or ?m.seccion is null)�E �? 			and (em.centro_pag = ?m.centropago or ?m.centropago is null)�| �v 			and (case when ISNULL(em.Activo,'N') = 1 then 'A' else 'I' end = ?m.EstadoAF or ?m.EstadoAF='T')                   �+ �% 	order by s.descripcion,em.idempleado� �  � � ��C � �	 rSuelsiml� �� U  SECCION
 CENTROPAGO CMDSQL SQL
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 `� A � A � Q��1a�qQ��a A �3 q 2                       ;
        b
  l
  4    )   f                                                                           