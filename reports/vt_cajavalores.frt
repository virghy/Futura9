  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Resumen de Valores en Caja"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "Registrado
"      Arial      "Declarado
"      Arial      "
"      Arial      "Fecha"      Arial      "Inicio"      Arial      "Fin"      Arial      "Efectivo
"      Arial      
"Cheque
"      Arial      "Tarjeta Cred"      Arial      "Tarjeta Deb
"      Arial      "Efectivo
"      Arial      
"Cheque
"      Arial      "Tarjeta Cred"      Arial      "Tarjeta Deb
"      Arial      Fecha      Arial      IdUsuario,first_name      Arial      Inicio      Arial      Fin      Arial      F01      "99,999,999"      Arial      F02      "99,999,999"      Arial      F03      "99,999,999"      Arial      F05      "99,999,999"      Arial      C01      "99,999,999"      Arial      C02      "99,999,999"      Arial      C03      "99,999,999"      Arial      C05      "99,999,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Totales:
"      Arial      F01      "99,999,999"      Arial      F02      "99,999,999"      Arial      F03      "99,999,999"      Arial      F05      "99,999,999"      Arial      C01      "99,999,999"      Arial      C02      "99,999,999"      Arial      C03      "99,999,999"      Arial      C05      "99,999,999"      Arial      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     MPROCEDURE Init


IF EMPTY(m.Usuario)
	m.Usuario= null
ENDIF


TEXT TO cmdSQL NOSHOW 

SELECT     c.IdCaja, c.IdUsuario, c.Fecha, convert(char(10),c.Inicio,8) as Inicio, 
convert(char(10),c.Fin,8) as Fin, cd.IdMoneda, SUM(CASE WHEN IdForma = '01' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C01, 
                      SUM(CASE WHEN IdForma = '02' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C02, SUM(CASE WHEN IdForma = '03' AND 
                      Operacion = 'C' THEN Importe ELSE 0 END) AS C03, SUM(CASE WHEN IdForma = '04' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C04, 
                      SUM(CASE WHEN IdForma = '05' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C05, SUM(CASE WHEN IdForma = '01' AND 
                      Operacion = 'F' THEN Importe ELSE 0 END) AS F01, SUM(CASE WHEN IdForma = '02' AND Operacion = 'F' THEN Importe ELSE 0 END) AS F02, 
                      SUM(CASE WHEN IdForma = '03' AND Operacion = 'F' THEN Importe ELSE 0 END) AS F03, SUM(CASE WHEN IdForma = '04' AND 
                      Operacion = 'F' THEN Importe ELSE 0 END) AS F04, SUM(CASE WHEN IdForma = '05' AND Operacion = 'F' THEN Importe ELSE 0 END) AS F05, u.last_name, 
                      u.first_name
FROM         vt_caja AS c INNER JOIN
                      vt_CajaDet AS cd ON c.IdCaja = cd.IdCaja INNER JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id
WHERE     c.Fecha between ?m.dFecha and ?m.hFecha
and c.IdEmpresa=?oApp.Empresa
AND (isnull(cd.Importe,0) > 0) 
and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
GROUP BY c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, cd.IdMoneda, u.last_name, u.first_name
order by Fecha,Inicio

ENDTEXT

sql(cmdSQL,'rlnegocio')

SELECT rlnegocio

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     \���    C  C                        �   %   �      �     �          �  U   %�C��  ��� � T��  ���� �	 M(� �� �  �Y �S SELECT     c.IdCaja, c.IdUsuario, c.Fecha, convert(char(10),c.Inicio,8) as Inicio, �� �� convert(char(10),c.Fin,8) as Fin, cd.IdMoneda, SUM(CASE WHEN IdForma = '01' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C01, �� ��                       SUM(CASE WHEN IdForma = '02' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C02, SUM(CASE WHEN IdForma = '03' AND �� ��                       Operacion = 'C' THEN Importe ELSE 0 END) AS C03, SUM(CASE WHEN IdForma = '04' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C04, �� ��                       SUM(CASE WHEN IdForma = '05' AND Operacion = 'C' THEN Importe ELSE 0 END) AS C05, SUM(CASE WHEN IdForma = '01' AND �� ��                       Operacion = 'F' THEN Importe ELSE 0 END) AS F01, SUM(CASE WHEN IdForma = '02' AND Operacion = 'F' THEN Importe ELSE 0 END) AS F02, �� ��                       SUM(CASE WHEN IdForma = '03' AND Operacion = 'F' THEN Importe ELSE 0 END) AS F03, SUM(CASE WHEN IdForma = '04' AND �� ��                       Operacion = 'F' THEN Importe ELSE 0 END) AS F04, SUM(CASE WHEN IdForma = '05' AND Operacion = 'F' THEN Importe ELSE 0 END) AS F05, u.last_name, �( �"                       u.first_name�* �$ FROM         vt_caja AS c INNER JOIN�O �I                       vt_CajaDet AS cd ON c.IdCaja = cd.IdCaja INNER JOIN�m �g                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id�7 �1 WHERE     c.Fecha between ?m.dFecha and ?m.hFecha�# � and c.IdEmpresa=?oApp.Empresa�% � AND (isnull(cd.Importe,0) > 0) �: �4 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)�f �` GROUP BY c.IdCaja, c.IdUsuario, c.Fecha, c.Inicio, c.Fin, cd.IdMoneda, u.last_name, u.first_name� � order by Fecha,Inicio� �  � � ��C � �	 rlnegocio� �� F� � U  USUARIO CMDSQL SQL	 RLNEGOCIO
  �  � U  SETEO Init,     �� BeforeOpenTableso    ��1 � A � a �q��	��	��
����q1Q�a�a A �r 3 q 2                               8  B  %    )   C                  