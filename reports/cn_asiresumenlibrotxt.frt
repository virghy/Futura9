  p                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'ORIENTATION=1
PAPERSIZE=130
COLOR=2
      Courier New      DTOS(Fecha) + Tipo      Courier New      Courier New      �"         1         2         3         4         5         6         7         8         9
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"      Courier New      1=2      
"Cuenta
"      Courier New      "Debe"      Courier New      "Haber"      Courier New      Replicate('=',125)      Courier New      "Fecha:"      Courier New      fecha      Courier New      cuenta             Courier New      descripci�n      Courier New      debe      "@Z 999,999,999.99"      Courier New      dec > 0      debe      "@Z 999,999,999,999"      Courier New      dec <= 0      haber      "@Z 999,999,999,999"             Courier New      dec <= 0      haber      "@Z 999,999,999.99"             Courier New      dec > 0      "POR LOS REGISTROS DEL DIA"      Courier New      
"Totales:"      Courier New      haber      "@Z 999,999,999,999"             Courier New      dec <= 0      haber      "@Z 999,999,999.99"             Courier New      dec > 0      debe      "@Z 999,999,999,999"             Courier New      dec <= 0      debe      "@Z 999,999,999.99"             Courier New      dec > 0      Replicate('-',60)      Courier New      "Transporte:"      Courier New      +m.ImprimeResumen='N' or _PAGETOTAL<>_PAGENO      haber      "999,999,999,999"      Courier New      +m.ImprimeResumen='N' or _PAGETOTAL<>_PAGENO      debe      "999,999,999,999"      Courier New      +m.ImprimeResumen='N' or _PAGETOTAL<>_PAGENO      debe      "999,999,999,999"      Courier New      m.ImprimeResumen='S'      haber      "999,999,999,999"      Courier New      m.ImprimeResumen='S'      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "rasientos"
DataSource = .NULL.
Name = "Dataenvironment"
     GPROCEDURE BeforeOpenTables
DO seteo
ENDPROC
PROCEDURE Destroy
RELEASE dec
ENDPROC
PROCEDURE Init
PUBLIC dec
dec = 0

*!*	SELECT sucursalNombre, fecha, sucursal, 
*!*	cuenta, SUM(debe) as Debe, SUM(haber) as haber, CuentaNombre as descripci�n
*!*	FROM cn_vAsientos
*!*	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND sucursal like  ?m.sucursal 
*!*		and IdEmpresa=?oApp.Empresa
*!*	group by 	sucursalNombre, fecha, sucursal, 
*!*	cuenta, CuentaNombre
*!*		ORDER BY Fecha,cuenta

IF EMPTY(m.sucursal)
	m.sucursal=null
ENDIF


TEXT TO cmdSQL noshow
SELECT * from(
	SELECT sucursalNombre, fecha, sucursal, 
	cuenta, SUM(debe) as Debe, 0 as haber, CuentaNombre as descripci�n, 1 as Orden,Tipo=Case when LEN(RTRIM(Tipo))=0 then 'B' else Tipo end 
	FROM cn_vAsientos
	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (sucursal = ?m.sucursal or  ?m.sucursal is null)
	and IdEmpresa=?oApp.Empresa
	--and haber=0
	group by 	sucursalNombre, fecha, sucursal, 
	cuenta, CuentaNombre, Tipo
	union
	SELECT sucursalNombre, fecha, sucursal, 
	cuenta, 0 as Debe, SUM(haber) as haber, CuentaNombre as descripci�n, 2 as Orden,Tipo=Case when LEN(RTRIM(Tipo))=0 then 'B' else Tipo end
	FROM cn_vAsientos
	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (sucursal = ?m.sucursal or  ?m.sucursal is null)
	and IdEmpresa=?oApp.Empresa
	--and  debe=0
	group by 	sucursalNombre, fecha, sucursal, 
	cuenta, CuentaNombre, Tipo
	) s
	where debe<>0 or haber<>0 
	ORDER BY Fecha,Tipo,Orden,cuenta

ENDTEXT


IF sql(cmdSQL ,'rAsientos') > 0
     SELECT rasientos
ENDIF

ENDPROC
     ���    �  �                        ?�   %   �      �  &   5          �  U  
  �  � U  SETEO
  <�  � U  DEC} 7�  � T�  �� �� %�C�� ���2 � T�� ���� �	 M(� �� � SELECT * from(�/ �) 	SELECT sucursalNombre, fecha, sucursal, �� �� 	cuenta, SUM(debe) as Debe, 0 as haber, CuentaNombre as descripci�n, 1 as Orden,Tipo=Case when LEN(RTRIM(Tipo))=0 then 'B' else Tipo end � � 	FROM cn_vAsientos�j �d 	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (sucursal = ?m.sucursal or  ?m.sucursal is null)�" � 	and IdEmpresa=?oApp.Empresa� � 	--and haber=0�2 �, 	group by 	sucursalNombre, fecha, sucursal, �! � 	cuenta, CuentaNombre, Tipo� � 	union�/ �) 	SELECT sucursalNombre, fecha, sucursal, �� �� 	cuenta, 0 as Debe, SUM(haber) as haber, CuentaNombre as descripci�n, 2 as Orden,Tipo=Case when LEN(RTRIM(Tipo))=0 then 'B' else Tipo end� � 	FROM cn_vAsientos�j �d 	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (sucursal = ?m.sucursal or  ?m.sucursal is null)�" � 	and IdEmpresa=?oApp.Empresa� � 	--and  debe=0�2 �, 	group by 	sucursalNombre, fecha, sucursal, �! � 	cuenta, CuentaNombre, Tipo�
 � 	) s�! � 	where debe<>0 or haber<>0 �' �! 	ORDER BY Fecha,Tipo,Orden,cuenta� �  � �" %�C � �	 rAsientos� � ��v� F� � � U  DEC SUCURSAL CMDSQL SQL	 RASIENTOS BeforeOpenTables,     �� DestroyA     �� InitT     ��1 q 2 q 2 q � � A � A����!A!� ����!A!� qa A #q A 2                       $         B   M         h   <      )   �                  