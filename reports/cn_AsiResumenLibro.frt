                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=5
COLOR=2
      Arial      DTOS(Fecha) + Tipo      Arial      Arial      Arial      Arial      Arial      Arial             
"Cuenta
"      Arial      "Debe"      Arial      "Haber"      Arial             fecha      "@D"      Arial      "Fecha:"      Arial      cuenta             Arial      descripci�n      Arial      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      
"Totales:"      Arial      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      "POR LOS REGISTROS DEL DIA"      Arial      "Transporte:"      Arial      +m.ImprimeResumen='N' or _PAGETOTAL<>_PAGENO      haber      "999,999,999,999"      Arial      +m.ImprimeResumen='N' or _PAGETOTAL<>_PAGENO      debe      "999,999,999,999"      Arial      +m.ImprimeResumen='N' or _PAGETOTAL<>_PAGENO      debe      "999,999,999,999"      Arial      m.ImprimeResumen='S'      haber      "999,999,999,999"      Arial      m.ImprimeResumen='S'      dataenvironment      �Top = 220
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