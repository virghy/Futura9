  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Fecha      Arial      Arial      Arial      Arial      Arial      Arial      Arial      $'Lista de Asiento Resumen por Fecha'      Arial      empresa             Arial      "Sucursal:"      Arial      *iif(m.sucursal='%','Todos',SucursalNombre)             Arial      
"Per�odo:"      Arial      'dtoc(m.dfecha) + ' al ' +dtoc(m.hfecha)             Arial             
"Cuenta
"      Arial      "Debe"      Arial      "Haber"      Arial             fecha             Arial      "Fecha:"      Arial      cuenta             Arial      descripci�n      Arial      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      
"Totales:"      Arial      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      debe      "999,999,999,999"             Arial      dec <= 0      haber      "999,999,999,999"             Arial      dec <= 0      debe      "999,999,999.99"             Arial      dec > 0      haber      "999,999,999.99"             Arial      dec > 0      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "rasientos"
DataSource = .NULL.
Name = "Dataenvironment"
     UPROCEDURE BeforeOpenTables
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


TEXT TO cmdSQL noshow
SELECT * from(
	SELECT sucursalNombre, fecha, sucursal, 
	cuenta, SUM(debe) as Debe, 0 as haber, CuentaNombre as descripci�n
	FROM cn_vAsientos
	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND sucursal like  ?m.sucursal 
	and IdEmpresa=?oApp.Empresa
	--and haber=0
	group by 	sucursalNombre, fecha, sucursal, 
	cuenta, CuentaNombre
	union
	SELECT sucursalNombre, fecha, sucursal, 
	cuenta, 0 as Debe, SUM(haber) as haber, CuentaNombre as descripci�n
	FROM cn_vAsientos
	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND sucursal like  ?m.sucursal 
	and IdEmpresa=?oApp.Empresa
	--and  debe=0
	group by 	sucursalNombre, fecha, sucursal, 
	cuenta, CuentaNombre
	) s
	where debe<>0 or haber<>0 
	ORDER BY Fecha,haber,Debe,cuenta

ENDTEXT


IF sql(cmdSQL ,'rAsientos') > 0
     SELECT rasientos
ENDIF

ENDPROC
     ���    �  �                        p   %         �  #   H          �  U  
  �  � U  SETEO
  <�  � U  DEC� 7�  � T�  �� ��	 M(� �� � SELECT * from(�/ �) 	SELECT sucursalNombre, fecha, sucursal, �I �C 	cuenta, SUM(debe) as Debe, 0 as haber, CuentaNombre as descripci�n� � 	FROM cn_vAsientos�U �O 	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND sucursal like  ?m.sucursal �" � 	and IdEmpresa=?oApp.Empresa� � 	--and haber=0�2 �, 	group by 	sucursalNombre, fecha, sucursal, � � 	cuenta, CuentaNombre� � 	union�/ �) 	SELECT sucursalNombre, fecha, sucursal, �J �D 	cuenta, 0 as Debe, SUM(haber) as haber, CuentaNombre as descripci�n� � 	FROM cn_vAsientos�U �O 	WHERE 	(fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND sucursal like  ?m.sucursal �" � 	and IdEmpresa=?oApp.Empresa� � 	--and  debe=0�2 �, 	group by 	sucursalNombre, fecha, sucursal, � � 	cuenta, CuentaNombre�
 � 	) s�! � 	where debe<>0 or haber<>0 �' �! 	ORDER BY Fecha,haber,Debe,cuenta� �  � �" %�C � �	 rAsientos� � ���� F� � � U  DEC CMDSQL SQL	 RASIENTOS BeforeOpenTables,     �� DestroyA     �� InitT     ��1 q 2 q 2 q � � A���Q!A!�� ���Q!A!�� qa A #q A 2                       $         B   M         h   J      )   �                  