  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      n�mero      Arial      Arial      Arial      Arial      Arial      Arial      'Lista de Asientos por Fecha'             Arial      empresa             Arial      *iif(m.sucursal='%','Todos',SucursalNombre)             Arial      "Sucursal:"      Arial      
"Per�odo:"      Arial      'dtoc(m.dfecha) + ' al ' +dtoc(m.hfecha)             Arial                    
"Cuenta
"      Arial      "Debe"      Arial      "Haber"      Arial      n�mero      "99999"             Arial      	"N�mero:"      Arial      fecha             Arial      "Fecha:"      Arial      nota             Arial      "Nota:"      Arial      cuenta             Arial      descripci�n             Arial      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 999,999,999,999"             Arial      dec <= 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      
"Totales:"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      debe      "999,999,999,999"             Arial      dec <= 0      haber      "999,999,999,999"             Arial      dec <= 0      debe      "999,999,999.99"             Arial      dec > 0      haber      "999,999,999.99"             Arial      dec > 0      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "rasientos_fecha"
DataSource = .NULL.
Name = "Dataenvironment"
     ]PROCEDURE Init
PUBLIC dec
dec = 0
IF EMPTY(m.sucursal)
	m.Sucursal=null
ENDIF


TEXT TO cmdSQL noshow
SELECT sucursalNombre,n�mero,fecha, descripci�n AS nota, sucursal, CentroNombre,
cuenta, debe, haber, idconcepto, documento, centro, detalle, CuentaNombre as descripci�n,iddetalle 
FROM cn_vAsientos
WHERE (fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (sucursal =?m.sucursal or ?m.sucursal is null)
	and IdEmpresa=?oApp.Empresa
	ORDER BY Fecha ,iddetalle

ENDTEXT


IF sql(cmdSQL ,'rAsientos') > 0
     SELECT rasientos
ENDIF
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     '���                              c[   %   c      �     �          �  U  � 7�  � T�  �� �� %�C�� ���2 � T�� ���� �	 M(� ��V �P SELECT sucursalNombre,n�mero,fecha, descripci�n AS nota, sucursal, CentroNombre,�i �c cuenta, debe, haber, idconcepto, documento, centro, detalle, CuentaNombre as descripci�n,iddetalle � � FROM cn_vAsientos�f �` WHERE (fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (sucursal =?m.sucursal or ?m.sucursal is null)�" � 	and IdEmpresa=?oApp.Empresa�  � 	ORDER BY Fecha ,iddetalle� �  � �" %�C � �	 rAsientos� � ���� F� � � U  DEC SUCURSAL CMDSQL SQL	 RASIENTOS
  �  � U  SETEO Init,     �� BeforeOpenTablesN    ��1 q � � A � a�qa!a A #q A 2 q 2                       !        H  R      )                     