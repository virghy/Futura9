   �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\192.168.0.58\Brother HL-5350DN series (Copiar 1)
OUTPUT=USB002
ORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2970
PAPERWIDTH=2100
COPIES=1
DEFAULTSOURCE=7
DUPLEX=1
COLLATE=1
                                                    ^  F  winspool  \\192.168.0.58\Brother HL-5350DN series (Copiar 1)  USB002                                                 P\\192.168.0.58\Brother HL-5350  � ��   �4d   X  X  A4                                                                                             ��  ��                                                                                                                      �      �  p      �  X              ��                          7  �  '  '  '  '  �
  �  �  ,  @  �        '''  '                                                                                                                                                                                                                 ��                                                                                                                                d             ���                                                                                               	 	 d �  �  ��    '�                                                                                                                                                                                                                                                                                                                                      v g o n z a l e z                                                                                                                                                 V I R G I N I O P C                                                                                                                                                                                                      $   ����                       A r i a l                                                           ���                                                                                                                                                                                                                                                          '                                                                  SPANISH            �  Q  W  PRIVҪ�&�HL-5350DN                                                       Arial                                                         "Orden de Pagos"                                                                                                            Arial                                                         empresa                                                                                                                     Arial                                                         "Proveedor
"                                                 Arial                                                         "Numero"                                                      Arial                                                         "Fecha"                                                       Arial                                                         	"Importe"                                                     Arial                                                         idproveedor,proveedor                                         Arial                                                         nroorden                                                                                                                    Arial                                                         IdMoneda                                                      Arial                                                         ttod(cp_lisordpag.fecha)                                      "@D"                                                          Arial                                                         cp_lisordpag.importe                                          "999,999,999,999.99"                                                                                                        Arial                                                         cp_lisordpag.importe                                          "999,999,999,999.99"                                          Arial                                                         	"Total
"                                                     Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 120
Left = 41
Width = 520
Height = 200
InitialSelectedAlias = "cp_lisordpag"
DataSource = .NULL.
Name = "Dataenvironment"
                                                      PROCEDURE Init


TEXT TO cmdSQL noshow
	sELECT op.nroorden,
	 op.sucursal, op.idproveedor,isnull(op.nombre,c.razon) Proveedor,
	 op.nrocheque, op.fecha,op.Importe,op.idmoneda 
	 FROM  cp_ordenpago op left join cp_proveedor c on op.idempresa = c.IdEmpresa and op.IdProveedor= c.IdProveedor
	 WHERE op.idempresa= ?oApp.Empresa
	 and op.fecha BETWEEN ?m.dFecha AND ?m.hFecha
	 order by op.nroorden
ENDTEXT


 = sql(cmdSQL ,'cp_lisordpag')
SELECT cp_lisordpag
*brow
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                    ����    �  �                        D   %         a     C          �  U  �	 M(�  �� � 	sELECT op.nroorden,�H �B 	 op.sucursal, op.idproveedor,isnull(op.nombre,c.razon) Proveedor,�6 �0 	 op.nrocheque, op.fecha,op.Importe,op.idmoneda �v �p 	 FROM  cp_ordenpago op left join cp_proveedor c on op.idempresa = c.IdEmpresa and op.IdProveedor= c.IdProveedor�) �# 	 WHERE op.idempresa= ?oApp.Empresa�4 �. 	 and op.fecha BETWEEN ?m.dFecha AND ?m.hFecha� � 	 order by op.nroorden� � ��C �  � cp_lisordpag� �� F� � U  CMDSQL SQL CP_LISORDPAG
  �  � U  SETEO Init,     �� BeforeOpenTables    ��1 � ��aa�A�A �q 3 q 2                       �                )   �                                                             �DRIVER=winspool
DEVICE=\\192.168.0.58\Brother HL-5350DN series (Copiar 1)
OUTPUT=USB002
ORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2970
PAPERWIDTH=2100
COPIES=1
DEFAULTSOURCE=7
DUPLEX=1
COLLATE=1
                                                    ^  F  winspool  \\192.168.0.58\Brother HL-5350DN series (Copiar 1)  USB002                                                 P\\192.168.0.58\Brother HL-5350  � ��   �4d   X  X  A4                                                                                             ��  ��                                                                                                                      �      �  p      �  X              ��                          7  �  '  '  '  '  �
  �  �  ,  @  �        '''  '                                                                                                                                                                                                                 ��                                                                                                                                d             ���                                                                                               	 	 d �  �  ��    '�                                                                                                                                                                                                                                                                                                                                      v g o n z a l e z                                                                                                                                                 V I R G I N I O P C                                                                                                                                                                                                      $   ����                       A r i a l                                                           ���                                                                                                                                                                                                                                                          '                                                                  SPANISH            �  Q  W  PRIVҪ�&�HL-5350DN                                                       Arial                                                         "Orden de Pagos"                                                                                                            Arial                                                         empresa                                                                                                                     Arial                                                         "Proveedor
"                                                 Arial                                                         "Numero"                                                      Arial                                                         "Fecha"                                                       Arial                                                         	"Importe"                                                     Arial                                                         idproveedor,proveedor                                         Arial                                                         nroorden                                                                                                                    Arial                                                         IdMoneda                                                      Arial                                                         ttod(cp_lisordpag.fecha)                                      "@D"                                                          Arial                                                         cp_lisordpag.importe                                          "999,999,999,999.99"                                                                                                        Arial                                                         cp_lisordpag.importe                                          "999,999,999,999.99"                                          Arial                                                         	"Total
"                                                     Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 120
Left = 41
Width = 520
Height = 200
InitialSelectedAlias = "cp_lisordpag"
DataSource = .NULL.
Name = "Dataenvironment"
                                                      PROCEDURE Init
LOCAL strsql
SET DATABASE TO DATOS 

TEXT TO cmdSQL noshow
	sELECT op.nroorden,
	 op.sucursal, op.idproveedor,isnull(op.nombre,c.razon) Proveedor,
	 op.nrocheque, op.fecha,op.Importe,op.idmoneda 
	 FROM  cp_ordenpago op left join cp_proveedor c on op.idempresa = c.IdEmpresa and op.IdProveedor= c.IdProveedor
	 WHERE op.idempresa= ?oApp.Empresa
	 and op.fecha BETWEEN ?m.dFecha AND ?m.hFecha
	 order by op.nroorden
ENDTEXT


 = sql(cmdSQL ,'cp_lisordpag')
SELECT cp_lisordpag
*brow
ENDPROC
                                                 ����    �  �                        aK   %   *      V     8          �  U  � ��  � G(� DATOS�	 M(� �� � 	sELECT op.nroorden,�H �B 	 op.sucursal, op.idproveedor,isnull(op.nombre,c.razon) Proveedor,�6 �0 	 op.nrocheque, op.fecha,op.Importe,op.idmoneda �v �p 	 FROM  cp_ordenpago op left join cp_proveedor c on op.idempresa = c.IdEmpresa and op.IdProveedor= c.IdProveedor�) �# 	 WHERE op.idempresa= ?oApp.Empresa�4 �. 	 and op.fecha BETWEEN ?m.dFecha AND ?m.hFecha� � 	 order by op.nroorden� � ��C � � cp_lisordpag� �� F� � U  STRSQL DATOS CMDSQL SQL CP_LISORDPAG Init,     ��1 q � � ��aa�A�A �q 2                             )   �                  