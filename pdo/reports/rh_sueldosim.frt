  �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                                          T  <   winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Courier New                                                   rSuelsiml.centro_pag                                          "Basico"                                                      Arial                                                         "Nombre"                                                      Arial                                                         	"Haberes"                                                     Arial                                                         
"Periodo:"                                                                                                                  Arial                                                         totalgen                                                      "999"                                                                                                                       Arial                                                         "Total de la Empresa"                                         Arial                                                         rSuelsiml.tn                                                  "999,999,999"                                                                                                               Arial                                                         rSuelsiml.td - IPS                                            "999,999,999"                                                                                                               Arial                                                         rSuelsiml.th - Bonificacion                                   "999,999,999"                                                                                                               Arial                                                         total                                                         "999"                                                                                                                       Arial                                                         "Total del Centro de Pago"                                    Arial                                                         rSuelsiml.tn                                                  "999,999,999"                                                                                                               Arial                                                         rSuelsiml.td - IPS                                            "999,999,999"                                                                                                               Arial                                                         rSuelsiml.th - Bonificacion                                   "999,999,999"                                                                                                               Arial                                                         rSuelsiml.centro                                                                                                            Arial                                                         rSuelsiml.centro_pag                                                                                                        Arial                                                          rSuelsiml.sueldo_bas                                         "999,999,999"                                                 Arial                                                         "Neto"                                                        Arial                                                         rSuelsiml.tn                                                  "999,999,999"                                                                                                               Arial                                                         rSuelsiml.td - IPS                                            "999,999,999"                                                                                                               Arial                                                         rSuelsiml.th - Bonificacion                                   "999,999,999"                                                                                                               Arial                                                         Bidempleado,'-',alltrim(rSuelsiml.apellido) + " " +rSuelsiml.nombre                                                                                                                          Arial                                                         "Deducciones"                                                 Arial                                                         "Legajo"                                                      Arial                                                         <letrames(rSuelsiml.mes)+' de '+ alltrim(Str( rSuelsiml.a�o))                                                                                                                                Arial                                                         ""Planilla Simplificada de Sueldos"                                                                                          Arial                                                         empresa                                                                                                                     Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         "Fecha:"                                                                                                                    Arial                                                         m.fecha                                                                                                                     Arial                                                         "Frecuencia:"                                                                                                               Arial                                                         
frecuencia                                                                                                                  Arial                                                         "Cargo/Ocup."                                                 Arial                                                         rSuelSiml.ocupacion                                                                                                         Arial                                                         rSuelsiml.IPS                                                 "999,999,999"                                                                                                               Arial                                                         rSuelsiml.IPS                                                 "999,999,999"                                                                                                               Arial                                                         rSuelsiml.IPS                                                 "999,999,999"                                                                                                               Arial                                                         "IPS"                                                         Arial                                                         rSuelsiml.Bonificacion                                        "999,999,999"                                                                                                               Arial                                                         rSuelsiml.Bonificacion                                        "999,999,999"                                                                                                               Arial                                                         rSuelsiml.Bonificacion                                        "999,999,999"                                                                                                               Arial                                                         
"Bonific."                                                    Arial                                                         total                                                         0                                                             0                                                             totalgen                                                      0                                                             0                                                             Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               |Top = 178
Left = 211
Width = 520
Height = 219
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
                                                                 �PROCEDURE Init
SELECT rh_liquidacion_base.nroliquidacion, rh_liquidacion_base.descripcion, ;
rh_liquidacion_base.fecha, rh_liquidacion_base.mes, rh_liquidacion_base.a�o, ;
rh_liquidacion_base.confirmado, rh_liquidacion_base.idfrecuencia, ;
rh_liquidet_base.idempleado, rh_liquidet_base.thi, rh_liquidet_base.thn, ;
rh_liquidet_base.th, rh_liquidet_base.td, rh_liquidet_base.tn, rh_empleado_base.nombre, ;
rh_empleado_base.apellido, rh_centro_pago.centro, rh_empleado_base.ocupacion, ;
rh_empleado_base.sueldo_bas, rh_empleado_base.centro_pag, rh_frecuencia.descripcion AS frecuencia, ;
rh_liquidet_base.IdLiquiDet;
FROM datos!rh_empleado_base, rh_frecuencia, datos!rh_centro_pago, ;
datos!rh_liquidacion_base ;
INNER JOIN datos!rh_liquidet_base;
ON rh_liquidacion_base.idliquidacion = rh_liquidet_base.idliquidacion ;
WHERE rh_liquidet_base.idempleado = rh_empleado_base.idempleado ;
AND rh_empleado_base.centro_pag = rh_centro_pago.idcentro ;
AND rh_liquidacion_base.idfrecuencia = rh_frecuencia.idfrecliqui ;
AND (rh_liquidacion_base.idempresa = oapp.empresa AND rh_empleado_base.idempresa = oapp.empresa ;
AND rh_empleado_base.centro_pag = m.centropago AND rh_liquidacion_base.fecha = m.fecha ;
AND rh_liquidacion_base.idfrecuencia = m.idfrecuencia) ;
ORDER BY rh_centro_pago.centro, rh_liquidet_base.idempleado ;
INTO CURSOR rSueldo1 NOFILTER 
SELECT rSueldo1



SET ENGINEBEHAVIOR 70
SELECT a.*,;
sum(IIF(b.IdConcepto="100",Monto,0)) as IPS,; 
sum(IIF(b.IdConcepto="200",Monto,0)) as Bonificacion;
from rSueldo1 a ;
left JOIN datos!rh_liquida_conceptos b ;
ON (a.IdLiquiDet = b.IdLiquiDet AND (b.IdConcepto="100" OR b.IdConcepto="200" ));
group BY a.Centro,a.IdEmpleado;
ORDER BY a.centro, a.idempleado ;
into cursor rsuelsiml

SELECT rsuelsiml

SET ENGINEBEHAVIOR 90
*ON (a.IdLiquiDet = b.IdLiquiDet AND (b.IdConcepto="253" OR b.IdConcepto="120" ));

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                         ����    �  �                        ��   %   �      (               �  U  ��o� datos!rh_empleado_base� rh_frecuencia� datos!rh_centro_pago� datos!rh_liquidacion_base��� datos!rh_liquidet_base ��  � � � ���  � ���  � ���  � ���  � ���  � ���  � ���  � ��� �	 ��� �
 ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � �Q� �� � ���� �	 � �	 � � � � � 	� �  � � � 	�P �  � � �  � � � � �  	� � � ��! 	� �  � �� 	� �  � �� 		���� � ��� �	 ���� rSueldo1�� F�" �
 G���F��� o� rSueldo1Q�# X�� datos!rh_liquida_conceptosQ�(  ��� �� � ��$ � 100� ��$ � 200	��# ��CC��$ � 100� �% � � 6���Q�& �CC��$ � 200� �% � � 6���Q�' ���� ����	 ����� ����	 ����	 rsuelsiml� F�) �
 G���Z�� U*  RH_LIQUIDACION_BASE NROLIQUIDACION DESCRIPCION FECHA MES A�O
 CONFIRMADO IDFRECUENCIA RH_LIQUIDET_BASE
 IDEMPLEADO THI THN TH TD TN RH_EMPLEADO_BASE NOMBRE APELLIDO RH_CENTRO_PAGO CENTRO	 OCUPACION
 SUELDO_BAS
 CENTRO_PAG RH_FRECUENCIA
 FRECUENCIA
 IDLIQUIDET DATOS IDLIQUIDACION IDCENTRO IDFRECLIQUI	 IDEMPRESA OAPP EMPRESA
 CENTROPAGO RSUELDO1 A
 IDCONCEPTO MONTO IPS BONIFICACION B	 RSUELSIML
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 0Aq � r � 4 q 2                       h     	   �  �  +    )   �                                                      �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                                          T  <   winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  Carta                                                                           DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Courier New                                                   rSuelsiml.centro_pag                                          "Basico"                                                      Arial                                                         "Nombre"                                                      Arial                                                         	"Haberes"                                                     Arial                                                         
"Periodo:"                                                                                                                  Arial                                                         totalgen                                                      "999"                                                                                                                       Arial                                                         "Total de la Empresa"                                         Arial                                                         rSuelsiml.tn                                                  "999,999,999"                                                                                                               Arial                                                         rSuelsiml.td - IPS                                            "999,999,999"                                                                                                               Arial                                                         rSuelsiml.th - Bonificacion                                   "999,999,999"                                                                                                               Arial                                                         total                                                         "999"                                                                                                                       Arial                                                         "Total del Centro de Pago"                                    Arial                                                         rSuelsiml.tn                                                  "999,999,999"                                                                                                               Arial                                                         rSuelsiml.td - IPS                                            "999,999,999"                                                                                                               Arial                                                         rSuelsiml.th - Bonificacion                                   "999,999,999"                                                                                                               Arial                                                         rSuelsiml.centro                                                                                                            Arial                                                         rSuelsiml.centro_pag                                                                                                        Arial                                                          rSuelsiml.sueldo_bas                                         "999,999,999"                                                 Arial                                                         "Neto"                                                        Arial                                                         rSuelsiml.tn                                                  "999,999,999"                                                                                                               Arial                                                         rSuelsiml.td - IPS                                            "999,999,999"                                                                                                               Arial                                                         rSuelsiml.th - Bonificacion                                   "999,999,999"                                                                                                               Arial                                                         Bidempleado,'-',alltrim(rSuelsiml.apellido) + " " +rSuelsiml.nombre                                                                                                                          Arial                                                         "Deducciones"                                                 Arial                                                         "Legajo"                                                      Arial                                                         <letrames(rSuelsiml.mes)+' de '+ alltrim(Str( rSuelsiml.a�o))                                                                                                                                Arial                                                         ""Planilla Simplificada de Sueldos"                                                                                          Arial                                                         empresa                                                                                                                     Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         "Fecha:"                                                                                                                    Arial                                                         m.fecha                                                                                                                     Arial                                                         "Frecuencia:"                                                                                                               Arial                                                         
frecuencia                                                                                                                  Arial                                                         "Cargo/Ocup."                                                 Arial                                                         rSuelSiml.ocupacion                                                                                                         Arial                                                         rSuelsiml.IPS                                                 "999,999,999"                                                                                                               Arial                                                         rSuelsiml.IPS                                                 "999,999,999"                                                                                                               Arial                                                         rSuelsiml.IPS                                                 "999,999,999"                                                                                                               Arial                                                         "IPS"                                                         Arial                                                         rSuelsiml.Bonificacion                                        "999,999,999"                                                                                                               Arial                                                         rSuelsiml.Bonificacion                                        "999,999,999"                                                                                                               Arial                                                         rSuelsiml.Bonificacion                                        "999,999,999"                                                                                                               Arial                                                         
"Bonific."                                                    Arial                                                         total                                                         0                                                             0                                                             totalgen                                                      0                                                             0                                                             Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               |Top = 178
Left = 211
Width = 520
Height = 219
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
                                                                 �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SELECT rh_liquidacion_base.nroliquidacion, rh_liquidacion_base.descripcion, ;
rh_liquidacion_base.fecha, rh_liquidacion_base.mes, rh_liquidacion_base.a�o, ;
rh_liquidacion_base.confirmado, rh_liquidacion_base.idfrecuencia, ;
rh_liquidet_base.idempleado, rh_liquidet_base.thi, rh_liquidet_base.thn, ;
rh_liquidet_base.th, rh_liquidet_base.td, rh_liquidet_base.tn, rh_empleado_base.nombre, ;
rh_empleado_base.apellido, rh_centro_pago.centro, rh_empleado_base.ocupacion, ;
rh_empleado_base.sueldo_bas, rh_empleado_base.centro_pag, rh_frecuencia.descripcion AS frecuencia, ;
rh_liquidet_base.IdLiquiDet;
FROM datos!rh_empleado_base, rh_frecuencia, datos!rh_centro_pago, ;
datos!rh_liquidacion_base ;
INNER JOIN datos!rh_liquidet_base;
ON rh_liquidacion_base.idliquidacion = rh_liquidet_base.idliquidacion ;
WHERE rh_liquidet_base.idempleado = rh_empleado_base.idempleado ;
AND rh_empleado_base.centro_pag = rh_centro_pago.idcentro ;
AND rh_liquidacion_base.idfrecuencia = rh_frecuencia.idfrecliqui ;
AND (rh_liquidacion_base.idempresa = oapp.empresa AND rh_empleado_base.idempresa = oapp.empresa ;
AND rh_empleado_base.centro_pag = m.centropago AND rh_liquidacion_base.fecha = m.fecha ;
AND rh_liquidacion_base.idfrecuencia = m.idfrecuencia) ;
ORDER BY rh_centro_pago.centro, rh_liquidet_base.idempleado ;
INTO CURSOR rSueldo1 NOFILTER 
SELECT rSueldo1



SET ENGINEBEHAVIOR 70
SELECT a.*,;
sum(IIF(b.IdConcepto="100",Monto,0)) as IPS,; 
sum(IIF(b.IdConcepto="200",Monto,0)) as Bonificacion;
from rSueldo1 a ;
left JOIN datos!rh_liquida_conceptos b ;
ON (a.IdLiquiDet = b.IdLiquiDet AND (b.IdConcepto="100" OR b.IdConcepto="200" ));
group BY a.Centro,a.IdEmpleado;
ORDER BY a.centro, a.idempleado ;
into cursor rsuelsiml

SELECT rsuelsiml

SET ENGINEBEHAVIOR 90
*ON (a.IdLiquiDet = b.IdLiquiDet AND (b.IdConcepto="253" OR b.IdConcepto="120" ));

ENDPROC
                         ����    �  �                        ��   %   �      (               �  U  
  �  � U  SETEO��o� datos!rh_empleado_base� rh_frecuencia� datos!rh_centro_pago� datos!rh_liquidacion_base��� datos!rh_liquidet_base ��  � � � ���  � ���  � ���  � ���  � ���  � ���  � ���  � ��� �	 ��� �
 ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � ��� � �Q� �� � ���� �	 � �	 � � � � � 	� �  � � � 	�P �  � � �  � � � � �  	� � � ��! 	� �  � �� 	� �  � �� 		���� � ��� �	 ���� rSueldo1�� F�" �
 G���F��� o� rSueldo1Q�# X�� datos!rh_liquida_conceptosQ�(  ��� �� � ��$ � 100� ��$ � 200	��# ��CC��$ � 100� �% � � 6���Q�& �CC��$ � 200� �% � � 6���Q�' ���� ����	 ����� ����	 ����	 rsuelsiml� F�) �
 G���Z�� U*  RH_LIQUIDACION_BASE NROLIQUIDACION DESCRIPCION FECHA MES A�O
 CONFIRMADO IDFRECUENCIA RH_LIQUIDET_BASE
 IDEMPLEADO THI THN TH TD TN RH_EMPLEADO_BASE NOMBRE APELLIDO RH_CENTRO_PAGO CENTRO	 OCUPACION
 SUELDO_BAS
 CENTRO_PAG RH_FRECUENCIA
 FRECUENCIA
 IDLIQUIDET DATOS IDLIQUIDACION IDCENTRO IDFRECLIQUI	 IDEMPRESA OAPP EMPRESA
 CENTROPAGO RSUELDO1 A
 IDCONCEPTO MONTO IPS BONIFICACION B	 RSUELSIML BeforeOpenTables,     �� InitA     ��1 q 3 0Aq � r � 3                       &         A   �      )   �                                                