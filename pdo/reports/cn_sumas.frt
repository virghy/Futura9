     @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC� 	 �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Arial                                                         left( cuenta, 1)                                              "Balance de Sumas y Saldo"                                                                                                  Arial                                                         cCuentas.Descripci�n                                                                                                        Arial                                                         "Cuenta"                                                      Arial                                                         "Descripci�n"                                                Arial                                                         cuenta                                                                                                                      Arial                                                         descripci�n                                                                                                                 Arial                                                         debe - haber                                                  "999,999,999,999"                                                                                                           Arial                                                         dec = 0 .and. debe > haber                                    debe - haber                                                  "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0 .and. debe > haber                                    cCuentas.Descripci�n                                                                                                        Arial                                                         haber - debe                                                  "999,999,999,999"                                                                                                           Arial                                                         dec = 0 .and. debe < haber                                    haber - debe                                                  "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0 .and. debe < haber                                    debe                                                          "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       debe                                                          "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       haber                                                         "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       haber                                                         "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       #iif( debe > haber, debe - haber, 0)                           "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       #iif( debe > haber, debe - haber, 0)                           "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       $iif( debe < haber,  haber - debe, 0)                          "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       $iif( debe < haber,  haber - debe, 0)                          "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       debe                                                          "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       debe                                                          "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       haber                                                         "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       haber                                                         "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       "SUMAS"                                                      Arial                                                         	"SALDOS"                                                     Arial                                                         "Debe"                                                       Arial                                                         "Haber"                                                       Arial                                                         "Deudor"                                                      Arial                                                         
"Acreedor"                                                    Arial                                                         "Total General"                                                                                                             Arial                                                         #iif( debe > haber, debe - haber, 0)                           "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       #iif( debe > haber, debe - haber, 0)                           "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       $iif( debe < haber,  haber - debe, 0)                          "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       $iif( debe < haber,  haber - debe, 0)                          "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       debe                                                          "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       debe                                                          "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       haber                                                         "999,999,999,999"                                                                                                           Arial                                                         dec = 0                                                       haber                                                         "9,999,999,999.99"                                                                                                          Arial                                                         dec > 0                                                       empresa                                                                                                                     Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
"Per�odo:"                                                    Arial                                                         1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                           Arial                                                         "Sucursal:"                                                   Arial                                                         *iif(m.sucursal='%','Todos',SucursalNombre)                                                                                  Arial                                                         	"Centro:"                                                     Arial                                                         ,iif(isnull(m.centro),'Todos',cCentro.Centro)                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                              �PROCEDURE Init
DO seteo
PUBLIC dec
dec = 0
IF EMPTY(m.sucursal)
     m.sucursal = '%'
ENDIF
IF EMPTY(m.centro)
	m.centro=null
ENDIF

csql = 'SELECT sucursalNombre,CuentaNombre as descripci�n, cuenta, ' + 'sum(debe) as Debe, sum(haber) as haber ' +;
 'FROM cn_vAsientos ' + ;
 'WHERE (fecha BETWEEN ?m.dfecha AND ?m.hfecha)' + ;
 ' AND sucursal like  ?m.sucursal ' +;
 ' AND (Centro =?m.centro or ?m.centro is null) ' +;
  ' AND idEmpresa = ?oApp.Empresa ' + ' AND ejercicio = ?oApp.Ejercicio ' +;
  ' GROUP BY sucursalNombre,CuentaNombre , cuenta ' + ' ORDER BY cuenta '

IF sql(csql,'rSumas') > 0
     csql = 'Select LEFT(cuenta,1) as cuenta, descripci�n  ' + 'from cn_cuentas ' + 'where idEmpresa = ?oApp.Empresa and ' + ' ejercicio = ?oApp.Ejercicio and ' + ' nivel = 1 '
     IF sql(csql,'cCuentas') > 0
          SELECT ccuentas
          INDEX ON cuenta TAG cuenta
          SELECT rsumas
          SET RELATION TO LEFT(cuenta, 1) INTO ccuentas

	IF !ISNULL(m.centro)
		sql("Select Centro=Descripci�n from centros where IdEmpresa = ?oApp.Empresa and Centro = ?m.centro",'cCentro')
	ENDIF
		


     ENDIF
     SELECT rsumas
ELSE
     RETURN .F.
ENDIF

ENDPROC
        ����    �  �                        ތ   %   ;           I          �  U  � �  � 7� � T� �� �� %�C�� ���< � T�� �� %�� � %�C�� ���^ � T�� ���� ��T� ��; SELECT sucursalNombre,CuentaNombre as descripci�n, cuenta, �' sum(debe) as Debe, sum(haber) as haber � FROM cn_vAsientos �- WHERE (fecha BETWEEN ?m.dfecha AND ?m.hfecha)�   AND sucursal like  ?m.sucursal �.  AND (Centro =?m.centro or ?m.centro is null) �  AND idEmpresa = ?oApp.Empresa �!  AND ejercicio = ?oApp.Ejercicio �/  GROUP BY sucursalNombre,CuentaNombre , cuenta �  ORDER BY cuenta �� %�C � � rSumas� � ����� T� ��. Select LEFT(cuenta,1) as cuenta, descripci�n  � from cn_cuentas �$ where idEmpresa = ?oApp.Empresa and �!  ejercicio = ?oApp.Ejercicio and �  nivel = 1 ��! %�C � � cCuentas� � ���� F� � & �� ��� � F� � G-(�C� �=��� � %�C�� �
����t ��C�] Select Centro=Descripci�n from centros where IdEmpresa = ?oApp.Empresa and Centro = ?m.centro� cCentro� �� � � F� � ��� B�-�� � U	  SETEO DEC SUCURSAL CENTRO CSQL SQL CCUENTAS CUENTA RSUMAS Init,     ��1 q q � A � A ��
q � q A"AA D q � q A 2                       �      )   �                  ntro,�M �G        dbo.c