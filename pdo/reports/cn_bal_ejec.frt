  d   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=HP DeskJet 840C
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=5
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
       <  !  winspool HP DeskJet 840C LPT1:                                                           �HP DeskJet 840C                  � @ n�          ,  ,                                                                              @ MSUD&HP DeskJet 850C                 �       6      d                      Arial                          1"Balance Comparativo de Ejecuci�n Presupuestaria"                                              Arial                          	m.empresa                                                     Arial                          "Cuenta"                       Arial                          "Descripci�n"                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          "Presupuestado"                "@J"                                                          Arial                          saldos.mes2                    "999,999,999,999"                                             Arial                          "Ejecutado"                    "@J"                                                          Arial                          	ejecutado                      "999,999,999,999"                                             Arial                          "Saldo"                        "@J"                                                          Arial                          mes2 -  ejecutado              "999,999,999,999"                                             Arial                          Eiif(empty(m.centro),"Todos",m.centro + " - " +  xxcentro.descripci�n)                                                           Arial                          "Centro Costo:"                Arial                          %space(saldos.nivel*3) + saldos.cuenta                                                          Arial                          "%"                            "@I"                                                          Arial                          v1                             "99,999.99"                                                   Arial                          *space(saldos.nivel*3) + saldos.descripci�n                                                     Arial                          &dtoc(m.dfecha) +" al " +dtoc(m.hfecha)                                                         Arial                          
"Periodo:"                     Arial                          totmes                         "999,999,999,999"                                             Arial                          totejecutado                   "999,999,999,999"                                             Arial                          totmes -  totejecutado         "999,999,999,999"                                             Arial                          (totmes*100/ totejecutado)-100                                  "99,999.99"                                                   Arial                          "Total General"                "@J"                                                          Arial                          estado                         &val(NewID('_'+substr(cuenta,1,1),.f.))                          0                              	ejecutado                      saldos.mes1 * estado           0                              v1                             ?iif( ejecutado = 0 or mes2 = 0 , 0, (mes2*100/ejecutado) - 100)                                  0                              totejecutado                   iif(asentable,ejecutado,0)                                      0                              totmes                         iif(asentable,mes2,0)          0                              Arial                          Arial                          Arial                          Arial                          dataenvironment                kLeft = 20
Top = 66
Width = 792
Height = 419
InitialSelectedAlias = "saldos"
Name = "Dataenvironment"
                      ����    �  �                        ��   %   �                          �  U  
  �  � U  SETEO? G(� datos� T� �C��]�� %�C�� �
��v �F o� centros_base�� ���� � �� � � � � � 	���� xxcentro� �o� datos!asientos_baseQ� � datos!detalle� datos!cuentas��	 �
 �Q� ��	 � ��C�	 � �	 � ���Q� ��� � � C� � � �	 � 	� � � � � 	� �	 �
 � �
 	� �	 � �� 	� C�	 �
 �� �� �	� C� � �� �� �	� C� � g�	�������� xsaldo1�� o� datos!presupuestoQ� � datos!cuentas�� �
 �Q�
 �� � �Q� �� �Q� �� � ��� � �Q� �C� � ���Q� ��� �
 � �
 � � � �� 	� C� �
 �� �� �	� CC� �  � �! �$�� �� �	�������� xsaldo2�7 o� xsaldo1Y�� xsaldo2 �� � �" �
 �Ǽ�� xsaldo� F�# � #)� +�C+
���� F�$ � � T�% �� cuentaCC�# � Z���oreplace proceso with xproceso, cuenta with xsaldo.cuenta, &cuentaNivel with xSaldo.Cuenta, integradora with xsaldo.integradora, mes1 with iif(isnull(xsaldo.mes1),0,xsaldo.mes1), mes2 with xsaldo.mes2, nivel with xsaldo.nivel, dec with 0, descripci�n with xsaldo.descripci�n, asentable with xsaldo.asentable, centro with iif(isnull(xsaldo.centro),"",xsaldo.centro)
 T�& ��# � �� +�C�& �
���� F� � -��& � �
 �� %�C4���� F�$ � E�� � �
 �� T�% �� cuentaCC� � Z��� %�C4
��r� � >�' ��� �� >�
 ��� �
 ��, replace &CuentaNivel with cuentas.cuenta
 >� ��� � �� >�( ��� �� >� ��� � �� >� ��� � �� >� ��� � �� �8 >� ��� CC�# � �� � � �# � 6�� ��� �# � �� T�& �� � �� ��� !� � %�C�& ����� !� � � F�# � H� � F� �% G(�� � �� � � � � � 	�� F�$ � G(�� �� �� U)  DATOS XPROCESO CENTRO DESCRIPCI�N CENTROS_BASE	 IDEMPRESA OAPP EMPRESA XXCENTRO DETALLE CUENTA PCUENTA DEBE HABER MES1 ASIENTOS TIPO	 IDASIENTO CUENTAS DCUENTA HCUENTA FECHA DFECHA HFECHA N�MERO XSALDO1 PR INTEGRADORA NIVEL	 ASENTABLE IMPORTE MES2 A�O MES XSALDO2 XSALDO SALDOS CUENTANIVEL XINTEGRADORA PROCESO DEC
  <�  � U  CABEZA BeforeOpenTables,     �� InitA     �� Destroy�    ��1 q 2 � "cA �t~ Q � q Q ��q � q �� Q � �� B �� A A � A A A q A A q Qq 3 q 1  )   �                                                cursor                        �Left = 11
Top = 20
Width = 96
Height = 90
Alias = "saldos"
BufferModeOverride = 5
Order = "saldos_01"
Database = ..\data\datos.dbc
CursorSource = "saldos"
Name = "Cursor1"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            cursor                        CComment = "centros_base.centro = m.centro and centros_Base.idempresa = oApp.Empresa"
Left = 113
Top = 156
Width = 96
Height = 92
Alias = "centros_base"
Database = ..\data\datos.dbc
CursorSource = "centros_base"
Filter = "centros_base.centro = m.centro and centros_Base.idempresa = oApp.Empresa"
Name = "Cursor10"
                                      relation                       {ParentAlias = "saldos"
RelationalExpr = "centro"
ChildAlias = "centros_base"
ChildOrder = "centro"
Name = "Relation1"
�