   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=HP DeskJet 690C Series Printer
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
DUPLEX=1
YRESOLUTION=300
TTOPTION=1
COLLATE=0
                           H  0  winspool HP DeskJet 690C Series Printer LPT1:                                           �HP DeskJet 690C Series Printer   7� ��      d   ,  ,                                                                             HP DeskJet 690C Series Printer                                  LPT1                                                                                                                                     ��	                                                 ,,                 Arial                          left( cuenta, 1 )              "Plan de Cuentas"                                             Arial                          empresa                                                       Arial                          cuenta                                                        	nivel = 1                      Arial                          "Cuenta"                       Arial                          descripci�n                                                   	nivel = 1                      Arial                          "Descripci�n"                 Arial                          "Asentable"                    Arial                          "Integradora"                  Arial                          "Nivel"                        Arial                          cuenta                                                        	nivel = 2                      Arial                          descripci�n                                                   	nivel = 2                      Arial                           iif( asentable = .T.,"Si","No" )                                                               	nivel = 2                      Arial                          integradora                                                   	nivel = 2                      Arial                          nivel                                                         	nivel = 2                      Arial                          cuenta                                                        	nivel = 3                      Arial                          descripci�n                                                   	nivel = 3                      Arial                           iif( asentable = .T.,"Si","No" )                                                               	nivel = 3                      Arial                          integradora                                                   	nivel = 3                      Arial                          nivel                                                         	nivel = 3                      Arial                          cuenta                                                        
nivel = 4                      Arial                          descripci�n                                                   	nivel = 4                      Arial                           iif( asentable = .T.,"Si","No" )                                                               	nivel = 4                      Arial                          integradora                                                   	nivel = 4                      Arial                          nivel                                                         	nivel = 4                      Arial                          cuenta                                                        	nivel = 5                      Arial                          descripci�n                                                   	nivel = 5                      Arial                           iif( asentable = .T.,"Si","No" )                                                               	nivel = 5                      Arial                          integradora                                                   	nivel = 5                      Arial                          nivel                                                         	nivel = 5                      Arial                          cuenta                                                        	nivel = 6                      Arial                          descripci�n                                                   	nivel = 6                      Arial                           iif( asentable = .T.,"Si","No" )                                                               	nivel = 6                      Arial                          integradora                                                   	nivel = 6                      Arial                          nivel                                                         	nivel = 6                      Arial                          "Rango:"                       Arial                          m.dcuenta + " al " + m.hcuenta                                                                 Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                mLeft = 1
Top = 220
Width = 520
Height = 200
InitialSelectedAlias = "rcuentas"
Name = "Dataenvironment"
                     cursor                         �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "rcuentas"
Database = ..\data\datos.dbc
CursorSource = "rcuentas"
Name = "Cursor1"
q��9�����P��Έ