   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
                            D  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                                               �HP DeskJet 692C                  � @ n�  ro    ,  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d                      Courier New                    rh_rcompl_empleado.idempleado                                   "Nombre"                       Arial                          "Legajo"                       Arial                          
"Concepto"                     Arial                          rh_rcompl_empleado.idconcepto                                                                  Arial                          rh_rcompl_empleado.idempleado                                                                  Arial                          rh_rcompl_empleado.total       "@Z 999,999,999"                                              Arial                          rh_rcompl_empleado.concepto                                                                    Arial                          Dalltrim(rh_rcompl_empleado.apellido) +" "+ rh_rcompl_empleado.nombre                                                            Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          '"Deducciones y Beneficios por Empleado"                                                        Arial                          empresa                                                       Arial                          
"Periodo:"                                                    Arial                          m.dfecha, ' al ' , m.hfecha                                                                    Arial                          "Fecha"                        Arial                          rh_rcompl_empleado.fecha                                      Arial                          
"Aplicado"                     Arial                          "Saldo"                        Arial                          rh_rcompl_empleado.cuota       "999,999,999"                                                 Arial                          rh_rcompl_empleado.aplicado                                     "999,999,999"                                                 Arial                          m.saldo                        "999,999,999"                                                 rh_rcompl_empleado.total>0                                      Arial                          	"Importe"                      Arial                          "Cuota"                        Arial                          rh_rcompl_empleado.total       "@Z 999,999,999"                                              Arial                          rh_rcompl_empleado.aplicado                                     "999,999,999"                                                 Arial                          m.saldo                        "999,999,999"                                                 rh_rcompl_empleado.total>0                                      Arial                          "Total Concepto"               Arial                          "Nro."                         Arial                          rh_rcompl_empleado.nro                                        Arial                          m.saldo                        <iif( rh_rcompl_empleado.saldo>0, rh_rcompl_empleado.saldo,0)                                     0                              Courier New                    Arial                          Arial                          Arial                          Arial                          dataenvironment                nLeft = 222
Top = 53
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
Name = "Dataenvironment"
                    ����    �   �                         !�   %   A                           �  U  
  �  � U  SETEO BeforeOpenTables,     ��1 q 1  )   �                                                    cursor                         �Comment = ""
Tag = ""
Left = 10
Top = 20
Width = 90
Height = 90
Alias = "rh_rcompl_empleado"
Database = ..\data\datos.dbc
CursorSource = "rh_rcompl_empleado"
Name = "Cursor1"
                                               ���q�lB��nHj.�m��