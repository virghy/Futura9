  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 400
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
      6     winspool HP DeskJet 400 LPT1:                      �HP DeskJet 400                   � 0C�  �4d   ,  ,  A4                                                            ����                DINU"   0  <a�w                            	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Courier New      rsueldos.idempleado      Arial      Courier New      Courier New      Arial      DATE()             Courier New      TIME()             Courier New      _pageno      "999"             Courier New      "Pag."      Courier New      3"Planilla de Sueldos y Jornales (preliquidaci�n)
"             Arial      :letrames(rsueldos.mes)+' de '+ alltrim(Str( rsueldos.a�o))             Courier New      
"Periodo:"             Courier New      "Legajo"      Courier New      "Nombre"      Courier New      
"Concepto"      Courier New      "Unidad"      Courier New      	"Haberes"      Courier New      "Deducciones"      Courier New      rsueldos.idempleado             Courier New      rsueldos.nombre             Courier New      rsueldos.idconcepto             Courier New      rsueldos.concepto             Courier New      rsueldos.cantidad      "99.99"             Courier New      'iif(rsueldos.tipo='I',rsueldos.monto,0)      "@Z 999,999,999"             Courier New      'iif(rsueldos.tipo='I',0,rsueldos.monto)      "@Z 999,999,999"             Courier New      "Total Imponible"      Courier New      rsueldos.thi      "999,999,999"             Courier New      rsueldos.th      "999,999,999"             Courier New      rsueldos.td      "999,999,999"             Courier New      "Total Legajo"      Courier New      rsueldos.tn      "999,999,999"             Courier New      "Neto"      Courier New      "Total General"      Arial      'iif(rsueldos.tipo='I',rsueldos.monto,0)      "@Z 999,999,999"             Arial      "Total Empresa"      Arial      "Total Deducciones"      Arial      'iif(rsueldos.tipo='I',0,rsueldos.monto)      "@Z 999,999,999"             Arial      "Total Neto"      Arial      7iif(rsueldos.tipo='I',rsueldos.monto,rsueldos.monto*-1)      "@Z 999,999,999"             Arial      dataenvironment      nLeft = 222
Top = 53
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
Name = "Dataenvironment"
      /PROCEDURE BeforeOpenTables
do seteo
ENDPROC
      cursor      �Left = 10
Top = 20
Width = 96
Height = 90
Alias = "rsueldos"
Database = ..\data\datos.dbc
CursorSource = "rsueldos"
Name = "Cursor1"
      ����    �   �                         ��   %   A       a      [           �  U  
  �  � U  SETEO BeforeOpenTables,     ��1 q 1                       $       )   �                   