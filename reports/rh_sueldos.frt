  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
      D  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                        �HP DeskJet 692C                  � @ n�  ro    ,  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d               Courier New      rsueldos.idempleado      Arial      Arial      Arial      Courier New      Arial       "Planilla de Sueldos y Jornales"             Arial      empresa             Arial      m.fecha             Arial      "Fecha:"             Arial      :letrames(rSueldos.mes)+' de '+ alltrim(Str( rSueldos.a�o))             Arial      
"Periodo:"             Arial      "Legajo"      Arial      "Nombre"      Arial      
"Concepto"      Arial      "Unidad"      Arial      	"Haberes"      Arial      "Deducciones"      Arial      rsueldos.idempleado             Arial      0alltrim(rsueldos.apellido) +" "+ rsueldos.nombre             Arial      rsueldos.idconcepto             Arial      rsueldos.concepto             Arial      rsueldos.cantidad      "99.99"             Arial      )iif(rsueldos.idtipo='I',rsueldos.monto,0)      "@Z 999,999,999"             Arial      )iif(rsueldos.idtipo='I',0,rsueldos.monto)      "@Z 999,999,999"             Arial      "Total Imponible"      Arial      rsueldos.thi      "999,999,999"             Arial      rsueldos.th      "999,999,999"             Arial      rsueldos.td      "999,999,999"             Arial      "Total Legajo"      Arial      rsueldos.tn      "999,999,999"             Arial      "Neto"      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      "Total General"      Arial      )iif(rsueldos.idtipo='I',rsueldos.monto,0)      "@Z 999,999,999"             Arial      "Total Empresa"      Arial      "Total Deducciones"      Arial      )iif(rsueldos.idtipo='I',0,rsueldos.monto)      "@Z 999,999,999"             Arial      "Total Neto"      Arial      9iif(rsueldos.idtipo='I',rsueldos.monto,rsueldos.monto*-1)      "@Z 999,999,999"             Arial      dataenvironment      nLeft = 222
Top = 53
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
Name = "Dataenvironment"
      cursor      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "rsueldos"
Database = ..\data\datos.dbc
CursorSource = "rh_rsueldos"
Name = "Cursor1"
