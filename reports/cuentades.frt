  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
      H  0  winspool HP DeskJet 690C Series Printer LPT1:                        �HP DeskJet 690C Series Printer   7� ��      d   ,  ,                                                                             HP DeskJet 690C Series Printer                                  LPT1                                                                                                                                     ��	                                                 ,,        Arial      Arial      Arial      Arial      Arial      Arial      "Plan de Cuentas"             Arial      empresa             Arial      "Descripci�n
"      Arial      "Cuenta"      Arial      "Asentable"      Arial      "Nivel"      Arial      "Integradora"      Arial      descripci�n             Arial      cuenta             Arial       iif( asentable = .T.,"Si","No" )             Arial      nivel             Arial      integradora             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      yLeft = 1
Top = 220
Width = 520
Height = 200
InitialSelectedAlias = "rcuentas_descripcion"
Name = "Dataenvironment"
      cursor      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "rcuentas_descripcion"
Database = ..\data\datos.dbc
CursorSource = "rcuentas_descripcion"
Name = "Cursor1"
