  d                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
      H  0  winspool HP DeskJet 690C Series Printer LPT1:                        �HP DeskJet 690C Series Printer   7� ��      d   ,  ,                                                                             HP DeskJet 690C Series Printer                                  LPT1                                                                                                                                     ��	                                                 ,,        Arial      left( cuenta, 1 )      Arial      Arial      Arial      Arial      Arial      Arial      "Plan de Cuentas"             Arial      empresa             Arial      "Rango:"      Arial      m.dcuenta + " al " + m.hcuenta             Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "Asentable"      Arial      "Nivel"      Arial      "Integradora"      Arial      cuenta             Arial      	nivel = 1      descripci�n             Arial      	nivel = 1      cuenta             Arial      	nivel = 2      descripci�n             Arial      	nivel = 2       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 2      nivel             Arial      	nivel = 2      integradora             Arial      	nivel = 2      cuenta             Arial      	nivel = 3      descripci�n             Arial      	nivel = 3       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 3      nivel             Arial      	nivel = 3      integradora             Arial      	nivel = 3      cuenta             Arial      
nivel = 4       descripci�n             Arial      	nivel = 4       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 4      nivel             Arial      	nivel = 4      integradora             Arial      	nivel = 4      cuenta             Arial      	nivel = 5      descripci�n             Arial      	nivel = 5       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 5      nivel             Arial      	nivel = 5      integradora             Arial      	nivel = 5      cuenta             Arial      	nivel = 6      descripci�n             Arial      	nivel = 6       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 6      nivel             Arial      	nivel = 6      integradora             Arial      	nivel = 6      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      mLeft = 1
Top = 220
Width = 520
Height = 200
InitialSelectedAlias = "rcuentas"
Name = "Dataenvironment"
      cursor      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "rcuentas"
Database = ..\data\datos.dbc
CursorSource = "rcuentas"
Name = "Cursor1"
