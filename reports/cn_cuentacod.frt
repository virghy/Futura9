  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Microsoft Office Document Image Writer
OUTPUT=Microsoft Document Imaging Writer Port:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
      s  :  winspool  Microsoft Office Document Image Writer  Microsoft Document Imaging Writer Port:                       ,Microsoft Office Document Imag   � � /        d   ,  ,   Letter                                                                                widm               �          �                                                                                                                     Arial      left( cuenta, 1 )      Arial      Arial      Arial      Arial      Arial      Arial      "Plan de Cuentas"             Arial      empresa             Arial      "Rango:"      Arial      m.dcuenta + " al " + m.hcuenta             Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "Asentable"      Arial      "Nivel"      Arial      "Integradora"      Arial      "Asentable"      Arial      cuenta             Arial      	nivel = 1      descripci�n             Arial      	nivel = 1      cuenta             Arial      	nivel = 2      descripci�n             Arial      	nivel = 2       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 2      nivel             Arial      	nivel = 2      integradora             Arial      	nivel = 2      iif( Vigente = .T.,"Si","No" )             Arial      	nivel = 2      cuenta             Arial      	nivel = 3      descripci�n             Arial      	nivel = 3       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 3      nivel             Arial      	nivel = 3      integradora             Arial      	nivel = 3      iif( vigente = .T.,"Si","No" )             Arial      	nivel = 3      cuenta             Arial      
nivel = 4       descripci�n             Arial      	nivel = 4       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 4      nivel             Arial      	nivel = 4      integradora             Arial      	nivel = 4      iif( Vigente = .T.,"Si","No" )             Arial      	nivel = 4      cuenta             Arial      	nivel = 5      descripci�n             Arial      	nivel = 5       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 5      nivel             Arial      	nivel = 5      integradora             Arial      	nivel = 5      iif( Vigente = .T.,"Si","No" )             Arial      	nivel = 5      cuenta             Arial      	nivel = 6      descripci�n             Arial      	nivel = 6       iif( asentable = .T.,"Si","No" )             Arial      	nivel = 6      nivel             Arial      	nivel = 6      integradora             Arial      	nivel = 6      iif( Vigente = .T.,"Si","No" )             Arial      	nivel = 6      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      zTop = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = ""
DataSource = .NULL.
Name = "Dataenvironment"
     aPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.dCuenta) 
	m.dCuenta=''
ENDIF

IF EMPTY(m.hCuenta) 
	m.hCuenta='9'
ENDIF

sql("Select * from cn_Cuentas where IdEmpresa=?oApp.Empresa and Ejercicio=?oApp.Ejercicio and cuenta between ?m.dCuenta and ?m.hCuenta order by cuenta",'rCuentas')
SELECT rCuentas


ENDPROC
     ���    �  �                        lb   %   b      �     �          �  U  
  �  � U  SETEO�  %�C��  ���  � T��  ��  �� � %�C�� ���E � T�� �� 9�� �� ��Cّ Select * from cn_Cuentas where IdEmpresa=?oApp.Empresa and Ejercicio=?oApp.Ejercicio and cuenta between ?m.dCuenta and ?m.hCuenta order by cuenta� rCuentas� �� F� � U  DCUENTA HCUENTA SQL RCUENTAS BeforeOpenTables,     �� InitA     ��1 q 3 � A A �
q 3                       &         A   V      )   �                  