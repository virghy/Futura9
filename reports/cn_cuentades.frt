  
t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Microsoft Office Document Image Writer
OUTPUT=Microsoft Document Imaging Writer Port:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
      s  :  winspool  Microsoft Office Document Image Writer  Microsoft Document Imaging Writer Port:                       ,Microsoft Office Document Imag   � � /        d   ,  ,   Letter                                                                                widm               �          �                                                                                                                     Arial      Arial      Arial      Arial      Arial      Arial      "Plan de Cuentas"             Arial      empresa             Arial      "Descripci�n
"      Arial      "Cuenta"      Arial      "Asentable"      Arial      "Nivel"      Arial      "Integradora"      Arial      	"Vigente"      Arial      descripci�n      Arial      cuenta             Arial       iif( asentable = .T.,"Si","No" )             Arial      nivel             Arial      integradora             Arial      iif( vigente = .T.,"Si","No" )             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "rcuentas_descripcion"
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE Init
sql("Select * from cn_Cuentas where IdEmpresa=?oApp.Empresa and Ejercicio=?oApp.Ejercicio order by Descripci�n",'rCuentas')
SELECT rCuentas


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    l  l                        qk   %   �                      �  U  � � ��C�i Select * from cn_Cuentas where IdEmpresa=?oApp.Empresa and Ejercicio=?oApp.Ejercicio order by Descripci�n� rCuentas�  �� F� � U  SQL RCUENTAS
  �  � U  SETEO Init,     �� BeforeOpenTables�     ��1 q 4 q 2                       �         �   �       )   l                  