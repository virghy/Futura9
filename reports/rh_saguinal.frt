                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    �DRIVER=winspool
DEVICE=HP DeskJet Plus
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=1300
PAPERWIDTH=2159
DEFAULTSOURCE=7
PRINTQUALITY=300
YRESOLUTION=300
TTOPTION=1
      8  !  winspool HP DeskJet Plus LPT1:                        �HP DeskJet Plus                  � @ f   o    ,  ,                                                                                 @ MSUDHP DeskJet Plus                 �             d           Courier New      
idempleado      totg      0      0      aguin      iif( tipo = "I",tsueldo/12,0)      0      sueld      iif(tipo="I",th,0)      0      gasto      iif(tipo="E",td,0)      0      Arial      Courier New      Courier New      Arial      DATE()             Courier New      TIME()             Courier New      _pageno      "999"             Courier New      #"Planilla de Sueldos y Aguinaldo
"      Arial      "Pag."      Courier New      (letrames(mes)+' de '+ alltrim(Str( a�o))             Courier New      
"Periodo:"             Courier New      "Legajo"      Courier New      "Nombre"      Courier New      
"Concepto"      Courier New      "Unidad"      Courier New      	"Haberes"      Courier New      "Deducciones"      Courier New      
idempleado             Courier New      nombre             Courier New      
idconcepto             Courier New      concepto             Courier New      cantidad      "99.99"             Courier New      iif(tipo='I',monto,0)      "@Z 999,999,999"             Courier New      iif(tipo='I',0,monto)      "@Z 999,999,999"             Courier New      "Total Imponible"      Courier New      th      "999,999,999"             Courier New      td      "999,999,999"             Courier New      "Total Legajo"      Courier New      "Aguinaldo"      Courier New      
tsueldo/12      "999,999,999"             Courier New      tn+(tsueldo/12)      "999,999,999"             Courier New      "Neto"      Courier New      "Total General"      Arial      sueld      "999,999,999"             Courier New      "Total Empresa"      Arial      "Total Aguinaldo"      Arial      aguin      "999,999,999"             Courier New      "Total Deducciones"      Arial      gasto      "999,999,999"             Courier New      "Total Neto"      Arial      sueld+aguin-gasto      "999,999,999"             Courier New      dataenvironment      nLeft = 216
Top = 44
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
Name = "Dataenvironment"
     -PROCEDURE Init
select sum(thi) as tsueldo,idempleado;
from rhistorico ;
group by 2;
into cursor xxaguinaldo


select sue.*,  tsueldo;
from rsueldos sue, xxaguinaldo rA;
where sue.idempleado = ra.idempleado;
into cursor xxzaguinaldo

ENDPROC
PROCEDURE BeforeOpenTables
do seteo
ENDPROC
      cursor      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "rsueldos"
Database = ..\data\datos.dbc
CursorSource = "rsueldos"
Name = "Cursor1"
      cursor      �Left = 150
Top = 20
Width = 96
Height = 90
Alias = "rhistorico"
Database = ..\data\datos.dbc
CursorSource = "rhistorico"
Name = "Cursor2"
     ����    �  �                        ��   %   -      a     U          �  U  � : o�
 rhistorico�C� ���Q� �� �������� xxaguinaldo�R o� rsueldosQ� � xxaguinaldoQ� � ��� ���� � � � ���� xxzaguinaldo� U
  SUM THI TSUELDO
 IDEMPLEADO
 RHISTORICO XXAGUINALDO SUE RSUELDOS RA XXZAGUINALDO
  �  � U  SETEO Init,     �� BeforeOpenTables    ��1 �&3 q 1                       �           "      )   �                  