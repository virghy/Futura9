  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet Plus
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=256
PAPERLENGTH=1400
PAPERWIDTH=2150
DEFAULTSOURCE=7
PRINTQUALITY=300
YRESOLUTION=300
TTOPTION=1
      D  !  winspool HP DeskJet Plus LPT1:                                    �HP DeskJet Plus                  � @ f   xf    ,  ,                                                                                 @ MSUDHP DeskJet Plus                 �             d           Courier New      
idempleado      primer      0      0      numero      1      0      par      1      0      salto      3iif( par = int( numero/2) and  par =  numero/2,1,0)      0      nsueldo      iif(tipo='I',monto,0)      0      
ndescuento      iif(tipo='I',0,monto)      0      Times New Roman      Courier New      Arial      Courier New      Arial      titulo             Courier New      "Liquidaci�n de Salarios
"      Arial      idliquid             Courier New      H'Mes de '+letrames(mes)+iif(a�o>=2000,' del ',' de ')+alltrim(str( a�o))             Arial      DATE()      "@D"             Courier New      "Nombre"      Courier New      "Legajo"      Courier New      
"Personal"      Courier New      "Sueldo/Jornal"      Courier New      nombre             Courier New      
idempleado             Courier New      
'Empleado'             Courier New      
sueldo_bas      "999,999,999"             Courier New      
"Concepto"      Courier New      "Documento"      Courier New      
"Unidades"      Courier New      	"Haberes"      Courier New      "Deducciones"      Courier New      concepto             Times New Roman      numero      "99.99"             Times New Roman      par      "99.99"             Times New Roman      .f.      salto      "99.99"             Times New Roman      .f.      cantidad      "99.99"             Times New Roman      iif(tipo='I',monto,0)      "@Z 999,999,999"             Times New Roman      iif(tipo='I',0,monto)      "@Z 999,999,999"             Times New Roman      "Aguinaldo:"      Courier New      
tsueldo/12      "99,999,999"             Times New Roman      ,"Recib� conforme de la empresa, original e "      Courier New      th+(tsueldo/12)      "999,999,999"             Times New Roman      td      "999,999,999"             Times New Roman      *"importe neto de la presente liquidaci�n."      Courier New      "Firma:"      Courier New      th+(tsueldo/12)-td      "@$ 99,999,999,999"             Times New Roman      "Neto a cobrar"      Courier New      dataenvironment      nLeft = 222
Top = 53
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
do seteo
ENDPROC
PROCEDURE Init
public titulo 
local longitud,nomcentro
select centro from datos!centrop where idcentro = m.centrop into cursor xxcentrop

*set step on
if _tally > 0
	longitud= at("(",xxcentrop.centro)-1
	if longitud =0
		longitud= len(xxcentrop.centro)
	endif
	nomcentro= substr(xxcentrop.centro,1,longitud)
	titulo = nomcentro
else
	titulo = "Agropecuaria Oro Verde S.A."
endif

select rsueldos

select sum(thi) as tsueldo,idempleado;
from rhistorico ;
group by 2;
into cursor xxaguinaldo


select sue.*,  tsueldo;
from rsueldos sue, xxaguinaldo rA;
where sue.idempleado = ra.idempleado;
into cursor xxzaguinaldo

ENDPROC
      cursor      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "rsueldos"
Database = ..\data\datos.dbc
CursorSource = "rsueldos"
Name = "Cursor1"
     0���                              �   %   n      �     �          �  U  
  �  � U  SETEO� 7�  � �� � �4 o� datos!centrop�� ���� �� ����	 xxcentrop� %�� � ��� � T� �C� (� � ��� %�� � ��� � T� �C� � >�� � T� �C� � �� \�� T�  �� �� �� �( T�  �� Agropecuaria Oro Verde S.A.�� � F� �: o�
 rhistorico�C�
 ���Q� �� �������� xxaguinaldo�R o� rsueldosQ� � xxaguinaldoQ� � ��� ���� � � � ���� xxzaguinaldo� U  TITULO LONGITUD	 NOMCENTRO CENTRO DATOS IDCENTRO CENTROP	 XXCENTROP RSUELDOS SUM THI TSUELDO
 IDEMPLEADO
 RHISTORICO XXAGUINALDO SUE RA XXZAGUINALDO BeforeOpenTables,     �� InitA     ��1 q 2 q � A�!A �� � �A r �&2                       $         ?   �      )                     