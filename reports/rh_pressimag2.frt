  @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
centro_pag      total      0      0      totalgen      0      0      Courier New      Courier New      Arial      Arial      DATE()             Courier New      TIME()             Courier New      A"Planilla de Sueldos y Aguinaldo Simplificada (preliquidaci�n)
"      Arial      _pageno      "999"             Courier New      "Pag."      Courier New      (letrames(mes)+' de '+ alltrim(Str( a�o))             Arial      
"Periodo:"             Arial      "Legajo"      Courier New      "Nombre"      Courier New      
"Concepto"      Courier New      "Basico"      Courier New      	"Haberes"      Courier New      "Deducciones"      Courier New      "Aguinaldo"      Courier New      "Neto"      Courier New      
centro_pag             Arial      centro             Arial      
idempleado             Courier New      nombre             Courier New      	ocupacion             Courier New      
sueldo_bas      "999,999,999"             Courier New      th      "999,999,999"             Courier New      td      "999,999,999"             Courier New      
tsueldo/12      "999,999,999"             Courier New      tn+(tsueldo/12)      "999,999,999"             Courier New      total      "99,999"             Courier New      th      "999,999,999"             Courier New      td      "999,999,999"             Courier New      
tsueldo/12      "999,999,999"             Courier New      tn+(tsueldo/12)      "999,999,999"             Courier New      "Total del Centro de Pago"      Courier New      totalgen      "999"             Courier New      th      "999,999,999"             Courier New      td      "999,999,999"             Courier New      
tsueldo/12      "999,999,999"             Courier New      tn+(tsueldo/12)      "999,999,999"             Courier New      "Total de la Empresa"      Courier New      dataenvironment      oLeft = 112
Top = 79
Width = 520
Height = 219
InitialSelectedAlias = "rSuelsiml"
Name = "Dataenvironment"
     oPROCEDURE BeforeOpenTables
do seteo
ENDPROC
PROCEDURE Init
*!*	select sum(thi) as tsueldo,idempleado;
*!*	from rhistorico ;
*!*	group by 2;
*!*	into cursor xxaguinaldo


*!*	select sue.*,  tsueldo;
*!*	from rsuelsiml sue, xxaguinaldo rA;
*!*	where sue.idempleado = ra.idempleado;
*!*	order by centro_pag,sue.idempleado;
*!*	into cursor xaguinaldo


select aguinaldo

select idempleado,fecha,fechaini,fechafin,mes,a�o,total,thi,;
thn,th,td,tn,nombre,ocupacion,sueldo_bas,centro,centro_pag,;
iif(isnull(tsueldo),(sueldo_bas/12)*11,tsueldo) as tsueldo;
from aguinaldo into cursor  xaguinaldo

ENDPROC
      cursor      �Left = 150
Top = 20
Width = 96
Height = 90
Alias = "rhistorico"
Database = ..\data\datos.dbc
CursorSource = "rhistorico"
Name = "Cursor2"
      cursor      �Left = 290
Top = 20
Width = 96
Height = 90
Alias = "aguinaldo"
Database = ..\data\datos.dbc
CursorSource = "sueldo_agui"
Name = "Cursor3"
      cursor      �Left = 21
Top = 30
Width = 96
Height = 90
Alias = "rsuelsiml"
Database = ..\data\datos.dbc
CursorSource = "rsuelsiml"
Name = "Cursor4"
     >���    %  %                        ��   %   �      �     �          �  U  
  �  � U  SETEO�  F�  �� o�	 aguinaldo�� ��� ��� ��� ��� ��� ��� ��� ���	 ���
 ��� ��� ��� ��� ��� ��� ��� ��CC� �� � ��� � 6�Q� ���
 xaguinaldo� U 	 AGUINALDO
 IDEMPLEADO FECHA FECHAINI FECHAFIN MES A�O TOTAL THI THN TH TD TN NOMBRE	 OCUPACION
 SUELDO_BAS CENTRO
 CENTRO_PAG TSUELDO
 XAGUINALDO BeforeOpenTables,     �� InitA     ��1 q 2 ~ �
2                       $         ?   d      )   %                  