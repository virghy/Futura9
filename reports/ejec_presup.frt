  :                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
      L  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                                �HP DeskJet 692C                  � @ n�  ro    ,  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d           Arial      ccomparacion.idgrupopres      saldo      Viif( isnull(debe - haber), 0 , (debe - haber)* val(NewID('_'+substr(cuenta,1,1),.f.)))      0      presupuestado      Ciif(isnull( ccomparacion.presupuesto),0, ccomparacion.presupuesto)       0      tpres      presupuestado      0      tejec      saldo      0      
tgrupoPres      presupuestado      0      
tgrupoEjec      saldo      0      Arial      Arial      Arial      Arial      Arial      "Ejecuci�n Presupuestaria "             Arial      empresa             Arial      	"Periodo"      Arial      Acmonth(date(2000,m.dmes,1)) + ' a ' + cmonth(date(2000,m.hmes,1))             Arial      "Rango"      Arial      m.dcuenta + ' al ' + m.hcuenta             Arial      "Centro Costo"      Arial      m.centro             Arial             "Descripci�n
"      Arial      "Cuenta"      Arial      "Presupuestado"      Arial      "Ejecutado"      Arial      "Saldo"      Arial      "% Ejecutado"      Arial      2"Grupo Presupuestario: "+ ccomparacion.idgrupopres             Arial      cn_grupopres_base.descripcion      Arial      cuenta             Arial      descripci�n             Arial      presupuestado      "9,999,999,999.99"             Arial      dec > 0      presupuestado      "999,999,999,999"             Arial      dec = 0      saldo      "9,999,999,999.99"             Arial      dec > 0       saldo      "999,999,999,999"             Arial      dec = 0      presupuestado -  saldo      "999,999,999,999"             Arial      dec = 0      presupuestado -  saldo      "9,999,999,999.99"             Arial      dec > 0       $round(saldo *100 /  presupuestado,2)      "999,999.99 %"             Arial      presupuestado      "999,999,999,999"             Arial      dec = 0      presupuestado      "9,999,999,999.99"             Arial      dec > 0      saldo      "999,999,999,999"             Arial      dec = 0      saldo      "9,999,999,999.99"             Arial      dec > 0       presupuestado -  saldo      "9,999,999,999.99"             Arial      dec > 0       presupuestado -  saldo      "999,999,999,999"             Arial      dec = 0      &round(tGrupoEjec *100 /  tGrupoPres,2)      "999,999.99 %"             Arial      "Total Grupo"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      presupuestado      "999,999,999,999"             Arial      dec = 0      presupuestado      "9,999,999,999.99"             Arial      dec > 0      saldo      "999,999,999,999"             Arial      dec = 0      saldo      "9,999,999,999.99"             Arial      dec > 0       presupuestado -  saldo      "9,999,999,999.99"             Arial      dec > 0       presupuestado -  saldo      "999,999,999,999"             Arial      dec = 0      round(tEjec *100 /  tPres,2)      "999,999.99 %"             Arial      "Totales
"      Arial      dataenvironment      sLeft = 174
Top = 221
Width = 520
Height = 200
InitialSelectedAlias = "ccomparacion"
Name = "Dataenvironment"
      cursor      �Left = 106
Top = 22
Width = 95
Height = 90
Alias = "ccomparacion"
Database = ..\data\datos.dbc
CursorSource = "ccomparacion"
Name = "Cursor2"
      cursor      �Left = 280
Top = 25
Width = 95
Height = 90
Alias = "cn_grupopres_base"
Database = ..\data\datos.dbc
CursorSource = "cn_grupopres_base"
Filter = "idEmpresa = oApp.Empresa"
Name = "Cursor1"
      relation      �ParentAlias = "ccomparacion"
RelationalExpr = "idgrupopres"
ChildAlias = "cn_grupopres_base"
ChildOrder = "idgrupopre"
Name = "Relation1"
