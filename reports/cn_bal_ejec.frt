  <                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 840C
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=5
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
      <  !  winspool HP DeskJet 840C LPT1:                            �HP DeskJet 840C                  � @ n�          ,  ,                                                                              @ MSUD&HP DeskJet 850C                 �       6      d           Arial      estado      &val(NewID('_'+substr(cuenta,1,1),.f.))      0      	ejecutado      saldos.mes1 * estado      0      v1      ?iif( ejecutado = 0 or mes2 = 0 , 0, (mes2*100/ejecutado) - 100)      0      totejecutado      iif(asentable,ejecutado,0)      0      totmes      iif(asentable,mes2,0)      0      Arial      Arial      Arial      Arial      1"Balance Comparativo de Ejecuci�n Presupuestaria"             Arial      	m.empresa             Arial      Eiif(empty(m.centro),"Todos",m.centro + " - " +  xxcentro.descripci�n)             Arial      "Centro Costo:"      Arial      &dtoc(m.dfecha) +" al " +dtoc(m.hfecha)             Arial      
"Periodo:"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "Presupuestado"      "@J"             Arial      "Saldo"      "@J"             Arial      "%"      "@I"             Arial      "Ejecutado"      "@J"             Arial      saldos.mes2      "999,999,999,999"             Arial      	ejecutado      "999,999,999,999"             Arial      mes2 -  ejecutado      "999,999,999,999"             Arial      v1      "99,999.99"             Arial      %space(saldos.nivel*3) + saldos.cuenta             Arial      *space(saldos.nivel*3) + saldos.descripci�n             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      totejecutado      "999,999,999,999"             Arial      totmes -  totejecutado      "999,999,999,999"             Arial      (totmes*100/ totejecutado)-100      "99,999.99"             Arial      "Total General"      "@J"             Arial      totmes      "999,999,999,999"             Arial      dataenvironment      kLeft = 20
Top = 66
Width = 792
Height = 419
InitialSelectedAlias = "saldos"
Name = "Dataenvironment"
      cursor      �Left = 11
Top = 20
Width = 96
Height = 90
Alias = "saldos"
BufferModeOverride = 5
Order = "saldos_01"
Database = ..\data\datos.dbc
CursorSource = "saldos"
Name = "Cursor1"
      cursor     CComment = "centros_base.centro = m.centro and centros_Base.idempresa = oApp.Empresa"
Left = 113
Top = 156
Width = 96
Height = 92
Alias = "centros_base"
Database = ..\data\datos.dbc
CursorSource = "centros_base"
Filter = "centros_base.centro = m.centro and centros_Base.idempresa = oApp.Empresa"
Name = "Cursor10"
      relation      {ParentAlias = "saldos"
RelationalExpr = "centro"
ChildAlias = "centros_base"
ChildOrder = "centro"
Name = "Relation1"
