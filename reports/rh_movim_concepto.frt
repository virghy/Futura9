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
      D  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                        �HP DeskJet 692C                  � @ n�  ro    ,  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d           Courier New      rh_rRecaDesc.idconcepto      Arial      Arial      Arial      Courier New      Arial      "Movimientos por Conceptos"             Arial      empresa             Arial      m.dfecha, ' al ' , m.hfecha             Arial      
"Periodo:"             Arial      6iif(empty(m.centropago),'Todos',  rh_rRecaDesc.centro)             Arial      "Centro Pago:"             Arial      "Legajo"      Arial      
"Concepto"      Arial      "Fecha"      Arial      	"Periodo"      Arial      "Nombre"             Arial      "Unidad"      Arial      	"Haberes"      Arial      "Deducciones"      Arial      rh_rrecadesc.idconcepto             Arial      rh_rrecadesc.concepto             Arial      rh_rRecaDesc.fecha             Arial      'rh_rRecaDesc.mes, '/', rh_rRecaDesc.a�o             Arial      rh_rrecadesc.idempleado             Arial      8alltrim(rh_rrecadesc.apellido) +" "+ rh_rrecadesc.nombre             Arial      rh_rrecadesc.cantidad      "99.99"             Arial      1iif(rh_rrecadesc.idtipo='I',rh_rrecadesc.monto,0)      "@Z 999,999,999,999"             Arial      1iif(rh_rrecadesc.idtipo='I',0,rh_rrecadesc.monto)      "@Z 999,999,999,999"             Arial      rh_rrecadesc.cantidad      "999,999.99"             Arial      1iif(rh_rrecadesc.idtipo='I',rh_rrecadesc.monto,0)      "@Z 999,999,999,999"             Arial      1iif(rh_rrecadesc.idtipo='I',0,rh_rrecadesc.monto)      "@Z 999,999,999,999"             Arial      "Total Concepto"      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      "Total Empresa"      Arial      1iif(rh_rrecadesc.idtipo='I',rh_rrecadesc.monto,0)      "@Z 999,999,999,999"             Arial      1iif(rh_rrecadesc.idtipo='I',0,rh_rrecadesc.monto)      "@Z 999,999,999,999"             Arial      dataenvironment      nLeft = 222
Top = 53
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
Name = "Dataenvironment"
      cursor      �Left = 60
Top = 23
Width = 90
Height = 90
Alias = "rh_rRecaDesc"
Database = ..\data\datos.dbc
CursorSource = "rh_rmovim_concepto"
Name = "Cursor2"
