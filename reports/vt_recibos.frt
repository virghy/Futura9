  
@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
      L  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                                �HP DeskJet 692C                  � @ n�  ro    ,  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d           Arial      vt_recibos.idcliente      Arial      Arial      Arial      Arial      "Totales de Recibos"             Arial      empresa             Arial      7iif(empty(m.sucursal),'Todos',vt_recibos.desc_sucursal)             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Cpbte."      Arial      	"Fecha
"      Arial      "Nro."      Arial      "Cuota"      Arial      	"Importe"      Arial      vt_recibos.idcliente             Arial      vt_recibos.razsocial             Arial      	"Cliente"      Arial      vt_recibos.fecha             Arial      vt_recibos.idcomprob             Arial      vt_recibos.nrocomprob             Arial      vt_recibos.cuota      "999,999,999"             Arial      vt_recibos.importe      "999,999,999"             Arial      9"Total "+  vt_recibos.idcliente+" "+ vt_recibos.razsocial             Arial      vt_recibos.importe      "999,999,999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      vt_recibos.importe      "999,999,999"             Arial      "Total General"      Arial      dataenvironment      xLeft = -20
Top = 161
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
Name = "Dataenvironment"
      cursor      �Left = 275
Top = 20
Width = 90
Height = 90
Alias = "vt_recibos"
Database = ..\data\datos.dbc
CursorSource = "vt_rrecibos"
Name = "Cursor2"
