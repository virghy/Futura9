  
�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
      P  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                                    �HP DeskJet 692C                  � @ n�  ro    ,  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d           Arial      vt_rec_concep.idcomprob      Arial      Arial      Arial      Arial      "Recibos por Conceptos"             Arial      empresa             Arial      :iif(empty(m.sucursal),'Todos',vt_rec_concep.desc_sucursal)             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Cliente"      Arial      	"Fecha
"      Arial      "Nro."      Arial      "Cuota"      Arial      	"Importe"      Arial      "Tipo de Comprobante."      Arial      vt_rec_concep.idcomprob             Arial      vt_rec_concep.desc_comprob             Arial      vt_rec_concep.idcliente             Arial      vt_rec_concep.razsocial             Arial      vt_rec_concep.fecha             Arial      vt_rec_concep.nrocomprob             Arial      vt_rec_concep.cuota      "999,999,999"             Arial      vt_rec_concep.importe      "999,999,999"             Arial      B"Total "+ vt_rec_concep.idcomprob+" "+  vt_rec_concep.desc_comprob             Arial      vt_rec_concep.importe      "999,999,999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      vt_rec_concep.importe      "999,999,999"             Arial      "Total General"      Arial      dataenvironment      xLeft = -20
Top = 161
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
Name = "Dataenvironment"
      cursor      �Left = 10
Top = 20
Width = 90
Height = 90
Alias = "vt_rec_concep"
Database = ..\data\datos.dbc
CursorSource = "vt_rec_concep"
Name = "Cursor1"
