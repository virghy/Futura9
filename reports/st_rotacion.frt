                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=9
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=300
TTOPTION=1
      D  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                        �HP DeskJet 692C                  � @ n� 	 ro    X  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d           Arial      familia      familia+linea      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      alltrim( empresa )             Arial      titulo             Arial      dep�sito             Arial      "Sucursal :"      Arial      
"Per�odo:"      Arial      rper�odo             Arial      
"Familia:"      Arial      rgrupo2             Arial      "Linea:"      Arial      rgrupo3             Arial      *"Total venta en "+str(m.hfecha-m.dfecha,3)             Arial      "Prom. D�as
"      Arial      "
"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      "alltrim( familia) + " - " + grupo2             Arial      !alltrim( linea ) + " - " + grupo3             Arial      producto             Arial      descripcion             Arial      round(salida,2)      "9,999,999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "9,999,999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999,999,999.99"             Arial      ."Totales " + alltrim( linea ) + " - " + grupo3             Arial      salida      "99,999,999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999,999,999.99"             Arial      /"Totales " + alltrim( familia) + " - " + grupo2             Arial      salida      "99,999,999,999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total general "             Arial      salida      "99,999,999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999,999,999.99"             Arial      dataenvironment      JLeft = 10
Top = 31
Width = 381
Height = 355
Name = "Dataenvironment"
      cursor      �Left = 10
Top = 20
Width = 90
Height = 90
Alias = "cp_factura"
Database = ..\data\datos.dbc
CursorSource = "cp_factura"
Name = "Cursor1"
      cursor      �Left = 150
Top = 20
Width = 91
Height = 90
Alias = "vt_factura"
Database = ..\data\datos.dbc
CursorSource = "vt_factura"
Name = "Cursor2"
      cursor      �Left = 10
Top = 140
Width = 90
Height = 90
Alias = "fa_detfactu"
Database = ..\data\datos.dbc
CursorSource = "fa_detfactu"
Name = "Cursor3"
      cursor      �Left = 150
Top = 140
Width = 145
Height = 90
Alias = "sucursal_base"
Database = ..\data\datos.dbc
CursorSource = "sucursal_base"
Name = "Cursor4"
      cursor      �Left = 29
Top = 222
Width = 90
Height = 141
Alias = "st_producto"
Database = ..\data\datos.dbc
CursorSource = "st_producto"
Name = "Cursor5"
      cursor      �Left = 208
Top = 66
Width = 90
Height = 157
Alias = "bs_familia"
Database = ..\data\datos.dbc
CursorSource = "bs_familia"
Name = "Cursor6"
      cursor      �Left = 238
Top = 120
Width = 90
Height = 202
Alias = "bs_linea"
Database = ..\data\datos.dbc
CursorSource = "bs_linea"
Name = "Cursor7"
