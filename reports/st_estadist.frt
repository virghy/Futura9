  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP LaserJet 1100
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=9
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
DUPLEX=1
YRESOLUTION=600
TTOPTION=2
      8  "  winspool HP LaserJet 1100 LPT1:                      0HP LaserJet 1100                  � �w 	 �4d   X  X   210 x 297 mm lg                                                                      H P   L a s e r J e t   1 1 0 0                                 L P T 1 :                                                                                                                                                                                                                                           p  @  @     p           �  �                                                 d                                                            '''  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  H                                                                                                                                                                                                                                                                                                   Arial      familia      familia+linea      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      alltrim( empresa )             Arial      titulo             Arial      dep�sito             Arial      "Sucursal :"      Arial      
"Per�odo:"      Arial      rper�odo             Arial      
"Familia:"      Arial      rgrupo2             Arial      "Linea:"      Arial      rgrupo3             Arial      *"Total venta en "+str(m.hfecha-m.dfecha,3)             Arial      "Prom. D�as
"      Arial      "Saldo Act.
"      Arial      "Proyectado
"      Arial      "
"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      "alltrim( familia) + " - " + grupo2             Arial      !alltrim( linea ) + " - " + grupo3             Arial      round(salida,2)      "9,999,999,999.99"             Arial      producto             Arial      descripcion             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "99,999.99"             Arial      salida      "99,999,999,999.99"             Arial      ."Totales " + alltrim( linea ) + " - " + grupo3             Arial      salida      "99,999,999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "999,999.99"             Arial      %round( salida /(m.hfecha-m.dfecha),2)      "999,999.99"             Arial      /"Totales " + alltrim( familia) + " - " + grupo2             Arial      "P�g. " + str( _pageno,3 )             Arial      
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
