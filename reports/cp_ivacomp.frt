  Б                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 DRIVER=winspool
DEVICE=HP DeskJet 840C
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=1
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=1
      8  !  winspool HP DeskJet 840C LPT1:                        дHP DeskJet 840C                   @ n          X  X                                                                              @ MSUD&HP DeskJet 850C                        5      d           Arial      sucursal      Arial      Arial      Arial      Arial      Arial      Arial      titulo             Arial      empresa             Arial      rperэodo             Arial      
"Perэodo:"      Arial      " Fecha"      Arial      
" Factura"      Arial      " RUC"      Arial      " Proveedor"      Arial      
"Gravadas"      Arial      	"Exentas"      Arial      	"    Iva"      Arial      "Total"      Arial      desc_sucursal             Arial      "Sucursal:"      Arial      fecha             Arial      factura             Arial      alltrim( ruc)             Arial      len( ALLTRIM(ruc) ) > 8      	proveedor             Arial      ROUND( gravada * cotizacion,0)      "999,999,999"             Arial      ROUND(exenta  * cotizacion,0)      "999,999,999"             Arial      ROUND( iva  * cotizacion,0)      "999,999,999"             Arial      (ROUND((gravada+exenta+iva)*cotizacion,0)      "999,999,999"             Arial      "Pсg. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total General"             Arial      ROUND( gravada * cotizacion,0)      "999,999,999"             Arial      ROUND(exenta  * cotizacion,0)      "999,999,999"             Arial      ROUND( iva  * cotizacion,0)      "999,999,999"             Arial      (ROUND((gravada+exenta+iva)*cotizacion,0)      "999,999,999"             Arial      dataenvironment      JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
      cursor     _Left = 10
Top = 20
Width = 90
Height = 90
Alias = "sucursal_base"
Database = ..\data\datos.dbc
CursorSource = "sucursal_base"
Name = "Cursor1"
ТФћСЗІ5њѕъћ[Хdтћзfvцјеб№qІN^=/Л8!єKUZ6ШЋ Ѕђ^PГЌYюзЕgtЉPt ѓў2"fF+
ї^)ћбBQIџB%RЌk&8CЬѓbБЇйцм("'ё|гБѓKu!я=кЄчуЊPХљ,!Њ§*ЧЃлv	 Ма]%Z`)З_e4kLъгоQЪЬВPdОВЩyтбПт|ZјhЌPrЙ% 	ЁЬњАП:ЌQo#Cї@ќўЯФ!!iжз№7s1пoя`n~ЙI$њрgєmЃ`mЌCІfѓлБFgИнdЉЂЖКDФXж§$XВж
	КУЏpycv19w%жF&@Чc'цуЌвv=љРУцVmyUGљ!J %QЭ:ДзїњГ;о`ћMWкЎMќЩz6NPZa*ч§Ўx8ИSВТ^JњxEЩGуъ~@ЬЧu\Йj6аz+YСьд3Х,іѕIАЬдюрWУ kЗБkЛжI<їЎ^@и5NHUНкzyќ	NБ]ЉыMD.6ФРп\.уѓJ'4vаЯрЁБс>ўџ	ўџџџўџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџџ      cursor      Left = 150
Top = 20
Width = 90
Height = 90
Alias = "cp_factura"
Database = ..\data\datos.dbc
CursorSource = "cp_factura"
Name = "Cursor2"
      cursor       Left = 290
Top = 20
Width = 90
Height = 90
Alias = "cp_proveedor_base"
Database = ..\data\datos.dbc
CursorSource = "cp_proveedor_base"
Name = "Cursor3"
      cursor      Left = 10
Top = 140
Width = 90
Height = 90
Alias = "bs_monedas"
Database = ..\data\datos.dbc
CursorSource = "bs_monedas"
Name = "Cursor4"
