  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Microsoft Office Document Image Writer
OUTPUT=Microsoft Document Imaging Writer Port:
ORIENTATION=0
PAPERSIZE=9
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=200
COLOR=2
YRESOLUTION=200
      s  :  winspool  Microsoft Office Document Image Writer  Microsoft Document Imaging Writer Port:                       ,Microsoft Office Document Imag   � � /   	     d   �   �    Letter                                                                                widm              �          �                                                                                                                     Arial      idinventario      Arial      Arial      Arial      Arial      Arial      Arial      "Resultado de Inventario"      Arial      alltrim( oApp.Nombreempresa )             Arial      m.dfecha,'al',m.hfecha      Arial      
"Periodo:"      Arial       alltrim(iddeposito)+"-"+deposito      Arial      "Dep�sito Entrada:"      Arial      "
"      Arial      obs      Arial      "Referencia:"      Arial      
"Producto"      Arial      
"Cantidad"      Arial      "Inventario"      Arial      
"Faltante"      Arial      
"Sobrante"      Arial      	"Importe"      Arial      idinventario      "@Z 999,999.99"      Arial      "Inventario Nro:"      Arial      )alltrim(idproducto) + " - " + descripcion      Arial      cantidad      "@Z 999,999.99"      Arial      
inventario      "@Z 999,999.99"      Arial      "IIF(difresultado>0,difresultado,0)      "999,999.99"      Arial      difresultado*importe      "999,999,999.99"      Arial      'IIF(difresultado<0,abs(difresultado),0)      "@Z 999,999.99"      Arial      "IIF(difresultado>0,difresultado,0)      "999,999.99"      Arial      difresultado*importe      "999,999,999.99"      Arial      'IIF(difresultado<0,abs(difresultado),0)      "@Z 999,999.99"      Arial      "Total Inventario"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "IIF(difresultado>0,difresultado,0)      "999,999.99"      Arial      difresultado*importe      "999,999,999.99"      Arial      'IIF(difresultado<0,abs(difresultado),0)      "@Z 999,999.99"      Arial      "Total General"      Arial      dataenvironment      aTop = 350
Left = 195
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     \PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.deposito)
	m.Deposito = null
ENDIF
	

TEXT TO cmdSQL noshow
SELECT     st_inventario.IdDeposito, st_inventario.IdInventario, st_inventario.Fecha, st_inventario.Obs, st_Depositos.Deposito, st_Producto.Descripcion, 
                      (st_inventariodet.Inventario- st_inventariodet.Cantidad) as difresultado, st_inventariodet.IdProducto, st_inventariodet.Cantidad, st_inventariodet.Inventario, st_inventariodet.Importe
FROM         st_Depositos INNER JOIN
                      st_inventario ON st_Depositos.IdEmpresa = st_inventario.IdEmpresa AND st_Depositos.IdDeposito = st_inventario.IdDeposito INNER JOIN
                      st_inventariodet ON st_inventario.IdInventario = st_inventariodet.IdInventario INNER JOIN
                      st_Producto ON st_Depositos.IdEmpresa = st_Producto.IdEmpresa AND st_inventariodet.IdProducto = st_Producto.IdProducto AND 
                      st_inventario.IdEmpresa = st_Producto.IdEmpresa
where st_producto.IdEmpresa=?oApp.Empresa and 
	  (st_inventario.idinventario= ?m.inventario or ?m.inventario = 0)
      and st_inventario.Fecha between ?m.dfecha and ?m.hfecha
      and (st_inventario.IdDeposito = ?m.Deposito or ?m.Deposito is null)        

            
ENDTEXT

sql(cmdSQL,'Inven')
SELECT Inven

  


ENDPROC
     =���    $  $                        �   %   s      �     �          �  U  
  �  � U  SETEO %�C��  ��� � T��  ���� �	 M(� ��� �� SELECT     st_inventario.IdDeposito, st_inventario.IdInventario, st_inventario.Fecha, st_inventario.Obs, st_Depositos.Deposito, st_Producto.Descripcion, �� ��                       (st_inventariodet.Inventario- st_inventariodet.Cantidad) as difresultado, st_inventariodet.IdProducto, st_inventariodet.Cantidad, st_inventariodet.Inventario, st_inventariodet.Importe�* �$ FROM         st_Depositos INNER JOIN�� ��                       st_inventario ON st_Depositos.IdEmpresa = st_inventario.IdEmpresa AND st_Depositos.IdDeposito = st_inventario.IdDeposito INNER JOIN�u �o                       st_inventariodet ON st_inventario.IdInventario = st_inventariodet.IdInventario INNER JOIN�� ��                       st_Producto ON st_Depositos.IdEmpresa = st_Producto.IdEmpresa AND st_inventariodet.IdProducto = st_Producto.IdProducto AND �K �E                       st_inventario.IdEmpresa = st_Producto.IdEmpresa�4 �. where st_producto.IdEmpresa=?oApp.Empresa and �I �C 	  (st_inventario.idinventario= ?m.inventario or ?m.inventario = 0)�C �=       and st_inventario.Fecha between ?m.dfecha and ?m.hfecha�W �Q       and (st_inventario.IdDeposito = ?m.Deposito or ?m.Deposito is null)        � �  � �             � � ��C � � Inven� �� F� � U  DEPOSITO CMDSQL SQL INVEN BeforeOpenTables,     �� InitA     ��1 q 3 � A � �	1��	Qq	�A�1qa !A bq 5                       &         A   Q      )   $                  