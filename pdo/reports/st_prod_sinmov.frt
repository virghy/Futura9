   p   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                                                           8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S� 	 �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             D�M      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Arial                                                         "Productos sin Movimiento"                                                                                                  Arial                                                         
"Producto"                                                    Arial                                                         
Idproducto                                                    Arial                                                         descripcion                                                                                                                 Arial                                                         ""                                                           Arial                                                         "Existencia actual"                                          Arial                                                         Cantidad                                                      "999,999.99"                                                  Arial                                                         "Descripci�n"                                                 Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
"Periodo:"                                                    Arial                                                         "Dep�sito:"                                                   Arial                                                         m.dfecha, ' al ' , m.hfecha                                                                                                 Arial                                                         Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)                                                   Arial                                                         unidad                                                                                                                      Arial                                                         "Fecha Ultimo Mov."                                         "@I"                                                          Arial                                                         fecha                                                         "@D"                                                                                                                        Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             �PROCEDURE Init
IF EMPTY(m.Deposito)
	m.deposito = null
ELSE
	sql("Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito",'xdeposito')	
ENDIF

IF EMPTY(m.hproducto)
	m.hproducto= 'ZZZ'
ENDIF


TEXT TO cmdSQL noshow
SELECT p.IdProducto,Descripcion,Unidad,Cantidad
	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s
			ON p.IdEmpresa=s.IdEmpresa AND 
				p.IdProducto = s.IdProducto 
	where p.IdEmpresa=?oApp.Empresa			
		and p.IdProducto between ?m.dproducto and ?m.hProducto
		and Cantidad > 0
		order by p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos
	 
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                    ����    �  �                        ��   %   �      <               �  U  u %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� �	 M(� ��5 �/ SELECT p.IdProducto,Descripcion,Unidad,Cantidad�b �\ 	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�( �" 			ON p.IdEmpresa=s.IdEmpresa AND �& �  				p.IdProducto = s.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto� � 		and Cantidad > 0� � 		order by p.IdProducto� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO CMDSQL SALDOS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � � QA !A � Q!�a����A rq 3 q 2                       �        �  �      )   �                  ilia �Q �K