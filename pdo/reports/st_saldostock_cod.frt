   �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=hp LaserJet 1150
OUTPUT=hpLaserJet1150
ORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2970
PAPERWIDTH=2100
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=-3
COLOR=2
DUPLEX=1
TTOPTION=3
COLLATE=0
                                        T  $  winspool  hp LaserJet 1150  hpLaserJet1150                                                                           hp LaserJet 1150                !@� d߀ 	 �4d   ��                                                                                         B�e�               �� ��             4  �  d  	                                                                                                                                     A r i a l                                                       ��� H   �      B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   L a s e r J e t   1 1 5 0 , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Arial                                                         "Saldos de Stock por C�digo"                                                                                                Arial                                                         
"Producto"                                                    Arial                                                         
Idproducto                                                    Arial                                                         descripcion                                                                                                                 Arial                                                         ""                                                           Arial                                                         "Existencia"                                                 Arial                                                         Cantidad                                                      "999,999.99"                                                  Arial                                                         "Descripci�n"                                                 Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         "Hasta Fecha:"                                                Arial                                                         "Dep�sito:"                                                   Arial                                                         m.hfecha                                                                                                                    Arial                                                         Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)                                                   Arial                                                         unidad                                                        Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
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
		and p.AfectaStock = 1
		and Cantidad <> 0
		order by p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos
	 
ENDPROC
          ����    �  �                        8�   %         \     *          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� �	 M(� ��5 �/ SELECT p.IdProducto,Descripcion,Unidad,Cantidad�b �\ 	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�( �" 			ON p.IdEmpresa=s.IdEmpresa AND �& �  				p.IdProducto = s.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto� � 		and p.AfectaStock = 1� � 		and Cantidad <> 0� � 		order by p.IdProducto� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO CMDSQL SALDOS BeforeOpenTables,     �� InitA     ��1 q 3 � � QA !A � Q!�a�����A rq 2                       &         A   �      )   �                                                                  �DRIVER=winspool
DEVICE=hp LaserJet 1150
OUTPUT=hpLaserJet1150
ORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2970
PAPERWIDTH=2100
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=-3
COLOR=2
DUPLEX=1
TTOPTION=3
COLLATE=0
                                        T  $  winspool  hp LaserJet 1150  hpLaserJet1150                                                                           hp LaserJet 1150                !@� d߀ 	 �4d   ��                                                                                         B�e�               �� ��             4  �  d  	                                                                                                                                     A r i a l                                                       ��� H   �      B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   L a s e r J e t   1 1 5 0 , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Arial                                                         "Saldos de Stock por C�digo"                                                                                                Arial                                                         
"Producto"                                                    Arial                                                         
Idproducto                                                    Arial                                                         descripcion                                                                                                                 Arial                                                         ""                                                           Arial                                                         "Existencia"                                                 Arial                                                         Cantidad                                                      "999,999.99"                                                  Arial                                                         "Descripci�n"                                                 Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         "Hasta Fecha:"                                                Arial                                                         "Dep�sito:"                                                   Arial                                                         m.hfecha                                                                                                                    Arial                                                         Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)                                                   Arial                                                         unidad                                                        Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
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
		and p.IdTipo in ('P','I')
		and Cantidad <> 0
		order by p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos
	 
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
      ����    �  �                        �    %         `     .          �  U  � %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� �	 M(� ��5 �/ SELECT p.IdProducto,Descripcion,Unidad,Cantidad�b �\ 	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�( �" 			ON p.IdEmpresa=s.IdEmpresa AND �& �  				p.IdProducto = s.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto�! � 		and p.IdTipo in ('P','I')� � 		and Cantidad <> 0� � 		order by p.IdProducto� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO CMDSQL SALDOS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � � QA !A � Q!�a����A rq 3 q 2                       �        �  �      )   �                                                        