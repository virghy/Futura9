  !g                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=hp LaserJet 1150
OUTPUT=hpLaserJet1150
ORIENTATION=1
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
      T  $   winspool  hp LaserJet 1150  hpLaserJet1150                                       hp LaserJet 1150                !@� d߀ 	 �4d   ��                                                                                         B�e�               �� ��             4  �  d  	                                                                                                                                     A r i a l                                                       ��� H   �      B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   L a s e r J e t   1 1 5 0 , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Politica de Inventario"      Arial      alltrim( empresa )             Arial      m.hfecha             Arial      "Hasta Fecha:"      Arial      Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)      Arial      "Dep�sito:"      Arial      "
"      Arial      "Rotaci�n 30 Dias
"      Arial      
"Minimo
"      Arial      "Existencia
"      Arial      "Requerido
"      Arial      "Stock en Otro Deposito
"      Arial      "Ult. Costo"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      "Ult. Compra"      Arial      "Vendido
"      Arial      "Dias
"      Arial      
Idproducto      Arial      descripcion      Arial      unidad      Arial      
Fecha_�lti      "@D"      Arial      	Ult_Costo      "99,999,999.99"      Arial      
Stock_Mini      "999,999.99"      Arial      Cantidad      "999,999.99"      Arial      abs(Cantidad - Stock_Mini)      "999,999.99"      Arial      abs(Vendido)      "999,999.99"      Arial      &round( (30*Cantidad)/(abs(Vendido)),2)      "999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     sPROCEDURE BeforeOpenTables
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
SELECT p.IdProducto,Descripcion,Unidad,s.Cantidad, Stock_mini, Fecha_�lti, Ult_Costo, Vendido=SUM(m.Cantidad)
	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s
			ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto 
				left join (Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Ent,
					Cantidad = Sum(Cantidad)
					from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura
					where 
						IdDeposito_Ent is not null and 
						d.IdEmpresa = ?oApp.Empresa and 
						m.Fecha between ?m.dFecha and ?m.hFecha and
						(IdDeposito_Ent = ?m.deposito or ?m.deposito is null)
					group by d.IdEmpresa,IdProducto,IdDeposito_Ent 
					union
					Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Sal,
					Cantidad = Sum(Cantidad*-1)
					from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura
					where IdDeposito_Sal is not null and 
						d.IdEmpresa = ?oApp.Empresa and 
						m.Fecha between ?m.dFecha and ?m.hFecha  and
						(IdDeposito_Sal = ?m.Deposito or ?m.Deposito is null)
					group by d.IdEmpresa,IdProducto,IdDeposito_Sal) m
			ON p.IdEmpresa=m.IdEmpresa AND p.IdProducto = m.IdProducto 
	where p.IdEmpresa=?oApp.Empresa			
		and p.IdProducto between ?m.dproducto and ?m.hProducto
		and s.Cantidad <> 0
		and ISNULL(Stock_mini,0)>0
		and ISNULL(Stock_mini,0)>s.Cantidad
	group by p.IdProducto,Descripcion,Unidad,s.Cantidad, Stock_mini, Fecha_�lti, Ult_Costo	
		order by p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos
	 
ENDPROC
     ����    �  �                        mD   %   �      T  -   �          �  U  
  �  � U  SETEOc %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� �	 M(� ��s �m SELECT p.IdProducto,Descripcion,Unidad,s.Cantidad, Stock_mini, Fecha_�lti, Ult_Costo, Vendido=SUM(m.Cantidad)�b �\ 	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�D �> 			ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto �O �I 				left join (Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Ent,�# � 					Cantidad = Sum(Cantidad)�X �R 					from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura� � 					where �+ �% 						IdDeposito_Ent is not null and �, �& 						d.IdEmpresa = ?oApp.Empresa and �7 �1 						m.Fecha between ?m.dFecha and ?m.hFecha and�A �; 						(IdDeposito_Ent = ?m.deposito or ?m.deposito is null)�: �4 					group by d.IdEmpresa,IdProducto,IdDeposito_Ent � �
 					union�E �? 					Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Sal,�& �  					Cantidad = Sum(Cantidad*-1)�X �R 					from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura�0 �* 					where IdDeposito_Sal is not null and �, �& 						d.IdEmpresa = ?oApp.Empresa and �8 �2 						m.Fecha between ?m.dFecha and ?m.hFecha  and�A �; 						(IdDeposito_Sal = ?m.Deposito or ?m.Deposito is null)�< �6 					group by d.IdEmpresa,IdProducto,IdDeposito_Sal) m�D �> 			ON p.IdEmpresa=m.IdEmpresa AND p.IdProducto = m.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto� � 		and s.Cantidad <> 0�" � 		and ISNULL(Stock_mini,0)>0�+ �% 		and ISNULL(Stock_mini,0)>s.Cantidad�^ �X 	group by p.IdProducto,Descripcion,Unidad,s.Cantidad, Stock_mini, Fecha_�lti, Ult_Costo	� � 		order by p.IdProducto� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO CMDSQL SALDOS BeforeOpenTables,     �� InitA     ��1 q 3 � � QA !A � 1!A�1���q�Qa����A���!���A rq 2                       &         A   h      )   �                  