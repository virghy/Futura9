   �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
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
                                                           8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S� 	 �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             D�M      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Arial                                                         familia                                                       familia+linea                                                 ,"Rotaci�n de Inventario por Familia y Linea"                                                                                Arial                                                         
"Producto"                                                    Arial                                                         
Idproducto                                                    Arial                                                         descripcion                                                                                                                 Arial                                                         ""                                                           Arial                                                         Cantidad                                                      "999,999.99"                                                  Arial                                                         "Descripci�n"                                                 Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         Familia                                                       Arial                                                         Linea                                                         Arial                                                         
"Periodo:"                                                    Arial                                                         "Dep�sito:"                                                   Arial                                                         m.dfecha,' al ',  m.hfecha                                                                                                  Arial                                                         Jiif(empty(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)                                                                                                                  Arial                                                         unidad                                                                                                                      Arial                                                         "Dias de Rotaci�n"                                           Arial                                                         !round( (dias*stock)/(Cantidad),2)                             "999,999.99"                                                  Arial                                                         2"Total vendido en " + alltrim(Str(dias)) + " dias"            Arial                                                         "Stock Actual"                                                                                                             Arial                                                         Stock                                                         "999,999.99"                                                  Arial                                                         dias                                                          m.hfecha-m.dfecha                                             m.hfecha-m.dfecha                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             �PROCEDURE Init

IF EMPTY(m.Deposito)
	m.deposito = null
ELSE
	sql("Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito",'xdeposito')	
ENDIF

IF EMPTY(m.hproducto)
	m.hproducto= 'ZZZ'
ENDIF

IF EMPTY(m.familia)
	m.familia= null
ENDIF

IF EMPTY(m.linea)
	m.linea= null
ENDIF

TEXT TO cmdSQL noshow
SELECT p.IdProducto,p.Descripcion,Unidad,Cantidad=m.Cantidad*-1, Stock=s.Cantidad, Familia = f.IdFamilia + f.Descripcion,
	Linea = l.IdLinea + l.Descripcion
	from st_Producto p left join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s
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
			left join st_Familia f on p.IdEmpresa=f.IdEmpresa AND p.Familia = f.IdFamilia 
			left join st_Linea l on p.IdEmpresa=l.IdEmpresa AND p.Linea = l.IdLinea 
	where p.IdEmpresa=?oApp.Empresa			
		and p.IdProducto between ?m.dproducto and ?m.hProducto
		and (p.Familia = ?m.familia or ?m.Familia is null)
		and (p.linea = ?m.Linea or ?m.linea is null)
		and s.Cantidad>0
		order by p.Familia, p.Linea, p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos


TEXT TO cmdSQL noshow
Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Ent,
	Cantidad = Sum(Cantidad)
	from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura
	where 
		IdDeposito_Ent is not null and 
		d.IdEmpresa = @IdEmpresa and 
		m.Fecha <= @Fecha and
		(IdProducto = @IdProducto or @IdProducto is null) and 
		(IdDeposito_Ent = @IdDeposito or @IdDeposito is null)
	group by d.IdEmpresa,IdProducto,IdDeposito_Ent 
	union
	Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Sal,
	Cantidad = Sum(Cantidad*-1)
	from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura
	where IdDeposito_Sal is not null and 
		d.IdEmpresa = @IdEmpresa and 
		m.Fecha <= @Fecha and
		(IdProducto = @IdProducto or @IdProducto is null) and 
		(IdDeposito_Sal = @IdDeposito or @IdDeposito is null)
	group by d.IdEmpresa,IdProducto,IdDeposito_Sal
ENDTEXT

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                                               ����    �  �                        ��   %   �      V  K   �          �  U   %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� � %�C�� ���� � T�� ���� � %�C�� ���� T�� ���� �	 M(� �� �y SELECT p.IdProducto,p.Descripcion,Unidad,Cantidad=m.Cantidad*-1, Stock=s.Cantidad, Familia = f.IdFamilia + f.Descripcion,�( �" 	Linea = l.IdLinea + l.Descripcion�a �[ 	from st_Producto p left join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�B �< 	ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto �L �F 	left join (Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Ent,�# � 					Cantidad = Sum(Cantidad)�X �R 					from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura� � 					where �+ �% 						IdDeposito_Ent is not null and �, �& 						d.IdEmpresa = ?oApp.Empresa and �7 �1 						m.Fecha between ?m.dFecha and ?m.hFecha and�A �; 						(IdDeposito_Ent = ?m.deposito or ?m.deposito is null)�: �4 					group by d.IdEmpresa,IdProducto,IdDeposito_Ent � �
 					union�E �? 					Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Sal,�& �  					Cantidad = Sum(Cantidad*-1)�X �R 					from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura�0 �* 					where IdDeposito_Sal is not null and �, �& 						d.IdEmpresa = ?oApp.Empresa and �8 �2 						m.Fecha between ?m.dFecha and ?m.hFecha  and�A �; 						(IdDeposito_Sal = ?m.Deposito or ?m.Deposito is null)�< �6 					group by d.IdEmpresa,IdProducto,IdDeposito_Sal) m�D �> 			ON p.IdEmpresa=m.IdEmpresa AND p.IdProducto = m.IdProducto �W �Q 			left join st_Familia f on p.IdEmpresa=f.IdEmpresa AND p.Familia = f.IdFamilia �Q �K 			left join st_Linea l on p.IdEmpresa=l.IdEmpresa AND p.Linea = l.IdLinea �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto�: �4 		and (p.Familia = ?m.familia or ?m.Familia is null)�4 �. 		and (p.linea = ?m.Linea or ?m.linea is null)� � 		and s.Cantidad>0�1 �+ 		order by p.Familia, p.Linea, p.IdProducto� � ��C � � saldos� �� F� �	 M(� ��@ �: Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Ent,� � 	Cantidad = Sum(Cantidad)�T �N 	from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura� � 	where �' �! 		IdDeposito_Ent is not null and �% � 		d.IdEmpresa = @IdEmpresa and � � 		m.Fecha <= @Fecha and�> �8 		(IdProducto = @IdProducto or @IdProducto is null) and �= �7 		(IdDeposito_Ent = @IdDeposito or @IdDeposito is null)�6 �0 	group by d.IdEmpresa,IdProducto,IdDeposito_Ent � � 	union�A �; 	Select d.IdEmpresa,IdProducto,IdDeposito = IdDeposito_Sal,�" � 	Cantidad = Sum(Cantidad*-1)�T �N 	from st_Movimiento_Det d inner join vt_factura m on d.IdFactura = m.IdFactura�, �& 	where IdDeposito_Sal is not null and �% � 		d.IdEmpresa = @IdEmpresa and � � 		m.Fecha <= @Fecha and�> �8 		(IdProducto = @IdProducto or @IdProducto is null) and �= �7 		(IdDeposito_Sal = @IdDeposito or @IdDeposito is null)�5 �/ 	group by d.IdEmpresa,IdProducto,IdDeposito_Sal� � U  DEPOSITO SQL	 HPRODUCTO FAMILIA LINEA CMDSQL SALDOS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � � QA !A � A � A � ��!�1���q�Qa����Aq���A�A rq � �A� qQ���a� !A�Q���QA 3 q 2                       �     I   �  �  S    )   �                  �� � 5���� T� �C� condCredito�N�� ��C