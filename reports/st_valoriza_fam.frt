   �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      familia      familia+linea      Arial      Arial      Arial      Arial      Arial      Arial      alltrim( empresa )             Arial      &"Stock Valorizado por Familia y Linea"             Arial      m.dfecha, ' al ', m.hfecha      Arial      
"Periodo:"      Arial      Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)      Arial      "Dep�sito:"      Arial      �iif(m.tipoCosto = '"Costo_Prom"', 'Costo Promedio Ponderado', iif(m.tipoCosto='"Costo_repo"','Costo de Reposici�n','Costo Ultima Compra'))      Arial      "Tipo Costo:"      Arial      	"Valor
"      Arial      
"Precio
"      Arial      	"Valor
"      Arial      "Existencia
"      Arial      	"Otros
"      Arial      "
"      Arial      "Existencia
"      Arial      "Comprado
"      Arial      "Vendido
"      Arial      "Costo Unit
"      Arial      "Utilidad
"      Arial      
"Producto"      Arial      	"Movim
"      Arial      "Inventario
"      Arial      "Unitario
"      Arial      "Mercaderia
"      Arial      "Anterior
"      Arial      
"Actual
"      Arial      Familia      Arial      Linea      Arial      
Idproducto      Arial      descripcion             Arial      unidad             Arial      7Cantidad+nvl(Vendido,0)-nvl(Comprado,0)+nvl(OtrosMov,0)      "999,999.99"      Arial      Comprado      "999,999.99"      Arial      Vendido      "999,999.99"      Arial      OtrosMov      "999,999.99"      Arial      Cantidad      "999,999.99"      Arial      costo      "999,999,999.99"      Arial      Cantidad * costo      "999,999,999,999.99"      Arial      precio      "999,999,999.99"      Arial      Cantidad*precio      "999,999,999.99"      Arial      (PrecioVenta)-(CostoVenta)      "9,999,999,999.99"      Arial      "Total " +Linea      Arial      Cantidad * costo      "999,999,999.99"      Arial      !empty(linea)      Cantidad * precio      "999,999,999.99"      Arial      !empty(linea)      (PrecioVenta)-(CostoVenta)      "9,999,999,999.99"      Arial      !empty(linea)      "Total " + Familia      Arial      Cantidad * costo      "999,999,999.99"      Arial      !empty(familia)      Cantidad * precio      "999,999,999.99"      Arial      !empty(familia)      (PrecioVenta)-(CostoVenta)      "9,999,999,999.99"      Arial      !empty(familia)      
datetime()             Arial      "P�g. " + str( _pageno,3 )             Arial      "Total General"             Arial      Cantidad * costo      "999,999,999.99"      Arial      Cantidad * precio      "999,999,999.99"      Arial      (PrecioVenta)-(CostoVenta)      "9,999,999,999.99"      Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     	+PROCEDURE BeforeOpenTables
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

IF EMPTY(m.familia)
	m.familia= null
ENDIF

IF EMPTY(m.linea)
	m.linea= null
ENDIF

*!*	DO CASE  
*!*		CASE m.tipoCosto='P'
*!*			m.Costo='"Costo_Prom"'
*!*		CASE m.tipoCosto='R'
*!*			m.Costo='"Costo_repo"'
*!*		CASE m.tipoCosto='C' 
*!*			m.Costo='"Ult_Costo"'
*!*		 
*!*	ENDCASE




TEXT TO cmdSQL NOSHOW TEXTMERGE 
SELECT p.IdProducto,p.Descripcion,Unidad,Cantidad, Familia = f.IdFamilia + f.Descripcion,Comprado,Vendido,OtrosMov,CostoVenta,PrecioVenta,
	Linea = l.IdLinea + l.Descripcion, Costo=<<m.tipoCosto>>, Precio = dbo.VT_TraerPrecio(p.IdEmpresa,p.IdProducto,'<<m.IdLista>>')
	from st_Producto p inner join (Select Idempresa,IdProducto,Sum(Cantidad) as Cantidad from dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s1 group by Idempresa,IdProducto) s 
			ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto 
			left join st_Familia f on p.IdEmpresa=f.IdEmpresa AND p.Familia = f.IdFamilia 
			left join st_Linea l on p.IdEmpresa=l.IdEmpresa AND p.Linea = l.IdLinea
			left join (Select IdProducto,
						SUM(Case Origen when 'C' then Entrada-Salida else 0 end) as Comprado,
						SUM(Case Origen  when 'V' then Salida-Entrada else 0 end) as Vendido,
						SUM(Case Origen  when 'S' then Salida-Entrada else 0 end) as OtrosMov,
						sum(Case Origen  when 'V' then Costo_Pro*(Salida-Entrada) else 0 end) as CostoVenta,
						sum(Case Origen  when 'V' then Precio * (Salida-Entrada) else 0 end) as PrecioVenta
						from [dbo].[st_MoviProducto](?oApp.Empresa,null,?m.Deposito,?m.dFecha,?m.hFecha) m
						  where m.Origen in('V','C','S') group by IdProducto) m1 on p.IdProducto = m1.IdProducto 
	where p.IdEmpresa=?oApp.Empresa			
		and p.IdProducto between ?m.dproducto and ?m.hProducto
		and (p.Familia = ?m.familia or ?m.Familia is null)
		and (p.linea = ?m.Linea or ?m.linea is null)
		and p.AfectaStock = 1
		and (Cantidad<>0 or comprado<>0 or vendido<>0 or OtrosMov<>0 )
		order by p.Familia, p.Linea, p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos




ENDPROC
     	o���    V	  V	                           %         �  +   �          �  U  
  �  � U  SETEO  %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� � %�C�� ���� � T�� ���� � %�C�� ���� T�� ���� �
 M(� `��� �� SELECT p.IdProducto,p.Descripcion,Unidad,Cantidad, Familia = f.IdFamilia + f.Descripcion,Comprado,Vendido,OtrosMov,CostoVenta,PrecioVenta,�� �� 	Linea = l.IdLinea + l.Descripcion, Costo=<<m.tipoCosto>>, Precio = dbo.VT_TraerPrecio(p.IdEmpresa,p.IdProducto,'<<m.IdLista>>')�� �� 	from st_Producto p inner join (Select Idempresa,IdProducto,Sum(Cantidad) as Cantidad from dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s1 group by Idempresa,IdProducto) s �D �> 			ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto �W �Q 			left join st_Familia f on p.IdEmpresa=f.IdEmpresa AND p.Familia = f.IdFamilia �P �J 			left join st_Linea l on p.IdEmpresa=l.IdEmpresa AND p.Linea = l.IdLinea�& �  			left join (Select IdProducto,�Q �K 						SUM(Case Origen when 'C' then Entrada-Salida else 0 end) as Comprado,�Q �K 						SUM(Case Origen  when 'V' then Salida-Entrada else 0 end) as Vendido,�R �L 						SUM(Case Origen  when 'S' then Salida-Entrada else 0 end) as OtrosMov,�` �Z 						sum(Case Origen  when 'V' then Costo_Pro*(Salida-Entrada) else 0 end) as CostoVenta,�_ �Y 						sum(Case Origen  when 'V' then Precio * (Salida-Entrada) else 0 end) as PrecioVenta�^ �X 						from [dbo].[st_MoviProducto](?oApp.Empresa,null,?m.Deposito,?m.dFecha,?m.hFecha) m�e �_ 						  where m.Origen in('V','C','S') group by IdProducto) m1 on p.IdProducto = m1.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto�: �4 		and (p.Familia = ?m.familia or ?m.Familia is null)�4 �. 		and (p.linea = ?m.Linea or ?m.linea is null)� � 		and p.AfectaStock = 1�F �@ 		and (Cantidad<>0 or comprado<>0 or vendido<>0 or OtrosMov<>0 )�1 �+ 		order by p.Familia, p.Linea, p.IdProducto� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO FAMILIA LINEA CMDSQL SALDOS BeforeOpenTables,     �� InitA     ��1 q 3 � � QA !A � A � A � 	aAqa!��Q���A�aA rq 5                       &         A    	      )   V	                  