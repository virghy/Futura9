  l                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Lista de Precios"      Arial      alltrim( empresa )             Arial      	m.idLista             Arial      "Lista de Precio:"      Arial      "
"      Arial      "Precio"             Arial      
"Producto"      Arial      "Codigo"      Arial      descripcion      Arial      
Idproducto             Arial      precio      "999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     
PROCEDURE Init
IF EMPTY(m.Familia)
	m.Familia= null
ENDIF

IF EMPTY(m.Linea)
	m.Linea= null
ENDIF
		

IF EMPTY(m.Sucursal)
	TEXT TO cmdSQL noshow
		SELECT     CONVERT(CHAR(3),fam.IdFamilia) + '-' + fam.Descripcion AS Familia, CONVERT(CHAR(3),lin.IdLinea)+ '-' + lin.Descripcion AS Linea, prod.IdProducto, prod.Descripcion, Fecha_�lti AS FechaCosto, 
		                      prod.Ult_Costo AS Costo, pre.Precio, CASE WHEN pre.precio > 0 THEN ROUND(prod.Ult_Costo / pre.Precio, 2) ELSE 0 END AS Ratio
		FROM         dbo.st_Producto prod LEFT OUTER JOIN
		                      dbo.st_Linea lin ON prod.IdEmpresa = lin.IdEmpresa AND prod.Linea = lin.IdLinea LEFT OUTER JOIN
		                      dbo.st_Familia fam ON prod.IdEmpresa = fam.IdEmpresa AND prod.Familia = fam.IdFamilia LEFT OUTER JOIN
		                      dbo.vt_Precios pre ON prod.IdEmpresa = pre.IdEmpresa AND prod.IdProducto = pre.IdProducto
		WHERE     (prod.IdTipo in('P','S')) and Activo = 1 and Prod.IdEmpresa= ?oApp.Empresa and 
		(Prod.Familia = ?m.Familia or ?m.Familia is null) and 
		(Prod.Linea = ?m.Linea or ?m.Linea is null) 
		and pre.IdLista = ?m.IdLista
		Order by prod.Descripcion
		
	ENDTEXT
ELSE
	TEXT TO cmdSQL noshow
		SELECT     CONVERT(CHAR(3),fam.IdFamilia) + '-' + fam.Descripcion AS Familia, CONVERT(CHAR(3),lin.IdLinea)+ '-' + lin.Descripcion AS Linea, prod.IdProducto, prod.Descripcion, Fecha_�lti AS FechaCosto, 
		                      prod.Ult_Costo AS Costo, pre.Precio, CASE WHEN pre.precio > 0 THEN ROUND(prod.Ult_Costo / pre.Precio, 2) ELSE 0 END AS Ratio
		FROM         dbo.st_Producto prod LEFT OUTER JOIN
		                      dbo.st_Linea lin ON prod.IdEmpresa = lin.IdEmpresa AND prod.Linea = lin.IdLinea LEFT OUTER JOIN
		                      dbo.st_Familia fam ON prod.IdEmpresa = fam.IdEmpresa AND prod.Familia = fam.IdFamilia LEFT OUTER JOIN
		                      dbo.vt_Precios pre ON prod.IdEmpresa = pre.IdEmpresa AND prod.IdProducto = pre.IdProducto
		WHERE     (prod.IdTipo in('P','S')) and Activo = 1 and Prod.IdEmpresa= ?oApp.Empresa and 
		(Prod.Familia = ?m.Familia or ?m.Familia is null) and 
		(Prod.Linea = ?m.Linea or ?m.Linea is null)
		and (isnull(prod.FiltraSucursal,0)=0 or exists(Select idproducto from st_ProductoSucursal s where prod.IdEmpresa=s.IdEmpresa and prod.IdProducto=s.IdProducto and s.Sucursal=?m.Sucursal)) 
		and pre.IdLista = ?m.IdLista
		Order by prod.Descripcion
		
	ENDTEXT
ENDIF 


SQL(cmdSQL,'cLista')
SELECT cLista


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     :���    !  !                        �\   %   H
      �
  ,   p
          �  U  �	 %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � %�C�� �����	 M(� ��� �� 		SELECT     CONVERT(CHAR(3),fam.IdFamilia) + '-' + fam.Descripcion AS Familia, CONVERT(CHAR(3),lin.IdLinea)+ '-' + lin.Descripcion AS Linea, prod.IdProducto, prod.Descripcion, Fecha_�lti AS FechaCosto, �� �� 		                      prod.Ult_Costo AS Costo, pre.Precio, CASE WHEN pre.precio > 0 THEN ROUND(prod.Ult_Costo / pre.Precio, 2) ELSE 0 END AS Ratio�9 �3 		FROM         dbo.st_Producto prod LEFT OUTER JOIN�} �w 		                      dbo.st_Linea lin ON prod.IdEmpresa = lin.IdEmpresa AND prod.Linea = lin.IdLinea LEFT OUTER JOIN�� �} 		                      dbo.st_Familia fam ON prod.IdEmpresa = fam.IdEmpresa AND prod.Familia = fam.IdFamilia LEFT OUTER JOIN�w �q 		                      dbo.vt_Precios pre ON prod.IdEmpresa = pre.IdEmpresa AND prod.IdProducto = pre.IdProducto�a �[ 		WHERE     (prod.IdTipo in('P','S')) and Activo = 1 and Prod.IdEmpresa= ?oApp.Empresa and �> �8 		(Prod.Familia = ?m.Familia or ?m.Familia is null) and �4 �. 		(Prod.Linea = ?m.Linea or ?m.Linea is null) �$ � 		and pre.IdLista = ?m.IdLista�! � 		Order by prod.Descripcion� � 		� � ��	�	 M(� ��� �� 		SELECT     CONVERT(CHAR(3),fam.IdFamilia) + '-' + fam.Descripcion AS Familia, CONVERT(CHAR(3),lin.IdLinea)+ '-' + lin.Descripcion AS Linea, prod.IdProducto, prod.Descripcion, Fecha_�lti AS FechaCosto, �� �� 		                      prod.Ult_Costo AS Costo, pre.Precio, CASE WHEN pre.precio > 0 THEN ROUND(prod.Ult_Costo / pre.Precio, 2) ELSE 0 END AS Ratio�9 �3 		FROM         dbo.st_Producto prod LEFT OUTER JOIN�} �w 		                      dbo.st_Linea lin ON prod.IdEmpresa = lin.IdEmpresa AND prod.Linea = lin.IdLinea LEFT OUTER JOIN�� �} 		                      dbo.st_Familia fam ON prod.IdEmpresa = fam.IdEmpresa AND prod.Familia = fam.IdFamilia LEFT OUTER JOIN�w �q 		                      dbo.vt_Precios pre ON prod.IdEmpresa = pre.IdEmpresa AND prod.IdProducto = pre.IdProducto�a �[ 		WHERE     (prod.IdTipo in('P','S')) and Activo = 1 and Prod.IdEmpresa= ?oApp.Empresa and �> �8 		(Prod.Familia = ?m.Familia or ?m.Familia is null) and �3 �- 		(Prod.Linea = ?m.Linea or ?m.Linea is null)�� �� 		and (isnull(prod.FiltraSucursal,0)=0 or exists(Select idproducto from st_ProductoSucursal s where prod.IdEmpresa=s.IdEmpresa and prod.IdProducto=s.IdProducto and s.Sucursal=?m.Sucursal)) �$ � 		and pre.IdLista = ?m.IdLista�! � 		Order by prod.Descripcion� � 		� � � ��C � � cLista� �� F� � U  FAMILIA LINEA SUCURSAL CMDSQL SQL CLISTA
  �  � U  SETEO Init,     �� BeforeOpenTables3
    ��1 � A � A � �	��1q�AA� A � � �	��1q�11A� A A sq 4 q 2                       �	     *   �	  
  2    )   !                  