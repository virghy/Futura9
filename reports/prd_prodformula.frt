  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Fax en uit-senad (desde UIT13)
OUTPUT=TS001
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=1
PRINTQUALITY=200
YRESOLUTION=200
      I  2  winspool  Fax en uit-senad (desde UIT13)  TS001                       �Fax en uit-senad (desde UIT13)   � `&   �
od    �   �    Carta                                                                                 Dfax                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Arial      Producto      Arial      Arial      Arial      Arial      Arial      +"Distribuci�n de Ingredientes por Formulas"             Arial      empresa             Arial      "Producto:"      Arial      *iif(isnull(m.producto),'Todos',m.producto)             Arial      "Ingrediente"      Arial      "Ultimo Costo"      Arial      "Costo Repos."      Arial      "Ultima Compra"      Arial      "U.M."      Arial      "Menu"      Arial      "Cant. Original"      Arial      
"Cantidad"      Arial      Producto             Arial      	Ult_Costo      "999,999,999"             Arial      
Costo_Repo      "999,999,999"             Arial      
Fecha_�lti      "@D"             Arial      unidad             Arial      Menu             Arial      CantidadOriginal      "999,999.999"             Arial      Cantidad      "999,999.999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      oLeft = 202
Top = 250
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
Name = "Dataenvironment"
     �PROCEDURE Init
If Empty(m.producto)
	m.producto=null
Endif

cmdsql = "SELECT     RTRIM(fd.IdProducto) + '-' + p.Descripcion AS Producto, RTRIM(f.IdProducto) + '-' + fp.Descripcion AS Menu, fd.CantidadOriginal,  " +;
                      " fd.Cantidad, p.Ult_Costo, p.Fecha_�lti, p.Costo_Repo,p.Unidad " +;
		" FROM         dbo.st_Producto p INNER JOIN " +;
                      " dbo.Prd_FormulaDet fd ON p.IdEmpresa = fd.IdEmpresa AND p.IdProducto = fd.IdProducto INNER JOIN " +;
  	                  " dbo.Prd_Formula f ON fd.IdFormula = f.IdFormula INNER JOIN " + ;
                      " dbo.st_Producto fp ON f.IdEmpresa = fp.IdEmpresa AND f.IdProducto = fp.IdProducto " + ;
		" WHERE p.IdEmpresa = ?oApp.Empresa and (p.IdProducto = ?m.Producto or ?m.Producto is null) " + ;                       
		" ORDER BY fd.IdProducto, RTRIM(f.IdProducto) + '-' + fp.Descripcion " 
		
IF sql(cmdSQL,"ringre") > 0
	SELECT ringre
ENDIF
		
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���    �  �                        ��   %   b      �     �          �  U  � %�C��  ��� � T��  ���� ��T� �ٍ SELECT     RTRIM(fd.IdProducto) + '-' + p.Descripcion AS Producto, RTRIM(f.IdProducto) + '-' + fp.Descripcion AS Menu, fd.CantidadOriginal,  �?  fd.Cantidad, p.Ult_Costo, p.Fecha_�lti, p.Costo_Repo,p.Unidad �+  FROM         dbo.st_Producto p INNER JOIN �a  dbo.Prd_FormulaDet fd ON p.IdEmpresa = fd.IdEmpresa AND p.IdProducto = fd.IdProducto INNER JOIN �<  dbo.Prd_Formula f ON fd.IdFormula = f.IdFormula INNER JOIN �S  dbo.st_Producto fp ON f.IdEmpresa = fp.IdEmpresa AND f.IdProducto = fp.IdProducto �[  WHERE p.IdEmpresa = ?oApp.Empresa and (p.IdProducto = ?m.Producto or ?m.Producto is null) �D  ORDER BY fd.IdProducto, RTRIM(f.IdProducto) + '-' + fp.Descripcion �� %�C � � ringre� � ���� F� � � U  PRODUCTO CMDSQL SQL RINGRE
  �  � U  SETEO Init,     �� BeforeOpenTablesM    ��1 � A �*�q A 3 q 2                       �     	   �  �      )   �                  