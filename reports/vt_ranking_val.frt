  [                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      acum      importe      0      Arial      Arial      Arial      Arial      Arial      Arial      +"Ranking de Ventas en Valores por Sucursal"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      Oiif(isnull(m.Sucursal),'Consolidado',m.sucursal+" - " + rranking_val1.sucursal)             Arial      "Sucursal:"      Arial      "
"      Arial      "UM
"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      "Cantidad
"      "@I"      Arial      "Importe en
Moneda Local
"      "@I"      Arial      "Acumulado
"      Arial      " %
s/Total
"      "@I"      Arial      "% 
s/Acum.
"      "@I"      Arial      
idproducto             Arial      descripcion             Arial      unidad             Arial      cantidad      "999,999.99"      Arial      importe      "999,999,999,999.99"             Arial      acum      "999,999,999,999.99"             Arial      round(importe * 100 / total,2)      "999.99"             Arial      round(acum * 100 / total,2)      "999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      cantidad      "999,999.99"      Arial      importe      "999,999,999,999.99"      Arial      dataenvironment      ~Top = 32
Left = 177
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
Release m.total
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
Public m.Total
If Empty(m.sucursal)
	m.sucursal= null
ENDIF

TEXT TO cmdSQL NOSHOW 
	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,
	sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Importe * cotizacion) as importe,
			sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad
	FROM         dbo.vt_factura v INNER JOIN
	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN
	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto
	WHERE v.idempresa = ?oApp.empresa
	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)
	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal
	order by 5 desc,1

	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa

ENDTEXT


	
sql(cmdSQL,'rranking_val')
SELECT rranking_val

Sum Importe To m.Total

ENDPROC
     ����    �  �                        V�   %   �      f     *          �  U   	 <��  � U  TOTAL
  �  � U  SETEO`	 7��  � %�C�� ���' � T�� ���� �	 M(� ��D �> 	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,�> �8 	sum(case when s.IdDeposito_Sal is not null then 1 else �m �g 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Importe * cotizacion) as importe,�@ �: 			sum(case when s.IdDeposito_Sal is not null then 1 else �a �[ 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad�/ �) 	FROM         dbo.vt_factura v INNER JOIN�\ �V 	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN�k �e 	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto�( �" 	WHERE v.idempresa = ?oApp.empresa�g �a 	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)�@ �: 	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal� � 	order by 5 desc,1� �  �y �s 	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa� �  � � ��C � � rranking_val� �� F� � K(��  �� �� U  TOTAL SUCURSAL CMDSQL SQL RRANKING_VAL IMPORTE Destroy,     �� BeforeOpenTablesC     �� InitX     ��1 � 2 q 2 � � A � A������q�a �a A �q � 2                       "         I   Q         l   �      )   �                  