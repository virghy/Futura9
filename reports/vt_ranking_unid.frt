  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      acum      cantidad      0      Arial      Arial      Arial      Arial      Arial      Arial      ,"Ranking de Ventas en Unidades por Sucursal"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      "Periodo::"      Arial      Kiif(isnull(m.Sucursal),'Consolidado',m.sucursal+" - " + rranking1.sucursal)             Arial      "Sucursal:"      Arial      "
"      Arial      "Unid.Medida
"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      "Importe
"      Arial      "Unidades
"      Arial      "Acumulado
"      Arial      " %
s/Total
"      "@I"      Arial      "% 
s/Acum.
"      "@I"      Arial      rranking.idproducto             Arial      rranking.descripcion      Arial      rranking.unidad             Arial      importe      "9,999,999,999.99"      Arial      rranking.cantidad      "9,999,999.99"      Arial      cantidad      "9,999,999.99"      Arial      Eiif(rranking.cantidad>0,round(rranking.cantidad * 100 / m.total,2),0)      "999.99"      Arial      6iif(rranking.cantidad>0,round(acum * 100 / total,2),0)      
"9,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      importe      "9,999,999,999.99"      Arial      rranking.cantidad      "9,999,999.99"      Arial      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE Init
DO seteo
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
	order by 6 desc,1

	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa

ENDTEXT


	
sql(cmdSQL,'rranking')
SELECT rranking

Sum Cantidad To m.Total

ENDPROC
     ����    �  �                        (    %   �      m     /          �  U  
  �  � U  SETEO
  <�  � U  TOTALc �  �	 7�� � %�C�� ���. � T�� ���� �	 M(� ��D �> 	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,�> �8 	sum(case when s.IdDeposito_Sal is not null then 1 else �m �g 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Importe * cotizacion) as importe,�@ �: 			sum(case when s.IdDeposito_Sal is not null then 1 else �a �[ 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad�/ �) 	FROM         dbo.vt_factura v INNER JOIN�\ �V 	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN�k �e 	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto�( �" 	WHERE v.idempresa = ?oApp.empresa�g �a 	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)�@ �: 	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal� � 	order by 6 desc,1� �  �y �s 	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa� �  � � ��C � � rranking� �� F� � K(�� �� �� U  SETEO TOTAL SUCURSAL CMDSQL SQL RRANKING CANTIDAD BeforeOpenTables,     �� DestroyA     �� InitV     ��1 q 3 q 3 q � � A � A������q�a �a A �q � 2                       &         D   S         n   �  	    )   �                  