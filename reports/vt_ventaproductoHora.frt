  :                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      acum      cantidad      0      tImporte      importe      0      tCostos      Costos      0      Arial      Arial      Arial      Arial      Arial      Arial      %"Ranking de Ventas de Productos/Hora"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      "Periodo::"      Arial      8iif(isnull(m.Sucursal),'Consolidado', rranking.sucursal)      Arial      "Sucursal:"      Arial      "
"      Arial      "U.M.
"      "@I"      Arial      
"Producto"      Arial      "Cantidad
"      Arial      "Precio
Prom."      Arial      "Costo
Prom."      Arial      "Importe
Total"      "@I"      Arial      "Costo
Total"      "@I"      Arial      "% 
Util
"      "@I"      Arial      producto      Arial      rranking.unidad             Arial      rranking.cantidad      "99,999.99"      Arial      precio      "99,999,999.99"      Arial      Costo      "99,999,999.99"      Arial      Importe      "999,999,999.99"      Arial      Costos      "999,999,999.99"      Arial      $round((importe *100/costos) - 100,2)      
"9,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      rranking.cantidad      "999,999.99"      Arial      Importe      "99,999,999,999.99"      Arial      Costos      "99,999,999,999"      Arial      &round((tImporte *100/tCostos) - 100,2)      
"9,999.99"      Arial      	"Totales"      Arial      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE Init
PUBLIC total
If Empty(m.sucursal)
	m.sucursal= null
Endif

IF EMPTY(m.Producto)
	m.Producto=null
ENDIF

IF EMPTY(m.Familia)
	m.Familia=null
ENDIF
	

TEXT TO cmdSQL noshow
	SELECT     RTRIM(m.IdProducto) + '-' + p.Descripcion AS Producto, p.Unidad, SUM(m.Cantidad) AS Cantidad, AVG(m.Real*v.Cotizacion) AS Precio, AVG(m.Costo_Pro) AS Costo, 
	                      SUM(m.Importe*v.Cotizacion) AS Importe, SUM(m.Cantidad * m.Costo_Pro) AS Costos, s.Sucursal + s.Descripci�n AS Sucursal
	FROM         vt_factura AS v INNER JOIN
	                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN
	                      st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN
	                      sucursal AS s ON v.IdEmpresa = s.IdEmpresa AND v.Sucursal = s.Sucursal
	where v.IdEmpresa = ?oApp.Empresa and 
	v.fecha between ?m.dFecha and ?m.hFecha and
	(v.Sucursal = ?m.sucursal or ?m.sucursal is null)
	and p.AfectaStock=1
	and (p.IDProducto = ?m.Producto or ?m.Producto is null)
	and (p.Familia = ?m.Familia or ?m.Familia is null)	
GROUP BY RTRIM(m.IdProducto) + '-' + p.Descripcion, p.Unidad, s.Sucursal, s.Descripci�n
ORDER BY Sucursal, SUM(m.Cantidad) desc, Producto
ENDTEXT


sql(cmdSQL,'rranking')
SELECT rranking

ENDPROC
     ����    y  y                        �^   %   �        "   �          �  U  
  �  � U  SETEO
  <�  � U  TOTAL� 7�  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� � %�C�� ���i � T�� ���� �	 M(� ��� �� 	SELECT     RTRIM(m.IdProducto) + '-' + p.Descripcion AS Producto, p.Unidad, SUM(m.Cantidad) AS Cantidad, AVG(m.Real*v.Cotizacion) AS Precio, AVG(m.Costo_Pro) AS Costo, �� �� 	                      SUM(m.Importe*v.Cotizacion) AS Importe, SUM(m.Cantidad * m.Costo_Pro) AS Costos, s.Sucursal + s.Descripci�n AS Sucursal�. �( 	FROM         vt_factura AS v INNER JOIN�[ �U 	                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN�u �o 	                      st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN�c �] 	                      sucursal AS s ON v.IdEmpresa = s.IdEmpresa AND v.Sucursal = s.Sucursal�- �' 	where v.IdEmpresa = ?oApp.Empresa and �2 �, 	v.fecha between ?m.dFecha and ?m.hFecha and�8 �2 	(v.Sucursal = ?m.sucursal or ?m.sucursal is null)� � 	and p.AfectaStock=1�> �8 	and (p.IDProducto = ?m.Producto or ?m.Producto is null)�: �4 	and (p.Familia = ?m.Familia or ?m.Familia is null)	�] �W GROUP BY RTRIM(m.IdProducto) + '-' + p.Descripcion, p.Unidad, s.Sucursal, s.Descripci�n�7 �1 ORDER BY Sucursal, SUM(m.Cantidad) desc, Producto� � ��C � � rranking� �� F� � U  TOTAL SUCURSAL PRODUCTO FAMILIA CMDSQL SQL RRANKING BeforeOpenTables,     �� DestroyA     �� InitV     ��1 q 3 q 3 q � A � A � A � �
A	��Q1�!�����qA �q 2                       &         D   S         n   �  	    )   y                  