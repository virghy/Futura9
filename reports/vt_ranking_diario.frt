  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      acum      cantidad      0      tImporte      importe      0      tCostos      Costos      0      Arial      Arial      Arial      Arial      Arial      Arial      "Ranking de Ventas Diario"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      "Periodo::"      Arial      8iif(isnull(m.Sucursal),'Consolidado', rranking.sucursal)      Arial      "Sucursal:"      Arial      "
"      Arial      "Fecha"      Arial      "Cantidad
"      Arial      "Precio
Prom."      Arial      "Costo
Prom."      Arial      "Importe
Total"      "@I"      Arial      "Costo
Total"      "@I"      Arial      "% 
Util
"      "@I"      Arial      fecha      "@DYS"      Arial      rranking.cantidad      "999,999.99"      Arial      precio      "9,999,999.99"      Arial      Costo      "9,999,999.99"      Arial      Importe      "999,999,999.99"      Arial      Costos      "9,999,999.99"      Arial      $round((importe *100/costos) - 100,2)      
"9,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      rranking.cantidad      "999,999.99"      Arial      Importe      "9,999,999,999.99"             Arial      Costos      "9,999,999.99"      Arial      &round((tImporte *100/tCostos) - 100,2)      
"9,999.99"      Arial      	"Totales"      Arial      dataenvironment      ~Top = 75
Left = 208
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
PUBLIC total
If Empty(m.sucursal)
	m.sucursal= null
Endif


TEXT TO cmdSQL noshow
	SELECT     v.Fecha, SUM(m.Cantidad) AS Cantidad, AVG(m.Real) AS Precio, AVG(m.Costo_Pro) AS Costo, 
	                      SUM((m.Cantidad * m.Real)-(m.Cantidad * m.Real*ISNULL(m.Descuento,0)/100)) AS Importe, SUM(m.Cantidad * m.Costo_Pro) AS Costos, s.Sucursal + s.Descripci�n AS Sucursal
	FROM         vt_factura AS v INNER JOIN
	                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN
	                      sucursal AS s ON v.IdEmpresa = s.IdEmpresa AND v.Sucursal = s.Sucursal
	where v.IdEmpresa = ?oApp.Empresa and 
	v.fecha between ?m.dFecha and ?m.hFecha and
	(v.Sucursal = ?m.sucursal or ?m.sucursal is null)	                      
GROUP BY v.Fecha, s.Sucursal, s.Descripci�n
ORDER BY Sucursal, v.Fecha
ENDTEXT


sql(cmdSQL,'rranking')
SELECT rranking

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        �M   %   �      \     ,          �  U  q 7�  � %�C�� ���% � T�� ���� �	 M(� ��j �d 	SELECT     v.Fecha, SUM(m.Cantidad) AS Cantidad, AVG(m.Real) AS Precio, AVG(m.Costo_Pro) AS Costo, �� �� 	                      SUM((m.Cantidad * m.Real)-(m.Cantidad * m.Real*ISNULL(m.Descuento,0)/100)) AS Importe, SUM(m.Cantidad * m.Costo_Pro) AS Costos, s.Sucursal + s.Descripci�n AS Sucursal�. �( 	FROM         vt_factura AS v INNER JOIN�[ �U 	                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN�c �] 	                      sucursal AS s ON v.IdEmpresa = s.IdEmpresa AND v.Sucursal = s.Sucursal�- �' 	where v.IdEmpresa = ?oApp.Empresa and �2 �, 	v.fecha between ?m.dFecha and ?m.hFecha and�O �I 	(v.Sucursal = ?m.sucursal or ?m.sucursal is null)	                      �1 �+ GROUP BY v.Fecha, s.Sucursal, s.Descripci�n�  � ORDER BY Sucursal, v.Fecha� � ��C � � rranking� �� F� � U  TOTAL SUCURSAL CMDSQL SQL RRANKING
  <�  � U  TOTAL
  �  � U  SETEO Init,     �� Destroy�    �� BeforeOpenTables�    ��1 q � A � �1��1�!�A �q 3 q 3 q 2                       �        �  �        �  �      )   �                  