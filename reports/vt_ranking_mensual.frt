  V                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      IdMoneda      acum      cantidad      0      tImporte      importe      0      tCostos      Costos      0      	tmImporte      Importe      0      tmCostos      Costos      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Ranking de Ventas Mensual"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      "Periodo::"      Arial      8iif(isnull(m.Sucursal),'Consolidado', rranking.sucursal)      Arial      "Sucursal:"      Arial      "
"      Arial      "Mes"      Arial      "Cantidad
"      Arial      "Precio
Prom."      Arial      "Costo
Prom."      Arial      "Importe
Total"      "@I"      Arial      "Costo
Total"      "@I"      Arial      "% 
Util
"      "@I"      Arial      "Moneda: ",IdMoneda      Arial      NombreMes(Mes)      Arial      Cantidad      "999,999.99"      Arial      precio      "9,999,999.99"      Arial      Costo      "9,999,999.99"      Arial      Importe      "999,999,999.99"      Arial      Costos      "999,999,999.99"      Arial      $round((importe *100/costos) - 100,2)      
"9,999.99"      Arial      rranking.cantidad      "999,999.99"      Arial      Importe      "999,999,999.99"      Arial      Costos      "9,999,999.99"      Arial      (round((tmImporte *100/tmCostos) - 100,2)      
"9,999.99"      Arial      "Total: ",IdMoneda      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      rranking.cantidad      "999,999.99"      Arial      .f.      Importe      "999,999,999.99"      Arial      .f.      Costos      "9,999,999.99"      Arial      .f.      &round((tImporte *100/tCostos) - 100,2)      
"9,999.99"      Arial      .f.      	"Totales"      Arial      .f.      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     ^PROCEDURE Init
PUBLIC total
If Empty(m.sucursal)
	m.sucursal= null
Endif
If Empty(m.Moneda)
	m.Moneda= null
Endif



TEXT TO cmdSQL noshow
	SELECT     v.IdMoneda,MONTH(v.Fecha) as Mes, SUM(m.Cantidad) AS Cantidad, AVG(m.Precio) AS Precio, AVG(m.Costo_Pro) AS Costo, 
	                      SUM(m.Importe) AS Importe, SUM(m.Cantidad * m.Costo_Pro) AS Costos, s.Sucursal + s.Descripci�n AS Sucursal
	FROM         vt_factura AS v INNER JOIN
	                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN
	                      sucursal AS s ON v.IdEmpresa = s.IdEmpresa AND v.Sucursal = s.Sucursal
	where v.IdEmpresa = ?oApp.Empresa and 
	v.fecha between ?m.dFecha and ?m.hFecha and
	(v.Sucursal = ?m.sucursal or ?m.sucursal is null)
	and (v.IdMoneda = ?m.Moneda or ?m.Moneda is null)	                      
GROUP BY v.IdMoneda, s.Sucursal, s.Descripci�n,MONTH(v.Fecha)
ORDER BY v.Idmoneda, Sucursal, month(v.Fecha)
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
     L���    3  3                        ��   %   Y      �     �          �  U  � 7�  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� �	 M(� ��� � 	SELECT     v.IdMoneda,MONTH(v.Fecha) as Mes, SUM(m.Cantidad) AS Cantidad, AVG(m.Precio) AS Precio, AVG(m.Costo_Pro) AS Costo, �� �� 	                      SUM(m.Importe) AS Importe, SUM(m.Cantidad * m.Costo_Pro) AS Costos, s.Sucursal + s.Descripci�n AS Sucursal�. �( 	FROM         vt_factura AS v INNER JOIN�[ �U 	                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN�c �] 	                      sucursal AS s ON v.IdEmpresa = s.IdEmpresa AND v.Sucursal = s.Sucursal�- �' 	where v.IdEmpresa = ?oApp.Empresa and �2 �, 	v.fecha between ?m.dFecha and ?m.hFecha and�8 �2 	(v.Sucursal = ?m.sucursal or ?m.sucursal is null)�O �I 	and (v.IdMoneda = ?m.Moneda or ?m.Moneda is null)	                      �C �= GROUP BY v.IdMoneda, s.Sucursal, s.Descripci�n,MONTH(v.Fecha)�3 �- ORDER BY v.Idmoneda, Sucursal, month(v.Fecha)� � ��C � � rranking� �� F� � U  TOTAL SUCURSAL MONEDA CMDSQL SQL RRANKING
  <�  � U  TOTAL
  �  � U  SETEO Init,     �� Destroy/    �� BeforeOpenTablesD    ��1 q � A � A � Qq��1�!��11A �q 3 q 3 q 2                       �          "        I  S  #    )   3                  