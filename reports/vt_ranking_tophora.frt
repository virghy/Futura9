   ]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      '"Ranking de Ventas de Productos / Hora"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      "Periodo::"      Arial      m.mTop      Arial      "Top:"      Arial      "
"      Arial      
"Producto"      Arial      "- 07
"      Arial      "07
"      Arial      "08
"      Arial      "09
"      Arial      "10
"      Arial      "11
"      Arial      "12
"      Arial      "13
"      Arial      "14
"      Arial      "15
"      Arial      "16
"      Arial      "17
"      Arial      "18
"      Arial      "19
"      Arial      "20
"      Arial      "+ 20
"      Arial      	"Total
"      Arial      rranking.producto      Arial      H06      "99,999"      Arial      H07      "99,999"      Arial      H08      "99,999"      Arial      H09      "99,999"      Arial      H10      "99,999"      Arial      H11      "99,999"      Arial      H12      "99,999"      Arial      H13      "99,999"      Arial      H14      "99,999"      Arial      H15      "99,999"      Arial      H16      "99,999"      Arial      H17      "99,999"      Arial      H18      "99,999"      Arial      H19      "99,999"      Arial      H20      "99,999"      Arial      H21      "99,999"      Arial      ?H06+H07+H08+H09+H10+H11+H12+H13+H14+H15+H16+H17+H18+H19+H20+H21      "99,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      H06      "99,999"      Arial      H07      "99,999"      Arial      H08      "99,999"      Arial      H09      "99,999"      Arial      H10      "99,999"      Arial      H11      "99,999"      Arial      H12      "99,999"      Arial      H13      "99,999"      Arial      H14      "99,999"      Arial      H15      "99,999"      Arial      H16      "99,999"      Arial      H17      "99,999"      Arial      H18      "99,999"      Arial      H19      "99,999"      Arial      H20      "99,999"      Arial      H21      "99,999"      Arial      ?H06+H07+H08+H09+H10+H11+H12+H13+H14+H15+H16+H17+H18+H19+H20+H21      "99,999"      Arial      	"Total
"      Arial      dataenvironment      ~Top = 75
Left = 208
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     	PROCEDURE Init

TEXT TO cmdSQL NOSHOW 
Declare @Top int
Set @top=?m.mTop
SELECT top(@Top) Producto,
SUM(H06) AS H06,SUM(H07) AS H07,SUM(H08) AS H08,
SUM(H09) AS H09,SUM(H10) AS H10,SUM(H11) AS H11,
SUM(H12) AS H12,SUM(H13) AS H13,SUM(H14) AS H14,
SUM(H15) AS H15,SUM(H16) AS H16,SUM(H17) AS H17,
SUM(H18) AS H18,SUM(H19) AS H19,SUM(H20) AS H20,
SUM(H21) AS H21
FROM (
	SELECT RTRIM(m.IdProducto) + '-' + p.Descripcion AS Producto, 
	case when left(Hora,2)<='06' then sum(m.Cantidad) else 0 end  as [H06],
	case when left(Hora,2)='07' then sum(m.Cantidad) else 0 end  as [H07],
	case when left(Hora,2)='08' then sum(m.Cantidad) else 0 end  as [H08],
	case when left(Hora,2)='09' then sum(m.Cantidad) else 0 end  as [H09],
	case when left(Hora,2)='10' then sum(m.Cantidad) else 0 end  as [H10],
	case when left(Hora,2)='11' then sum(m.Cantidad) else 0 end  as [H11],
	case when left(Hora,2)='12' then sum(m.Cantidad) else 0 end  as [H12],
	case when left(Hora,2)='13' then sum(m.Cantidad) else 0 end  as [H13],
	case when left(Hora,2)='14' then sum(m.Cantidad) else 0 end  as [H14],
	case when left(Hora,2)='15' then sum(m.Cantidad) else 0 end  as [H15],
	case when left(Hora,2)='16' then sum(m.Cantidad) else 0 end  as [H16],
	case when left(Hora,2)='17' then sum(m.Cantidad) else 0 end  as [H17],
	case when left(Hora,2)='18' then sum(m.Cantidad) else 0 end  as [H18],
	case when left(Hora,2)='19' then sum(m.Cantidad) else 0 end  as [H19],
	case when left(Hora,2)='20' then sum(m.Cantidad) else 0 end  as [H20],
	case when left(Hora,2)>='21' then sum(m.Cantidad) else 0 end  as [H21],
	sum(m.Cantidad) Cantidad 
		FROM         vt_factura AS v INNER JOIN
		                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN
		                      st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto 
		where v.IdEmpresa = ?oApp.Empresa
		and p.AfectaStock=1
		and v.fecha between ?m.dfecha and  ?m.hfecha
	GROUP BY RTRIM(m.IdProducto) + '-' + p.Descripcion, left(Hora,2)
--	ORDER BY sum(m.Cantidad) desc, Producto
) S 
GROUP BY producto 
order by SUM(cantidad) desc, Producto
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
     
����    �
  �
                        ��   %   �	      !
  1   �	          �  U  		 M(�  �� � Declare @Top int� � Set @top=?m.mTop�  � SELECT top(@Top) Producto,�6 �0 SUM(H06) AS H06,SUM(H07) AS H07,SUM(H08) AS H08,�6 �0 SUM(H09) AS H09,SUM(H10) AS H10,SUM(H11) AS H11,�6 �0 SUM(H12) AS H12,SUM(H13) AS H13,SUM(H14) AS H14,�6 �0 SUM(H15) AS H15,SUM(H16) AS H16,SUM(H17) AS H17,�6 �0 SUM(H18) AS H18,SUM(H19) AS H19,SUM(H20) AS H20,� � SUM(H21) AS H21� � FROM (�E �? 	SELECT RTRIM(m.IdProducto) + '-' + p.Descripcion AS Producto, �N �H 	case when left(Hora,2)<='06' then sum(m.Cantidad) else 0 end  as [H06],�M �G 	case when left(Hora,2)='07' then sum(m.Cantidad) else 0 end  as [H07],�M �G 	case when left(Hora,2)='08' then sum(m.Cantidad) else 0 end  as [H08],�M �G 	case when left(Hora,2)='09' then sum(m.Cantidad) else 0 end  as [H09],�M �G 	case when left(Hora,2)='10' then sum(m.Cantidad) else 0 end  as [H10],�M �G 	case when left(Hora,2)='11' then sum(m.Cantidad) else 0 end  as [H11],�M �G 	case when left(Hora,2)='12' then sum(m.Cantidad) else 0 end  as [H12],�M �G 	case when left(Hora,2)='13' then sum(m.Cantidad) else 0 end  as [H13],�M �G 	case when left(Hora,2)='14' then sum(m.Cantidad) else 0 end  as [H14],�M �G 	case when left(Hora,2)='15' then sum(m.Cantidad) else 0 end  as [H15],�M �G 	case when left(Hora,2)='16' then sum(m.Cantidad) else 0 end  as [H16],�M �G 	case when left(Hora,2)='17' then sum(m.Cantidad) else 0 end  as [H17],�M �G 	case when left(Hora,2)='18' then sum(m.Cantidad) else 0 end  as [H18],�M �G 	case when left(Hora,2)='19' then sum(m.Cantidad) else 0 end  as [H19],�M �G 	case when left(Hora,2)='20' then sum(m.Cantidad) else 0 end  as [H20],�N �H 	case when left(Hora,2)>='21' then sum(m.Cantidad) else 0 end  as [H21],�  � 	sum(m.Cantidad) Cantidad �/ �) 		FROM         vt_factura AS v INNER JOIN�\ �V 		                      st_movimiento_Det AS m ON v.IdFactura = m.IdFactura INNER JOIN�l �f 		                      st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto �) �# 		where v.IdEmpresa = ?oApp.Empresa� � 		and p.AfectaStock=1�4 �. 		and v.fecha between ?m.dfecha and  ?m.hfecha�G �A 	GROUP BY RTRIM(m.IdProducto) + '-' + p.Descripcion, left(Hora,2)�0 �* --	ORDER BY sum(m.Cantidad) desc, Producto�
 � ) S � � GROUP BY producto �+ �% order by SUM(cantidad) desc, Producto� � ��C �  � rranking� �� F� � U  CMDSQL SQL RRANKING
  <�  � U  TOTAL
  �  � U  SETEO Init,     �� Destroy\	    �� BeforeOpenTablesq	    ��1 � aaaaaaaQ� Q���������������������Aq� ��A �q 3 q 3 q 2                       �     -   �  �  3   /    	  
	  7    )   �
                  