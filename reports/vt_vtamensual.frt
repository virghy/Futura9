  ,�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'ORIENTATION=1
PAPERSIZE=140
COLOR=2
      Arial      Sucursal      Arial      Arial      Arial      Arial      Arial      Arial      Arial      ("Ranking de Ventas Mensual por Sucursal"      Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      "Periodo::"      Arial      (iif(isnull(m.Sucursal),'Todos',sucursal)      Arial      "Sucursal:"      Arial      	"Enero
"      Arial      "Feb
"      Arial      "Mar"      Arial      "Abr
"      Arial      "May
"      Arial      "Jun
"      Arial      "Jul
"      Arial      "Ago
"      Arial      "Set
"      Arial      "Oct
"      Arial      "Nov
"      Arial      "Dic
"      Arial      	"Total
"      Arial      
"Producto"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      "Importe
"      Arial      "Cant
"      Arial      Sucursal      Arial       rranking.idproducto, Descripcion      Arial      I1      "999,999,999"      Arial      C1      	"999,999"      Arial      I2      "999,999,999"      Arial      C2      	"999,999"      Arial      I3      "999,999,999"      Arial      C3      	"999,999"      Arial      I4      "999,999,999"      Arial      C4      	"999,999"      Arial      I5      "999,999,999"      Arial      C5      	"999,999"      Arial      I6      "999,999,999"      Arial      C6      	"999,999"      Arial      I7      "999,999,999"      Arial      C7      	"999,999"      Arial      I8      "999,999,999"      Arial      C8      	"999,999"      Arial      I9      "999,999,999"      Arial      C9      	"999,999"      Arial      I10      "999,999,999"      Arial      C10      	"999,999"      Arial      I11      "999,999,999"      Arial      C11      	"999,999"      Arial      I12      "999,999,999"      Arial      C12      	"999,999"      Arial      IT      "999,999,999"      Arial      CT      	"999,999"      Arial      "Total Sucursal"      Arial      I1      "999,999,999"      Arial      C1      	"999,999"      Arial      I2      "999,999,999"      Arial      C2      	"999,999"      Arial      I3      "999,999,999"      Arial      C3      	"999,999"      Arial      I4      "999,999,999"      Arial      C4      	"999,999"      Arial      I5      "999,999,999"      Arial      C5      	"999,999"      Arial      I6      "999,999,999"      Arial      C6      	"999,999"      Arial      I7      "999,999,999"      Arial      C7      	"999,999"      Arial      I8      "999,999,999"      Arial      C8      	"999,999"      Arial      I9      "999,999,999"      Arial      C9      	"999,999"      Arial      I10      "999,999,999"      Arial      C10      	"999,999"      Arial      I11      "999,999,999"      Arial      C11      	"999,999"      Arial      I12      "999,999,999"      Arial      C12      	"999,999"      Arial      IT      "999,999,999"      Arial      CT      	"999,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total General"      Arial      I1      "999,999,999"      Arial      C1      	"999,999"      Arial      I2      "999,999,999"      Arial      C2      	"999,999"      Arial      I3      "999,999,999"      Arial      C3      	"999,999"      Arial      I4      "999,999,999"      Arial      C4      	"999,999"      Arial      I5      "999,999,999"      Arial      C5      	"999,999"      Arial      I6      "999,999,999"      Arial      C6      	"999,999"      Arial      I7      "999,999,999"      Arial      C7      	"999,999"      Arial      I8      "999,999,999"      Arial      C8      	"999,999"      Arial      I9      "999,999,999"      Arial      C9      	"999,999"      Arial      I10      "999,999,999"      Arial      C10      	"999,999"      Arial      I11      "999,999,999"      Arial      C11      	"999,999"      Arial      I12      "999,999,999"      Arial      C12      	"999,999"      Arial      IT      "999,999,999"      Arial      CT      	"999,999"      Arial      dataenvironment      `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     
'PROCEDURE Init
DO seteo

If Empty(m.sucursal)
	m.sucursal= null
ENDIF

TEXT TO cmdSQL NOSHOW 
	SELECT     d.IdProducto, p.Descripcion, v.sucursal + s.descripci�n as sucursal,
	SUM(CASE when MONTH(v.Fecha)=1 then d.Importe else 0 end) as I1,
	SUM(CASE when MONTH(v.Fecha)=1 then d.Cantidad else 0 end) as C1,
	SUM(CASE when MONTH(v.Fecha)=2 then d.Importe else 0 end) as I2,
	SUM(CASE when MONTH(v.Fecha)=2 then d.Cantidad else 0 end) as C2,
	SUM(CASE when MONTH(v.Fecha)=3 then d.Importe else 0 end) as I3,
	SUM(CASE when MONTH(v.Fecha)=3 then d.Cantidad else 0 end) as C3,
	SUM(CASE when MONTH(v.Fecha)=4 then d.Importe else 0 end) as I4,
	SUM(CASE when MONTH(v.Fecha)=4 then d.Cantidad else 0 end) as C4,
	SUM(CASE when MONTH(v.Fecha)=5 then d.Importe else 0 end) as I5,
	SUM(CASE when MONTH(v.Fecha)=5 then d.Cantidad else 0 end) as C5,
	SUM(CASE when MONTH(v.Fecha)=6 then d.Importe else 0 end) as I6,
	SUM(CASE when MONTH(v.Fecha)=6 then d.Cantidad else 0 end) as C6,
	SUM(CASE when MONTH(v.Fecha)=7 then d.Importe else 0 end) as I7,
	SUM(CASE when MONTH(v.Fecha)=7 then d.Cantidad else 0 end) as C7,
	SUM(CASE when MONTH(v.Fecha)=8 then d.Importe else 0 end) as I8,
	SUM(CASE when MONTH(v.Fecha)=8 then d.Cantidad else 0 end) as C8,
	SUM(CASE when MONTH(v.Fecha)=9 then d.Importe else 0 end) as I9,
	SUM(CASE when MONTH(v.Fecha)=9 then d.Cantidad else 0 end) as C9,
	SUM(CASE when MONTH(v.Fecha)=10 then d.Importe else 0 end) as I10,
	SUM(CASE when MONTH(v.Fecha)=10 then d.Cantidad else 0 end) as C10,
	SUM(CASE when MONTH(v.Fecha)=11 then d.Importe else 0 end) as I11,
	SUM(CASE when MONTH(v.Fecha)=11 then d.Cantidad else 0 end) as C11,
	SUM(CASE when MONTH(v.Fecha)=12 then d.Importe else 0 end) as I12,
	SUM(CASE when MONTH(v.Fecha)=12 then d.Cantidad else 0 end) as C12,
	SUM(d.Importe) as IT,
	SUM(d.Cantidad) as CT			
	FROM         dbo.vt_factura v INNER JOIN
	                      dbo.st_movimiento_Det d ON v.IdFactura = d.IdFactura INNER JOIN
	                      dbo.st_Producto p ON d.IdEmpresa = p.IdEmpresa AND d.IdProducto = p.IdProducto
	                      left join sucursal  s on v.IdEmpresa=s.IdEmpresa and v.Sucursal = s.Sucursal
	WHERE v.idempresa = ?oApp.empresa
	AND v.fecha between ?m.dfecha and  ?m.hfecha and 
	(v.sucursal=?m.sucursal or ?m.sucursal is null)
	group by v.sucursal + s.descripci�n,  d.IdProducto, p.Descripcion
	order by v.sucursal + s.descripci�n,  d.IdProducto, p.Descripcion


ENDTEXT


	
sql(cmdSQL,'rranking')
SELECT rranking


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        x�   %   �
      4  2   �
          �  U  ;
 �  � %�C�� ���% � T�� ���� �	 M(� ��V �P 	SELECT     d.IdProducto, p.Descripcion, v.sucursal + s.descripci�n as sucursal,�G �A 	SUM(CASE when MONTH(v.Fecha)=1 then d.Importe else 0 end) as I1,�H �B 	SUM(CASE when MONTH(v.Fecha)=1 then d.Cantidad else 0 end) as C1,�G �A 	SUM(CASE when MONTH(v.Fecha)=2 then d.Importe else 0 end) as I2,�H �B 	SUM(CASE when MONTH(v.Fecha)=2 then d.Cantidad else 0 end) as C2,�G �A 	SUM(CASE when MONTH(v.Fecha)=3 then d.Importe else 0 end) as I3,�H �B 	SUM(CASE when MONTH(v.Fecha)=3 then d.Cantidad else 0 end) as C3,�G �A 	SUM(CASE when MONTH(v.Fecha)=4 then d.Importe else 0 end) as I4,�H �B 	SUM(CASE when MONTH(v.Fecha)=4 then d.Cantidad else 0 end) as C4,�G �A 	SUM(CASE when MONTH(v.Fecha)=5 then d.Importe else 0 end) as I5,�H �B 	SUM(CASE when MONTH(v.Fecha)=5 then d.Cantidad else 0 end) as C5,�G �A 	SUM(CASE when MONTH(v.Fecha)=6 then d.Importe else 0 end) as I6,�H �B 	SUM(CASE when MONTH(v.Fecha)=6 then d.Cantidad else 0 end) as C6,�G �A 	SUM(CASE when MONTH(v.Fecha)=7 then d.Importe else 0 end) as I7,�H �B 	SUM(CASE when MONTH(v.Fecha)=7 then d.Cantidad else 0 end) as C7,�G �A 	SUM(CASE when MONTH(v.Fecha)=8 then d.Importe else 0 end) as I8,�H �B 	SUM(CASE when MONTH(v.Fecha)=8 then d.Cantidad else 0 end) as C8,�G �A 	SUM(CASE when MONTH(v.Fecha)=9 then d.Importe else 0 end) as I9,�H �B 	SUM(CASE when MONTH(v.Fecha)=9 then d.Cantidad else 0 end) as C9,�I �C 	SUM(CASE when MONTH(v.Fecha)=10 then d.Importe else 0 end) as I10,�J �D 	SUM(CASE when MONTH(v.Fecha)=10 then d.Cantidad else 0 end) as C10,�I �C 	SUM(CASE when MONTH(v.Fecha)=11 then d.Importe else 0 end) as I11,�J �D 	SUM(CASE when MONTH(v.Fecha)=11 then d.Cantidad else 0 end) as C11,�I �C 	SUM(CASE when MONTH(v.Fecha)=12 then d.Importe else 0 end) as I12,�J �D 	SUM(CASE when MONTH(v.Fecha)=12 then d.Cantidad else 0 end) as C12,� � 	SUM(d.Importe) as IT,� � 	SUM(d.Cantidad) as CT			�/ �) 	FROM         dbo.vt_factura v INNER JOIN�\ �V 	                      dbo.st_movimiento_Det d ON v.IdFactura = d.IdFactura INNER JOIN�k �e 	                      dbo.st_Producto p ON d.IdEmpresa = p.IdEmpresa AND d.IdProducto = p.IdProducto�i �c 	                      left join sucursal  s on v.IdEmpresa=s.IdEmpresa and v.Sucursal = s.Sucursal�( �" 	WHERE v.idempresa = ?oApp.empresa�8 �2 	AND v.fecha between ?m.dfecha and  ?m.hfecha and �6 �0 	(v.sucursal=?m.sucursal or ?m.sucursal is null)�H �B 	group by v.sucursal + s.descripci�n,  d.IdProducto, p.Descripcion�H �B 	order by v.sucursal + s.descripci�n,  d.IdProducto, p.Descripcion� �  � �  � � ��C � � rranking� �� F� � U  SETEO SUCURSAL CMDSQL SQL RRANKING
  �  � U  SETEO Init,     �� BeforeOpenTables�
    ��1 q � A � aq�q�q�q�q�q�q�q�q���������������a��a a A �q 4 q 2                       �	     0   
  
  8    )   �                  