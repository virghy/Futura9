  K                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      acum      cant      0      Arial      Arial      Arial      Arial      Arial      Arial      "Ventas por Linea de Negocio"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      ,iif(isnull(m.IdNegocio),'Todos',m.IdNegocio)             Arial      
"Negocio:"      Arial      "
"      Arial      "Descripci�n"      Arial      "Unidades
"      Arial      	"Costo
"      Arial      " Total Venta
"      "@I"      Arial      
" Ratio
"      "@I"      Arial      negocio             Arial      rlnegocio.cant      "99,999,999.99"             Arial      rlnegocio.costo      "9,999,999"             Arial      rlnegocio.total      "999,999,999"             Arial      (round(rlnegocio.costo/rlnegocio.total,2)      "999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      rlnegocio.cant      "99,999,999.99"             Arial      rlnegocio.costo      "999,999,999"             Arial      rlnegocio.total      "999,999,999"             Arial      round(vcosto/vtotal,2)      "999.99"             Arial      "Totales:
"      Arial      dataenvironment      ~Top = 75
Left = 208
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
      PROCEDURE Init
PUBLIC vtotal,vcosto

IF EMPTY(m.IdNegocio)
	m.idNegocio = null
ENDIF


TEXT TO cmdSQL NOSHOW 
	select a.IdNegocio +'-'+ c.negocio as Negocio, sum(b.cantidad) cant, sum(b.cantidad*b.ult_costo) costo, 
	sum(b.cantidad*b.precio) total 
	from vt_factura a inner join st_movimiento_det b on a.idfactura=b.idfactura 
	left join vt_negocio c on a.idnegocio=c.idnegocio 
	where a.idempresa=?oApp.empresa and 
	(a.fecha between  ?m.dfecha and  ?m.hfecha )
	and (a.IdNegocio = ?m.IdNegocio or ?m.idNegocio is null)
	group by  a.IdNegocio, negocio 
	order by a.IdNegocio 

ENDTEXT

sql(cmdSQL,'rlnegocio')

SELECT rlnegocio
sum total To vtotal
Sum costo To vcosto

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���    �  �                        ��   %         �     R          �  U  ~ 7�  � � %�C�� ���) � T�� ���� �	 M(� ��o �i 	select a.IdNegocio +'-'+ c.negocio as Negocio, sum(b.cantidad) cant, sum(b.cantidad*b.ult_costo) costo, �& �  	sum(b.cantidad*b.precio) total �S �M 	from vt_factura a inner join st_movimiento_det b on a.idfactura=b.idfactura �9 �3 	left join vt_negocio c on a.idnegocio=c.idnegocio �+ �% 	where a.idempresa=?oApp.empresa and �3 �- 	(a.fecha between  ?m.dfecha and  ?m.hfecha )�? �9 	and (a.IdNegocio = ?m.IdNegocio or ?m.idNegocio is null)�& �  	group by  a.IdNegocio, negocio � � 	order by a.IdNegocio � �  � � ��C � �	 rlnegocio� �� F� � K(�  �� �� K(� �� �� U  VTOTAL VCOSTO	 IDNEGOCIO CMDSQL SQL	 RLNEGOCIO TOTAL COSTO
  <�  � U  TOTAL
  �  � U  SETEO Init,     �� Destroy�    �� BeforeOpenTables    ��1 � � A � �a1��1�a�a A �r � � 3 q 3 q 2                       �        �  �            !    )   �                  