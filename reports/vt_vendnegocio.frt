  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                       �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$         Arial      Vendedor      acum      cant      0      vtCosto      rlnegocio.costo      0      vtVenta      rlnegocio.total      0      Arial      Arial      Arial      Arial      Arial      Arial      ("Ventas por Vendedor y Linea de Negocio"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      .iif(isnull(m.idVendedor),'Todos',m.IdVendedor)             Arial      "Vendedor:"      Arial      ,iif(isnull(m.IdNegocio),'Todos',m.IdNegocio)             Arial      
"Negocio:"      Arial      "
"      Arial      "Unidades
"      Arial      	"Costo
"      Arial      " Total Venta
"      "@I"      Arial      "Utilidad
"      "@I"      Arial      	"Ratio
"      "@I"      Arial      	"Negocio"      Arial      "Vendedor: ",Vendedor             Arial      negocio             Arial      rlnegocio.cant      "99,999,999.99"             Arial      rlnegocio.costo      "999,999,999"             Arial      rlnegocio.total      "999,999,999"             Arial      rlnegocio.total - costo      "999,999,999"             Arial      (round(rlnegocio.costo/rlnegocio.total,2)      "99,999.99"             Arial      "Total Vendedor:
"      Arial      rlnegocio.cant      "99,999,999.99"             Arial      rlnegocio.costo      "999,999,999"             Arial      rlnegocio.total      "999,999,999"             Arial      rlnegocio.total - costo      "999,999,999"             Arial      round(vtcosto/vtVenta,2)      "99,999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      rlnegocio.cant      "99,999,999.99"             Arial      rlnegocio.costo      "999,999,999"             Arial      rlnegocio.total      "999,999,999"             Arial      rlnegocio.total - costo      "999,999,999"             Arial      round(vcosto/vtotal,2)      "99,999.99"             Arial      "Totales:
"      Arial      dataenvironment      KLeft = 208
Top = 75
Width = 381
Height = 355
Name = "Dataenvironment"
     �PROCEDURE Init
PUBLIC vtotal,vcosto

IF EMPTY(m.idnegocio)
	m.idnegocio = null
ENDIF
	
IF EMPTY(m.idVendedor)
	m.IdVendedor = null
ENDIF


TEXT TO cSQL NOSHOW 
SELECT     vt.IdVendedor + RTRIM(ps.Nombre) + ' ' + ps.Apellido AS Vendedor, ng.Negocio, SUM(det.Cantidad) AS cant, SUM(det.cantidad*det.Ult_Costo) 
                      AS costo, SUM(det.Cantidad * det.Precio) AS total
FROM         dbo.vt_factura vt INNER JOIN
                      dbo.st_movimiento_Det det ON vt.IdFactura = det.IdFactura INNER JOIN
                      dbo.vt_Vendedores vend ON vt.IdEmpresa = vend.IdEmpresa AND vt.IdVendedor = vend.IdVendedor INNER JOIN
                      dbo.BS_Personas ps ON vend.idpersona = ps.IdPersona LEFT OUTER JOIN
                      dbo.vt_Negocio ng ON vt.IdNegocio = ng.IdNegocio and vt.IdEmpresa = ng.IdEmpresa 
where vt.idempresa=?oApp.empresa and  (vt.fecha between  ?m.dfecha and  ?m.hfecha )
	and (vt.IdVendedor = ?m.IdVendedor or ?m.idVendedor is null)
	and (vt.IdNegocio = ?m.IdNegocio or ?m.IdNegocio is null)
GROUP BY ng.Negocio, ps.Nombre, vt.IdVendedor + RTRIM(ps.Nombre) + ' ' + ps.Apellido
ORDER BY vt.IdVendedor + RTRIM(ps.Nombre) + ' ' + ps.Apellido, ng.Negocio 

ENDTEXT

sql(cSQL,'rlnegocio')
SELECT rlnegocio
sum total To vtotal
Sum costo To vcosto

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Destroy
RELEASE vtotal,vcosto
ENDPROC
     ����    ~  ~                        ,$   %   �            �          �  U  � 7�  � � %�C�� ���) � T�� ���� � %�C�� ���K � T�� ���� �	 M(� ��� �� SELECT     vt.IdVendedor + RTRIM(ps.Nombre) + ' ' + ps.Apellido AS Vendedor, ng.Negocio, SUM(det.Cantidad) AS cant, SUM(det.cantidad*det.Ult_Costo) �M �G                       AS costo, SUM(det.Cantidad * det.Precio) AS total�/ �) FROM         dbo.vt_factura vt INNER JOIN�` �Z                       dbo.st_movimiento_Det det ON vt.IdFactura = det.IdFactura INNER JOIN�� �|                       dbo.vt_Vendedores vend ON vt.IdEmpresa = vend.IdEmpresa AND vt.IdVendedor = vend.IdVendedor INNER JOIN�_ �Y                       dbo.BS_Personas ps ON vend.idpersona = ps.IdPersona LEFT OUTER JOIN�m �g                       dbo.vt_Negocio ng ON vt.IdNegocio = ng.IdNegocio and vt.IdEmpresa = ng.IdEmpresa �Y �S where vt.idempresa=?oApp.empresa and  (vt.fecha between  ?m.dfecha and  ?m.hfecha )�C �= 	and (vt.IdVendedor = ?m.IdVendedor or ?m.idVendedor is null)�@ �: 	and (vt.IdNegocio = ?m.IdNegocio or ?m.IdNegocio is null)�Z �T GROUP BY ng.Negocio, ps.Nombre, vt.IdVendedor + RTRIM(ps.Nombre) + ' ' + ps.Apellido�P �J ORDER BY vt.IdVendedor + RTRIM(ps.Nombre) + ' ' + ps.Apellido, ng.Negocio � �  � � ��C � �	 rlnegocio� �� F� � K(�  �� �� K(� �� �� U	  VTOTAL VCOSTO	 IDNEGOCIO
 IDVENDEDOR CSQL SQL	 RLNEGOCIO TOTAL COSTO
  �  � U  SETEO  <�  � � U  VTOTAL VCOSTO Init,     �� BeforeOpenTablese    �� Destroyz    ��1 � � A � A � �	��!���1�a A �q � � 3 q 3 � 1                       '        N  X  #      v  �  '    )   ~                  