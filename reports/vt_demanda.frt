  a                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      T    winspool  PrimoPDF  PrimoPort:                  3C  USB001                       �PrimoPDF OneNote 2013           � �S� 	 �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        RTRAIT Resolution DPI600 ColorMode 24bpp                      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Demanda no Satisfecha"      Arial      empresa             Arial      (dtoc(m.dfecha) + ' al ' + dtoc(m.hfecha)      Arial      
"Periodo:"      Arial      	"Fecha
"      "@I"      Arial      "Comentario"      Arial      
"Producto"      Arial      	"Detalle"      Arial      Fecha      Arial      
Comentario      Arial      !alltrim(IdProducto) +" "+producto      Arial      ObsDet      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      ~Top = 32
Left = 177
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


TEXT TO cmdSQL noshow
	SELECT     d.Nro, d.Fecha, d.Comentario, dd.IdProducto, dd.Producto, dd.Comentario AS ObsDet
	FROM         vt_Demanda AS d INNER JOIN
	                      vt_DemandaDet AS dd ON d.IdDemanda = dd.IdDemanda
	where d.IdEmpresa=?oApp.Empresa
	and d.Fecha between ?m.dFecha and ?m.hFecha
	ORDER BY d.Nro
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
     ����    s  s                        �4   %   �           �          �  U  
  �  � U  SETEO{	 M(�  ��c �] 	SELECT     d.Nro, d.Fecha, d.Comentario, dd.IdProducto, dd.Producto, dd.Comentario AS ObsDet�. �( 	FROM         vt_Demanda AS d INNER JOIN�N �H 	                      vt_DemandaDet AS dd ON d.IdDemanda = dd.IdDemanda�& �  	where d.IdEmpresa=?oApp.Empresa�2 �, 	and d.Fecha between ?m.dFecha and ?m.hFecha� � 	ORDER BY d.Nro� � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO BeforeOpenTables,     �� InitA     ��1 q 2 � 1��a!QA �q 4                       $         ?   �      )   s                  