  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
      8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S� 	 �
od   ,  ,  Letter                                                                            PRIV�0                                                                                       '''  '          �                                  \K hC                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial      NroRemision      Arial      Arial      Arial      Arial      Arial      Arial      "Detalle de Remisi�n"      Arial      empresa             Arial      *iif(empty(m.sucursal),'Todos',descripci�n)             Arial      "Sucursal:"      Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "Nro. Remisi�n
"      "@I"      Arial      	"Fecha
"      "@I"      Arial      	"Cliente"      Arial      "Descripci�n
"      "@I"      Arial      
"Cantidad"      Arial      "
"      Arial      Nroremision      Arial      fecha             Arial      cliente      Arial      producto             Arial      cantidad      "@B"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      aTop = 392
Left = 160
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init

If Empty(m.sucursal)
	Store null To m.sucursal
endif

TEXT TO cmdSQL noshow
		select a.NroRemision,a.fecha,a.idcomprobante,
		a.sucursal,d.descripci�n, rtrim(a.idcliente)+'-'+c.razsocial cliente, 
		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad
		from os_remision a inner join st_movimiento_det b on 
		a.IdRemision=b.IdRemision and a.IdEmpresa = b.IdEmpresa 
		left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa
		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  
		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa
		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)
		and (a.sucursal=?m.sucursal or ?m.sucursal is null)
		order by a.fecha, a.IdCliente
ENDTEXT

sql(cmdSQL,'rpedidos')
SELECT rpedidos

*Sum Importe To m.Total

ENDPROC
     {���    b  b                        C?   %   �      	     �          �  U  
  �  � U  SETEOO %�C��  ��� � J���(��  � �	 M(� ��5 �/ 		select a.NroRemision,a.fecha,a.idcomprobante,�N �H 		a.sucursal,d.descripci�n, rtrim(a.idcliente)+'-'+c.razsocial cliente, �A �; 		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad�= �7 		from os_remision a inner join st_movimiento_det b on �@ �: 		a.IdRemision=b.IdRemision and a.IdEmpresa = b.IdEmpresa �X �R 		left join vt_clientes c on a.idcliente=c.idcliente and a.IdEmpresa = c.idempresa�U �O 		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  �Z �T 		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa�U �O 		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)�; �5 		and (a.sucursal=?m.sucursal or ?m.sucursal is null)�% � 		order by a.fecha, a.IdCliente� � ��C � � rpedidos� �� F� � U  SUCURSAL CMDSQL SQL RPEDIDOS BeforeOpenTables,     �� InitA     ��1 q 2 � A � Q���Q�Q�QA �q 4                       $         ?   �      )   b                  