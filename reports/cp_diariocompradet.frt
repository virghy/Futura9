                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   �DRIVER=winspool
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
od   ,  ,  Letter                                                                            PRIV�0                                                                                       '''  '          �                                  \K hC                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial      IdMoneda      Sucursal      	idfactura      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Diario de Compra Detallado"             Arial      empresa             Arial      *iif(empty(m.sucursal),'Todos',descripci�n)             Arial      "Sucursal:"      Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      	"Fecha
"      "@I"      Arial      "Proveedor"      Arial      "Fact.Nro."      Arial      "Forma Iva"      Arial      "
"      Arial      "Iva
"      "@I"      Arial      "Descripci�n
"      "@I"      Arial      "Cantidad
"      "@I"      Arial      
"Precio
"      "@I"      Arial      	"% Iva
"      "@I"      Arial      	"Total
"      "@I"      Arial      "Moneda ", Idmoneda      Arial      '"Sucursal ",Sucursal, " " , Descripci�n      Arial      fecha             Arial      	proveedor             Arial      IdComprobante, "-" , numero      Arial      FormaIva      Arial      obs             Arial      iif(RT,'RT','')             Arial      producto             Arial      cantidad      	"999,999"             Arial      precio      "999,999,999.99"      Arial      Iva      "999"      Arial      subtot      "999,999,999.99"             Arial      	 ValorIva      "999,999,999.99"      Arial      subtot      "999,999,999.99"      Arial      ValorIva      "999,999,999.99"      Arial      "TotalFactura:
"      Arial      -"Total Sucursal ",Sucursal, " " , Descripci�n      Arial      subtot      "999,999,999.99"      Arial      ValorIva      "999,999,999.99"      Arial      "Total Moneda ", Idmoneda      Arial      subtot      "999,999,999.99"      Arial      ValorIva      "999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init

If Empty(m.sucursal)
	Store null To m.sucursal
endif

TEXT TO cmdSQL noshow
		select a.idfactura,a.fecha,a.idcomprobante,a.facturaproveedor numero, a.FormaIva,a.IdMoneda,
		rtrim(a.idproveedor)+'-'+c.razon proveedor,a.exenta,a.gravada,
		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, 
		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,
		Precio = CASE when a.FormaIva='D' then b.precio else GravadaIncluido end ,
		subtot = CASE when a.FormaIva='D' then (b.cantidad*b.precio) + ValorIva else b.cantidad*b.GravadaIncluido end, 
		b.Iva,
		ISNULL(b.ValorIva,0) as ValorIva, RTRIM(ISNULL(IdCliente,'')) +' '+  RTRIM(ISNULL(b.Obs,'')) as Obs,
		b.RegimenTurismo as RT
		from cp_factura a inner join st_movimiento_det b on 
		a.idfactura=b.idcompra and a.IdEmpresa = b.IdEmpresa 
		left join cp_proveedor c on a.idproveedor=c.idproveedor and a.IdEmpresa = c.idempresa
		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  
		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa
		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)
		and (a.sucursal=?m.sucursal or ?m.sucursal is null)
		order by a.IdMoneda,a.Sucursal,a.fecha, a.IdProveedor
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
     s���    Z  Z                        �8   %   �           �          �  U  
  �  � U  SETEO< %�C��  ��� � J���(��  � �	 M(� ��d �^ 		select a.idfactura,a.fecha,a.idcomprobante,a.facturaproveedor numero, a.FormaIva,a.IdMoneda,�F �@ 		rtrim(a.idproveedor)+'-'+c.razon proveedor,a.exenta,a.gravada,�L �F 		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, �B �< 		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,�R �L 		Precio = CASE when a.FormaIva='D' then b.precio else GravadaIncluido end ,�w �q 		subtot = CASE when a.FormaIva='D' then (b.cantidad*b.precio) + ValorIva else b.cantidad*b.GravadaIncluido end, � � 		b.Iva,�l �f 		ISNULL(b.ValorIva,0) as ValorIva, RTRIM(ISNULL(IdCliente,'')) +' '+  RTRIM(ISNULL(b.Obs,'')) as Obs,� � 		b.RegimenTurismo as RT�< �6 		from cp_factura a inner join st_movimiento_det b on �= �7 		a.idfactura=b.idcompra and a.IdEmpresa = b.IdEmpresa �] �W 		left join cp_proveedor c on a.idproveedor=c.idproveedor and a.IdEmpresa = c.idempresa�U �O 		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  �Z �T 		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa�U �O 		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)�; �5 		and (a.sucursal=?m.sucursal or ?m.sucursal is null)�= �7 		order by a.IdMoneda,a.Sucursal,a.fecha, a.IdProveedor� � ��C � � rpedido� �� F� � U  SUCURSAL CMDSQL SQL RPEDIDO BeforeOpenTables,     �� InitA     ��1 q 2 � A � Aa�!!q� �����Q�Q��A �q 4                       $         ?   �      )   Z                  