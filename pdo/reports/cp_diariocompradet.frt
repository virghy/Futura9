  z   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           `\\futura5\HP DeskJet 840C/841C   � XC� 	 �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                                                                                                                             Arial                                                         	idfactura                                                     "Diario de Compra Detallado"                                                                                                Arial                                                         "Fact.Nro."                                                   Arial                                                         	proveedor                                                                                                                   Arial                                                         ""                                                           Arial                                                         "Fecha"                                                      "@I"                                                          Arial                                                         "Proveedor"                                                   Arial                                                         empresa                                                                                                                     Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
"Periodo:"                                                    Arial                                                         m.dfecha, ' al ' ,m.hfecha                                                                                                  Arial                                                         numero                                                                                                                      Arial                                                         producto                                                                                                                    Arial                                                         cantidad                                                      	"999,999"                                                                                                                   Arial                                                         "Cantidad"                                                   "@I"                                                          Arial                                                         "Descripci�n"                                                "@I"                                                          Arial                                                         precio                                                        "999,999,999.99"                                              Arial                                                         	"Precio"                                                     "@I"                                                          Arial                                                         "Total"                                                      "@I"                                                          Arial                                                         subtot                                                        "999,999,999.99"                                                                                                            Arial                                                         subtot                                                        "99,999,999,999.99"                                                                                                         Arial                                                         	"Total:"                                                     Arial                                                         subtot                                                        "999,999,999.99"                                                                                                            Arial                                                         fecha                                                                                                                       Arial                                                         "TotalFactura:"                                              Arial                                                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                                                  Arial                                                         "Sucursal:"                                                   Arial                                                         Iva                                                           "999"                                                         Arial                                                         "% Iva"                                                      "@I"                                                          Arial                                                         ValorIva                                                      "999,999,999.99"                                              Arial                                                         ValorIva                                                      "99,999,999,999.99"                                           Arial                                                         	 ValorIva                                                     "999,999,999.99"                                              Arial                                                         "Iva"                                                        "@I"                                                          Arial                                                         iif(RT,'RT','')                                                                                                             Arial                                                         obs                                                                                                                         Arial                                                         "Forma Iva"                                                   Arial                                                         FormaIva                                                      Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             pPROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init

If Empty(m.sucursal)
	Store null To m.sucursal
endif

TEXT TO cmdSQL noshow
		select a.idfactura,a.fecha,a.idcomprobante,a.facturaproveedor numero, a.FormaIva,
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
		order by a.fecha, a.IdProveedor
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
             R���    9  9                        ��   %   �      �     �          �  U  
  �  � U  SETEO %�C��  ��� � J���(��  � �	 M(� ��Y �S 		select a.idfactura,a.fecha,a.idcomprobante,a.facturaproveedor numero, a.FormaIva,�F �@ 		rtrim(a.idproveedor)+'-'+c.razon proveedor,a.exenta,a.gravada,�L �F 		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, �B �< 		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,�R �L 		Precio = CASE when a.FormaIva='D' then b.precio else GravadaIncluido end ,�w �q 		subtot = CASE when a.FormaIva='D' then (b.cantidad*b.precio) + ValorIva else b.cantidad*b.GravadaIncluido end, � � 		b.Iva,�l �f 		ISNULL(b.ValorIva,0) as ValorIva, RTRIM(ISNULL(IdCliente,'')) +' '+  RTRIM(ISNULL(b.Obs,'')) as Obs,� � 		b.RegimenTurismo as RT�< �6 		from cp_factura a inner join st_movimiento_det b on �= �7 		a.idfactura=b.idcompra and a.IdEmpresa = b.IdEmpresa �] �W 		left join cp_proveedor c on a.idproveedor=c.idproveedor and a.IdEmpresa = c.idempresa�U �O 		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  �Z �T 		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa�U �O 		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)�; �5 		and (a.sucursal=?m.sucursal or ?m.sucursal is null)�' �! 		order by a.fecha, a.IdProveedor� � ��C � � rpedido� �� F� � U  SUCURSAL CMDSQL SQL RPEDIDO BeforeOpenTables,     �� InitA     ��1 q 2 � A � �a�!!q� �����Q�Q�qA �q 4                       $         ?   e      )   9                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           `\\futura5\HP DeskJet 840C/841C   � XC� 	 �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                                                                                                                             Arial                                                         	idfactura                                                     "Diario de Compra Detallado"                                                                                                Arial                                                         "Fact.Nro."                                                   Arial                                                         	proveedor                                                                                                                   Arial                                                         ""                                                           Arial                                                         "Fecha"                                                      "@I"                                                          Arial                                                         "Proveedor"                                                   Arial                                                         empresa                                                                                                                     Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
"Periodo:"                                                    Arial                                                         m.dfecha, ' al ' ,m.hfecha                                                                                                  Arial                                                         numero                                                                                                                      Arial                                                         producto                                                                                                                    Arial                                                         cantidad                                                      	"999,999"                                                                                                                   Arial                                                         "Cantidad"                                                   "@I"                                                          Arial                                                         "Descripci�n"                                                "@I"                                                          Arial                                                         precio                                                        "999,999,999.99"                                              Arial                                                         	"Precio"                                                     "@I"                                                          Arial                                                         "Total"                                                      "@I"                                                          Arial                                                         subtot                                                        "999,999,999.99"                                                                                                            Arial                                                         subtot                                                        "99,999,999,999.99"                                                                                                         Arial                                                         	"Total:"                                                     Arial                                                         subtot                                                        "999,999,999.99"                                                                                                            Arial                                                         fecha                                                                                                                       Arial                                                         "TotalFactura:"                                              Arial                                                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                                                  Arial                                                         "Sucursal:"                                                   Arial                                                         Iva                                                           "999"                                                         Arial                                                         "% Iva"                                                      "@I"                                                          Arial                                                         ValorIva                                                      "999,999,999.99"                                              Arial                                                         ValorIva                                                      "99,999,999,999.99"                                           Arial                                                         	 ValorIva                                                     "999,999,999.99"                                              Arial                                                         "Iva"                                                        "@I"                                                          Arial                                                         iif(RT,'RT','')                                                                                                             Arial                                                         obs                                                                                                                         Arial                                                         "Forma Iva"                                                   Arial                                                         FormaIva                                                      Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             ePROCEDURE Init

If Empty(m.sucursal)
	Store null To m.sucursal
endif

TEXT TO cmdSQL noshow
		select a.idfactura,a.fecha,a.idcomprobante,a.facturaproveedor numero, 
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
		order by a.fecha, a.IdProveedor
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
                        G���    .  .                        9�   %   u      �     �          �  U   %�C��  ��� � J���(��  � �	 M(� ��N �H 		select a.idfactura,a.fecha,a.idcomprobante,a.facturaproveedor numero, �F �@ 		rtrim(a.idproveedor)+'-'+c.razon proveedor,a.exenta,a.gravada,�L �F 		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, �B �< 		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,�R �L 		Precio = CASE when a.FormaIva='D' then b.precio else GravadaIncluido end ,�w �q 		subtot = CASE when a.FormaIva='D' then (b.cantidad*b.precio) + ValorIva else b.cantidad*b.GravadaIncluido end, � � 		b.Iva,�l �f 		ISNULL(b.ValorIva,0) as ValorIva, RTRIM(ISNULL(IdCliente,'')) +' '+  RTRIM(ISNULL(b.Obs,'')) as Obs,� � 		b.RegimenTurismo as RT�< �6 		from cp_factura a inner join st_movimiento_det b on �= �7 		a.idfactura=b.idcompra and a.IdEmpresa = b.IdEmpresa �] �W 		left join cp_proveedor c on a.idproveedor=c.idproveedor and a.IdEmpresa = c.idempresa�U �O 		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  �Z �T 		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa�U �O 		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)�; �5 		and (a.sucursal=?m.sucursal or ?m.sucursal is null)�' �! 		order by a.fecha, a.IdProveedor� � ��C � � rpedido� �� F� � U  SUCURSAL CMDSQL SQL RPEDIDO
  �  � U  SETEO Init,     �� BeforeOpenTables`    ��1 � A � �a�!!q� �����Q�Q�qA �q 5 q 1                       +        R  Z  !    )   .                                                                   