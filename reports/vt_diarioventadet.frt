  !�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
      W    winspool  PrimoPDF  PrimoPort:                  3C  USB001                          �PrimoPDF OneNote 2013           � �S� 	 �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        RTRAIT Resolution DPI600 ColorMode 24bpp                      Arial      IdMoneda      Sucursal      	idfactura      3_VFP.SetVar('tfSucursal',tfSucursal + TotalFactura)      -_VFP.SetVar('tfMoneda',tfMoneda + tfSucursal)      SubTot1      subTot      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Diario de Venta Detallado"      Arial      empresa             Arial      +iif(isnull(m.sucursal),'Todos',descripci�n)      Arial      "Sucursal:"      Arial      (iif(isnull(m.IdCliente),'Todos',Cliente)      Arial      
"Cliente:"      Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      	"Fecha
"      "@I"      Arial      	"Cliente"      Arial      "Fact.Nro."      Arial      "Forma Iva"      Arial      "
"      Arial      "Iva
"      "@I"      Arial      "Descripci�n
"      "@I"      Arial      "Cantidad
"      "@I"      Arial      
"Precio
"      "@I"      Arial      	"% Iva
"      "@I"      Arial      
"% Desc
"      "@I"      Arial      	"Total
"      "@I"      Arial      "Moneda ", Idmoneda      Arial      '"Sucursal ",Sucursal, " " , Descripci�n      Arial      fecha             Arial      Cliente      Arial      1IdCondicion, alltrim(IdComprobante), "-" , numero      Arial      FormaIva      Arial      obs             Arial      iif(RT,'RT','')             Arial      producto             Arial      Dtransform(cantidad,"999,999."+REPLICATE('9', OAPP.PRODUCTO_DECIMAL))      Arial      precio      "999,999,999.99"      Arial      Iva      "999"      Arial      	Descuento      "999"      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      "Total Factura:
"      Arial      -"Total Sucursal ",Sucursal, " " , Descripci�n      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      "Total Moneda ", Idmoneda      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      ~Top = 32
Left = 177
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     hPROCEDURE Destroy
release tfSucursal,tiSucursal,tdSucursal,tfMoneda,tiMoneda,tiMoneda

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init

If Empty(m.sucursal)
	Store null To m.sucursal
endif


If Empty(m.IdCliente)
	Store null To m.Idcliente
endif

TEXT TO cmdSQL noshow
		select a.idfactura,a.fecha,a.idcomprobante,a.numero,a.IdMoneda,v.Tipo_Iva as FormaIva,
		rtrim(a.Idcliente)+'-'+c.RazSocial Cliente,a.exenta,a.gravada,
		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, 
		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,
--		Precio = CASE when v.Tipo_Iva='C' then b.precio else Real end ,
		Precio = Real,
--		subtot = CASE when v.Tipo_Iva='C' then (b.cantidad*b.precio) else (b.cantidad*b.Real)+ ValorIva   end, 
		subtot = (b.cantidad*b.Real), 
		b.Iva,
		ISNULL(b.ValorIva,0) as ValorIva,   RTRIM(ISNULL(b.Obs,'')) as Obs,
		b.RegimenTurismo as RT,
		b.Descuento,a.ImpDesc,a.TotalFactura,a.Iva as TotalIva,a.IdCondicion 
		from vt_Factura a inner join st_movimiento_det b on 
		a.idfactura=b.IdFactura and a.IdEmpresa = b.IdEmpresa 
		left join vt_clientes c on a.IdCliente=c.Idcliente and a.IdEmpresa = c.idempresa
		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  
		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa
		left join vt_cpbt v on a.IdComprobante = v.IdComprobante and a.Idempresa = v.IdEmpresa
		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)
		and (a.sucursal=?m.sucursal or ?m.sucursal is null)	
		and (a.Idcliente=?m.Idcliente or ?m.Idcliente is null)
		order by a.IdMoneda,a.Sucursal,a.fecha, a.Numero
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

PUBLIC tfSucursal,tiSucursal,tdSucursal,tfMoneda,tiMoneda,tiMoneda

tfSucursal=0
tfMoneda=0

*Sum Importe To m.Total

ENDPROC
     ����    q  q                        5@   %   }        )   �          �  U    <�  � � � � � � U 
 TFSUCURSAL
 TISUCURSAL
 TDSUCURSAL TFMONEDA TIMONEDA
  �  � U  SETEO{ %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � �	 M(� ��^ �X 		select a.idfactura,a.fecha,a.idcomprobante,a.numero,a.IdMoneda,v.Tipo_Iva as FormaIva,�F �@ 		rtrim(a.Idcliente)+'-'+c.RazSocial Cliente,a.exenta,a.gravada,�L �F 		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, �B �< 		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,�I �C --		Precio = CASE when v.Tipo_Iva='C' then b.precio else Real end ,� � 		Precio = Real,�q �k --		subtot = CASE when v.Tipo_Iva='C' then (b.cantidad*b.precio) else (b.cantidad*b.Real)+ ValorIva   end, �& �  		subtot = (b.cantidad*b.Real), � � 		b.Iva,�K �E 		ISNULL(b.ValorIva,0) as ValorIva,   RTRIM(ISNULL(b.Obs,'')) as Obs,� � 		b.RegimenTurismo as RT,�M �G 		b.Descuento,a.ImpDesc,a.TotalFactura,a.Iva as TotalIva,a.IdCondicion �< �6 		from vt_Factura a inner join st_movimiento_det b on �> �8 		a.idfactura=b.IdFactura and a.IdEmpresa = b.IdEmpresa �X �R 		left join vt_clientes c on a.IdCliente=c.Idcliente and a.IdEmpresa = c.idempresa�U �O 		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  �Z �T 		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa�^ �X 		left join vt_cpbt v on a.IdComprobante = v.IdComprobante and a.Idempresa = v.IdEmpresa�U �O 		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)�< �6 		and (a.sucursal=?m.sucursal or ?m.sucursal is null)	�> �8 		and (a.Idcliente=?m.Idcliente or ?m.Idcliente is null)�8 �2 		order by a.IdMoneda,a.Sucursal,a.fecha, a.Numero� � ��C � � rpedido� �� F� � 7� � � � �	 �	 � T� �� �� T� �� �� U
  SUCURSAL	 IDCLIENTE CMDSQL SQL RPEDIDO
 TFSUCURSAL
 TISUCURSAL
 TDSUCURSAL TFMONEDA TIMONEDA Destroy,     �� BeforeOpenTables�     �� Init�     ��1 �3 q 2 � A � A � �a�!�aa� ������Q��Q���A �q �� � 4                       X            �         �   ]      )   q                  