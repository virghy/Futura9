  %W                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=1
      Arial      IdMoneda      Franquiciador      cliente      	idfactura      3_VFP.SetVar('tfSucursal',tfSucursal + TotalFactura)      -_VFP.SetVar('tfMoneda',tfMoneda + tfSucursal)      SubTot1      subTot      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Detalle Comisi�n Franquicia"      Arial      empresa             Arial      +iif(isnull(m.sucursal),'Todos',descripci�n)      Arial      "Sucursal:"      Arial      (iif(isnull(m.IdCliente),'Todos',Cliente)      Arial      
"Cliente:"      Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      	"Cliente"      Arial      	"Fecha
"      "@I"      Arial      "Fact.Nro."      Arial      "Forma Iva"      Arial      "Franquicia"      "@I"      Arial      "
"      Arial      "Iva
"      "@I"      Arial      "Descripci�n
"      "@I"      Arial      "Cantidad
"      "@I"      Arial      
"Precio
"      "@I"      Arial      	"% Iva
"      "@I"      Arial      
"% Desc
"      "@I"      Arial      	"Total
"      "@I"      Arial      "Moneda ", Idmoneda      Arial      "Franquiciador:",Franquiciador      Arial      Cliente      Arial      fecha      "@D"      Arial      $alltrim(IdComprobante), "-" , numero      Arial      FormaIva      Arial      obs             Arial      iif(RT,'RT','')             Arial      producto             Arial      Dtransform(cantidad,"999,999."+REPLICATE('9', OAPP.PRODUCTO_DECIMAL))      Arial      precio      "999,999,999.99"      Arial      Iva      "999"      Arial      	Descuento      "999"      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      
Franquicia      "999,999,999.99"      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      
Franquicia      "999,999,999.99"      Arial      "Total Factura:
"      Arial      "Total Cliente "      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      
Franquicia      "999,999,999.99"      Arial      "Total Franq "      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      
Franquicia      "999,999,999.99"      Arial      "Total Moneda ", Idmoneda      Arial      &subtot - (subtot*nvl(Descuento,0)/100)      "999,999,999.99"      Arial      4round(ValorIva - (ValorIva *nvl(Descuento,0)/100),0)      "999,999,999.99"      Arial      
Franquicia      "999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      ~Top = 32
Left = 177
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     
wPROCEDURE Init

If Empty(m.sucursal)
	Store null To m.sucursal
endif


If Empty(m.IdCliente)
	Store null To m.Idcliente
endif
If Empty(m.IdFranquiciador)
	Store null To m.IdFranquiciador
endif

TEXT TO cmdSQL noshow
WITH CTE_Empleados (IdCliente,IdEmpresa,IdFranquiciador, Nivel)
AS
(
	SELECT IdCliente,IdEmpresa,IdFranquiciador, 0 as Nivel 
	FROM vt_Clientes where 
	(IDCliente=?m.IdFranquiciador or (?m.IdFranquiciador is null and IdFranquiciador is null))
	and idempresa=?oApp.Empresa
	UNION ALL
    SELECT e.IdCliente,e.IdEmpresa,e.IdFranquiciador, Nivel + 1
	FROM vt_clientes e
    INNER JOIN CTE_Empleados cte ON e.IdFranquiciador = cte.IdCliente and e.IDEmpresa=cte.IdEmpresa
)
		select a.idfactura,a.fecha,a.idcomprobante,a.numero,a.IdMoneda,v.Tipo_Iva as FormaIva,
		rtrim(a.Idcliente)+'-'+c.RazSocial Cliente,a.exenta,a.gravada,
		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, 
		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,
		Precio = Real,
		subtot = (b.cantidad*b.Real), 
		b.Iva,
		ISNULL(b.ValorIva,0) as ValorIva,   RTRIM(ISNULL(b.Obs,'')) as Obs,
		b.RegimenTurismo as RT,
		b.Descuento,a.ImpDesc,a.TotalFactura,a.Iva as TotalIva,
		cf.Valor as Franquicia, f.Nivel,f.IdFranquiciador + cl.RazSocial as Franquiciador
		from vt_Factura a inner join st_movimiento_det b on 
		a.idfactura=b.IdFactura and a.IdEmpresa = b.IdEmpresa 
		left join vt_clientes c on a.IdCliente=c.Idcliente and a.IdEmpresa = c.idempresa
		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  
		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa
		left join vt_cpbt v on a.IdComprobante = v.IdComprobante and a.Idempresa = v.IdEmpresa
		left join vt_ComisionFranquicia cf on a.Idfactura=cf.IdFactura
		inner join  cte_Empleados f on f.IdCliente=a.IDCliente and f.IdEmpresa=a.IdEmpresa
		inner join vt_Clientes cl on f.IdFranquiciador=cl.IdCliente and f.IdEmpresa=cl.Idempresa
		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)
		and (a.sucursal=?m.sucursal or ?m.sucursal is null)	
		and (a.Idcliente=?m.Idcliente or ?m.Idcliente is null)
		and (f.IdFranquiciador=?m.IdFranquiciador or ?m.IdFranquiciador is null)
		order by a.IdMoneda,f.IdFranquiciador,a.IdCliente,a.fecha, a.Numero
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido

PUBLIC tfSucursal,tiSucursal,tdSucursal,tfMoneda,tiMoneda,tiMoneda

tfSucursal=0
tfMoneda=0

*Sum Importe To m.Total

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Destroy
release tfSucursal,tiSucursal,tdSucursal,tfMoneda,tiMoneda,tiMoneda

ENDPROC
     ����    �  �                        <#   %   �
      d  ;   �
          �  U  �	 %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � � %�C�� ���b � J���(�� � �	 M(� ��E �? WITH CTE_Empleados (IdCliente,IdEmpresa,IdFranquiciador, Nivel)� � AS� � (�> �8 	SELECT IdCliente,IdEmpresa,IdFranquiciador, 0 as Nivel � � 	FROM vt_Clientes where �a �[ 	(IDCliente=?m.IdFranquiciador or (?m.IdFranquiciador is null and IdFranquiciador is null))�" � 	and idempresa=?oApp.Empresa� �
 	UNION ALL�E �?     SELECT e.IdCliente,e.IdEmpresa,e.IdFranquiciador, Nivel + 1� � 	FROM vt_clientes e�i �c     INNER JOIN CTE_Empleados cte ON e.IdFranquiciador = cte.IdCliente and e.IDEmpresa=cte.IdEmpresa� � )�^ �X 		select a.idfactura,a.fecha,a.idcomprobante,a.numero,a.IdMoneda,v.Tipo_Iva as FormaIva,�F �@ 		rtrim(a.Idcliente)+'-'+c.RazSocial Cliente,a.exenta,a.gravada,�L �F 		a.exenta+a.gravada+a.iva total,a.idmoneda,a.sucursal,d.descripci�n, �B �< 		rtrim(b.idproducto)+'-'+e.descripcion producto,b.cantidad,� � 		Precio = Real,�& �  		subtot = (b.cantidad*b.Real), � � 		b.Iva,�K �E 		ISNULL(b.ValorIva,0) as ValorIva,   RTRIM(ISNULL(b.Obs,'')) as Obs,� � 		b.RegimenTurismo as RT,�? �9 		b.Descuento,a.ImpDesc,a.TotalFactura,a.Iva as TotalIva,�Y �S 		cf.Valor as Franquicia, f.Nivel,f.IdFranquiciador + cl.RazSocial as Franquiciador�< �6 		from vt_Factura a inner join st_movimiento_det b on �> �8 		a.idfactura=b.IdFactura and a.IdEmpresa = b.IdEmpresa �X �R 		left join vt_clientes c on a.IdCliente=c.Idcliente and a.IdEmpresa = c.idempresa�U �O 		left join sucursal d on a.sucursal=d.sucursal and a.IdEmpresa = d.IdEmpresa  �Z �T 		left join st_producto e on b.idproducto=e.idproducto and a.IdEmpresa = e.idempresa�^ �X 		left join vt_cpbt v on a.IdComprobante = v.IdComprobante and a.Idempresa = v.IdEmpresa�F �@ 		left join vt_ComisionFranquicia cf on a.Idfactura=cf.IdFactura�Z �T 		inner join  cte_Empleados f on f.IdCliente=a.IDCliente and f.IdEmpresa=a.IdEmpresa�` �Z 		inner join vt_Clientes cl on f.IdFranquiciador=cl.IdCliente and f.IdEmpresa=cl.Idempresa�U �O 		where a.idempresa=?oApp.Empresa and (a.fecha between ?m.dfecha and ?m.hfecha)�< �6 		and (a.sucursal=?m.sucursal or ?m.sucursal is null)	�> �8 		and (a.Idcliente=?m.Idcliente or ?m.Idcliente is null)�P �J 		and (f.IdFranquiciador=?m.IdFranquiciador or ?m.IdFranquiciador is null)�K �E 		order by a.IdMoneda,f.IdFranquiciador,a.IdCliente,a.fecha, a.Numero� � ��C � � rpedido� �� F� � 7� � � �	 �
 �
 � T� �� �� T�	 �� �� U  SUCURSAL	 IDCLIENTE IDFRANQUICIADOR CMDSQL SQL RPEDIDO
 TFSUCURSAL
 TISUCURSAL
 TDSUCURSAL TFMONEDA TIMONEDA
  �  � U  SETEO  <�  � � � � � � U 
 TFSUCURSAL
 TISUCURSAL
 TDSUCURSAL TFMONEDA TIMONEDA Init,     �� BeforeOpenTablesF
    �� Destroy[
    ��1 � A � A � A � Q� q ��!Q��q �a�!aa� �������Q��a�Q���A �q �� � 5 q 2 �2                       �	     7   
  	
  B   9   '
  l
  E    )   �                  