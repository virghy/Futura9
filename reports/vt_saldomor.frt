  [                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      IdMoneda      	idcliente      Arial      Arial      Arial      Arial      Arial      Arial      "Facturas Pendientes de Cobro "             Arial      empresa             Arial      
"Sucursal"      Arial      ,iif(empty(m.sucursal),'Todos',desc_sucursal)             Arial      "  Cliente"      Arial      
"Tel�fono"      Arial      "Comprobante"      Arial      "Fecha Factura
"      Arial      " Vencimiento"      Arial      
" Importe"      Arial      
"Pagado
"      Arial      	"Saldo
"      Arial      "Pedido"      Arial      "Moneda:", rclimoroso.IdMoneda      Arial      nombre             Arial      telefono      Arial      comprobante,numero      Arial      	nropedido      "9,999,999,999"             Arial      importe      "999,999,999.99"      Arial      pagado      "999,999,999.99"      Arial      saldo      "999,999,999.99"             Arial      ttod(fecha)      "@D"      Arial      ttod(�ltimovencimiento)      "@D"      Arial      "Total Cliente:"      Arial      importe      "999,999,999.99"             Arial      pagado      "999,999,999.99"             Arial      saldo      "999,999,999.99"             Arial      "Total Moneda:"      Arial      importe      "999,999,999.99"      Arial      pagado      "999,999,999.99"      Arial      saldo      "999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      date()             Arial      time()             Arial      dataenvironment      ^Top = 6
Left = 10
Width = 381
Height = 380
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
If Empty(m.idcliente)
	Store null To m.idcliente
endif



TEXT TO cmdSQL noshow
	SELECT a.id_forma_pago,a.idcliente,rtrim(a.idcliente)+' - '+ b.razsocial AS nombre, 
	rtrim(f.Idcomprobante) AS comprobante,a.numero, a.fecha, 
	MIN(a.vencimiento) AS primervencimiento, 
	MAX(a.vencimiento) AS �ltimovencimiento, SUM(a.importe) as Importe, Sum(a.importe-a.saldo) pagado,
	SUM(a.saldo) as Saldo,a.idmoneda, c.descripcion AS moneda, b.telefono, 
	d.descripci�n AS desc_sucursal, g.nropedido 
	FROM vt_forma_pago a inner join vt_clientes b on a.idcliente = b.idcliente and a.idempresa=b.idempresa
	inner join bs_monedas c on  a.idmoneda = c.idmoneda 
	inner join sucursal d on a.sucursal = d.sucursal and a.idempresa=d.idempresa
	inner join vt_cpbt e on a.cod_docu = e.idcomprobante and a.idempresa=e.idempresa 
	inner join vt_factura f on a.idfactura=f.idfactura 
	left join vt_pedido g  on f.idpedido=g.idpedido
	WHERE a.idempresa = ?oapp.empresa
	AND (a.idcliente = ?m.idcliente  or ?m.idcliente is null)
	AND (a.Vencimiento BETWEEN ?m.dfecha and ?m.hfecha) 
	AND a.saldo <> 0 
	GROUP BY a.idmoneda, a.idcliente, a.FECHA ,b.razsocial,f.IdComprobante,b.Telefono,c.Descripcion, 
	d.Descripci�n,a.fecha,g.nropedido,a.id_forma_pago,a.numero
	order by a.IdMoneda,a.IdCliente,a.Fecha 

ENDTEXT
sql(cmdSQL,'rclimoroso')
SELECT rclimoroso



ENDPROC
     ����    �  �                        ��   %   �      4     �          �  U  
  �  � U  SETEOe %�C��  ��� � J���(��  � �	 M(� ��[ �U 	SELECT a.id_forma_pago,a.idcliente,rtrim(a.idcliente)+' - '+ b.razsocial AS nombre, �@ �: 	rtrim(f.Idcomprobante) AS comprobante,a.numero, a.fecha, �0 �* 	MIN(a.vencimiento) AS primervencimiento, �i �c 	MAX(a.vencimiento) AS �ltimovencimiento, SUM(a.importe) as Importe, Sum(a.importe-a.saldo) pagado,�N �H 	SUM(a.saldo) as Saldo,a.idmoneda, c.descripcion AS moneda, b.telefono, �3 �- 	d.descripci�n AS desc_sucursal, g.nropedido �m �g 	FROM vt_forma_pago a inner join vt_clientes b on a.idcliente = b.idcliente and a.idempresa=b.idempresa�; �5 	inner join bs_monedas c on  a.idmoneda = c.idmoneda �S �M 	inner join sucursal d on a.sucursal = d.sucursal and a.idempresa=d.idempresa�X �R 	inner join vt_cpbt e on a.cod_docu = e.idcomprobante and a.idempresa=e.idempresa �: �4 	inner join vt_factura f on a.idfactura=f.idfactura �6 �0 	left join vt_pedido g  on f.idpedido=g.idpedido�( �" 	WHERE a.idempresa = ?oapp.empresa�@ �: 	AND (a.idcliente = ?m.idcliente  or ?m.idcliente is null)�; �5 	AND (a.Vencimiento BETWEEN ?m.dfecha and ?m.hfecha) � � 	AND a.saldo <> 0 �h �b 	GROUP BY a.idmoneda, a.idcliente, a.FECHA ,b.razsocial,f.IdComprobante,b.Telefono,c.Descripcion, �A �; 	d.Descripci�n,a.fecha,g.nropedido,a.id_forma_pago,a.numero�/ �) 	order by a.IdMoneda,a.IdCliente,a.Fecha � �  � � ��C � �
 rclimoroso� �� F� � U 	 IDCLIENTE CMDSQL SQL
 RCLIMOROSO BeforeOpenTables,     �� InitA     ��1 q 2 � A � ���1��1��a�����a A �q 4                       $         ?   �      )   �                  