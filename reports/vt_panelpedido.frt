  V                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 )ORIENTATION=1
PAPERSIZE=32767
COLOR=2
      Arial      idpedido      Arial      Arial      Arial      Arial      Arial      Arial      alltrim( oApp.Nombreempresa )      Arial      "Control de Pedidos Detallado"      Arial      m.fecha      Arial      "Desde Fecha:"      Arial      `IIF(m.Estado='P','Pendientes',iif(m.Estado='E','Entregado',iif(m.Estado='T','Todos','Anulado')))      Arial      	"Estado:"      Arial      IIIF(m.TipoEntrega='D','Delivery',iif(m.TipoEntrega='R','Retira','Todos'))      Arial      "Tipo Entrega:"      Arial      "
"      Arial      "Entregar el
"      "@I"      Arial      "Nro"      Arial      	"Cliente"      Arial      "Direccion"      Arial      
"Telefono"      Arial      
"Anticipo"      Arial      "Facturado"      Arial      "Repartidor
"      Arial      " Estado
"      "@I"      Arial      "Tipo
"      "@I"      Arial      	"Fecha
"      "@I"      Arial      "Personaliza Taza"      Arial      "Texto Tarjeta"      Arial      "Texto Taza"      Arial      "Producto
"      "@I"      Arial      "Cantidad
"      "@I"      Arial      
"Precio
"      "@I"      Arial      	"Total
"      "@I"      Arial      "dtoc(fechaentrega),' ',horaentrega      Arial      	nropedido      "999999"      Arial      RazonSocial      Arial      Direccion,' ', Referencia      Arial      Telefono      Arial      Anticipo      "999,999,999"      Arial      "SI"      Arial      	Facturado      
repartidor             Arial      )iif(idestado='P','Pendiente','Entregado')             Arial      (iif(TipoEntrega='D','Delivery','Retira')      Arial      fechapedido             Arial      TextoTarjeto      Arial      #iif(nvl(personaliza,.f.),'SI','NO')      Arial      	TextoTaza      Arial      Obs      Arial      descripcion             Arial      cantidad      	"999,999"             Arial      precio      	"999,999"             Arial      cantidad*precio      "99,999,999"             Arial      cantidad*precio      "99,999,999"             Arial      
datetime()             Arial      "P�g. " + str( _pageno,3 )             Arial      
"Total:
"      Arial      cantidad*precio      "99,999,999"             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
Release m.total
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
TEXT TO cmdsql noshow
SELECT     a.IdPedido, a.NroPedido, RTRIM(a.IdCliente) + '-' + h.RazSocial AS cliente, a.Importe, RTRIM(d.Nombre) + ' ' + RTRIM(d.Apellido) AS receptor, RTRIM(f.Nombre) 
                      + ' ' + f.Apellido AS repartidor, b.Cantidad, b.Precio, RTRIM(g.IdProducto) + '-' + g.Descripcion AS descripcion, a.FechaPedido, a.HoraPedido, a.FechaEntrega, 
                      a.IdCliente, a.IdEstado, RTRIM(a.IdNegocio) + '-' + i.Negocio AS negocio, a.Facturado, a.Anticipo, a.Referencia, a.TipoEntrega, a.RazonSocial, a.Obs, a.Direccion, 
                      a.HoraEntrega, h.Telefono,a.Personaliza, a.TextoTarjeto, a.TextoTaza
FROM         VT_Pedido AS a INNER JOIN
                      VT_PedidoDet AS b ON a.IdPedido = b.IdPedido LEFT OUTER JOIN
                      vt_clientes AS h ON a.IdEmpresa = h.IdEmpresa AND a.IdCliente = h.IdCliente LEFT OUTER JOIN
                      vt_Negocio AS i ON a.IdEmpresa = i.IdEmpresa AND a.IdNegocio = i.IdNegocio LEFT OUTER JOIN
                      st_Producto AS g ON b.IdEmpresa = g.IdEmpresa AND b.IdProducto = g.IdProducto LEFT OUTER JOIN
                      VT_Repartidor AS e ON a.IdEmpresa = e.IdEmpresa AND a.IdRepartidor = e.IdRepartidor LEFT OUTER JOIN
                      VT_Receptor AS c ON a.IdEmpresa = c.IdEmpresa AND a.IdReceptor = c.IdReceptor LEFT OUTER JOIN
                      BS_Personas AS d ON c.IdPersona = d.IdPersona LEFT OUTER JOIN
                      BS_Personas AS f ON e.IdPersona = f.IdPersona
WHERE     (a.IdEmpresa = ?oApp.empresa) 
AND (a.FechaEntrega >= ?m.fecha) 
AND (a.IdEstado = ?m.Estado or ?m.Estado='T') 
and (a.TipoEntrega = ?m.TipoEntrega or ?m.TipoEntrega='T')
ORDER BY a.FechaEntrega, a.HoraEntrega, a.NroPedido

ENDTEXT
sql(cmdsql,'rpedido')

SELECT rpedido

*Sum Importe To m.Total

ENDPROC
     ����    �  �                        ��   %   �      )     �          �  U   	 <��  � U  TOTAL
  �  � U  SETEOD	 M(�  ��� �� SELECT     a.IdPedido, a.NroPedido, RTRIM(a.IdCliente) + '-' + h.RazSocial AS cliente, a.Importe, RTRIM(d.Nombre) + ' ' + RTRIM(d.Apellido) AS receptor, RTRIM(f.Nombre) �� ��                       + ' ' + f.Apellido AS repartidor, b.Cantidad, b.Precio, RTRIM(g.IdProducto) + '-' + g.Descripcion AS descripcion, a.FechaPedido, a.HoraPedido, a.FechaEntrega, �� ��                       a.IdCliente, a.IdEstado, RTRIM(a.IdNegocio) + '-' + i.Negocio AS negocio, a.Facturado, a.Anticipo, a.Referencia, a.TipoEntrega, a.RazonSocial, a.Obs, a.Direccion, �` �Z                       a.HoraEntrega, h.Telefono,a.Personaliza, a.TextoTarjeto, a.TextoTaza�, �& FROM         VT_Pedido AS a INNER JOIN�X �R                       VT_PedidoDet AS b ON a.IdPedido = b.IdPedido LEFT OUTER JOIN�w �q                       vt_clientes AS h ON a.IdEmpresa = h.IdEmpresa AND a.IdCliente = h.IdCliente LEFT OUTER JOIN�v �p                       vt_Negocio AS i ON a.IdEmpresa = i.IdEmpresa AND a.IdNegocio = i.IdNegocio LEFT OUTER JOIN�y �s                       st_Producto AS g ON b.IdEmpresa = g.IdEmpresa AND b.IdProducto = g.IdProducto LEFT OUTER JOIN� �y                       VT_Repartidor AS e ON a.IdEmpresa = e.IdEmpresa AND a.IdRepartidor = e.IdRepartidor LEFT OUTER JOIN�y �s                       VT_Receptor AS c ON a.IdEmpresa = c.IdEmpresa AND a.IdReceptor = c.IdReceptor LEFT OUTER JOIN�Y �S                       BS_Personas AS d ON c.IdPersona = d.IdPersona LEFT OUTER JOIN�I �C                       BS_Personas AS f ON e.IdPersona = f.IdPersona�. �( WHERE     (a.IdEmpresa = ?oApp.empresa) �' �! AND (a.FechaEntrega >= ?m.fecha) �4 �. AND (a.IdEstado = ?m.Estado or ?m.Estado='T') �@ �: and (a.TipoEntrega = ?m.TipoEntrega or ?m.TipoEntrega='T')�9 �3 ORDER BY a.FechaEntrega, a.HoraEntrega, a.NroPedido� �  � � ��C �  � rpedido� �� F� � U  CMDSQL SQL RPEDIDO Destroy,     �� BeforeOpenTablesC     �� InitX     ��1 � 2 q 2 � �
����qa������qA�a A �r 4                       "         I   Q         l   �      )   �                  