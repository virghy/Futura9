  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=1
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=200
COLOR=2
YRESOLUTION=200
TTOPTION=3
COLLATE=1
      H    winspool  PrimoPDF  PrimoPort:                                       �PrimoPDF OneNote 2013           � �S�  �4d   �   �   A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        RTRAIT Resolution DPI600 ColorMode 24bpp                      Arial      rvendedor.vendedor      Arial      Arial      Arial      Arial      Arial      "Ventas por Vendedor"             Arial      empresa             Arial      
"Per�odo:"      Arial      m.dfecha," al " ,m.hfecha      Arial      "Nro.
Pedido"      "@I"      Arial      
"Comprob."      Arial      "Nro."      Arial      	"Cliente"      Arial      	"Fecha
"      Arial      "Condicion"      Arial      	"Negocio"      Arial      "Total"      Arial      
"Comisi�n"      Arial      "TotalComisi�n"      Arial      rvendedor.vendedor             Arial      !SoloResumen      rvendedor.nropedido      Arial      rvendedor.comp             Arial      rvendedor.numero             Arial      rvendedor.cliente             Arial      rvendedor.fecha             Arial      rvendedor.condicion             Arial      negocio             Arial      rvendedor.comision      "@Z 999.99"             Arial      total * rvendedor.comision/100      "@Z 999,999,999.99"      Arial      rvendedor.total      "9,999,999,999.99"             Arial      "Total ", rvendedor.vendedor      Arial      SoloResumen      rvendedor.total      "9,999,999,999,999.99"             Arial      total * rvendedor.comision /100      "9,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "rvendedor.total+rvendedor.comision      "9,999,999,999.99"             Arial       total * rvendedor.comision / 100      "9,999,999,999.99"             Arial      "Total General"      Arial      dataenvironment      �Top = 169
Left = -26
Width = 793
Height = 439
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rventacliente"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
If Empty(m.idvendedor)
	Store null To m.idvendedor
endif
TEXT TO cmdSQL noshow
SELECT     ISNULL(d.NroPedido, 0) AS nropedido, RTRIM(a.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido) AS vendedor, e.Descripcion AS condicion, f.Negocio, 
                      ISNULL(a.Comision, 0) AS comision, a.Fecha, a.Gravada + a.Exenta + a.Iva AS total, a.IdComprobante AS comp, a.Numero, RTRIM(a.IdCliente) 
                      + '-' + g.RazSocial AS cliente
FROM         vt_factura AS a LEFT OUTER JOIN
                      VT_Pedido AS d ON a.IdPedido = d.IdPedido AND a.IdEmpresa = d.IdEmpresa LEFT OUTER JOIN
                      vt_Vendedores AS b ON a.IdVendedor = b.IdVendedor AND a.IdEmpresa = b.IdEmpresa LEFT OUTER JOIN
                      BS_Personas AS c ON b.idpersona = c.IdPersona LEFT OUTER JOIN
                      vt_Condicion AS e ON a.IdCondicion = e.IdCondicion AND a.IdEmpresa = e.IdEmpresa LEFT OUTER JOIN
                      vt_Negocio AS f ON a.IdNegocio = f.IdNegocio AND a.IdEmpresa = f.IdEmpresa LEFT OUTER JOIN
                      vt_clientes AS g ON a.IdCliente = g.IdCliente AND a.IdEmpresa = g.IdEmpresa
WHERE     (a.Fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (a.IdVendedor = ?m.idvendedor or ?m.idvendedor IS NULL)
and a.IdEmpresa = ?oApp.Empresa

ORDER BY a.IdVendedor, a.Fecha

ENDTEXT


sql(cmdSQL,'rvendedor')



SELECT rvendedor

ENDPROC
     ����    �  �                        M�   %   �      C               �  U  
  �  � U  SETEO~ %�C��  ��� � J���(��  � �	 M(� ��� �� SELECT     ISNULL(d.NroPedido, 0) AS nropedido, RTRIM(a.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido) AS vendedor, e.Descripcion AS condicion, f.Negocio, �� ��                       ISNULL(a.Comision, 0) AS comision, a.Fecha, a.Gravada + a.Exenta + a.Iva AS total, a.IdComprobante AS comp, a.Numero, RTRIM(a.IdCliente) �: �4                       + '-' + g.RazSocial AS cliente�2 �, FROM         vt_factura AS a LEFT OUTER JOIN�s �m                       VT_Pedido AS d ON a.IdPedido = d.IdPedido AND a.IdEmpresa = d.IdEmpresa LEFT OUTER JOIN�{ �u                       vt_Vendedores AS b ON a.IdVendedor = b.IdVendedor AND a.IdEmpresa = b.IdEmpresa LEFT OUTER JOIN�Y �S                       BS_Personas AS c ON b.idpersona = c.IdPersona LEFT OUTER JOIN�| �v                       vt_Condicion AS e ON a.IdCondicion = e.IdCondicion AND a.IdEmpresa = e.IdEmpresa LEFT OUTER JOIN�v �p                       vt_Negocio AS f ON a.IdNegocio = f.IdNegocio AND a.IdEmpresa = f.IdEmpresa LEFT OUTER JOIN�g �a                       vt_clientes AS g ON a.IdCliente = g.IdCliente AND a.IdEmpresa = g.IdEmpresa�u �o WHERE     (a.Fecha BETWEEN ?m.dfecha AND ?m.hfecha) AND (a.IdVendedor = ?m.idvendedor or ?m.idvendedor IS NULL)�% � and a.IdEmpresa = ?oApp.Empresa� �  �$ � ORDER BY a.IdVendedor, a.Fecha� �  � � ��C � �	 rvendedor� �� F� � U 
 IDVENDEDOR CMDSQL SQL	 RVENDEDOR BeforeOpenTables,     �� InitA     ��1 q 2 � A � Q
�!1���aqQQa Aa A �t 2                       $         ?   �      )   �                  