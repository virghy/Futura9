  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=1
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Courier New      Arial      Arial      Arial      Courier New      Arial      !"Control de Emisi�n de Contratos"      Arial      oApp.NombreEmpresa      Arial      m.dFecha, " " ,m.hFecha      Arial      	"Periodo"      Arial      
"Contrato"      Arial      "Fecha"      Arial      
"Vigencia"      Arial      "Tipo Cobertura"      Arial      "Estado"      Arial      
"Producto"      Arial      "Cuotas 
Pagadas"      Arial      "Importe Pagado"      Arial      
"Vendedor"      Arial      "Direcci�n"      Arial      "Condici�n"      Arial      5NroContrato," ",IdCliente, " ", NVL(Nombre,RazSocial)      Arial      dtoc(Fecha)      "@D"      Arial       dtoc(dFecha), " - ",dtoc(hFecha)      Arial      IdTipoCobertura,TipoCobertura      Arial      IdEstado      Arial      IdProducto,Descripcion      Arial      1alltrim(str(Cantidad)),"/",alltrim(str(NroCuota))      Arial      Pagado      "999,999,999"      Arial      DireccionServicio      Arial      IdVendedor,NombreVendedor      Arial      IdCondicion,Condicion      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      IdEstado      "99999"      Arial      "Total"      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo

TEXT TO cmdSQL noshow
SELECT     cn.NroContrato, cn.IdCliente, cl.RazSocial, cn.Fecha, cn.DireccionServicio, cn.IdTipoCobertura, tc.TipoCobertura, cn.IdCondicion, cn.IdEstado, cn.FechaBaja, cn.Obs, 
                      cn.IdVendedor, cn.dFecha, cn.hFecha, cn.DiaVence, cn.IdFormaPago, cn.PrimerVto, cn.ImporteEntrega, cn.NroCuota, cn.nombre, vn.NombreVendedor, 
                      cd.Descripcion AS Condicion, cn.Importe, p.Descripcion, cb.IdProducto, s.Cantidad,
                      s.Pagado
FROM         st_Producto AS p RIGHT OUTER JOIN
                      sas_Cobertura AS cb ON p.IdEmpresa = cb.IdEmpresa AND p.IdProducto = cb.IdProducto RIGHT OUTER JOIN
                      sas_Contrato AS cn INNER JOIN
                      vt_clientes AS cl ON cn.IdEmpresa = cl.IdEmpresa AND cn.IdCliente = cl.IdCliente INNER JOIN
                      sas_TipoCobertura AS tc ON cn.IdEmpresa = tc.IdEmpresa AND cn.IdTipoCobertura = tc.IdTipoCobertura ON cb.IdContrato = cn.IdContrato LEFT OUTER JOIN
                      vt_Condicion AS cd ON cn.IdEmpresa = cd.IdEmpresa AND cn.IdCondicion = cd.IdCondicion LEFT OUTER JOIN
                      vvt_Vendedores AS vn ON cn.IdEmpresa = vn.IdEmpresa AND cn.IdVendedor = vn.IdVendedor          
Left join (
Select count(*) as Cantidad,Sum(ISNULL(Importe-Saldo,TotalFactura)) as Pagado,NroContrato from vt_Forma_Pago  fp right join vt_Factura f
on fp.IdFactura = f.IdFactura
		where Saldo=0 and Importe<>0 and f.IdEmpresa=?oApp.Empresa   
group by NroContrato) S on cn.NroContrato = s.NroContrato
where cn.IdEmpresa=?oApp.Empresa
and cn.Fecha between ?m.dFecha and ?m.hFecha 
order by cn.NroContrato
ENDTEXT
sql(cmdSQL,'cContratos')
SELECT cContratos

ENDPROC
     ����    �  �                        ��   %   E      �     S          �  U  � �  �	 M(� ��� �� SELECT     cn.NroContrato, cn.IdCliente, cl.RazSocial, cn.Fecha, cn.DireccionServicio, cn.IdTipoCobertura, tc.TipoCobertura, cn.IdCondicion, cn.IdEstado, cn.FechaBaja, cn.Obs, �� ��                       cn.IdVendedor, cn.dFecha, cn.hFecha, cn.DiaVence, cn.IdFormaPago, cn.PrimerVto, cn.ImporteEntrega, cn.NroCuota, cn.nombre, vn.NombreVendedor, �n �h                       cd.Descripcion AS Condicion, cn.Importe, p.Descripcion, cb.IdProducto, s.Cantidad,�$ �                       s.Pagado�4 �. FROM         st_Producto AS p RIGHT OUTER JOIN� �y                       sas_Cobertura AS cb ON p.IdEmpresa = cb.IdEmpresa AND p.IdProducto = cb.IdProducto RIGHT OUTER JOIN�9 �3                       sas_Contrato AS cn INNER JOIN�w �q                       vt_clientes AS cl ON cn.IdEmpresa = cl.IdEmpresa AND cn.IdCliente = cl.IdCliente INNER JOIN�� ��                       sas_TipoCobertura AS tc ON cn.IdEmpresa = tc.IdEmpresa AND cn.IdTipoCobertura = tc.IdTipoCobertura ON cb.IdContrato = cn.IdContrato LEFT OUTER JOIN�� �{                       vt_Condicion AS cd ON cn.IdEmpresa = cd.IdEmpresa AND cn.IdCondicion = cd.IdCondicion LEFT OUTER JOIN�{ �u                       vvt_Vendedores AS vn ON cn.IdEmpresa = vn.IdEmpresa AND cn.IdVendedor = vn.IdVendedor          � � Left join (�� �� Select count(*) as Cantidad,Sum(ISNULL(Importe-Saldo,TotalFactura)) as Pagado,NroContrato from vt_Forma_Pago  fp right join vt_Factura f�# � on fp.IdFactura = f.IdFactura�E �? 		where Saldo=0 and Importe<>0 and f.IdEmpresa=?oApp.Empresa   �? �9 group by NroContrato) S on cn.NroContrato = s.NroContrato�& �  where cn.IdEmpresa=?oApp.Empresa�3 �- and cn.Fecha between ?m.dFecha and ?m.hFecha � � order by cn.NroContrato� � ��C � �
 cContratos� �� F� � U  SETEO CMDSQL SQL
 CCONTRATOS Init,     ��1 q � a�
�AA��q�
��1Q�a1�A �q 2                       �      )   �                  