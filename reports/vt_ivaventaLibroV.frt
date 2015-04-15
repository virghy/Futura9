  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'ORIENTATION=0
PAPERSIZE=130
COLOR=2
      Arial      Comprobante      Arial      Arial      %NombreMes(Month(cn_rivaventas.fecha))      Arial      year(cn_rivaventas.fecha)      "9999"      Arial      "MES:
"             Arial      "A�O:
"      Arial      "Facturas/Notas
"      Arial      "Valor Ventas"      Arial      " RUC del 
Comprador"      Arial      "Dia"      Arial      "Numero"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      "Comprobante: ", Comprobante      Arial      day(cn_rivaventas.fecha)      "@L 99"      Arial      4transform(cn_rivaventas.numero,"@L 999-999-9999999")      Arial      cn_rivaventas.ruc      Arial      len( ALLTRIM(ruc) ) > 8      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"       cn_rivacompra.iva5      Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"      Arial      "Total: ", Comprobante      Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      "Transporte"      Arial      _PAGETOTAL <> _PAGENO      	gravadas5      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      iva5      "999,999,999"      Arial      _PAGETOTAL <> _PAGENO      
gravadas10      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      iva10      "999,999,999"      Arial      _PAGETOTAL <> _PAGENO      exentas      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      "Total General"             Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      dataenvironment      �Top = 104
Left = 2
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
SET TEXTMERGE ON 
   
TEXT TO cmdSQL noshow
SELECT     c.Numero, c.Fecha, c.Exenta* c.Cotizacion as Exentas, c.Gravada, c.Iva, total = c.TotalFactura, p.RazSocial, 
ISNULL(p.rucHechauka,p.Ruc) as RUC, 
                      SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.precio * cantidad* c.Cotizacion, 0) ELSE 0 END) AS gravadas5, 
                      SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.precio * cantidad* c.Cotizacion, 0) ELSE 0 END) AS gravadas10, 
                      SUM(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* c.Cotizacion, 0) ELSE 0 END) AS imponible, 
                      SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.Valoriva* c.Cotizacion, 0) ELSE 0 END) AS iva5, 
                      SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.Valoriva* c.Cotizacion, 0) ELSE 0 END) AS iva10,
                      CASE when c.IdComprobante='NC' then 'NOTAS DE CREDITO' ELSE 'FACTURAS' END AS Comprobante
FROM         vt_factura AS c INNER JOIN
                      st_movimiento_Det AS d ON c.IdFactura = d.IdFactura INNER JOIN
                      vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente
WHERE c.idempresa = ?oApp.Empresa  
        AND MONTH(c.fecha) = ?m.mes  
        AND YEAR(c.fecha) = ?m.a�o  
	  GROUP BY CASE when c.IdComprobante='NC' then 'NOTAS DE CREDITO' ELSE 'FACTURAS' END, c.Numero, c.Fecha, c.Exenta, c.Gravada, c.Iva, c.TotalFactura, p.RazSocial, ISNULL(p.rucHechauka,p.Ruc) , c.Cotizacion
 ORDER BY CASE when c.IdComprobante='NC' then 'NOTAS DE CREDITO' ELSE 'FACTURAS' END, c.Fecha, c.Numero  

ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        p_   %   �      .     �          �  U  q G` �	 M(�  ��~ �x SELECT     c.Numero, c.Fecha, c.Exenta* c.Cotizacion as Exentas, c.Gravada, c.Iva, total = c.TotalFactura, p.RazSocial, �* �$ ISNULL(p.rucHechauka,p.Ruc) as RUC, �� �z                       SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.precio * cantidad* c.Cotizacion, 0) ELSE 0 END) AS gravadas5, �� �|                       SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.precio * cantidad* c.Cotizacion, 0) ELSE 0 END) AS gravadas10, �} �w                       SUM(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* c.Cotizacion, 0) ELSE 0 END) AS imponible, �r �l                       SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.Valoriva* c.Cotizacion, 0) ELSE 0 END) AS iva5, �s �m                       SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.Valoriva* c.Cotizacion, 0) ELSE 0 END) AS iva10,�u �o                       CASE when c.IdComprobante='NC' then 'NOTAS DE CREDITO' ELSE 'FACTURAS' END AS Comprobante�- �' FROM         vt_factura AS c INNER JOIN�Z �T                       st_movimiento_Det AS d ON c.IdFactura = d.IdFactura INNER JOIN�g �a                       vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente�) �# WHERE c.idempresa = ?oApp.Empresa  �+ �%         AND MONTH(c.fecha) = ?m.mes  �* �$         AND YEAR(c.fecha) = ?m.a�o  �� �� 	  GROUP BY CASE when c.IdComprobante='NC' then 'NOTAS DE CREDITO' ELSE 'FACTURAS' END, c.Numero, c.Fecha, c.Exenta, c.Gravada, c.Iva, c.TotalFactura, p.RazSocial, ISNULL(p.rucHechauka,p.Ruc) , c.Cotizacion�o �i  ORDER BY CASE when c.IdComprobante='NC' then 'NOTAS DE CREDITO' ELSE 'FACTURAS' END, c.Fecha, c.Numero  � �  � � ��C �  � cn_rIvaVentas� �� F� � U  CMDSQL SQL CN_RIVAVENTAS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 a � ��!�!1Q��q���A�a A �r 3 q 2                       u        �  �      )   �                  