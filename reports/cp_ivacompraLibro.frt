  -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Enviar a OneNote 2013
OUTPUT=nul:
ORIENTATION=1
PAPERSIZE=5
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
      ?  )  winspool  Enviar a OneNote 2013  nul:                       �Enviar a OneNote 2013           � /    �4d   X  X  A4                                                            ����                DINU" �   ���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         �   SMTJ     �                                                                                                                                                                                       Arial      COMPROBANTE      Arial      Arial      Arial      %NombreMes(MONTH(cn_rivacompra.fecha))      Arial      year(cn_rivacompra.fecha)      "9999"      Arial      "MES:
"             Arial      "A�O:
"      Arial      "Documento
"      Arial       " Proveedor de Bienes/Servicios"      Arial      "Valor de Compras / Servicios"      Arial      !"Importaciones
Base Imponible
"      "@I"      Arial      "Dia"      Arial      "Numero"      Arial      $"Razon Social / Apellidos / Nombres"      Arial      "RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      "Comprobante: ", COMPROBANTE      Arial      day(cn_rivacompra.fecha)      Arial      cn_rivacompra.FacturaProveedor      Arial      cn_rivacompra.Proveedor      Arial      ruc             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial       cn_rivacompra.iva5      "999,999,999"       cn_rivacompra.iva5      Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      "Total: ", COMPROBANTE      Arial      cn_rivacompra.gravadas5      "999,999,999,999"      Arial       cn_rivacompra.iva5      "999,999,999"       cn_rivacompra.iva5      Arial      cn_rivacompra.gravadas10      "999,999,999,999"      Arial      cn_rivacompra.iva10      "999,999,999"      Arial      cn_rivacompra.exentas      "999,999,999,999"      Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"      Arial      	Imponible      "999,999,999,999"      Arial      "Transporte"      Arial      _PAGETOTAL <> _PAGENO      cn_rivacompra.gravadas5      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      cn_rivacompra.iva5      "999,999,999"      Arial      _PAGETOTAL <> _PAGENO      cn_rivacompra.gravadas10      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      cn_rivacompra.iva10      "999,999,999"      Arial      _PAGETOTAL <> _PAGENO      cn_rivacompra.exentas      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      	Imponible      "999,999,999,999"      Arial      _PAGETOTAL <> _PAGENO      "Total General"             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial      cn_rivacompra.iva5      "999,999,999"             Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     DPROCEDURE BeforeOpenTables
DO seteo 
ENDPROC
PROCEDURE Init


TEXT TO cmdSQL noshow

SELECT case when c.IdComprobante='NC' THEN 'NOTAS DE CREDITO' ELSE 'FACTURAS' END AS COMPROBANTE, c.FacturaProveedor, c.Fecha, 
p.Razon as Proveedor,p.Ruc,
 round(ISNULL(c.Gravada5,0) * c.Cotizacion,0) as Gravadas5,
 round(ISNULL(c.iva5,0) * c.Cotizacion,0) as Iva5,
 round(ISNULL(c.Gravada,0) * c.Cotizacion,0) AS Gravadas10, 
 round(ISNULL(c.iva10,0) * c.Cotizacion,0) as Iva10, 
 round(ISNULL(c.Exenta,0) * c.Cotizacion,0) as Exentas , 
round(c.Total * c.cotizacion,0) as Total, 
c.Total as TotalOriginal,
c.Cotizacion,
round(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* c.cotizacion, 0) ELSE 0 END,0) AS imponible, 
c.Sucursal + s.Descripci�n AS Sucursal,
c.TipoAplica as Tipo,c.IdComprobante, Timbrado
FROM         cp_factura AS c left JOIN
                      cn_conceptos AS d ON c.IdEmpresa = d.idempresa AND c.IdConcepto = d.idconcepto left JOIN
                      vt_Iva ON d.IdIva = vt_Iva.Iva INNER JOIN
                      cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN
                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal
      WHERE c.idempresa = ?oApp.Empresa  
        and   c.fecha  between ?m.dFecha and ?m.hFecha and c.Tipo='G'
        and  not c.IdComprobante in ('IM','RC','RE','BL')
union all
SELECT case when c.IdComprobante='NC' THEN 'NOTAS DE CREDITO' ELSE 'FACTURAS' END AS COMPROBANTE,    c.FacturaProveedor, c.Fecha, 
p.Razon as Proveedor, p.Ruc,
ISNULL(c.Exenta,0)* Cotizacion as Exentas , 
	ISNULL(c.Gravada,0)* Cotizacion AS Gravadas10, ISNULL(c.iva10,0)* Cotizacion as Iva10, 
	ISNULL(c.iva5,0)* Cotizacion as Iva5, ISNULL(c.Gravada5,0) * Cotizacion as Gravadas5, round(c.Total * c.cotizacion,0) as Total, 
c.Total as TotalOriginal,
c.Cotizacion,
(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* c.cotizacion, 0) ELSE 0 END) AS imponible, 
                      c.Sucursal + s.Descripci�n AS Sucursal,
ISNULL(c.TipoAplica,'D') as Tipo,                      
                      c.IdComprobante, Timbrado
FROM         cp_factura AS c INNER JOIN
                      cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN
                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal
      WHERE c.idempresa = ?oApp.Empresa  
      and   c.fecha  between ?m.dFecha and ?m.hFecha and c.Tipo='C'
      and  not c.IdComprobante in ('IM','RC','RE','DE','BL')
      ORDER BY 1, c.fecha, c.FacturaProveedor

ENDTEXT

**--        and   MONTH(c.fecha)= ?m.Mes and YEAR(c.fecha)=?m.A�o and c.Tipo='G'
**--                and   MONTH(c.fecha)= ?m.Mes and YEAR(c.fecha)=?m.A�o and c.Tipo='C'

sql(cmdsql,'cn_rivacompra')
SELECT cn_rivacompra

ENDPROC
     .���                              (   %   0      �  2   X          �  U  
  �  � U  SETEO�
	 M(�  �� �  �� � SELECT case when c.IdComprobante='NC' THEN 'NOTAS DE CREDITO' ELSE 'FACTURAS' END AS COMPROBANTE, c.FacturaProveedor, c.Fecha, �! � p.Razon as Proveedor,p.Ruc,�A �;  round(ISNULL(c.Gravada5,0) * c.Cotizacion,0) as Gravadas5,�8 �2  round(ISNULL(c.iva5,0) * c.Cotizacion,0) as Iva5,�B �<  round(ISNULL(c.Gravada,0) * c.Cotizacion,0) AS Gravadas10, �; �5  round(ISNULL(c.iva10,0) * c.Cotizacion,0) as Iva10, �? �9  round(ISNULL(c.Exenta,0) * c.Cotizacion,0) as Exentas , �0 �* round(c.Total * c.cotizacion,0) as Total, � � c.Total as TotalOriginal,� � c.Cotizacion,�k �e round(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* c.cotizacion, 0) ELSE 0 END,0) AS imponible, �- �' c.Sucursal + s.Descripci�n AS Sucursal,�4 �. c.TipoAplica as Tipo,c.IdComprobante, Timbrado�, �& FROM         cp_factura AS c left JOIN�t �n                       cn_conceptos AS d ON c.IdEmpresa = d.idempresa AND c.IdConcepto = d.idconcepto left JOIN�E �?                       vt_Iva ON d.IdIva = vt_Iva.Iva INNER JOIN�w �q                       cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN�b �\                       sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal�/ �)       WHERE c.idempresa = ?oApp.Empresa  �K �E         and   c.fecha  between ?m.dFecha and ?m.hFecha and c.Tipo='G'�? �9         and  not c.IdComprobante in ('IM','RC','RE','BL')� �	 union all�� �� SELECT case when c.IdComprobante='NC' THEN 'NOTAS DE CREDITO' ELSE 'FACTURAS' END AS COMPROBANTE,    c.FacturaProveedor, c.Fecha, �" � p.Razon as Proveedor, p.Ruc,�2 �, ISNULL(c.Exenta,0)* Cotizacion as Exentas , �^ �X 	ISNULL(c.Gravada,0)* Cotizacion AS Gravadas10, ISNULL(c.iva10,0)* Cotizacion as Iva10, �� �� 	ISNULL(c.iva5,0)* Cotizacion as Iva5, ISNULL(c.Gravada5,0) * Cotizacion as Gravadas5, round(c.Total * c.cotizacion,0) as Total, � � c.Total as TotalOriginal,� � c.Cotizacion,�d �^ (CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* c.cotizacion, 0) ELSE 0 END) AS imponible, �C �=                       c.Sucursal + s.Descripci�n AS Sucursal,�= �7 ISNULL(c.TipoAplica,'D') as Tipo,                      �5 �/                       c.IdComprobante, Timbrado�- �' FROM         cp_factura AS c INNER JOIN�w �q                       cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN�b �\                       sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal�/ �)       WHERE c.idempresa = ?oApp.Empresa  �I �C       and   c.fecha  between ?m.dFecha and ?m.hFecha and c.Tipo='C'�B �<       and  not c.IdComprobante in ('IM','RC','RE','DE','BL')�3 �-       ORDER BY 1, c.fecha, c.FacturaProveedor� �  � � ��C �  � cn_rivacompra� �� F� � U  CMDSQL SQL CN_RIVACOMPRA BeforeOpenTables,     �� InitA     ��1 q 2 � a Q�!���1��A�AQq!���� �!!�q�1A1�Q�q!��!1a A �q 2                       %         @   9      )                     