  69                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=1
PAPERSIZE=5
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=0
      s    winspool  PrimoPDF  PrimoPort:                    Microsoft Document Imaging Writer Port:                       �PrimoPDF -STUDIO451c PS3         � lS�  �
od   X  X   Letter                                                                            PRIV�0                                                                                       '''  '        � l                                  \K hC                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       �   SMTJ     � P r i m o P D F   Resolution 600dpi PageSize Letter PageRegion  LeadingEdge  InputSlot OnlyOne                                          ne Collate True OutputBin Bin2 Stapling Off HolePunch Off PrintMode Normal DINDigit1 0 DINDigit2 0 DINDigit3 0 DINDigit4 0 DINDigit5 0 DeptCode False DCDigit1 0 DCDigit2 0 DCDigit3 0 DCDigit4 0 DCDigit5 0 ColorResType ColorLowGeneral DistinguishThinLines True BlackOverPrint True PureBlackGray BlackGrayAuto TonerSave False BlankPage False Smoothing True                                                                            Arial      sucursal      Arial      Arial      Arial      Arial      empresa             Arial      "Libro de Iva Compra"             Arial      CMONTH(cn_rivacompra.fecha)      Arial      year(cn_rivacompra.fecha)      "9999"      Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "MES:
"             Arial      "A�O:
"      Arial      "Documento
"      Arial       " Proveedor de Bienes/Servicios"      Arial      "Valor de Compras / Servicios"      Arial      !"Importaciones
Base Imponible
"      "@I"      Arial      "Dia"      Arial      "Numero"      Arial      " Fecha"      Arial      $"Razon Social / Apellidos / Nombres"      Arial      "RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      cn_rivacompra.sucursal             Arial      "Sucursal:"      Arial      cn_rivacompra.FacturaProveedor      Arial      cn_rivacompra.fecha      Arial      cn_rivacompra.razon             Arial      ruc             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial       cn_rivacompra.iva5      "999,999,999"       cn_rivacompra.iva5      Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"      Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      
"Directos"             Arial      'iif(Tipo='D',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='D',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='D',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='D',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='D',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='D',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='D',Imponible,0)      "999,999,999,999"             Arial      "Indirectos"             Arial      'iif(Tipo='I',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='I',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='I',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='I',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='I',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='I',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='I',Imponible,0)      "999,999,999,999"             Arial      "Importaciones"             Arial      'iif(Tipo='M',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='M',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='M',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='M',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='M',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='M',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='M',Imponible,0)      "999,999,999,999"             Arial      "Total General"             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial      cn_rivacompra.iva5      "999,999,999"             Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo


ENDPROC
PROCEDURE Init


TEXT TO cmdSQL noshow

SELECT     c.FacturaProveedor, c.Fecha, c.Exenta* Cotizacion as Exentas , c.Gravada, c.Iva, c.Total, p.Razon, p.Ruc, c.TipoAplica as Tipo, CASE WHEN TIPOAplica <> 'M' AND 
                      Valor = 5 THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END AS gravadas5, CASE WHEN TIPOAplica <> 'M' AND Valor = 10 THEN ISNULL(c.gravada* Cotizacion, 0) 
                      ELSE 0 END AS gravadas10, CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END AS imponible, CASE WHEN TIPOAplica <> 'M' AND 
                      Valor = 5 THEN ISNULL(c.iva* Cotizacion, 0) ELSE 0 END AS iva5, CASE WHEN TIPOAplica <> 'M' AND Valor = 10 THEN ISNULL(c.iva* Cotizacion, 0) ELSE 0 END AS iva10, 
                      c.Sucursal + s.Descripci�n AS Sucursal
FROM         cp_factura AS c left JOIN
                      cn_conceptos AS d ON c.IdEmpresa = d.idempresa AND c.IdConcepto = d.idconcepto left JOIN
                      vt_Iva ON d.IdIva = vt_Iva.Iva INNER JOIN
                      cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN
                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal
      WHERE c.idempresa = ?oApp.Empresa  
       AND c.sucursal = ?m.sucursal    
        AND MONTH(c.fecha) = ?m.mes  
        AND YEAR(c.fecha) = ?m.a�o  
        and   c.fecha between ?m.dFecha and ?m.hFecha
union all
SELECT     c.FacturaProveedor, c.Fecha, c.Exenta* Cotizacion as Exentas, c.Gravada, c.Iva, c.Total, p.Razon, p.Ruc, ISNULL(c.TipoAplica,'D') as Tipo, 
                      SUM(CASE WHEN d .iva = 5 THEN ISNULL(d .precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas5, 
                      SUM(CASE WHEN d .Iva = 10 THEN ISNULL(d .precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas10, 
                      SUM(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END) AS imponible, SUM(CASE WHEN d .iva = 5 THEN ISNULL(d .Valoriva* Cotizacion, 0) 
                      ELSE 0 END) AS iva5, SUM(CASE WHEN d .Iva = 10 THEN ISNULL(d .Valoriva* Cotizacion, 0) ELSE 0 END) AS iva10, c.Sucursal + s.Descripci�n AS Sucursal
FROM         cp_factura AS c INNER JOIN
                      cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN
                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal INNER JOIN
                      st_movimiento_Det AS d ON c.IdFactura = d.IdCompra
  
      WHERE c.idempresa = ?oApp.Empresa  
       AND c.sucursal = ?m.sucursal    
        AND MONTH(c.fecha) = ?m.mes  
        AND YEAR(c.fecha) = ?m.a�o  
        and   c.fecha between ?m.dFecha and ?m.hFecha
	  GROUP BY c.FacturaProveedor, c.Fecha, c.Exenta, c.Gravada, c.Iva, c.Total, p.Razon, p.Ruc, c.TipoAplica, c.Sucursal + s.Descripci�n, c.Cotizacion
      ORDER BY c.fecha, c.FacturaProveedor

ENDTEXT


sql(cmdsql,'cn_rivacompra')
SELECT cn_rivacompra

ENDPROC
     ����    o  o                        V=   %   �        +   �          �  U  
  �  � U  SETEO7	 M(�  �� �  �� �� SELECT     c.FacturaProveedor, c.Fecha, c.Exenta* Cotizacion as Exentas , c.Gravada, c.Iva, c.Total, p.Razon, p.Ruc, c.TipoAplica as Tipo, CASE WHEN TIPOAplica <> 'M' AND �� ��                       Valor = 5 THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END AS gravadas5, CASE WHEN TIPOAplica <> 'M' AND Valor = 10 THEN ISNULL(c.gravada* Cotizacion, 0) �� ��                       ELSE 0 END AS gravadas10, CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END AS imponible, CASE WHEN TIPOAplica <> 'M' AND �� ��                       Valor = 5 THEN ISNULL(c.iva* Cotizacion, 0) ELSE 0 END AS iva5, CASE WHEN TIPOAplica <> 'M' AND Valor = 10 THEN ISNULL(c.iva* Cotizacion, 0) ELSE 0 END AS iva10, �B �<                       c.Sucursal + s.Descripci�n AS Sucursal�, �& FROM         cp_factura AS c left JOIN�t �n                       cn_conceptos AS d ON c.IdEmpresa = d.idempresa AND c.IdConcepto = d.idconcepto left JOIN�E �?                       vt_Iva ON d.IdIva = vt_Iva.Iva INNER JOIN�w �q                       cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN�b �\                       sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal�/ �)       WHERE c.idempresa = ?oApp.Empresa  �- �'        AND c.sucursal = ?m.sucursal    �+ �%         AND MONTH(c.fecha) = ?m.mes  �* �$         AND YEAR(c.fecha) = ?m.a�o  �; �5         and   c.fecha between ?m.dFecha and ?m.hFecha� �	 union all�� �� SELECT     c.FacturaProveedor, c.Fecha, c.Exenta* Cotizacion as Exentas, c.Gravada, c.Iva, c.Total, p.Razon, p.Ruc, ISNULL(c.TipoAplica,'D') as Tipo, �� �z                       SUM(CASE WHEN d .iva = 5 THEN ISNULL(d .precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas5, �� �|                       SUM(CASE WHEN d .Iva = 10 THEN ISNULL(d .precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas10, �� ��                       SUM(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END) AS imponible, SUM(CASE WHEN d .iva = 5 THEN ISNULL(d .Valoriva* Cotizacion, 0) �� ��                       ELSE 0 END) AS iva5, SUM(CASE WHEN d .Iva = 10 THEN ISNULL(d .Valoriva* Cotizacion, 0) ELSE 0 END) AS iva10, c.Sucursal + s.Descripci�n AS Sucursal�- �' FROM         cp_factura AS c INNER JOIN�w �q                       cp_proveedor AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdProveedor = p.IdProveedor INNER JOIN�m �g                       sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal INNER JOIN�N �H                       st_movimiento_Det AS d ON c.IdFactura = d.IdCompra� �   �/ �)       WHERE c.idempresa = ?oApp.Empresa  �- �'        AND c.sucursal = ?m.sucursal    �+ �%         AND MONTH(c.fecha) = ?m.mes  �* �$         AND YEAR(c.fecha) = ?m.a�o  �; �5         and   c.fecha between ?m.dFecha and ?m.hFecha�� �� 	  GROUP BY c.FacturaProveedor, c.Fecha, c.Exenta, c.Gravada, c.Iva, c.Total, p.Razon, p.Ruc, c.TipoAplica, c.Sucursal + s.Descripci�n, c.Cotizacion�0 �*       ORDER BY c.fecha, c.FacturaProveedor� �  � � ��C �  � cn_rivacompra� �� F� � U  CMDSQL SQL CN_RIVACOMPRA BeforeOpenTables,     �� InitA     ��1 q 4 � a a�!�AQq!������ �	!��
�q��� ������	a A �q 2                       (         C         )   o                  