     @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=5
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=0
                                          �Top = 104
Left = 2
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
                                                       dataenvironment                                               Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         "Total: ", Comprobante                                        Arial                                                                                                                       "999,999,999,999"                                             
gravadas10                                                    Arial                                                         Arial                                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC�  �4d   X  X   A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     sucursal                                                      Comprobante                                                   
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         "Sucursal:"                                                   Arial                                                         " Fecha"                                                      Arial                                                         cn_rivaventas.fecha                                           Arial                                                         cn_rivaventas.sucursal                                                                                                      Arial                                                         cn_rivaventas.numero                                          Arial                                                         "Numero"                                                      Arial                                                         cn_rivaventas.razsocial                                                                                                     Arial                                                         
"Clientes"                                                    Arial                                                         empresa                                                                                                                     Arial                                                         cn_rivaventas.ruc                                             Arial                                                         len( ALLTRIM(ruc) ) > 8                                       " RUC"                                                        Arial                                                         "Total General"                                                                                                             Arial                                                         #"Detalle de Ventas por Comprobante"                                                                                         Arial                                                         "MES:"                                                                                                                     Arial                                                         "A�O:"                                                       Arial                                                         $"Razon Social / Apellidos / Nombres"                          Arial                                                         "Valor Ventas"                                                Arial                                                         "Retenciones"                                                "@I"                                                          Arial                                                         cmonth(cn_rivaventas.fecha)                                   Arial                                                         year(cn_rivaventas.fecha)                                     "9999"                                                        Arial                                                         "Facturas/Notas"                                             Arial                                                         	gravadas5                                                     "999,999,999,999"                                                                                                           Arial                                                         "Gravadas 5%"                                                 Arial                                                         iva10                                                         "999,999,999"                                                                                                               Arial                                                         	"Exentas"                                                     Arial                                                         -gravadas5 + gravadas10+ exentas+ iva5 + iva10                 "999,999,999,999"                                                                                                           Arial                                                         "Total"                                                       Arial                                                         exentas                                                       "999,999,999,999"                                                                                                           Arial                                                         "    Iva 10%"                                                 Arial                                                         	gravadas5                                                     "999,999,999,999"                                                                                                           Arial                                                         iva10                                                         "999,999,999"                                                                                                               Arial                                                         -gravadas5 + gravadas10+ exentas+ iva5 + iva10                 "999,999,999,999"                                                                                                           Arial                                                         exentas                                                       "999,999,999,999"                                                                                                           Arial                                                         iva5                                                          "999,999,999"                                                  cn_rivacompra.iva5                                           Arial                                                         "    Iva 5%"                                                  Arial                                                         iva5                                                          "999,999,999"                                                                                                               Arial                                                         
gravadas10                                                    "999,999,999,999"                                                                                                           Arial                                                         "Gravadas 10%"                                                Arial                                                         
gravadas10                                                    "999,999,999,999"                                                                                                           Arial                                                         	"Orden:"                                                     Arial                                                         1IIF(m.ordenfactura='F','Fecha','Nro Comprobante')                                                                           Arial                                                         "Comprobante: ", Comprobante                                  Arial                                                         	gravadas5                                                     "999,999,999,999"                                                                                                           Arial                                                         iva10                                                         "999,999,999"                                                                                                               Arial                                                         -gravadas5 + gravadas10+ exentas+ iva5 + iva10                 "999,999,999,999"                                                                                                           Arial                                                         exentas                                                       "999,999,999,999"                                                                                                           Arial                                                         iva5                                                          "999,999,999"                                                                                                              PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SET TEXTMERGE ON 
IF m.ordenfactura='F'
	m.orden = 'c.Fecha, c.Numero'
ELSE
	m.orden = 'c.Numero, c.Fecha'
ENDIF
IF EMPTY(m.vt_cpbt)
	m.vt_cpbt=null
ENDIF
	
		      

TEXT TO cmdSQL noshow
SELECT     c.Numero, c.Fecha, c.Exenta* Cotizacion as Exentas, c.Gravada, c.Iva, total = c.TotalFactura, p.RazSocial, p.Ruc, 
                      SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas5, 
                      SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas10, 
                      SUM(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END) AS imponible, 
                      SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.Valoriva* Cotizacion, 0) ELSE 0 END) AS iva5, 
                      SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.Valoriva* Cotizacion, 0) ELSE 0 END) AS iva10,
                      c.Sucursal + s.Descripci�n AS Sucursal,
                      c.IdComprobante +'-' + vt_cpbt.Descripcion as Comprobante
FROM         vt_factura AS c INNER JOIN
                      st_movimiento_Det AS d ON c.IdFactura = d.IdFactura INNER JOIN
                      vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente INNER JOIN
                      sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal INNER JOIN
                      vt_cpbt ON c.IdEmpresa = vt_cpbt.IdEmpresa AND c.IdComprobante = vt_cpbt.IdComprobante
WHERE c.idempresa = ?oApp.Empresa  
       AND c.sucursal = ?m.sucursal    
        AND MONTH(c.fecha) = ?m.mes  
        AND YEAR(c.fecha) = ?m.a�o  
        and   c.fecha between ?m.dFecha and ?m.hFecha
        and (c.IdComprobante = ?m.vt_cpbt or ?m.vt_cpbt is null)
	  GROUP BY c.Numero, c.Fecha, c.Exenta, c.Gravada, c.Iva, c.TotalFactura, p.RazSocial, p.Ruc, c.Sucursal + s.Descripci�n, c.Cotizacion,
	  c.IdComprobante, vt_cpbt.Descripcion
 ORDER BY c.Sucursal + s.Descripci�n,c.IdComprobante, <<m.orden>>

ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
                                                              	����    �	  �	                        R   %   �      5	  (   �          �  U  
  �  � U  SETEO> G` � %���  � F��: �  T�� �� c.Fecha, c.Numero�� �b �  T�� �� c.Numero, c.Fecha�� � %�C�� ���� � T�� ���� �	 M(� ��� �} SELECT     c.Numero, c.Fecha, c.Exenta* Cotizacion as Exentas, c.Gravada, c.Iva, total = c.TotalFactura, p.RazSocial, p.Ruc, �~ �x                       SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas5, �� �z                       SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.precio * cantidad* Cotizacion, 0) ELSE 0 END) AS gravadas10, �{ �u                       SUM(CASE WHEN TIPOAplica = 'M' THEN ISNULL(c.gravada* Cotizacion, 0) ELSE 0 END) AS imponible, �p �j                       SUM(CASE WHEN d.iva = 5 THEN ISNULL(d.Valoriva* Cotizacion, 0) ELSE 0 END) AS iva5, �q �k                       SUM(CASE WHEN d.Iva = 10 THEN ISNULL(d.Valoriva* Cotizacion, 0) ELSE 0 END) AS iva10,�C �=                       c.Sucursal + s.Descripci�n AS Sucursal,�U �O                       c.IdComprobante +'-' + vt_cpbt.Descripcion as Comprobante�- �' FROM         vt_factura AS c INNER JOIN�Z �T                       st_movimiento_Det AS d ON c.IdFactura = d.IdFactura INNER JOIN�r �l                       vt_clientes AS p ON c.IdEmpresa = p.IdEmpresa AND c.IdCliente = p.IdCliente INNER JOIN�m �g                       sucursal AS s ON c.IdEmpresa = s.IdEmpresa AND c.Sucursal = s.Sucursal INNER JOIN�r �l                       vt_cpbt ON c.IdEmpresa = vt_cpbt.IdEmpresa AND c.IdComprobante = vt_cpbt.IdComprobante�) �# WHERE c.idempresa = ?oApp.Empresa  �- �'        AND c.sucursal = ?m.sucursal    �+ �%         AND MONTH(c.fecha) = ?m.mes  �* �$         AND YEAR(c.fecha) = ?m.a�o  �; �5         and   c.fecha between ?m.dFecha and ?m.hFecha�F �@         and (c.IdComprobante = ?m.vt_cpbt or ?m.vt_cpbt is null)�� �� 	  GROUP BY c.Numero, c.Fecha, c.Exenta, c.Gravada, c.Iva, c.TotalFactura, p.RazSocial, p.Ruc, c.Sucursal + s.Descripci�n, c.Cotizacion,�- �' 	  c.IdComprobante, vt_cpbt.Descripcion�G �A  ORDER BY c.Sucursal + s.Descripci�n,c.IdComprobante, <<m.orden>>� �  � � ��C � � cn_rIvaVentas� �� F� � U  ORDENFACTURA ORDEN VT_CPBT CMDSQL SQL CN_RIVAVENTAS BeforeOpenTables,     �� InitA     ��1 q 3 a A� A � A � 1��1Q��!�!�����a��qa A �r 2                       &         A   t      )   �	                  dFactura)� �  �