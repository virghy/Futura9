     !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   X  X  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=2
COLLATE=1
                 rvendedor.vendedor             rvendedor.total                 rvendedor.total               Arial                          "999,999,999"                  #"Comision de  Vendedores x Cobrado"                                                            Arial                          empresa                                                       Arial                          Arial                          "Fecha"                       Arial                          
"Per�odo:"                     &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                         Arial                          rvendedor.fecha                                               Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          
"Vendedor"                     Arial                          	"Importe"                      Arial                          "Total General"                Arial                          "Condicion"                    rvendedor.vendedor                                            Arial                          !SoloResumen                   rvendedor.condicion                                           Arial                          rvendedor.comision                                            Arial                          "@Z 999.99"                    Arial                          "% "                           Arial                          "Total"                        total * rvendedor.comision/100                                                                 Arial                          "@Z 9,999,999,999"             rvendedor.total                                               Arial                          "9,999,999,999,999"            total * rvendedor.comision /100                                                                Arial                          "9,999,999,999"                rvendedor.total                                               Arial                          "9,999,999,999"                 total * rvendedor.comision / 100                                                               Arial                          "9,999,999,999"                Arial                          !SoloResumen                                                  "Total Vendedor"               rvendedor.comp                                                Arial                          rvendedor.numero                                              Arial                          Arial                          
"Comprob."                     Arial                          "Nro."                         rvendedor.cliente                                             Arial                          Arial                          	"Cliente"                      rvendedor.vendedor                                            Arial                          SoloResumen                    Importe                         rvendedor.total               Arial                          "999,999,999"                  Arial                          
"Comision"                     Arial                          	"Cobrado"                      Importe                         rvendedor.total               Arial                          "999,999,999"                  Arial                          Arial                          Arial                          Arial                          dataenvironment                wLeft = -26
Top = 169
Width = 793
Height = 439
InitialSelectedAlias = "vt_rventacliente"
Name = "Dataenvironment"
          �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
If Empty(m.idvendedor)
	Store null To m.idvendedor
ENDIF

TEXT TO cmdSQL

SELECT  RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido) AS vendedor, e.Descripcion AS condicion, 
                      ISNULL(fact.Comision, 0) AS comision, fact.Fecha, fact.Gravada + fact.Exenta + fact.Iva AS Importe, fact.IdComprobante AS comp, fact.Numero, 
                      RTRIM(fact.IdCliente) + '-' + g.RazSocial AS cliente, 
	sum(pagodet.Importe) as Total
FROM         dbo.vt_pagos Pago INNER JOIN
                      dbo.vt_det_pagos pagoDet ON Pago.idpago = pagoDet.idpago INNER JOIN
                      dbo.vt_clientes g ON pagoDet.idempresa = g.IdEmpresa AND pagoDet.idcliente = g.IdCliente INNER JOIN
                      dbo.vt_factura fact ON pagoDet.idfactura = fact.IdFactura LEFT OUTER JOIN
                      dbo.vt_Condicion e ON fact.IdEmpresa = e.IdEmpresa AND fact.IdCondicion = e.IdCondicion LEFT OUTER JOIN
                      dbo.vt_Vendedores b ON fact.IdEmpresa = b.IdEmpresa AND fact.IdVendedor = b.IdVendedor LEFT OUTER JOIN
                      dbo.BS_Personas c ON b.idpersona = c.IdPersona
where fact.idVendedor = ?m.IdVendedor or ?m.IdVendedor is null
and pago.fecha between ?m.dFecha and ?m.hFecha 
GROUP BY fact.IdVendedor, RTRIM(fact.IdCliente) + '-' + g.RazSocial, fact.Numero, fact.IdComprobante, fact.Gravada + fact.Exenta + fact.Iva, fact.Fecha, 
                      ISNULL(fact.Comision, 0), e.Descripcion, RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido)

ENDTEXT

sql(cmdSQL,'rvendedor')
SELECT rvendedor

ENDPROC
                      ����    �  �                        ^x   %   �      ,     �          �  U  
  �  � U  SETEOc %�C��  ��� � J���(��  � � M(� � �  �� �z SELECT  RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido) AS vendedor, e.Descripcion AS condicion, �� ��                       ISNULL(fact.Comision, 0) AS comision, fact.Fecha, fact.Gravada + fact.Exenta + fact.Iva AS Importe, fact.IdComprobante AS comp, fact.Numero, �R �L                       RTRIM(fact.IdCliente) + '-' + g.RazSocial AS cliente, �$ � 	sum(pagodet.Importe) as Total�/ �) FROM         dbo.vt_pagos Pago INNER JOIN�_ �Y                       dbo.vt_det_pagos pagoDet ON Pago.idpago = pagoDet.idpago INNER JOIN� �y                       dbo.vt_clientes g ON pagoDet.idempresa = g.IdEmpresa AND pagoDet.idcliente = g.IdCliente INNER JOIN�e �_                       dbo.vt_factura fact ON pagoDet.idfactura = fact.IdFactura LEFT OUTER JOIN�� �}                       dbo.vt_Condicion e ON fact.IdEmpresa = e.IdEmpresa AND fact.IdCondicion = e.IdCondicion LEFT OUTER JOIN�� �|                       dbo.vt_Vendedores b ON fact.IdEmpresa = b.IdEmpresa AND fact.IdVendedor = b.IdVendedor LEFT OUTER JOIN�J �D                       dbo.BS_Personas c ON b.idpersona = c.IdPersona�D �> where fact.idVendedor = ?m.IdVendedor or ?m.IdVendedor is null�5 �/ and pago.fecha between ?m.dFecha and ?m.hFecha �� �� GROUP BY fact.IdVendedor, RTRIM(fact.IdCliente) + '-' + g.RazSocial, fact.Numero, fact.IdComprobante, fact.Gravada + fact.Exenta + fact.Iva, fact.Fecha, �� ��                       ISNULL(fact.Comision, 0), e.Descripcion, RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido)� �  � � ��C � �	 rvendedor� �� F� � U 
 IDVENDEDOR CMDSQL SQL	 RVENDEDOR BeforeOpenTables,     �� InitA     ��1 q 2 � A � a �
!A���Q1!�AQ�	�a A �q 2                       $         ?   �      )   �                   %�� �  �
 � C�	