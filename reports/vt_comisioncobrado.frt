  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=EPSON LX-350 ESC/P
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=2
COLLATE=1
      ?  &  winspool  EPSON LX-350 ESC/P  LPT1:                         	EPSON LX-350 ESC/P               � hC�  �4d   X  X  A4                                                            ����                DINU" T �2�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         SMTJ     E P S O N   L X - 3 5 0   E S C / P   InputBin MANUAL RESDLL UniresDLL Orientation PORTRAIT Resolution RESOLUTION_120X72 PrintSpeed PRINTER_SETTING_SPEED PaperSize A4 PrintDirection PRINTER_SETTING Halftone HT_PATSIZE_AUTO                                             ISPE                      Arial      rvendedor.vendedor      Arial      Arial      Arial      Arial      #"Comision de  Vendedores x Cobrado"             Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      
"Comision"      Arial      	"Cobrado"      Arial      	"Fecha
"      Arial      "Condicion"      Arial      	"Importe"      Arial      
"Vendedor"      Arial      
"Comprob."      Arial      "Nro."      Arial      	"Cliente"      Arial      "% "      Arial      "Total"      Arial      rvendedor.vendedor             Arial      !SoloResumen      rvendedor.comp             Arial      rvendedor.numero             Arial      rvendedor.cliente             Arial      rvendedor.fecha             Arial      rvendedor.condicion             Arial      Importe      "999,999,999"       rvendedor.total      Arial      rvendedor.comision      "@Z 999.99"             Arial      total * rvendedor.comision/100      "@Z 9,999,999,999"             Arial      rvendedor.total      "999,999,999"       rvendedor.total      Arial      rvendedor.vendedor             Arial      SoloResumen      rvendedor.total      "9,999,999,999,999"             Arial      total * rvendedor.comision /100      "9,999,999,999"             Arial      "Total Vendedor"             Arial      !SoloResumen      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Importe      "999,999,999"       rvendedor.total      Arial      rvendedor.total      "9,999,999,999"             Arial       total * rvendedor.comision / 100      "9,999,999,999"             Arial      "Total General"      Arial      dataenvironment      �Top = 169
Left = -26
Width = 793
Height = 439
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rventacliente"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
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
where (fact.idVendedor = ?m.IdVendedor or ?m.IdVendedor is null)
and Fact.idEmpresa = ?oApp.Empresa
and pago.fecha between ?m.dFecha and ?m.hFecha 
GROUP BY fact.IdVendedor, RTRIM(fact.IdCliente) + '-' + g.RazSocial, fact.Numero, fact.IdComprobante, fact.Gravada + fact.Exenta + fact.Iva, fact.Fecha, 
                      ISNULL(fact.Comision, 0), e.Descripcion, RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido)

ENDTEXT

sql(cmdSQL,'rvendedor')
SELECT rvendedor

ENDPROC
     ����    �  �                        �M   %   �      X               �  U  
  �  � U  SETEO� %�C��  ��� � J���(��  � � M(� � �  �� �z SELECT  RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido) AS vendedor, e.Descripcion AS condicion, �� ��                       ISNULL(fact.Comision, 0) AS comision, fact.Fecha, fact.Gravada + fact.Exenta + fact.Iva AS Importe, fact.IdComprobante AS comp, fact.Numero, �R �L                       RTRIM(fact.IdCliente) + '-' + g.RazSocial AS cliente, �$ � 	sum(pagodet.Importe) as Total�/ �) FROM         dbo.vt_pagos Pago INNER JOIN�_ �Y                       dbo.vt_det_pagos pagoDet ON Pago.idpago = pagoDet.idpago INNER JOIN� �y                       dbo.vt_clientes g ON pagoDet.idempresa = g.IdEmpresa AND pagoDet.idcliente = g.IdCliente INNER JOIN�e �_                       dbo.vt_factura fact ON pagoDet.idfactura = fact.IdFactura LEFT OUTER JOIN�� �}                       dbo.vt_Condicion e ON fact.IdEmpresa = e.IdEmpresa AND fact.IdCondicion = e.IdCondicion LEFT OUTER JOIN�� �|                       dbo.vt_Vendedores b ON fact.IdEmpresa = b.IdEmpresa AND fact.IdVendedor = b.IdVendedor LEFT OUTER JOIN�J �D                       dbo.BS_Personas c ON b.idpersona = c.IdPersona�F �@ where (fact.idVendedor = ?m.IdVendedor or ?m.IdVendedor is null)�( �" and Fact.idEmpresa = ?oApp.Empresa�5 �/ and pago.fecha between ?m.dFecha and ?m.hFecha �� �� GROUP BY fact.IdVendedor, RTRIM(fact.IdCliente) + '-' + g.RazSocial, fact.Numero, fact.IdComprobante, fact.Gravada + fact.Exenta + fact.Iva, fact.Fecha, �� ��                       ISNULL(fact.Comision, 0), e.Descripcion, RTRIM(fact.IdVendedor) + '-' + RTRIM(c.Nombre) + ' ' + RTRIM(c.Apellido)� �  � � ��C � �	 rvendedor� �� F� � U 
 IDVENDEDOR CMDSQL SQL	 RVENDEDOR BeforeOpenTables,     �� InitA     ��1 q 2 � A � a �
!A���Q1!�a�Q�	�a A �q 2                       $         ?   �      )   �                  