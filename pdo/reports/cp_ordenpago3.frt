   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                 Importe                                                       Arial                          nvl(valorizado,0)=0            "999,999,999,999.99"           Arial                          "Nro."                         facturaproveedor                                              Arial                          
day(fecha)                                                    Arial                          "@L 99"                        nroorden                                                      Arial                          
"@L 99999"                     	proveedor                                                     Arial                          Importe                                                       Arial                          "999,999,999,999.99"           
valorizado                                                    Arial                          "9,999,999,999.99"             concepto                                                      Arial                          	nrocheque                                                     Arial                          banco                                                         Arial                          month(fecha)                                                  Arial                          "@L 99"                        year(fecha)                                                   Arial                          NUMERAL(Importe)                                              Arial                          "@!"                           "Vencimiento : ", fechadiferida                                                                Arial                          #!empty(nvl(fechadiferida,{ /  / }))                             Arial                          Arial                          Arial                          dataenvironment                tLeft = -11
Top = 178
Width = 759
Height = 448
InitialSelectedAlias = "cp_rordenpago"
Name = "Dataenvironment"
             �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
TEXT TO cmdSQL
		SELECT     a.fecha, RTRIM(ISNULL(a.idproveedor, '')) + '-' + a.nombre AS proveedor, a.nroorden, a.nrocheque, a.Detalle AS Concepto, b.facturaproveedor, 
                      b.idmoneda, b.valorizado, a.Importe, d.nombre AS banco, convert(char(10),ts_d.FechaDiferida,105) as FechaDiferida
FROM         dbo.ts_depositos_base ts_d RIGHT OUTER JOIN
                      dbo.cp_ordenpago a ON ts_d.iddeposito = a.iddeposito LEFT OUTER JOIN
                      dbo.cp_orddet_pago b ON a.IdOrdenPago = b.nroorden AND a.idempresa = b.idempresa LEFT OUTER JOIN
                      dbo.ts_cuentas d ON a.idcuenta = d.idcuenta LEFT OUTER JOIN
                      dbo.bs_bancos e ON d.idbanco = e.idbanco
	where a.idempresa=?oApp.Empresa and a.nroorden=?m.NroOrden
ENDTEXT
sql(cmdSQL,'rordenpago2')


Select rordenpago2
ENDPROC
                                 n���    U  U                        U>   %   �      �     �          �  U  
  �  � U  SETEOU M(�  �� �� 		SELECT     a.fecha, RTRIM(ISNULL(a.idproveedor, '')) + '-' + a.nombre AS proveedor, a.nroorden, a.nrocheque, a.Detalle AS Concepto, b.facturaproveedor, �� ��                       b.idmoneda, b.valorizado, a.Importe, d.nombre AS banco, convert(char(10),ts_d.FechaDiferida,105) as FechaDiferida�> �8 FROM         dbo.ts_depositos_base ts_d RIGHT OUTER JOIN�` �Z                       dbo.cp_ordenpago a ON ts_d.iddeposito = a.iddeposito LEFT OUTER JOIN�| �v                       dbo.cp_orddet_pago b ON a.IdOrdenPago = b.nroorden AND a.idempresa = b.idempresa LEFT OUTER JOIN�W �Q                       dbo.ts_cuentas d ON a.idcuenta = d.idcuenta LEFT OUTER JOIN�D �>                       dbo.bs_bancos e ON d.idbanco = e.idbanco�A �; 	where a.idempresa=?oApp.Empresa and a.nroorden=?m.NroOrden� � ��C �  � rordenpago2� �� F� � U  CMDSQL SQL RORDENPAGO2 BeforeOpenTables,     �� InitA     ��1 q 3 � 
���qAA �s 1                       &         A   �      )   U                  oducto,IdDepo