  �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=Enviar a OneNote 2013
OUTPUT=nul:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=120
COLOR=1
YRESOLUTION=144
                               ?  )  winspool  Enviar a OneNote 2013  nul:                                                                                (Enviar a OneNote 2013           � @/    �4d   x   �   A4                                                            ����GIS4            DINU" � $ ���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         �   SMTJ     � { 3 E E 3 9 1 1 4 - 3 0 B 4 - 4 5 a 4 - A 1 0 9 - 1 9 D 4 A 4 0 F C C 2 2 }   RESDLL UniresDLL PaperSize LETTER Orientation PORTRAIT Resolution DPI600 ColorMode 24bpp                     V4DM                                                                                                                     Arial                                                         idmoneda                                                      
IdCobrador                                                    Ciclo                                                         	idcliente                                                     *"An�lisis de Cartera por Ciclo y Cobrador"                    Arial                                                         empresa                                                       Arial                                                         vt_rresumensaldos.descripci�n                                                                                               Arial                                                         "Sucursal:"                                                   Arial                                                         1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                           Arial                                                         
"Per�odo:"                                                    Arial                                                         1"Desde " + dtoc(m.dvence)+ " al " +dtoc(m.hvence)                                                                           Arial                                                         "Vencimientos:"                                               Arial                                                         "Vto."                                                        Arial                                                         "Fch. a Cobrar"                                               Arial                                                         "Cuota"                                                       Arial                                                         	"Importe"                                                     Arial                                                         "Saldo"                                                       Arial                                                         "Nota"                                                        Arial                                                         	"Cliente"                                                                                                                   Arial                                                         	"Fecha
"                                                     Arial                                                         "Cpbte."                                                      Arial                                                         "Moneda: ", IdMoneda                                          Arial                                                         "Cobrador: " +  Cobrador                                      Arial                                                         "Ciclo: " + Ciclo                                             Arial                                                         �alltrim(idCliente) + " - " + Alltrim(razSocial), ", "+Alltrim(direccion),' ('+alltrim(NomContacto)+') ',' Tel. ' +alltrim(telefono)                                                           Arial                                                         ttod(fecha)                                                   "@D"                                                          Arial                                                         idcomprobante,numero                                                                                                        Arial                                                         #ttod(vt_rresumensaldos.vencimiento)                           "@D"                                                          Arial                                                         #ttod(vt_rresumensaldos.fec_acobrar)                           "@D"                                                          Arial                                                         vt_rresumensaldos.cuota                                                                                                     Arial                                                         vt_rresumensaldos.importe                                     "@Z 99,999,999,999.99"                                        Arial                                                         vt_rresumensaldos.saldo                                       "@Z 99,999,999,999.99"                                        Arial                                                         Notas                                                         Arial                                                         ;"Total Cliente : " + alltrim(idCliente) + " - " + razSocial                                                                                                                                 Arial                                                         vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         "Total Ciclo : " + Ciclo                                      Arial                                                         !isnull(ciclo)                                                vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         !isnull(ciclo)                                                "Total Cobrador : " + Cobrador                                Arial                                                         !isnull(cobrador)                                             vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         !isnull(cobrador)                                             -"Total Moneda : " +vt_rresumensaldos.idmoneda                                                                               Arial                                                         vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 78
Left = 169
Width = 555
Height = 347
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rresumensaldos"
DataSource = .NULL.
Name = "Dataenvironment"
                   XPROCEDURE Init
LOCAL strsql
DO SETEO

SET DATABASE TO datos

If Empty(m.idcliente)
	m.idcliente = null
ENDIF

IF EMPTY(m.Sucursal)
	m.Sucursal = null
ENDIF

IF EMPTY(m.idcobrador)
	m.idcobrador=null
ENDIF
	
	
	
TEXT TO cmdSQL noshow
	SELECT cb.IdCobrador + rtrim(p.Nombre) + ' ' + p.Apellido as Cobrador, 
	cf.Ciclo,
	f.IdCliente, f.Fecha, f.Sucursal, f.IdComprobante, f.IdFactura, f.Numero, a.Vencimiento, a.Cuota, a.Importe, a.Saldo, a.Fec_ACobrar, a.IdMoneda, c.RazSocial, 
	                      d.Descripci�n, f.Notas, c.NomContacto, c.Telefono, c.Direccion, f.NroContrato
	FROM         sas_Contrato AS cn LEFT OUTER JOIN
	                      sas_CicloFacturacion AS cf ON cn.IdEmpresa = cf.IdEmpresa AND cn.IdCiclo = cf.IdCiclo RIGHT OUTER JOIN
	                      vt_factura AS f INNER JOIN
	                      vt_forma_pago AS a ON a.IdFactura = f.IdFactura INNER JOIN
	                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa ON cn.IdEmpresa = f.IdEmpresa AND cn.NroContrato = f.NroContrato LEFT OUTER JOIN
	                      sucursal AS d ON f.Sucursal = d.Sucursal AND f.IdEmpresa = d.IdEmpresa
	left join vt_Cobradores cb on ISNULL(cn.IdEmpresa,c.IdEmpresa) = cb.IdEmpresa
	and isnull(cn.IdCobrador,c.IdCobrador) = cb.IdCobrador
	left join bs_personas p on cb.IdPersona = p.IdPersona
	WHERE f.IdEmpresa = ?oApp.Empresa 
	and (f.idcliente = ?m.idcliente or ?m.idcliente is null)
	and f.fecha BETWEEN ?m.dfecha and ?m.hfecha 
	and (f.sucursal = ?m.sucursal or ?m.sucursal is null)
	and (cb.IdCobrador = ?m.idcobrador  or ?m.idcobrador is null)
	and a.vencimiento BETWEEN ?m.dvence and ?m.hvence 
	and a.saldo <> 0 
	order by ISNULL(cn.IdCobrador,c.IdCobrador),Cf.Ciclo,f.Idcliente,F.Numero

ENDTEXT

=sql(cmdSQL ,'vt_rresumensaldos')
SELECT vt_rresumensaldos

ENDPROC
                                     a���    H  H                        �   %   �      �  (   �          �  U   ��  � � � G(� datos� %�C�� ���: � T�� ���� � %�C�� ���\ � T�� ���� � %�C�� ���~ � T�� ���� �	 M(� ��N �H 	SELECT cb.IdCobrador + rtrim(p.Nombre) + ' ' + p.Apellido as Cobrador, � �
 	cf.Ciclo,�� �� 	f.IdCliente, f.Fecha, f.Sucursal, f.IdComprobante, f.IdFactura, f.Numero, a.Vencimiento, a.Cuota, a.Importe, a.Saldo, a.Fec_ACobrar, a.IdMoneda, c.RazSocial, �j �d 	                      d.Descripci�n, f.Notas, c.NomContacto, c.Telefono, c.Direccion, f.NroContrato�6 �0 	FROM         sas_Contrato AS cn LEFT OUTER JOIN�� �} 	                      sas_CicloFacturacion AS cf ON cn.IdEmpresa = cf.IdEmpresa AND cn.IdCiclo = cf.IdCiclo RIGHT OUTER JOIN�7 �1 	                      vt_factura AS f INNER JOIN�W �Q 	                      vt_forma_pago AS a ON a.IdFactura = f.IdFactura INNER JOIN�� �� 	                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa ON cn.IdEmpresa = f.IdEmpresa AND cn.NroContrato = f.NroContrato LEFT OUTER JOIN�c �] 	                      sucursal AS d ON f.Sucursal = d.Sucursal AND f.IdEmpresa = d.IdEmpresa�T �N 	left join vt_Cobradores cb on ISNULL(cn.IdEmpresa,c.IdEmpresa) = cb.IdEmpresa�= �7 	and isnull(cn.IdCobrador,c.IdCobrador) = cb.IdCobrador�< �6 	left join bs_personas p on cb.IdPersona = p.IdPersona�) �# 	WHERE f.IdEmpresa = ?oApp.Empresa �? �9 	and (f.idcliente = ?m.idcliente or ?m.idcliente is null)�3 �- 	and f.fecha BETWEEN ?m.dfecha and ?m.hfecha �< �6 	and (f.sucursal = ?m.sucursal or ?m.sucursal is null)�D �> 	and (cb.IdCobrador = ?m.idcobrador  or ?m.idcobrador is null)�9 �3 	and a.vencimiento BETWEEN ?m.dvence and ?m.hvence � � 	and a.saldo <> 0 �P �J 	order by ISNULL(cn.IdCobrador,c.IdCobrador),Cf.Ciclo,f.Idcliente,F.Numero� �  � �" ��C � � vt_rresumensaldos� �� F� � U	  STRSQL SETEO DATOS	 IDCLIENTE SUCURSAL
 IDCOBRADOR CMDSQL SQL VT_RRESUMENSALDOS Init,     ��1 q q � � A � A � A � �Q
�a1qq�1A����1�A��a A "q 2                       M      )   H                                               �DRIVER=winspool
DEVICE=Enviar a OneNote 2013
OUTPUT=nul:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=120
COLOR=1
YRESOLUTION=144
                               ?  )  winspool  Enviar a OneNote 2013  nul:                                                                                (Enviar a OneNote 2013           � @/    �4d   x   �   A4                                                            ����GIS4            DINU" � $ ���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         �   SMTJ     � { 3 E E 3 9 1 1 4 - 3 0 B 4 - 4 5 a 4 - A 1 0 9 - 1 9 D 4 A 4 0 F C C 2 2 }   RESDLL UniresDLL PaperSize LETTER Orientation PORTRAIT Resolution DPI600 ColorMode 24bpp                     V4DM                                                                                                                     Arial                                                         idmoneda                                                      
IdCobrador                                                    Ciclo                                                         	idcliente                                                     *"An�lisis de Cartera por Ciclo y Cobrador"                    Arial                                                         empresa                                                       Arial                                                         vt_rresumensaldos.descripci�n                                                                                               Arial                                                         "Sucursal:"                                                   Arial                                                         1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                                           Arial                                                         
"Per�odo:"                                                    Arial                                                         1"Desde " + dtoc(m.dvence)+ " al " +dtoc(m.hvence)                                                                           Arial                                                         "Vencimientos:"                                               Arial                                                         "Vto."                                                        Arial                                                         "Fch. a Cobrar"                                               Arial                                                         "Cuota"                                                       Arial                                                         	"Importe"                                                     Arial                                                         "Saldo"                                                       Arial                                                         "Nota"                                                        Arial                                                         	"Cliente"                                                                                                                   Arial                                                         	"Fecha
"                                                     Arial                                                         "Cpbte."                                                      Arial                                                         "Moneda: ", IdMoneda                                          Arial                                                         "Cobrador: " +  Cobrador                                      Arial                                                         "Ciclo: " + Ciclo                                             Arial                                                         �alltrim(idCliente) + " - " + Alltrim(razSocial), ", "+Alltrim(direccion),' ('+alltrim(NomContacto)+') ',' Tel. ' +alltrim(telefono)                                                           Arial                                                         ttod(fecha)                                                   "@D"                                                          Arial                                                         idcomprobante,numero                                                                                                        Arial                                                         #ttod(vt_rresumensaldos.vencimiento)                           "@D"                                                          Arial                                                         #ttod(vt_rresumensaldos.fec_acobrar)                           "@D"                                                          Arial                                                         vt_rresumensaldos.cuota                                                                                                     Arial                                                         vt_rresumensaldos.importe                                     "@Z 99,999,999,999.99"                                        Arial                                                         vt_rresumensaldos.saldo                                       "@Z 99,999,999,999.99"                                        Arial                                                         Notas                                                         Arial                                                         ;"Total Cliente : " + alltrim(idCliente) + " - " + razSocial                                                                                                                                 Arial                                                         vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         "Total Ciclo : " + Ciclo                                      Arial                                                         !isnull(ciclo)                                                vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         !isnull(ciclo)                                                "Total Cobrador : " + Cobrador                                Arial                                                         !isnull(cobrador)                                             vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         !isnull(cobrador)                                             -"Total Moneda : " +vt_rresumensaldos.idmoneda                                                                               Arial                                                         vt_rresumensaldos.saldo                                       "@Z 999,999,999,999.99"                                       Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
datetime()                                                                                                                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 78
Left = 169
Width = 555
Height = 347
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rresumensaldos"
DataSource = .NULL.
Name = "Dataenvironment"
                   XPROCEDURE Init
LOCAL strsql
DO SETEO

SET DATABASE TO datos

If Empty(m.idcliente)
	m.idcliente = null
ENDIF

IF EMPTY(m.Sucursal)
	m.Sucursal = null
ENDIF

IF EMPTY(m.idcobrador)
	m.idcobrador=null
ENDIF
	
	
	
TEXT TO cmdSQL noshow
	SELECT cb.IdCobrador + rtrim(p.Nombre) + ' ' + p.Apellido as Cobrador, 
	cf.Ciclo,
	f.IdCliente, f.Fecha, f.Sucursal, f.IdComprobante, f.IdFactura, f.Numero, a.Vencimiento, a.Cuota, a.Importe, a.Saldo, a.Fec_ACobrar, a.IdMoneda, c.RazSocial, 
	                      d.Descripci�n, f.Notas, c.NomContacto, c.Telefono, c.Direccion, f.NroContrato
	FROM         sas_Contrato AS cn LEFT OUTER JOIN
	                      sas_CicloFacturacion AS cf ON cn.IdEmpresa = cf.IdEmpresa AND cn.IdCiclo = cf.IdCiclo RIGHT OUTER JOIN
	                      vt_factura AS f INNER JOIN
	                      vt_forma_pago AS a ON a.IdFactura = f.IdFactura INNER JOIN
	                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa ON cn.IdEmpresa = f.IdEmpresa AND cn.NroContrato = f.NroContrato LEFT OUTER JOIN
	                      sucursal AS d ON f.Sucursal = d.Sucursal AND f.IdEmpresa = d.IdEmpresa
	left join vt_Cobradores cb on ISNULL(cn.IdEmpresa,c.IdEmpresa) = cb.IdEmpresa
	and isnull(cn.IdCobrador,c.IdCobrador) = cb.IdCobrador
	left join bs_personas p on cb.IdPersona = p.IdPersona
	WHERE f.IdEmpresa = ?oApp.Empresa 
	and (f.idcliente = ?m.idcliente or ?m.idcliente is null)
	and f.fecha BETWEEN ?m.dfecha and ?m.hfecha 
	and (f.sucursal = ?m.sucursal or ?m.sucursal is null)
	and (cb.IdCobrador = ?m.idcobrador  or ?m.idcobrador is null)
	and a.vencimiento BETWEEN ?m.dvence and ?m.hvence 
	and a.saldo <> 0 
	order by ISNULL(cn.IdCobrador,c.IdCobrador),Cf.Ciclo,f.Idcliente,F.Numero

ENDTEXT

=sql(cmdSQL ,'vt_rresumensaldos')
SELECT vt_rresumensaldos

ENDPROC
                                     a���    H  H                        �   %   �      �  (   �          �  U   ��  � � � G(� datos� %�C�� ���: � T�� ���� � %�C�� ���\ � T�� ���� � %�C�� ���~ � T�� ���� �	 M(� ��N �H 	SELECT cb.IdCobrador + rtrim(p.Nombre) + ' ' + p.Apellido as Cobrador, � �
 	cf.Ciclo,�� �� 	f.IdCliente, f.Fecha, f.Sucursal, f.IdComprobante, f.IdFactura, f.Numero, a.Vencimiento, a.Cuota, a.Importe, a.Saldo, a.Fec_ACobrar, a.IdMoneda, c.RazSocial, �j �d 	                      d.Descripci�n, f.Notas, c.NomContacto, c.Telefono, c.Direccion, f.NroContrato�6 �0 	FROM         sas_Contrato AS cn LEFT OUTER JOIN�� �} 	                      sas_CicloFacturacion AS cf ON cn.IdEmpresa = cf.IdEmpresa AND cn.IdCiclo = cf.IdCiclo RIGHT OUTER JOIN�7 �1 	                      vt_factura AS f INNER JOIN�W �Q 	                      vt_forma_pago AS a ON a.IdFactura = f.IdFactura INNER JOIN�� �� 	                      vt_clientes AS c ON f.IdCliente = c.IdCliente AND f.IdEmpresa = c.IdEmpresa ON cn.IdEmpresa = f.IdEmpresa AND cn.NroContrato = f.NroContrato LEFT OUTER JOIN�c �] 	                      sucursal AS d ON f.Sucursal = d.Sucursal AND f.IdEmpresa = d.IdEmpresa�T �N 	left join vt_Cobradores cb on ISNULL(cn.IdEmpresa,c.IdEmpresa) = cb.IdEmpresa�= �7 	and isnull(cn.IdCobrador,c.IdCobrador) = cb.IdCobrador�< �6 	left join bs_personas p on cb.IdPersona = p.IdPersona�) �# 	WHERE f.IdEmpresa = ?oApp.Empresa �? �9 	and (f.idcliente = ?m.idcliente or ?m.idcliente is null)�3 �- 	and f.fecha BETWEEN ?m.dfecha and ?m.hfecha �< �6 	and (f.sucursal = ?m.sucursal or ?m.sucursal is null)�D �> 	and (cb.IdCobrador = ?m.idcobrador  or ?m.idcobrador is null)�9 �3 	and a.vencimiento BETWEEN ?m.dvence and ?m.hvence � � 	and a.saldo <> 0 �P �J 	order by ISNULL(cn.IdCobrador,c.IdCobrador),Cf.Ciclo,f.Idcliente,F.Numero� �  � �" ��C � � vt_rresumensaldos� �� F� � U	  STRSQL SETEO DATOS	 IDCLIENTE SUCURSAL
 IDCOBRADOR CMDSQL SQL VT_RRESUMENSALDOS Init,     ��1 q q � � A � A � A � �Q
�a1qq�1A����1�A��a A "q 2                       M      )   H                                         