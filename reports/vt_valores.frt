  ,O                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
      W    winspool  PrimoPDF  PrimoPort:                  1)  USB006                          	HPrimoPDF .0.195\HP Deskjet 100  � �S� 	 �
od   X  X  Letter                                                                            PRIV�0                                                                                       '''  '          �                                  \K �	                             �{��     � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         tocol pcl3gui hpImagingDll hpfime50.dll Orientation PORTRAIT ColorMode 24bpp HPGrayScale 0 HPTextThreshold 24 HPGraphicThreshold 22 hpSupportsREST 1 hpDPIInfo 0 HPHideQualitySettings 0 Resolution 300x300dpi Halftone HT_PATSIZE_DEFAULT HPMechOffset 60 HPRlt 1 HPPagesToPrint 4_AllPages PaperSize A4 HPMaxDpi 0_disabled MediaType 0_-2_300x300                                                                    P  LPPH   Q�`Q�`                                      X  ,  X  ,     �  �  �  }  K   K   <   <   <   <          '           '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Wed Jul 06 18:17:49:910 2011                                                                    w�`K   K                w�`      Courier New      	IdUsuario      IdCaja      	TipoValor      Forma      Arial      Arial      Arial      Arial      Courier New      Arial      empresa             Arial      "Detalle de Valores Recibidos"      Arial      "Sucursal:"      Arial      (iif(isnull(m.sucursal),'Todos',Sucursal)      Arial      
"Per�odo:"      Arial      m.dfecha,  ' al ', m.hfecha      Arial      	"Fecha
"      Arial      	"Factura"      Arial      	"Importe"      Arial      	"Cliente"      Arial      "Descuento"      Arial      &"Cajero/a: " + IdUsuario+ " " +Usuario      Arial      4"Abierto por : " + IdUsuarioApertura+ " " +UsuarioAP      Arial      Fin      Arial      Inicio      Arial      "Cierre"      Arial      "Inicio"      Arial      "Tipo Valor: ",tipovalor      Arial      "Forma: ",forma      Arial      not isnull(Forma)      Fecha      Arial      IdComprobante,Numero      Arial      IdCliente, ' ' , RazSocial      Arial      importe      "999,999,999,999.99"      Arial      	Descuento      "999,999,999,999.99"      Arial      "Total: ",Forma      Arial      not isnull(forma)      importe      "999,999,999,999.99"      Arial      not isnull(forma)      	Descuento      "999,999,999,999.99"      Arial      not isnull(forma)      "Total: ",tipovalor      Arial      importe      "999,999,999,999.99"      Arial      	Descuento      "999,999,999,999.99"      Arial      "Total Caja: "      Arial      importe      "999,999,999,999.99"      Arial      	Descuento      "999,999,999,999.99"      Arial      'P�g. '+alltrim(Str(_pageno))      Arial      
datetime()      Arial      "TOTAL GENERAL"      Arial      importe      "999,999,999,999.99"      Arial      	Descuento      "999,999,999,999.99"      Arial      dataenvironment      aTop = 219
Left = 361
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     zPROCEDURE Init
If Empty(m.sucursal)
	Store null To m.sucursal
ENDIF
If Empty(m.usuario)
	Store null To m.usuario
ENDIF



DO SETEO
TEXT TO CMDSQL NOSHOW
SELECT     F.Fecha, f.ImpDesc as Descuento, ISNULL(tv.tipovalor, cn.Descripcion) AS TipoValor, ISNULL(v.importe,f.TotalFactura) as Importe, s.Sucursal + '-' + s.Descripci�n AS Sucursal, c.IdCaja, c.IdUsuario, c.Fecha AS FechaCaja, 
                      c.Inicio, c.Fin, c.Estado, c.IdUsuarioApertura, RTRIM(u.last_name) + ' ' + u.first_name AS Usuario, RTRIM(ua.last_name) + ' ' + ua.first_name AS UsuarioAP, 
                      vt_tpvForma.Forma, F.IdComprobante, F.Numero, ISNULL(v.idmoneda,f.IdMoneda) as IdMoneda, f.Idcliente,cl.RazSocial
FROM         vt_factura AS F LEFT OUTER JOIN
                      ts_valores_base AS v ON F.IdFactura = v.IdFactura 
                      left join vt_Clientes cl on f.IdCliente = cl.Idcliente and f.IdEmpresa= cl.IdEmpresa 
                      INNER JOIN
                      vt_Condicion AS cn ON F.IdCondicion = cn.IdCondicion AND F.IdEmpresa = cn.IdEmpresa INNER JOIN
                      sucursal AS s ON F.Sucursal = s.Sucursal AND F.IdEmpresa = s.IdEmpresa INNER JOIN
                      vt_caja AS c ON F.IdHabilitacion = c.IdCaja INNER JOIN
                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id LEFT OUTER JOIN
                      ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor LEFT OUTER JOIN
                      vt_tpvForma ON v.idempresa = vt_tpvForma.IdEmpresa AND v.IdFormaTpv = vt_tpvForma.IdForma LEFT OUTER JOIN
                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
where c.idempresa=?oApp.Empresa and c.Fecha between ?m.dfecha and ?m.hfecha 
and (F.sucursal = ?m.sucursal or ?m.Sucursal is null)
 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
and c.Estado='C'
ORDER BY c.IdUsuario, c.Inicio,ISNULL(v.idmoneda,f.IdMoneda),c.IdCaja,ISNULL(tv.tipovalor, cn.Descripcion),Forma,IdComprobante,f.Numero
ENDTEXT
sql (cmdsql, "consulta")
SELECT CONSULTA


*!*	TEXT TO CMDSQL NOSHOW
*!*	SELECT     F.Fecha, tv.tipovalor, v.importe, s.Sucursal + '-' + s.Descripci�n AS Sucursal, c.IdCaja, c.IdUsuario, c.Fecha AS FechaCaja, c.Inicio, c.Fin, c.Estado, 
*!*	                      c.IdUsuarioApertura, RTRIM(u.last_name) + ' ' + u.first_name AS Usuario, RTRIM(ua.last_name) + ' ' + ua.first_name AS UsuarioAP, vt_tpvForma.Forma, 
*!*	                      F.IdComprobante, F.Numero, v.idmoneda
*!*	FROM         vt_factura AS F INNER JOIN
*!*	                      ts_valores_base AS v ON F.IdFactura = v.IdFactura INNER JOIN
*!*	                      ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor INNER JOIN
*!*	                      sucursal AS s ON F.Sucursal = s.Sucursal AND F.IdEmpresa = s.IdEmpresa INNER JOIN
*!*	                      vt_caja AS c ON F.IdHabilitacion = c.IdCaja INNER JOIN
*!*	                      usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id LEFT OUTER JOIN
*!*	                      vt_tpvForma ON v.idempresa = vt_tpvForma.IdEmpresa AND v.IdFormaTpv = vt_tpvForma.IdForma LEFT OUTER JOIN
*!*	                      usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id
*!*	where f.idempresa=?oApp.Empresa and F.Fecha between ?m.dfecha and ?m.hfecha 
*!*	and (F.sucursal = ?m.sucursal or ?m.Sucursal is null)
*!*	 and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)
*!*	and c.Estado='C'
*!*	ORDER BY c.IdUsuario, c.Inicio,v.idmoneda,c.IdCaja,tv.TipoValor,Forma,IdComprobante,f.Numero
*!*	ENDTEXT
ENDPROC
     	T���    ;	  ;	                        �.   %   �      �  !   �          �  U  A %�C��  ��� � J���(��  � � %�C�� ���@ � J���(�� � � � �	 M(� ��� �� SELECT     F.Fecha, f.ImpDesc as Descuento, ISNULL(tv.tipovalor, cn.Descripcion) AS TipoValor, ISNULL(v.importe,f.TotalFactura) as Importe, s.Sucursal + '-' + s.Descripci�n AS Sucursal, c.IdCaja, c.IdUsuario, c.Fecha AS FechaCaja, �� ��                       c.Inicio, c.Fin, c.Estado, c.IdUsuarioApertura, RTRIM(u.last_name) + ' ' + u.first_name AS Usuario, RTRIM(ua.last_name) + ' ' + ua.first_name AS UsuarioAP, �� ��                       vt_tpvForma.Forma, F.IdComprobante, F.Numero, ISNULL(v.idmoneda,f.IdMoneda) as IdMoneda, f.Idcliente,cl.RazSocial�2 �, FROM         vt_factura AS F LEFT OUTER JOIN�N �H                       ts_valores_base AS v ON F.IdFactura = v.IdFactura �q �k                       left join vt_Clientes cl on f.IdCliente = cl.Idcliente and f.IdEmpresa= cl.IdEmpresa �& �                        INNER JOIN�z �t                       vt_Condicion AS cn ON F.IdCondicion = cn.IdCondicion AND F.IdEmpresa = cn.IdEmpresa INNER JOIN�m �g                       sucursal AS s ON F.Sucursal = s.Sucursal AND F.IdEmpresa = s.IdEmpresa INNER JOIN�R �L                       vt_caja AS c ON F.IdHabilitacion = c.IdCaja INNER JOIN�} �w                       usuarios AS u ON c.IdUsuario COLLATE SQL_Latin1_General_CP1_CI_AS = u.employee_id LEFT OUTER JOIN�` �Z                       ts_tipovalor AS tv ON v.idtipovalor = tv.idtipovalor LEFT OUTER JOIN�� �                       vt_tpvForma ON v.idempresa = vt_tpvForma.IdEmpresa AND v.IdFormaTpv = vt_tpvForma.IdForma LEFT OUTER JOIN�w �q                       usuarios AS ua ON c.IdUsuarioApertura COLLATE SQL_Latin1_General_CP1_CI_AS = ua.employee_id�R �L where c.idempresa=?oApp.Empresa and c.Fecha between ?m.dfecha and ?m.hfecha �; �5 and (F.sucursal = ?m.sucursal or ?m.Sucursal is null)�; �5  and (c.IdUsuario = ?m.Usuario or ?m.Usuario is null)� � and c.Estado='C'�� �� ORDER BY c.IdUsuario, c.Inicio,ISNULL(v.idmoneda,f.IdMoneda),c.IdCaja,ISNULL(tv.tipovalor, cn.Descripcion),Forma,IdComprobante,f.Numero� � ��C � � consulta� �� F� � U  SUCURSAL USUARIO SETEO CMDSQL SQL CONSULTA Init,     ��1 � A � A t � ���!�a��!�Qq!��a�A �q @1                       o      )   ;	                  