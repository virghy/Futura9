  Y                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Enviar a OneNote 2007
OUTPUT=Send To Microsoft OneNote Port:
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
      Z  )  winspool  Enviar a OneNote 2007  Send To Microsoft OneNote Port:                       ,Enviar a OneNote 2007            � � /        d   ,  ,   Letter                                                                                wpno               �          �                                                                                                                     Arial      Arial      Arial      Arial      Arial      Arial      Arial      "..\bitmaps\bigfun.gif"      Eventos.Sucursal='01'      "..\bitmaps\bigfunextreme.gif"      Eventos.Sucursal='02'      "Resumen de Cuentas"      Arial      	NroEvento      Arial      	"N�mero:"      Arial      	Agasajado      Arial      Edad      Arial      "Nombre del cumplea�ero:"      Arial      "Edad:"      Arial      	RazSocial      Arial      "Nombre de los Padres"      Arial      NomContacto      Arial      Telefono      Arial      
"Telefono"      Arial      Email      Arial      "Email"      Arial      	Direccion      Arial      "Direccion"      Arial      Opcion      Arial      "Tipo Cumplea�o"      Arial      Fecha      "@D"      Arial      dhora,' a ', hHora      Arial      "Fecha"      Arial      "Fecha"      Arial      
"Cantidad"      Arial      "Precio Unitario"      Arial      "Total"      Arial      "Descripci�n"      Arial      Opcion      Arial      1      "9999"      Arial      ImporteBasico      "999,999,999"      Arial      ImporteBasico      "999,999,999"      Arial      "Adicional Ni�os"      Arial      NroAdicional      "9999"      Arial      AdicionalUnitario      "999,999,999"      Arial      CostoAdicional      "999,999,999"      Arial      Descripcion      Arial      Cantidad      "9999"      Arial      Precio      "999,999,999"      Arial      Precio*Cantidad      "999,999,999"      Arial      "Descuentos"      Arial      descuento>0      	Descuento      "@Z 999,999,999"      Arial      TotalGeneral - IVA      "999,999,999"      Arial      
"SubTotal"      Arial      Iva      "@Z 999,999,999"      Arial      "IVA"      Arial      iva>0      TotalGeneral      "999,999,999"      Arial      "Total Cumplea�o"      Arial      Adelando      "999,999,999"      Arial      "Se�a"      Arial      TotalGeneral - Adelando      "999,999,999"      Arial      "Saldo a Pagar"      Arial      D"Forma de Pago " + Valores.TipoValor,Valores.Banco,Valores.NroCheque      Arial      !empty(Valores.TipoValor)      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
     CPROCEDURE Init
TEXT TO cmdSQL NOSHOW 
SELECT     e.Fecha, NroEvento,e.Agasajado, e.Edad, c.RazSocial, c.NomContacto, c.Direccion, c.Telefono, o.Opcion, e.dhora, e.hHora, e.Obs, eo.IdProducto, eo.Descripcion, 
                      eo.Precio, eo.Cantidad, c.Email, e.ImporteBasico, e.Excedentes, e.CostoAdicional, e.NroAdicional, e.AdicionalUnitario, e.TotalGeneral, e.Descuento, 
                      e.Iva, e.Adelando,e.Sucursal
FROM         dbo.ev_Eventos e INNER JOIN
                      dbo.vt_clientes c ON e.IdEmpresa = c.IdEmpresa AND e.IdCliente = c.IdCliente INNER JOIN
                      dbo.ev_Opciones o ON e.IdEmpresa = o.IdEmpresa AND e.IdOpcion = o.IdOpcion INNER JOIN
                      dbo.Ev_EventosOp eo ON e.IdEmpresa = eo.IdEmpresa AND e.IdEvento = eo.IdEvento
where e.IdEvento = ?m.IdEvento and eo.Cantidad>0
ENDTEXT
=SQL(cmdSQL,'Eventos')

TEXT TO cmdSQL NOSHOW 
	SELECT     t.tipovalor, v.nrocheque, v.importe, b.descripcion AS Banco
	FROM         dbo.ts_valores_base v left JOIN
	                      dbo.ts_tipovalor t ON v.idtipovalor = t.idtipovalor left JOIN
	                      dbo.bs_bancos b ON v.idbanco = b.idbanco
	where IdEvento = ?m.IdEvento and v.Concepto <> 'Se�a'
ENDTEXT
=SQL(cmdSQL,'Valores')
SELECT Eventos


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���                              �R   %   T      �     |          �  U  �	 M(�  ��� �� SELECT     e.Fecha, NroEvento,e.Agasajado, e.Edad, c.RazSocial, c.NomContacto, c.Direccion, c.Telefono, o.Opcion, e.dhora, e.hHora, e.Obs, eo.IdProducto, eo.Descripcion, �� ��                       eo.Precio, eo.Cantidad, c.Email, e.ImporteBasico, e.Excedentes, e.CostoAdicional, e.NroAdicional, e.AdicionalUnitario, e.TotalGeneral, e.Descuento, �8 �2                       e.Iva, e.Adelando,e.Sucursal�. �( FROM         dbo.ev_Eventos e INNER JOIN�s �m                       dbo.vt_clientes c ON e.IdEmpresa = c.IdEmpresa AND e.IdCliente = c.IdCliente INNER JOIN�q �k                       dbo.ev_Opciones o ON e.IdEmpresa = o.IdEmpresa AND e.IdOpcion = o.IdOpcion INNER JOIN�j �d                       dbo.Ev_EventosOp eo ON e.IdEmpresa = eo.IdEmpresa AND e.IdEvento = eo.IdEvento�6 �0 where e.IdEvento = ?m.IdEvento and eo.Cantidad>0� � ��C �  � Eventos� ��	 M(�  ��M �G 	SELECT     t.tipovalor, v.nrocheque, v.importe, b.descripcion AS Banco�3 �- 	FROM         dbo.ts_valores_base v left JOIN�Z �T 	                      dbo.ts_tipovalor t ON v.idtipovalor = t.idtipovalor left JOIN�E �? 	                      dbo.bs_bancos b ON v.idbanco = b.idbanco�< �6 	where IdEvento = ?m.IdEvento and v.Concepto <> 'Se�a'� � ��C �  � Valores� �� F� � U  CMDSQL SQL EVENTOS
  �  � U  SETEO Init,     �� BeforeOpenTables?    ��1 � ��1�aA �� �1�Q�A �q 4 q 2                               .  8      )                     