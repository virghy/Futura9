                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      "..\bitmaps\logomedident.jpg"      "Presupuesto"      Arial      IdCliente,' ',RazSocial      Arial      "Paciente:"      Arial      NombreVendedor      Arial      "Profesional Responsable:"      Arial      Fecha      "@D"      Arial      "Fecha"      Arial      	"Superf."      Arial      
"Cantidad"      Arial      "Precio Unitario"      Arial      "Total"      Arial      "Descripci�n"      Arial      "Pieza"      Arial      Descripcion      Arial      Pieza      Arial      
Superficie      "9999"      Arial      Cantidad      "9999"      Arial       iif(isnull(idCliente1),Precio,0)      "999,999,999"      Arial      !iif(isnull(idCliente1),Importe,0)      "999,999,999"      Arial      �"Av. Aviadores del Chaco N� 2.875 c/Molas L�pez - Asunci�n - Paraguay
Telefax: 607 057 (R.A.) Cel. 0981 173 939
E-mail: coi.medident@gmail.com"      "@I"      Arial      "Total"      Arial      !iif(isnull(idCliente1),Importe,0)      "@Z 999,999,999"      Arial      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
     qPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
TEXT TO cmdSQL
	SELECT     c.RazSocial, t.Fecha, t.Pieza, t.Superficie, t.Diagnostico, t.IdTratamiento, t.IdProfesional, 
				t.Importe, s.Descripcion, t.IdEmpresa, p.NombreVendedor,
				t.Cantidad,t.Precio,t.IdCliente1
	FROM         odt_Tratamiento AS t  left join vvt_Vendedores AS p 
					ON p.IdEmpresa = t.IdEmpresa AND p.IdVendedor = t.IdProfesional 
	                      LEFT OUTER JOIN
	                      vt_clientes AS c ON t.IdEmpresa = c.IdEmpresa AND t.IdCliente = c.IdCliente LEFT OUTER JOIN
	                      st_Producto AS s ON t.IdEmpresa = s.IdEmpresa AND t.IdTratamiento = s.IdProducto
	where t.IdEmpresa=?oApp.Empresa
	and t.Fecha = ?m.Fecha
	and t.IdCliente = ?m.Idcliente                      
	and t.Estado='P'
ENDTEXT
sql(cmdSQL,'cEventos')
SELECT cEventos
ENDPROC
     b���    I  I                        ��   %   �      �     �          �  U  
  �  � U  SETEOD M(�  �p �j 	SELECT     c.RazSocial, t.Fecha, t.Pieza, t.Superficie, t.Diagnostico, t.IdTratamiento, t.IdProfesional, �B �< 				t.Importe, s.Descripcion, t.IdEmpresa, p.NombreVendedor,�* �$ 				t.Cantidad,t.Precio,t.IdCliente1�H �B 	FROM         odt_Tratamiento AS t  left join vvt_Vendedores AS p �K �E 					ON p.IdEmpresa = t.IdEmpresa AND p.IdVendedor = t.IdProfesional �, �& 	                      LEFT OUTER JOIN�x �r 	                      vt_clientes AS c ON t.IdEmpresa = c.IdEmpresa AND t.IdCliente = c.IdCliente LEFT OUTER JOIN�m �g 	                      st_Producto AS s ON t.IdEmpresa = s.IdEmpresa AND t.IdTratamiento = s.IdProducto�& �  	where t.IdEmpresa=?oApp.Empresa� � 	and t.Fecha = ?m.Fecha�; �5 	and t.IdCliente = ?m.Idcliente                      � � 	and t.Estado='P'� � ��C �  � cEventos� �� F� � U  CMDSQL SQL CEVENTOS BeforeOpenTables,     �� InitA     ��1 q 3 � !������a��qA �q 1                       &         A   f      )   I                  