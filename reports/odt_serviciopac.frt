  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      
IdPaciente      Arial      Arial      Arial      Arial      Arial      Arial      Arial      oApp.Nombreempresa      Arial      #"Servicios Realizados por Paciente"      Arial      m.dfecha, ' al ', m.hFecha      Arial      
"Per�odo:"      Arial      "Inicio"      Arial      "Fin"      Arial      "Tratamiento
"      Arial      "Estado"      Arial      "Profesional
"      Arial      
"Cantidad"      Arial      	"Arancel"      Arial      	"Importe"      Arial      "Cobertura"      Arial      IdPaciente,' ', Paciente      Arial      Fecha      "@D"      Arial      FechaFin      "@D"      Arial      "alltrim(Tratamiento) , ' ' + Pieza      Arial      Estado      Arial      IdProfesional,Profesional      Arial      Cantidad      	"999,999"      Arial      Precio      "999,999,999"      Arial      Importe      "9,999,999,999"      Arial      IdCliente,' ', RazSocial      Arial      	"Total: "      Arial      Importe      "9,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
IF EMPTY(m.Idcliente)
	m.Idcliente = null
ENDIF
	

TEXT TO cmdSQL noshow
	SELECT     c1.IdCliente, c1.RazSocial, t.Fecha, t.Pieza, t.Cantidad, t.Precio, t.Importe, t.IdProfesional, 
	t.IdCliente AS IdPaciente, c.RazSocial AS Paciente, c.nrosocio,
	p.Descripcion as Tratamiento,t.Estado,t.FechaFin, v.NombreVendedor as Profesional
	FROM         odt_Tratamiento AS t left JOIN
	                      vt_clientes AS c ON t.IdEmpresa = c.IdEmpresa AND t.IdCliente = c.IdCliente left JOIN
	                      vt_factura AS f ON t.IdFactura = f.IdFactura left JOIN
	                      st_Producto AS p ON t.IdEmpresa = p.IdEmpresa AND t.IdTratamiento = p.IdProducto left JOIN
	                      vt_clientes AS c1 ON t.IdEmpresa = c1.IdEmpresa AND t.IdCliente1 = c1.IdCliente inner join 
	                      vvt_Vendedores AS v ON t.IdEmpresa = v.IdEmpresa AND t.IdProfesional= v.IdVendedor
	where t.IdEmpresa=?oApp.Empresa
			and t.Fecha between ?m.dFecha and ?m.hFecha
			and (t.IdCliente = ?m.IdCliente or ?m.Idcliente is null) 
	order by t.IdCliente,t.Fecha			
ENDTEXT

sql(cmdSQL,'cComision')
SELECT cComision


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        ��   %   �      :     
          �  U  z %�C��  ��� � T��  ���� �	 M(� ��r �l 	SELECT     c1.IdCliente, c1.RazSocial, t.Fecha, t.Pieza, t.Cantidad, t.Precio, t.Importe, t.IdProfesional, �F �@ 	t.IdCliente AS IdPaciente, c.RazSocial AS Paciente, c.nrosocio,�X �R 	p.Descripcion as Tratamiento,t.Estado,t.FechaFin, v.NombreVendedor as Profesional�2 �, 	FROM         odt_Tratamiento AS t left JOIN�r �l 	                      vt_clientes AS c ON t.IdEmpresa = c.IdEmpresa AND t.IdCliente = c.IdCliente left JOIN�S �M 	                      vt_factura AS f ON t.IdFactura = f.IdFactura left JOIN�w �q 	                      st_Producto AS p ON t.IdEmpresa = p.IdEmpresa AND t.IdTratamiento = p.IdProducto left JOIN�x �r 	                      vt_clientes AS c1 ON t.IdEmpresa = c1.IdEmpresa AND t.IdCliente1 = c1.IdCliente inner join �o �i 	                      vvt_Vendedores AS v ON t.IdEmpresa = v.IdEmpresa AND t.IdProfesional= v.IdVendedor�& �  	where t.IdEmpresa=?oApp.Empresa�4 �. 			and t.Fecha between ?m.dFecha and ?m.hFecha�B �< 			and (t.IdCliente = ?m.IdCliente or ?m.Idcliente is null) �& �  	order by t.IdCliente,t.Fecha			� � ��C � �	 cComision� �� F� � U 	 IDCLIENTE CMDSQL SQL	 CCOMISION
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � A � !a�!!1q��aA!aA �q 4 q 2                       �        �  �      )   �                  