  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      	IdCliente      Arial      Arial      Arial      Arial      Arial      Arial      Arial      $"Servicios Realizados por Cobertura"      Arial      oApp.Nombreempresa      Arial      m.dfecha, ' al ', m.hFecha      Arial      
"Per�odo:"      Arial      "Paciente
"      Arial      "Tratamiento
"      Arial      	"Pieza
"      Arial      
"Cantidad"      Arial      	"Arancel"      Arial      	"Importe"      Arial      '"Cobertura: ", IdCliente,' ', RazSocial      Arial      NroSocio,' ', Paciente      Arial      Tratamiento      Arial      Pieza      Arial      Cantidad      	"999,999"      Arial      Precio      "999,999,999"      Arial      Importe      "9,999,999,999"      Arial      	"Total: "      Arial      Importe      "9,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     ,PROCEDURE Init
IF EMPTY(m.Idcliente)
	m.Idcliente = null
ENDIF
	

TEXT TO cmdSQL noshow
	SELECT     c1.IdCliente, c1.RazSocial, t.Fecha, t.Pieza, t.Cantidad, t.Precio, t.Importe, t.IdProfesional, 
	t.IdCliente AS IdPaciente, c.RazSocial AS Paciente, c.nrosocio,
	p.Descripcion as Tratamiento
	FROM         odt_Tratamiento AS t INNER JOIN
	                      vt_clientes AS c ON t.IdEmpresa = c.IdEmpresa AND t.IdCliente = c.IdCliente INNER JOIN
	                      vt_factura AS f ON t.IdFactura = f.IdFactura INNER JOIN
	                      st_Producto AS p ON t.IdEmpresa = p.IdEmpresa AND t.IdTratamiento = p.IdProducto INNER JOIN
	                      vt_clientes AS c1 ON t.IdEmpresa = c1.IdEmpresa AND t.IdCliente1 = c1.IdCliente
	where t.IdEmpresa=?oApp.Empresa
			and t.Fecha between ?m.dFecha and ?m.hFecha
			and (t.IdCliente1 = ?m.IdCliente or ?m.Idcliente is null) 
	order by c1.IdCliente,t.IdCliente,t.Fecha			
ENDTEXT

sql(cmdSQL,'cComision')
SELECT cComision


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���    �  �                        ,_   %   D      �     l          �  U  � %�C��  ��� � T��  ���� �	 M(� ��r �l 	SELECT     c1.IdCliente, c1.RazSocial, t.Fecha, t.Pieza, t.Cantidad, t.Precio, t.Importe, t.IdProfesional, �F �@ 	t.IdCliente AS IdPaciente, c.RazSocial AS Paciente, c.nrosocio,�# � 	p.Descripcion as Tratamiento�3 �- 	FROM         odt_Tratamiento AS t INNER JOIN�s �m 	                      vt_clientes AS c ON t.IdEmpresa = c.IdEmpresa AND t.IdCliente = c.IdCliente INNER JOIN�T �N 	                      vt_factura AS f ON t.IdFactura = f.IdFactura INNER JOIN�x �r 	                      st_Producto AS p ON t.IdEmpresa = p.IdEmpresa AND t.IdTratamiento = p.IdProducto INNER JOIN�l �f 	                      vt_clientes AS c1 ON t.IdEmpresa = c1.IdEmpresa AND t.IdCliente1 = c1.IdCliente�& �  	where t.IdEmpresa=?oApp.Empresa�4 �. 			and t.Fecha between ?m.dFecha and ?m.hFecha�C �= 			and (t.IdCliente1 = ?m.IdCliente or ?m.Idcliente is null) �3 �- 	order by c1.IdCliente,t.IdCliente,t.Fecha			� � ��C � �	 cComision� �� F� � U 	 IDCLIENTE CMDSQL SQL	 CCOMISION
  �  � U  SETEO Init,     �� BeforeOpenTables/    ��1 � A � !a111A��aA11A �q 4 q 2                       �          !      )   �                  