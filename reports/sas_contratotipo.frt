  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=1
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Courier New      IdTipoCobertura      Arial      Arial      Arial      Courier New      Arial      !"Contratos por Tipo de Cobertura"      Arial      oApp.NombreEmpresa      Arial      m.dFecha, " " ,m.hFecha      Arial      Diif(isnull(m.tipoCobertura),"Todos", IdTipoCobertura +TipoCobertura)      Arial      IdEstado      Arial      &iif(isnull(m.IdCiudad),'Todas',Ciudad)      Arial      	"Periodo"      Arial      "Tipo Cobertura"      Arial      "Estado"      Arial      "Ciudad"      Arial      "Estado"      Arial      
"Vendedor"      Arial      
"Contrato"      Arial      "Fecha"      Arial      
"Vigencia"      Arial      "Direcci�n"      Arial      IdTipoCobertura,TipoCobertura      Arial      "Tipo Cobertura"      Arial      5NroContrato," ",IdCliente, " ", NVL(Nombre,RazSocial)      Arial      dtoc(Fecha)      "@D"      Arial       dtoc(dFecha), " - ",dtoc(hFecha)      Arial      IdEstado      Arial      IdVendedor,NombreVendedor      Arial      #DireccionServicio, "("+Ciudad + ")"      Arial      IdEstado      "99999"      Arial      "Total Tipo Cobertura"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      IdEstado      "99999"      Arial      "Total General"      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo

IF EMPTY(m.Idciudad)
	m.Idciudad=null
ENDIF
	
IF EMPTY(m.TipoCobertura)
	m.TipoCobertura=null
ENDIF
	


TEXT TO cmdSQL noshow
SELECT     cn.NroContrato, cn.IdCliente, cl.RazSocial, cn.Fecha, cn.DireccionServicio, cn.IdTipoCobertura, tc.TipoCobertura, cn.IdCondicion, cn.IdEstado, cn.FechaBaja, cn.Obs, 
                      cn.IdVendedor, cn.dFecha, cn.hFecha, cn.nombre, vn.NombreVendedor, 
                      cd.Descripcion AS Condicion, cn.Importe, 
                      Ciudad.Descripcion as Ciudad
FROM   				  sas_Contrato AS cn 
				INNER JOIN vt_clientes AS cl ON cn.IdEmpresa = cl.IdEmpresa AND cn.IdCliente = cl.IdCliente 
				INNER JOIN sas_TipoCobertura AS tc ON cn.IdEmpresa = tc.IdEmpresa AND cn.IdTipoCobertura = tc.IdTipoCobertura
				LEFT OUTER JOIN vt_Condicion AS cd ON cn.IdEmpresa = cd.IdEmpresa AND cn.IdCondicion = cd.IdCondicion 
				LEFT OUTER JOIN vvt_Vendedores AS vn ON cn.IdEmpresa = vn.IdEmpresa AND cn.IdVendedor = vn.IdVendedor  
                left join       bs_Ciudades AS Ciudad on cn.IDCiudad = Ciudad.IdCiudad        where cn.IdEmpresa=?oApp.Empresa
and ((cn.Fecha between ?m.dFecha and ?m.hFecha) or cn.Fecha is null)
and (cn.Idciudad = ?m.Idciudad or ?m.Idciudad is null)
and (cn.IDTipoCobertura=?m.TipoCobertura or ?m.TipoCobertura is null)
and cn.IdEstado='A'
order by cn.IdTipoCobertura,cn.NroContrato
ENDTEXT
sql(cmdSQL,'cContratos')
SELECT cContratos

ENDPROC
     ����    �  �                        ��   %   �      @               �  U  � �  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� �	 M(� ��� �� SELECT     cn.NroContrato, cn.IdCliente, cl.RazSocial, cn.Fecha, cn.DireccionServicio, cn.IdTipoCobertura, tc.TipoCobertura, cn.IdCondicion, cn.IdEstado, cn.FechaBaja, cn.Obs, �_ �Y                       cn.IdVendedor, cn.dFecha, cn.hFecha, cn.nombre, vn.NombreVendedor, �E �?                       cd.Descripcion AS Condicion, cn.Importe, �8 �2                       Ciudad.Descripcion as Ciudad�& �  FROM   				  sas_Contrato AS cn �f �` 				INNER JOIN vt_clientes AS cl ON cn.IdEmpresa = cl.IdEmpresa AND cn.IdCliente = cl.IdCliente �w �q 				INNER JOIN sas_TipoCobertura AS tc ON cn.IdEmpresa = tc.IdEmpresa AND cn.IdTipoCobertura = tc.IdTipoCobertura�p �j 				LEFT OUTER JOIN vt_Condicion AS cd ON cn.IdEmpresa = cd.IdEmpresa AND cn.IdCondicion = cd.IdCondicion �q �k 				LEFT OUTER JOIN vvt_Vendedores AS vn ON cn.IdEmpresa = vn.IdEmpresa AND cn.IdVendedor = vn.IdVendedor  �� �~                 left join       bs_Ciudades AS Ciudad on cn.IDCiudad = Ciudad.IdCiudad        where cn.IdEmpresa=?oApp.Empresa�J �D and ((cn.Fecha between ?m.dFecha and ?m.hFecha) or cn.Fecha is null)�< �6 and (cn.Idciudad = ?m.Idciudad or ?m.Idciudad is null)�K �E and (cn.IDTipoCobertura=?m.TipoCobertura or ?m.TipoCobertura is null)� � and cn.IdEstado='A'�0 �* order by cn.IdTipoCobertura,cn.NroContrato� � ��C � �
 cContratos� �� F� � U  SETEO IDCIUDAD TIPOCOBERTURA CMDSQL SQL
 CCONTRATOS Init,     ��1 q � A � A � a�Q�aaqA����A �q 2                       �      )   �                  