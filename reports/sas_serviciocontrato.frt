  $                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Courier New      NroContrato      
IdSucursal      
m.Contador      1      0      Arial      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      ."Detalle de Servicios realizados por Contrato"      Arial      m.dfecha, ' al ' , m.hfecha      Arial      	"Periodo"      Arial      	"Fecha
"      Arial      "N� Solic.
"      Arial      "Paciente
"      Arial      
"Servicio"      Arial      "Destino Final"      Arial      "Formulario"      Arial      	"Importe"      Arial      "Contrato: ",NroContrato,Nombre      Arial      )"Sucursal: "+ IdSucursal + ' ' + Sucursal      Arial      FechaAgenda      "@D"      Arial      NroSolicitud      Arial      alltrim(NroSeguro)," ",Paciente      Arial      )alltrim(descripcion), ' Aut: ' + Autoriza      Arial      Destino      Arial      NroFS      Arial      precio * Cantidad      "999,999,999"      Arial      
m.Contador      	"999,999"      Arial      !isnull(Sucursal)      precio * Cantidad      "999,999,999,999"      Arial      !isnull(Sucursal)      "Total Suc."      Arial      !isnull(Sucursal)      
m.Contador      	"999,999"      Arial      !isnull(Sucursal)      precio* Cantidad      "999,999,999,999"      Arial      "Total"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      
m.Contador      	"999,999"      Arial      !isnull(Sucursal)      precio* Cantidad      "999,999,999,999"      Arial      "Total General"      Arial      dataenvironment      aTop = 120
Left = 315
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO SETEO

IF EMPTY(m.NroContrato)
	m.NroContrato= null
ENDIF

TEXT TO CMDSQL NOSHOW
SELECT     s.fechaAgenda, s.NroSolicitud, s.Paciente, c.NroContrato, c.nombre, s.Motivo, m.IdProducto, m.Cantidad, m.Precio, pr.Descripcion, s.NroSeguro, s.Autoriza, s.NroFS, 
                      dir.Descripcion as Sucursal,s.IdSucursal,s.Destino
FROM         sas_Servicios AS s INNER JOIN
                      sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN
                      st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN
                      st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto LEFT OUTER JOIN
                      sas_Direccion AS dir ON s.idEmpresa = dir.IdEmpresa AND s.IdSucursal = dir.IdDireccion
	WHERE  s.idempresa = ?oApp.Empresa 
			and (s.NroContrato = ?m.NroContrato or ?m.NroContrato is null)
			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha
ORDER BY c.Nombre, s.NroContrato, s.IdSucursal, s.fechaAgenda
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
     \���    C  C                           %   �      �     �          �  U  g �  � %�C�� ���% � T�� ���� �	 M(� ��� �� SELECT     s.fechaAgenda, s.NroSolicitud, s.Paciente, c.NroContrato, c.nombre, s.Motivo, m.IdProducto, m.Cantidad, m.Precio, pr.Descripcion, s.NroSeguro, s.Autoriza, s.NroFS, �N �H                       dir.Descripcion as Sucursal,s.IdSucursal,s.Destino�0 �* FROM         sas_Servicios AS s INNER JOIN�w �q                       sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN�\ �V                       st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN�| �v                       st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto LEFT OUTER JOIN�r �l                       sas_Direccion AS dir ON s.idEmpresa = dir.IdEmpresa AND s.IdSucursal = dir.IdDireccion�* �$ 	WHERE  s.idempresa = ?oApp.Empresa �G �A 			and (s.NroContrato = ?m.NroContrato or ?m.NroContrato is null)�f �` 			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha�C �= ORDER BY c.Nombre, s.NroContrato, s.IdSucursal, s.fechaAgenda� � ��C � � consulta� �� F� � U  SETEO NROCONTRATO CMDSQL SQL CONSULTA Init,     ��1 q � A � Q�q��!�qa1A �q 1                       �      )   C                  