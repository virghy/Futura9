  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Courier New      idtiposervicio      Arial      Arial      Arial      Arial      Arial      Courier New      Arial      "Planilla Diaria de Servicios"      Arial      alltrim( empresa )             Arial      m.dfechahora,'al', m.hfechahora      Arial      
"Periodo:"      Arial      $iif(isnull(m.seguro),"Todos",seguro)      Arial      "Seguro"      Arial      "Factura o F.S."      Arial      	"Cobrado"      Arial      
"A Cobrar"      Arial      "Obs"      Arial      "Zona"      Arial      "Inicio"      Arial      	"T�rmino"      Arial      "Nro.Salida"      Arial      "Nro.Socio"      Arial      "Nombre del Paciente"      Arial      "Direcci�n"      Arial      "Tipo Servicio: ",TipoServicio      Arial      FACTURA      Arial      Cobrado      "9,999,999"      Arial      nvl(importe,0)-nvl(cobrado,0)      "9,999,999"      Arial      Obs      Arial      IdZona      Arial      inicio      Arial      termino      Arial      	nrosalida      Arial      alltrim(seguro), ' ', nrosocio      Arial      alltrim(paciente),"("+edad+")"      Arial      	direccion      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      Cobrado      "9,999,999"      Arial      !"Fecha: ________________________"      Arial      %"Cobrado: ______________________ Gs."      Arial      4"Recaudacion retirado por: ________________________"      Arial      nvl(importe,0)-nvl(cobrado,0)      Arial      &"A Cobrar: ______________________ Gs."      Arial      nvl(importe,0)      Arial      #"Total: ______________________ Gs."      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO SETEO
IF empty(m.seguro)
	m.seguro = null
ENDIF
	
IF empty(m.tiposervicio)
	m.tiposervicio = null
ENDIF


TEXT TO cmdsql NOSHOW
SELECT     S.fechaAgenda AS FECHA, s.horaSalida AS INICIO, s.horaLlegada AS TERMINO, s.nroSalida AS NROSALIDA, S.NroSeguro AS NROSOCIO, 
                      S.Paciente AS PACIENTE, ISNULL(RTRIM(S.direccionRef),'') + '  ' + ISNULL(RTRIM(S.nrocasa),'') + ISNULL('  Y  ' + RTRIM(S.direccionRef2),'') AS direccion, 
                      ISNULL(s.NroFactura,'') + ' ' + RTRIM(ISNULL(s.NroFS,'')) AS FACTURA, s.Importe AS IMPORTE, s.cobrado,S.IdZona, Z.descripcion AS ZONA, S.idTipoServicio, 
                      TS.descripcion AS TipoServicio, SE.razonsocial AS Seguro,s.Observacion as Obs,s.edad
FROM         sas_Servicios AS S LEFT OUTER JOIN
                      sas_Zona AS Z ON S.IdEmpresa = Z.IdEmpresa AND S.idZona = Z.idZona LEFT OUTER JOIN
                      sas_Tiposervicios AS TS ON S.IdEmpresa = TS.idEmpresa AND S.idTipoServicio = TS.idtiposervicio LEFT OUTER JOIN
                      sas_Seguro AS SE ON S.idSeguro = SE.idSeguro
WHERE	S.idempresa = ?oApp.empresa  and
		S.fechaAgenda between ?m.dFechaHora and ?m.hFechaHora and 
		(S.IdSeguro = ?m.Seguro or ?m.Seguro is null) and 
		(S.idtiposervicio = ?m.tiposervicio or ?m.tiposervicio is null)
ORDER BY S.idTipoServicio, FECHA, INICIO		                    	
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA

ENDPROC
     ����    |  |                           %   �      3     �          �  U  � �  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� �	 M(� ��� �� SELECT     S.fechaAgenda AS FECHA, s.horaSalida AS INICIO, s.horaLlegada AS TERMINO, s.nroSalida AS NROSALIDA, S.NroSeguro AS NROSOCIO, �� ��                       S.Paciente AS PACIENTE, ISNULL(RTRIM(S.direccionRef),'') + '  ' + ISNULL(RTRIM(S.nrocasa),'') + ISNULL('  Y  ' + RTRIM(S.direccionRef2),'') AS direccion, �� ��                       ISNULL(s.NroFactura,'') + ' ' + RTRIM(ISNULL(s.NroFS,'')) AS FACTURA, s.Importe AS IMPORTE, s.cobrado,S.IdZona, Z.descripcion AS ZONA, S.idTipoServicio, �p �j                       TS.descripcion AS TipoServicio, SE.razonsocial AS Seguro,s.Observacion as Obs,s.edad�5 �/ FROM         sas_Servicios AS S LEFT OUTER JOIN�n �h                       sas_Zona AS Z ON S.IdEmpresa = Z.IdEmpresa AND S.idZona = Z.idZona LEFT OUTER JOIN�� ��                       sas_Tiposervicios AS TS ON S.IdEmpresa = TS.idEmpresa AND S.idTipoServicio = TS.idtiposervicio LEFT OUTER JOIN�H �B                       sas_Seguro AS SE ON S.idSeguro = SE.idSeguro�, �& WHERE	S.idempresa = ?oApp.empresa  and�B �< 		S.fechaAgenda between ?m.dFechaHora and ?m.hFechaHora and �: �4 		(S.IdSeguro = ?m.Seguro or ?m.Seguro is null) and �G �A 		(S.idtiposervicio = ?m.tiposervicio or ?m.tiposervicio is null)�E �? ORDER BY S.idTipoServicio, FECHA, INICIO		                    	� � ��C � � consulta� �� F� � U  SETEO SEGURO TIPOSERVICIO CMDSQL SQL CONSULTA Init,     ��1 q � A � A � �aQQ����!�qQA �q 2                       �      )   |                  