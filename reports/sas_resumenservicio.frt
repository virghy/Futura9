  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Courier New      razonsocial      Arial      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      )"Resumen Mensual de Servicios realizados"      Arial      m.dfecha, ' al ' , m.hfecha      Arial      	"Periodo"      Arial      	"Fecha
"      Arial      "N� Solic.
"      Arial      "Paciente
"      Arial      "Contrato
"      Arial      
"Servicio"      Arial      "Descripcion"      Arial      "Formulario"      Arial      	"Importe"      Arial      "Seguro: ",razonsocial      Arial      FechaAgenda      Arial      NroSolicitud      Arial      alltrim(NroSeguro)," ",paciente      Arial      Titular      Arial      descripcion      Arial      Motivo      Arial      NroFS      Arial      importe      "999,999,999"      Arial      importe      "999,999,999,999"      Arial      "Total"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      importe      "999,999,999,999"      Arial      "Total General"      Arial      dataenvironment      aTop = 120
Left = 315
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     RPROCEDURE Init
DO SETEO

IF EMPTY(m.seguro)
	m.seguro = null
ENDIF

TEXT TO CMDSQL NOSHOW
SELECT     sg.idSeguro, sg.razonsocial, s.idPaciente, s.Paciente, s.idTipoServicio, ts.descripcion, s.fechaAgenda, s.Importe, s.NroSeguro, d.NroFS, 
s.Comentario, s.Motivo,RTRIM(p.Nombre)+' ' +p.Apellido as Titular,s.NroSolicitud 
FROM sas_servicios AS s INNER JOIN sas_Contratos  
                      sas_Tiposervicios AS ts ON s.idTipoServicio = ts.idtiposervicio INNER JOIN
                      sas_DespachoServ AS d ON s.idSolicitud = d.idSolicitud
	WHERE  s.idempresa = ?oApp.Empresa 
			and (s.idseguro = ?m.seguro or ?m.Seguro is null)
			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha
ORDER BY sg.razonsocial, s.fechaAgenda
ENDTEXT
sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
     ���                              2�   %   �      �     �          �  U  0 �  � %�C�� ���% � T�� ���� �	 M(� ��� �� SELECT     sg.idSeguro, sg.razonsocial, s.idPaciente, s.Paciente, s.idTipoServicio, ts.descripcion, s.fechaAgenda, s.Importe, s.NroSeguro, d.NroFS, �W �Q s.Comentario, s.Motivo,RTRIM(p.Nombre)+' ' +p.Apellido as Titular,s.NroSolicitud �8 �2 FROM sas_servicios AS s INNER JOIN sas_Contratos  �f �`                       sas_Tiposervicios AS ts ON s.idTipoServicio = ts.idtiposervicio INNER JOIN�R �L                       sas_DespachoServ AS d ON s.idSolicitud = d.idSolicitud�* �$ 	WHERE  s.idempresa = ?oApp.Empresa �: �4 			and (s.idseguro = ?m.seguro or ?m.Seguro is null)�f �` 			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha�, �& ORDER BY sg.razonsocial, s.fechaAgenda� � ��C � � consulta� �� F� � U  SETEO SEGURO CMDSQL SQL CONSULTA Init,     ��1 q � A � �	q�a!��a�A �q 1                       G      )                     