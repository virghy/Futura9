   �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=9
COLOR=2
river
OUTPUT=192.168.0.18
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
DUPLEX=1
YRESOLUTION=600
COLLATE=0
                                                           D  &  winspool  RICOH Class Driver  192.168.0.18                                                                            Courier New                                                   
IdProducto                                                    alltrim( empresa )                                                                                                          Arial                                                         "Resumen de Servicios"                                        ""                                                            Arial                                                         #m.dfechahora, ' al ' , m.hfechahora                           Arial                                                         	"Periodo"                                                     Arial                                                         "Servicio
"                                                  Arial                                                         "Cantidad
"                                                  Arial                                                         descripcion                                                   Arial                                                         NroSolicitud                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
datetime()                                                                                                                  Arial                                                         NroSolicitud                                                  Arial                                                         "Total General
"                                             Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               Top = 120
Left = 315
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
                                                              aPROCEDURE Init
DO SETEO

TEXT TO CMDSQL NOSHOW
	SELECT        s.Paciente, p.Descripcion, s.idMovil, s.idUsuario, u.first_name, u.last_name, s.NroSolicitud, s.fechaAgenda, s.horaSalida, 
	s.HoraDespacho,s.horaLlegada,
--	dbo.sas_HoraDiff(convert(char(8),s.FechaAgenda,108),s.horaLlegada) TiempoLLegada,
--	p.sas_TiempoArrivo sas_TiempoArribo,
	s.NroContrato,
	s.IdProducto
	FROM            sas_Servicios AS s left JOIN
	                         st_Producto AS p ON s.idEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto INNER JOIN
	                         usuarios AS u ON s.idUsuario = LEFT(u.employee_id, 4)
	where s.IdEmpresa=?oApp.Empresa
	and s.FechaAgenda between ?m.dfechahora and ?m.hFechahora                         
	order by s.IdProducto, s.fechaAgenda, NroSolicitud
ENDTEXT
	
	
sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
                            6���                              �   %   �      �     �          �  U  R �  �	 M(� ��� �� 	SELECT        s.Paciente, p.Descripcion, s.idMovil, s.idUsuario, u.first_name, u.last_name, s.NroSolicitud, s.fechaAgenda, s.horaSalida, �$ � 	s.HoraDespacho,s.horaLlegada,�Z �T --	dbo.sas_HoraDiff(convert(char(8),s.FechaAgenda,108),s.horaLlegada) TiempoLLegada,�- �' --	p.sas_TiempoArrivo sas_TiempoArribo,� � 	s.NroContrato,� � 	s.IdProducto�3 �- 	FROM            sas_Servicios AS s left JOIN�x �r 	                         st_Producto AS p ON s.idEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto INNER JOIN�U �O 	                         usuarios AS u ON s.idUsuario = LEFT(u.employee_id, 4)�& �  	where s.IdEmpresa=?oApp.Empresa�Y �S 	and s.FechaAgenda between ?m.dfechahora and ?m.hFechahora                         �9 �3 	order by s.IdProducto, s.fechaAgenda, NroSolicitud� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA Init,     ��1 q � 	A��Q11�Qa��A �q 1                       V      )                             �DRIVER=winspool
DEVICE=RICOH Class Driver
OUTPUT=192.168.0.18
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
DUPLEX=1
YRESOLUTION=600
COLLATE=0
                                                           D  &  winspool  RICOH Class Driver  192.168.0.18                                                                            Courier New                                                   
IdProducto                                                    alltrim( empresa )                                                                                                          Arial                                                         "Resumen de Servicios"                                        ""                                                            Arial                                                         #m.dfechahora, ' al ' , m.hfechahora                           Arial                                                         	"Periodo"                                                     Arial                                                         "Servicio
"                                                  Arial                                                         "Cantidad
"                                                  Arial                                                         descripcion                                                   Arial                                                         NroSolicitud                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
datetime()                                                                                                                  Arial                                                         NroSolicitud                                                  Arial                                                         "Total General
"                                             Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               Top = 120
Left = 315
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
                                                              ]PROCEDURE Init
DO SETEO

TEXT TO CMDSQL NOSHOW
	SELECT        s.Paciente, p.Descripcion, s.idMovil, s.idUsuario, u.first_name, u.last_name, s.NroSolicitud, s.fechaAgenda, s.horaSalida, 
	s.HoraDespacho,s.horaLlegada,
	dbo.sas_HoraDiff(convert(char(8),s.FechaAgenda,108),s.horaLlegada) TiempoLLegada,
	p.sas_TiempoArrivo sas_TiempoArribo,
	s.NroContrato,
	s.IdProducto
	FROM            sas_Servicios AS s left JOIN
	                         st_Producto AS p ON s.idEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto INNER JOIN
	                         usuarios AS u ON s.idUsuario = LEFT(u.employee_id, 4)
	where s.IdEmpresa=?oApp.Empresa
	and s.FechaAgenda between ?m.dfechahora and ?m.hFechahora                         
	order by s.IdProducto, s.fechaAgenda, NroSolicitud
ENDTEXT
	
	
sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
                                2���                              �I   %   �      �     �          �  U  N �  �	 M(� ��� �� 	SELECT        s.Paciente, p.Descripcion, s.idMovil, s.idUsuario, u.first_name, u.last_name, s.NroSolicitud, s.fechaAgenda, s.horaSalida, �$ � 	s.HoraDespacho,s.horaLlegada,�X �R 	dbo.sas_HoraDiff(convert(char(8),s.FechaAgenda,108),s.horaLlegada) TiempoLLegada,�+ �% 	p.sas_TiempoArrivo sas_TiempoArribo,� � 	s.NroContrato,� � 	s.IdProducto�3 �- 	FROM            sas_Servicios AS s left JOIN�x �r 	                         st_Producto AS p ON s.idEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto INNER JOIN�U �O 	                         usuarios AS u ON s.idUsuario = LEFT(u.employee_id, 4)�& �  	where s.IdEmpresa=?oApp.Empresa�Y �S 	and s.FechaAgenda between ?m.dfechahora and ?m.hFechahora                         �9 �3 	order by s.IdProducto, s.fechaAgenda, NroSolicitud� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA Init,     ��1 q � 	A��Q11�Qa��A �q 1                       R      )                           