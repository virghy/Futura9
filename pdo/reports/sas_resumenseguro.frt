   �   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Courier New                                                   Adultos                                                       Courier New                                                   Pediatricos                                                   Courier New                                                   Adultos + Pediatricos                                         Courier New                                                   Descripcion                                                   Courier New                                                   Adultos                                                       Courier New                                                   Pediatricos                                                   Courier New                                                   Adultos + Pediatricos                                         Courier New                                                   	"Adultos"                                                     Arial                                                         "Pediatrico"                                                  Arial                                                         	"Totales"                                                     Arial                                                         )"Resumen General de Servicios realizados"                     Arial                                                         "Razon Social"                                                Arial                                                         "Servicios"                                                   Arial                                                         razon                                                         Arial                                                         
datetime()                                                    Arial                                                         ""Fecha de Elaboracion del Informe"                            Arial                                                         "Mes"                                                         Arial                                                         mes                                                           Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                              
PROCEDURE Init
DO SETEO

TEXT TO CMDSQL NOSHOW
	SELECT	C.razonsocial as razon,
			B.descripcion,
			datename(mm,a.fechaagenda) as mes,
			SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS ADULTOS,
			SUM(CASE WHEN edad <= 15 THEN 1 ELSE 0 END) AS PEDIATRICOS

	FROM         sas_SolicitudServ AS A INNER JOIN
	             sas_Seguro AS C ON A.idSeguro = C.idSeguro INNER JOIN
	             sas_Tiposervicios AS B ON A.idTipoServicio = B.idtiposervicio AND A.IdEmpresa = B.idEmpresa
	WHERE	a.idempresa = ?oApp.empresa  and
			a.idseguro = ?m.seguro and
			YEAR(a.fechaagenda) = ?m.a�o and
			MONTH(a.fechaagenda) = ?m.mes                    
	GROUP BY C.razonsocial, B.descripcion, a.fechaagenda
		
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA

ENDPROC
                                                   ����    �  �                        ��   %   W      �     e          �  U  	 �  �	 M(� ��% � 	SELECT	C.razonsocial as razon,� � 			B.descripcion,�+ �% 			datename(mm,a.fechaagenda) as mes,�? �9 			SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS ADULTOS,�C �= 			SUM(CASE WHEN edad <= 15 THEN 1 ELSE 0 END) AS PEDIATRICOS� �  �5 �/ 	FROM         sas_SolicitudServ AS A INNER JOIN�I �C 	             sas_Seguro AS C ON A.idSeguro = C.idSeguro INNER JOIN�o �i 	             sas_Tiposervicios AS B ON A.idTipoServicio = B.idtiposervicio AND A.IdEmpresa = B.idEmpresa�- �' 	WHERE	a.idempresa = ?oApp.empresa  and�# � 			a.idseguro = ?m.seguro and�) �# 			YEAR(a.fechaagenda) = ?m.a�o and�: �4 			MONTH(a.fechaagenda) = ?m.mes                    �; �5 	GROUP BY C.razonsocial, B.descripcion, a.fechaagenda� � 		� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA Init,     ��1 q � Qq��1a Q���1���� A �q 2                       �      )   �                             %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Courier New                                                   Adultos                                                       Courier New                                                   Pediatricos                                                   Courier New                                                   Adultos + Pediatricos                                         Courier New                                                   Descripcion                                                   Courier New                                                   Adultos                                                       Courier New                                                   Pediatricos                                                   Courier New                                                   Adultos + Pediatricos                                         Courier New                                                   	"Adultos"                                                     Arial                                                         "Pediatrico"                                                  Arial                                                         	"Totales"                                                     Arial                                                         )"Resumen General de Servicios realizados"                     Arial                                                         "Razon Social"                                                Arial                                                         "Servicios"                                                   Arial                                                         razon                                                         Arial                                                         
datetime()                                                    Arial                                                         ""Fecha de Elaboracion del Informe"                            Arial                                                         "Mes"                                                         Arial                                                         mes                                                           Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                              
PROCEDURE Init
DO SETEO

TEXT TO CMDSQL NOSHOW
	SELECT	C.razonsocial as razon,
			B.descripcion,
			datename(mm,a.fechaagenda) as mes,
			SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS ADULTOS,
			SUM(CASE WHEN edad <= 15 THEN 1 ELSE 0 END) AS PEDIATRICOS

	FROM         sas_SolicitudServ AS A INNER JOIN
	             sas_Seguro AS C ON A.idSeguro = C.idSeguro INNER JOIN
	             sas_Tiposervicios AS B ON A.idTipoServicio = B.idtiposervicio AND A.IdEmpresa = B.idEmpresa
	WHERE	a.idempresa = ?oApp.empresa  and
			a.idseguro = ?m.seguro and
			YEAR(a.fechaagenda) = ?m.a�o and
			MONTH(a.fechaagenda) = ?m.mes                    
	GROUP BY C.razonsocial, B.descripcion, a.fechaagenda
		
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA

ENDPROC
                                                   ����    �  �                        ��   %   W      �     e          �  U  	 �  �	 M(� ��% � 	SELECT	C.razonsocial as razon,� � 			B.descripcion,�+ �% 			datename(mm,a.fechaagenda) as mes,�? �9 			SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS ADULTOS,�C �= 			SUM(CASE WHEN edad <= 15 THEN 1 ELSE 0 END) AS PEDIATRICOS� �  �5 �/ 	FROM         sas_SolicitudServ AS A INNER JOIN�I �C 	             sas_Seguro AS C ON A.idSeguro = C.idSeguro INNER JOIN�o �i 	             sas_Tiposervicios AS B ON A.idTipoServicio = B.idtiposervicio AND A.IdEmpresa = B.idEmpresa�- �' 	WHERE	a.idempresa = ?oApp.empresa  and�# � 			a.idseguro = ?m.seguro and�) �# 			YEAR(a.fechaagenda) = ?m.a�o and�: �4 			MONTH(a.fechaagenda) = ?m.mes                    �; �5 	GROUP BY C.razonsocial, B.descripcion, a.fechaagenda� � 		� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA Init,     ��1 q � Qq��1a Q���1���� A �q 2                       �      )   �                       