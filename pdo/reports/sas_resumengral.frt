   P   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Courier New                                                   Razon                                                         Courier New                                                   Adultos                                                       Courier New                                                   Courier New                                                   dataenvironment                                               _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                              �PROCEDURE BeforeOpenTables


ENDPROC
PROCEDURE Init
DO SETEO

TEXT TO cmdsql NOSHOW
SELECT  c.razonsocial as Razon,
		datename(mm,a.fechaagenda) as Mes,
		SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS Adultos 

FROM    sas_SolicitudServ AS a INNER JOIN
        sas_Seguro AS c ON a.idSeguro = c.idSeguro INNER JOIN
	    sas_Tiposervicios AS b ON a.idTipoServicio = b.idtiposervicio AND a.IdEmpresa = b.idEmpresa

WHERE edad > 15 and
	a.idempresa = ?oApp.empresa  and 
	YEAR(a.fechaagenda) = ?m.a�o and
	MONTH(a.fechaagenda) = ?m.mes                    
	
	GROUP BY C.razonsocial, datename(mm,a.fechaagenda)
	
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
      ����    �  �                        ��   %   �      :               �  U    U  � �  �	 M(� ��% � SELECT  c.razonsocial as Razon,�* �$ 		datename(mm,a.fechaagenda) as Mes,�> �8 		SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS Adultos � �  �/ �) FROM    sas_SolicitudServ AS a INNER JOIN�C �=         sas_Seguro AS c ON a.idSeguro = c.idSeguro INNER JOIN�f �` 	    sas_Tiposervicios AS b ON a.idTipoServicio = b.idtiposervicio AND a.IdEmpresa = b.idEmpresa� �  � � WHERE edad > 15 and�( �" 	a.idempresa = ?oApp.empresa  and �' �! 	YEAR(a.fechaagenda) = ?m.a�o and�8 �2 	MONTH(a.fechaagenda) = ?m.mes                    � � 	�9 �3 	GROUP BY C.razonsocial, datename(mm,a.fechaagenda)� � 	� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA BeforeOpenTables,     �� Init3     ��1 4 q � Q��a �1aa ��q�q �q A �q 1                                9   �      )   �                                    %ORIENTATION=0
PAPERSIZE=9
COLOR=2
                         Courier New                                                   Razon                                                         Courier New                                                   Adultos                                                       Courier New                                                   Courier New                                                   dataenvironment                                               _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                              �PROCEDURE Init
DO SETEO

TEXT TO cmdsql NOSHOW
SELECT  c.razonsocial as Razon,
		datename(mm,a.fechaagenda) as Mes,
		SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS Adultos 

FROM    sas_SolicitudServ AS a INNER JOIN
        sas_Seguro AS c ON a.idSeguro = c.idSeguro INNER JOIN
	    sas_Tiposervicios AS b ON a.idTipoServicio = b.idtiposervicio AND a.IdEmpresa = b.idEmpresa

WHERE edad > 15 and
	a.idempresa = ?oApp.empresa  and 
	YEAR(a.fechaagenda) = ?m.a�o and
	MONTH(a.fechaagenda) = ?m.mes                    
	
	GROUP BY C.razonsocial, datename(mm,a.fechaagenda)
	
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
PROCEDURE BeforeOpenTables


ENDPROC
      ����    �  �                        ��   %   �      :               �  U  � �  �	 M(� ��% � SELECT  c.razonsocial as Razon,�* �$ 		datename(mm,a.fechaagenda) as Mes,�> �8 		SUM(CASE WHEN edad > 15 THEN 1 ELSE 0 END) AS Adultos � �  �/ �) FROM    sas_SolicitudServ AS a INNER JOIN�C �=         sas_Seguro AS c ON a.idSeguro = c.idSeguro INNER JOIN�f �` 	    sas_Tiposervicios AS b ON a.idTipoServicio = b.idtiposervicio AND a.IdEmpresa = b.idEmpresa� �  � � WHERE edad > 15 and�( �" 	a.idempresa = ?oApp.empresa  and �' �! 	YEAR(a.fechaagenda) = ?m.a�o and�8 �2 	MONTH(a.fechaagenda) = ?m.mes                    � � 	�9 �3 	GROUP BY C.razonsocial, datename(mm,a.fechaagenda)� � 	� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA  U   Init,     �� BeforeOpenTables�    ��1 q � Q��a �1aa ��q�q �q A �q 2 3                       �        �  �      )   �                              