  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Cumplea�os del Mes"      Arial      oApp.Nombreempresa      Arial      NombreMes(m.mes)      Arial      
"Per�odo:"      Arial      "Fecha Nac."      Arial      "Telefonos"      Arial      
"Nombre
"      Arial      Nombre,' ', Apellido      Arial      FechaCumple      Arial      Area,Nro, ' / ',Celular      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
DO seteo
TEXT TO cmdSQL NOSHOW 
	SELECT * FROM bs_guia
	where MONTH(FechaCumple)=?m.mes
	order by FechaCumple
ENDTEXT
sql(cmdSQL,'cFechas')
SELECT cFechas




ENDPROC
     ����    �  �                        �   %   �       5               �  U  
  �  � U  SETEO�  �  �	 M(� �� � 	SELECT * FROM bs_guia�& �  	where MONTH(FechaCumple)=?m.mes� � 	order by FechaCumple� � ��C � � cFechas� �� F� � U  SETEO CMDSQL SQL CFECHAS BeforeOpenTables,     �� InitA     ��1 q 3 q � �a�A �q 5                       &         A   �       )   �                  