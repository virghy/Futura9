  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Fecha      "@D"      Arial      Fecha      "@D"      Arial      "Fecha:"      Arial      "Fecha:"      Arial      IdCliente,' ',RazSocial      Arial      IdCliente,' ',RazSocial      Arial      "Paciente:"      Arial      "Paciente:"      Arial      Receta      Arial      Indicaciones      Arial      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
TEXT TO cmdSQL NOSHOW 
SELECT     c.RazSocial, sas_Sesiones.Fecha, sas_Sesiones.IdCliente, sas_Sesiones.Receta, sas_Sesiones.Indicaciones
FROM         vt_clientes AS c INNER JOIN
                      sas_Sesiones ON c.IdEmpresa = sas_Sesiones.IdEmpresa AND c.IdCliente = sas_Sesiones.IdCliente
                      where IdSesion = ?m.IdSesion
ENDTEXT
=SQL(cmdSQL,'Eventos')
SELECT Eventos
ENDPROC
     ����    z  z                        �   %   �      !     	          �  U  
  �  � U  SETEO�	 M(�  ��x �r SELECT     c.RazSocial, sas_Sesiones.Fecha, sas_Sesiones.IdCliente, sas_Sesiones.Receta, sas_Sesiones.Indicaciones�. �( FROM         vt_clientes AS c INNER JOIN�y �s                       sas_Sesiones ON c.IdEmpresa = sas_Sesiones.IdEmpresa AND c.IdCliente = sas_Sesiones.IdCliente�8 �2                       where IdSesion = ?m.IdSesion� � ��C �  � Eventos� �� F� � U  CMDSQL SQL EVENTOS BeforeOpenTables,     �� InitA     ��1 q 3 � ����A �q 1                       &         A   �      )   z                  