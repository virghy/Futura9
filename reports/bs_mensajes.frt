  >                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      	"MENSAJE"      Arial      oApp.Nombreempresa      Arial      Al      Arial      "Al Se�or:"      Arial      Fecha      Arial      "Fecha"      Arial      "Llamo"      Arial      "X"      Arial      llamovino=1      "Vino"      Arial      "X"      Arial      llamovino=2      De      Arial      "Sr."      Arial      Empresa      Arial      telefono      Arial      "De"      Arial      "Telef."      Arial      "Desea Verlo"      Arial      "X"      Arial      
DeseaVerlo      	"Volvera"      Arial      "X"      Arial      Volvera      "Que UD. lo llame"      Arial      "X"      Arial      
QueLoLlame      "Es importante"      Arial      "X"      Arial      
Importante      "Llamara de nuevo"      Arial      "X"      Arial      llamara      "Es urgente"      Arial      "X"      Arial      Urgente      	"Mensaje"      Arial      Mensaje      Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     *PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
DO seteo
TEXT TO cmdSQL NOSHOW 
SELECT     bs_Mensajes.IdMsg, bs_Mensajes.Fecha, bs_Mensajes.De, bs_Mensajes.A, bs_Mensajes.Asunto, bs_Mensajes.Mensaje, bs_Mensajes.Audit_Usuario, 
                      bs_Mensajes.Audit_Fecha, bs_Mensajes.Hora, bs_Mensajes.LlamoVino, bs_Mensajes.Empresa, bs_Mensajes.Telefono, bs_Mensajes.DeseaVerlo, 
                      bs_Mensajes.QueloLlame, bs_Mensajes.Llamara, bs_Mensajes.Volvera, bs_Mensajes.Importante, bs_Mensajes.Urgente, RTRIM(al.first_name) 
                      + ' ' + al.last_name AS Al, RTRIM(Recept.first_name) + ' ' + Recept.last_name AS Recepcionado
FROM         bs_Mensajes LEFT OUTER JOIN
                      usuarios AS Recept ON bs_Mensajes.Audit_Usuario COLLATE SQL_Latin1_General_CP1_CI_AS = Recept.employee_id LEFT OUTER JOIN
                      usuarios AS al ON bs_Mensajes.A COLLATE SQL_Latin1_General_CP1_CI_AS = al.employee_id
where IdMsg=?m.IdMsg
ENDTEXT
sql(cmdSQL,'cMensajes')
SELECT cMensajes




ENDPROC
     ����    �  �                        SQ   %   >      �     f          �  U  
  �  � U  SETEO� �  �	 M(� ��� �� SELECT     bs_Mensajes.IdMsg, bs_Mensajes.Fecha, bs_Mensajes.De, bs_Mensajes.A, bs_Mensajes.Asunto, bs_Mensajes.Mensaje, bs_Mensajes.Audit_Usuario, �� ��                       bs_Mensajes.Audit_Fecha, bs_Mensajes.Hora, bs_Mensajes.LlamoVino, bs_Mensajes.Empresa, bs_Mensajes.Telefono, bs_Mensajes.DeseaVerlo, �� ��                       bs_Mensajes.QueloLlame, bs_Mensajes.Llamara, bs_Mensajes.Volvera, bs_Mensajes.Importante, bs_Mensajes.Urgente, RTRIM(al.first_name) �y �s                       + ' ' + al.last_name AS Al, RTRIM(Recept.first_name) + ' ' + Recept.last_name AS Recepcionado�. �( FROM         bs_Mensajes LEFT OUTER JOIN�� ��                       usuarios AS Recept ON bs_Mensajes.Audit_Usuario COLLATE SQL_Latin1_General_CP1_CI_AS = Recept.employee_id LEFT OUTER JOIN�q �k                       usuarios AS al ON bs_Mensajes.A COLLATE SQL_Latin1_General_CP1_CI_AS = al.employee_id� � where IdMsg=?m.IdMsg� � ��C � �	 cMensajes� �� F� � U  SETEO CMDSQL SQL	 CMENSAJES BeforeOpenTables,     �� InitA     ��1 q 3 q � �	

��Q	�A �q 5                       &         A         )   �                  