                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Arial      fecha      menu      Arial      Arial      Arial      Arial      Arial      empresa             Arial      "Lista de Menus"             Arial      	"Fecha
"      Arial      "Opcion"      Arial      "Precio"      Arial      "Descripci�n"      Arial      "Plato"      Arial      fecha             Arial      menu             Arial      precio      "@ 99,999,999"             Arial      menudesc             Arial      plato             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      oLeft = 202
Top = 250
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
sql("exec prd_listamenu ?oapp.empresa,?dFecha, ?hFecha","rlistamenu")
SELECT rlistamenu

ENDPROC
     Q���    8  8                        8�   %   �       �      �           �  U  
  �  � U  SETEOU K ��C�1 exec prd_listamenu ?oapp.empresa,?dFecha, ?hFecha�
 rlistamenu�  �� F� � U  SQL
 RLISTAMENU BeforeOpenTables,     �� InitA     ��1 q 3 �q 2                       &         A   �       )   8                  