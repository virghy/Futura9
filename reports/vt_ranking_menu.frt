  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Arial      fechapedido      menu      acum      cantidad      0      Arial      Arial      Arial      Arial      Arial      Arial      "Cantidad de Men�s por Plato "             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "
"      Arial      "Fecha"      Arial      "Men�
"      "@I"      Arial      "Cantidad Platos
"      Arial      "Cantidad Men�s
"      Arial      fechapedido             Arial      menu             Arial      cantidad      "9,999,999"             Arial      plato             Arial      cantidad      "9,999,999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      KLeft = 208
Top = 75
Width = 381
Height = 355
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE Init
PUBLIC total

sql('exec vt_rankingmenu ?oapp.empresa,?m.dFecha,?m.hFecha','rrankingmenu')
SELECT rrankingmenu
sum cantidad To total

ENDPROC
     ����    �  �                        �r   %   �       :  
   &          �  U  
  �  � U  SETEO
  <�  � U  TOTALo  7�  �Q ��C�5 exec vt_rankingmenu ?oapp.empresa,?m.dFecha,?m.hFecha� rrankingmenu� �� F� � K(�  �� �� U  TOTAL SQL RRANKINGMENU CANTIDAD BeforeOpenTables,     �� DestroyA     �� InitV     ��1 q 3 q 3 q q � 2                       &         D   S         n   �   	    )   �                  