   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura1\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura1\HP DeskJet 840C/841C   � XC� 	 �
od   ,  ,  Letter                                                                          DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura1\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=9
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                 idpedido                       "Pedidos Anulados"                                            Arial                          Arial                          "Pedido"                       cliente                                                       Arial                          Arial                          ""                            "@I"                           Arial                          "Receptor"                    Arial                          	"Cliente"                      alltrim( empresa )                                            Arial                          
datetime()                                                    Arial                          "P�g. " + str( _pageno,3 )                                                                     Arial                          Arial                          
"Periodo:"                     m.dfecha, ' al ' ,m.hfecha                                                                     Arial                          Arial                          "Repartidor"                  "@I"                           Arial                          
" Estado"                     	nropedido                                                     Arial                          "9,999,999"                    'iif(idestado='A','Anulado','Entregado')                                                        Arial                          receptor                                                      Arial                          
repartidor                                                    Arial                          descripcion                                                   Arial                          cantidad                                                      Arial                          	"999,999"                      "@I"                           Arial                          "Cantidad"                    "@I"                           Arial                          "Descripci�n"                 precio                                                        Arial                          	"999,999"                      "@I"                           Arial                          	"Precio"                      "@I"                           Arial                          "Total"                       importe                                                       Arial                          "99,999,999"                   importe                                                       Arial                          "999,999,999"                  Arial                          	"Total:"                      cantidad*precio                                               Arial                          "99,999,999"                   Arial                          Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                KLeft = 177
Top = 32
Width = 381
Height = 355
Name = "Dataenvironment"
                     PROCEDURE Init
Public m.Total
	
sql('exec vt_pedido_anul ?oapp.empresa,?m.dFecha,?m.hFecha','rpedido_anul')
SELECT rpedido_anul

*Sum Importe To m.Total

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Destroy
Release m.total
ENDPROC
                               ����    �  �                        �   %   �       %  	             �  U  d 	 7��  �Q ��C�5 exec vt_pedido_anul ?oapp.empresa,?m.dFecha,?m.hFecha� rpedido_anul� �� F� � U  TOTAL SQL RPEDIDO_ANUL
  �  � U  SETEO 	 <��  � U  TOTAL Init,     �� BeforeOpenTables�     �� Destroy�     ��1 � q 5 q 2 � 1                       �         �   �   
      �   �       )   �                  ft join vt_negocio c on a.idneg