  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial       "Estados de Cheques Depositados"             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Fecha"      Arial      	"Importe"      Arial      "Descripci�n"      Arial      "Estado"      Arial      
"Cuenta
"      Arial      nombre             Arial      fecha             Arial      rcheques.importe      "@Z 999,999,999,999"             Arial      descripcion             Arial      rcheques.estado_cheque             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 81
Left = 180
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE Init
sql("exec ts_cambioestado ?dFecha, ?hFecha","rCheques")
SELECT rcheques

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     A���    (  (                        i�   %   �       �      �           �  U  G = ��C�% exec ts_cambioestado ?dFecha, ?hFecha� rCheques�  �� F� � U  SQL RCHEQUES
  �  � U  SETEO Init,     �� BeforeOpenTables�     ��1 �q 3 q 2                       Z         �   �       )   (                  