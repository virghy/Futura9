  
r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Resumen de Caja"      Arial      oApp.Nombreempresa      Arial      "Fecha: ", m.Fecha      Arial      	"Importe"      Arial      "Descripcion"      Arial      	"Importe"      Arial      "Descripcion
"      Arial      Campo1      Arial      H1="S"      Campo1      Arial      H1=" "      Importe1      "@Z 999,999,999,999"      Arial      H1=" "      Importe1      "@Z 999,999,999,999"      Arial      H1="S"      Campo2     <VFPData>
	<reportdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="" execute="H2" execwhen="cCaja.H2=&quot;S&quot;" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="9" fstyle="1"/>
</VFPData>
      Arial      H2="S"      Campo2     <VFPData>
	<reportdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="" execute="H2" execwhen="cCaja.H2=&quot;S&quot;" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="9" fstyle="1"/>
</VFPData>
      Arial      H2=" "      Importe2      "@Z 999,999,999,999"      Arial      H2=" "      Importe2      "@Z 999,999,999,999"      Arial      H2="S"      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 81
Left = 180
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SELECT cCaja

*CREATE CURSOR cCaja (Campo1 V(200),Importe1 Y null, Tipo1 C(2), H1 C(1),Campo2 V(200),Importe2 Y null, Tipo2 C(2),H2 C(1))
ENDPROC
      ����    �   �                         ƀ   %   V       �      ~           �  U  
  �  � U  SETEO
  F�  � U  CCAJA BeforeOpenTables,     �� InitA     ��1 q 3 q 3                       &         A   �       )   �                   