     @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Arial                                                         Importe                                                       "999,999,999,999.99"                                          Arial                                                         "Estado de Resultados"                                        Arial                                                         empresa                                                                                                                     Arial                                                         "Concepto"                                                   Arial                                                         
"Per�odo:"                                                    Arial                                                         m.dfecha, "  al " ,m.hfecha                                   Arial                                                         Concepto                                                     �<VFPData>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="" execute="len(rtrim(orden))&lt;=2" execwhen="len(rtrim(orden))&lt;=2" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="9" fstyle="1"/>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="space(5) + Concepto" execute="len(rtrim(orden))&lt;=4" execwhen="len(rtrim(orden))&lt;=4" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="8" fstyle="1"/>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="space(10) + Concepto" execute="len(rtrim(orden))&lt;=6" execwhen="len(rtrim(orden))&lt;=6" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="8" fstyle="0"/>
</VFPData>
                                      Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         	"Importe"                                                     Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 95
Left = 13
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
DataSource = .NULL.
Name = "Dataenvironment"
                                                 kPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init

CREATE CURSOR Saldos(IdConcepto c(10), Concepto c(50), IdPadre c(10), Importe Y, Orden C(20))

SET DATABASE TO datos
TEXT TO cmdSQL noshow
	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, d.IdConcepto, Orden=ISNULL(Orden,''), Importe=SUM(d.Importe) 
	FROM         ts_DepositoConceptoDet AS d INNER JOIN
	                      cn_conceptos AS c ON d.IdEmpresa = c.idempresa AND d.IdConcepto = c.idconcepto
	where d.IdEmpresa = ?oApp.Empresa
	and d.Fecha between ?m.dFecha and ?m.hFecha 
	group by c.concepto, c.Tipo, ISNULL(c.IdPadre,''), d.IdConcepto,Orden

ENDTEXT

=sql(cmdSQL,'xSaldo')

SELECT xsaldo
TEXT TO cmdSQL noshow
	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, c.IdConcepto,Orden=ISNULL(Orden,'')
	FROM   cn_conceptos AS c 
	       where c.IdEmpresa = ?oApp.Empresa
ENDTEXT

=sql(cmdSQL,'Cuentas')




GOTO TOP


DO WHILE  .NOT. EOF()
	SELECT SALDOS
	APPEND BLANK
	REPLACE IdConcepto WITH xsaldo.IdConcepto 
	REPLACE Importe WITH xsaldo.Importe
	REPLACE Orden WITH xsaldo.Orden 

	REPLACE Concepto WITH xsaldo.Concepto 
	XINTEGRADORA = xsaldo.IdPadre 
	
	DO WHILE .T. .AND.  .NOT. EMPTY(XINTEGRADORA)
		SELECT CUENTAS
		LOCATE FOR XINTEGRADORA=CUENTAS.Idconcepto
		IF FOUND()
			SELECT SALDOS
			GOTO TOP
*			SEEK XPROCESO+CUENTAS.CUENTA
			LOCATE FOR Saldos.IdConcepto=CUENTAS.Idconcepto
			IF  .NOT. FOUND()
				APPEND BLANK
				REPLACE IdConcepto WITH CUENTAS.IdConcepto 
				REPLACE Concepto WITH CUENTAS.Concepto
				REPLACE Orden WITH Cuentas.Orden	 
			ENDIF
			
 
			REPLACE Importe WITH Importe + xsaldo.Importe
			XINTEGRADORA = cuentas.IdPadre 
			
		ELSE
			EXIT
		ENDIF
		
		IF EMPTY(XINTEGRADORA)
			EXIT
		ENDIF
		
	ENDDO
	
	SELECT xsaldo
	SKIP
ENDDO
SELECT SALDOS
INDEX on Orden TAG Orden


ENDPROC
                  ~���    e  e                        |�   %   n        ;   �          �  U  
  �  � U  SETEO�S h�� Saldos� � C��
�� � C��2�� � C��
�� � Y� � C���� G(� datos�	 M(� ��� �{ 	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, d.IdConcepto, Orden=ISNULL(Orden,''), Importe=SUM(d.Importe) �: �4 	FROM         ts_DepositoConceptoDet AS d INNER JOIN�k �e 	                      cn_conceptos AS c ON d.IdEmpresa = c.idempresa AND d.IdConcepto = c.idconcepto�( �" 	where d.IdEmpresa = ?oApp.Empresa�3 �- 	and d.Fecha between ?m.dFecha and ?m.hFecha �L �F 	group by c.concepto, c.Tipo, ISNULL(c.IdPadre,''), d.IdConcepto,Orden� �  � � ��C � � xSaldo� �� F�	 �	 M(� ��g �a 	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, c.IdConcepto,Orden=ISNULL(Orden,'')�  � 	FROM   cn_conceptos AS c �/ �) 	       where c.IdEmpresa = ?oApp.Empresa� � ��C � � Cuentas� �� #)� +�C+
���� F�  � � >� ���	 � �� >� ���	 � �� >� ���	 � �� >� ���	 � �� T�
 ��	 � �� +�a� C�
 �
	���� F� � -��
 � � �� %�C4��m� F�  � #)� -��  � � � �� %�C4
��E� � >� ��� � �� >� ��� � �� >� ��� � �� � >� ��� �	 � �� T�
 �� � �� �y� !� � %�C�
 ����� !� � � F�	 � H� � F�  � & �� ��� � U  SALDOS
 IDCONCEPTO CONCEPTO IDPADRE IMPORTE ORDEN DATOS CMDSQL SQL XSALDO XINTEGRADORA CUENTAS BeforeOpenTables,     �� InitA     ��1 q 3 2� � ���1�a A rr � q�A �U � q Q bq � q Q B� Q A C� A A � A A B r A A q � 3                       &         A   `      )   e                                                                                  �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC�  �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Arial                                                         Importe                                                       "999,999,999,999.99"                                          Arial                                                         "Estado de Resultados"                                        Arial                                                         empresa                                                                                                                     Arial                                                         "Concepto"                                                   Arial                                                         
"Per�odo:"                                                    Arial                                                         m.dfecha, "  al " ,m.hfecha                                   Arial                                                         Concepto                                                     �<VFPData>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="" execute="len(rtrim(orden))&lt;=2" execwhen="len(rtrim(orden))&lt;=2" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="9" fstyle="1"/>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="space(5) + Concepto" execute="len(rtrim(orden))&lt;=4" execwhen="len(rtrim(orden))&lt;=4" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="8" fstyle="1"/>
	<memberdata name="Microsoft.VFP.Reporting.Builder.EvaluateContents" type="R" script="space(10) + Concepto" execute="len(rtrim(orden))&lt;=6" execwhen="len(rtrim(orden))&lt;=6" class="" classlib="" declass="" declasslib="" penrgb="0" fillrgb="-1" pena="255" filla="0" fname="Arial" fsize="8" fstyle="0"/>
</VFPData>
                                      Arial                                                         
datetime()                                                                                                                  Arial                                                         'P�g. '+alltrim(Str(_pageno))                                                                                               Arial                                                         
"Asientos"                                                    Arial                                                         	"Importe"                                                     Arial                                                         "Cheque"                                                      Arial                                                         "Referencia"                                                  "@I"                                                          Arial                                                         "Operacion"                                                   Arial                                                         "Fecha Dif."                                                 Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               �Top = 95
Left = 13
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
DataSource = .NULL.
Name = "Dataenvironment"
                                                 kPROCEDURE Init

CREATE CURSOR Saldos(IdConcepto c(10), Concepto c(50), IdPadre c(10), Importe Y, Orden C(20))

SET DATABASE TO datos
TEXT TO cmdSQL noshow
	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, d.IdConcepto, Orden=ISNULL(Orden,''), Importe=SUM(d.Importe) 
	FROM         ts_DepositoConceptoDet AS d INNER JOIN
	                      cn_conceptos AS c ON d.IdEmpresa = c.idempresa AND d.IdConcepto = c.idconcepto
	where d.IdEmpresa = ?oApp.Empresa
	and d.Fecha between ?m.dFecha and ?m.hFecha 
	group by c.concepto, c.Tipo, ISNULL(c.IdPadre,''), d.IdConcepto,Orden

ENDTEXT

=sql(cmdSQL,'xSaldo')

SELECT xsaldo
TEXT TO cmdSQL noshow
	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, c.IdConcepto,Orden=ISNULL(Orden,'')
	FROM   cn_conceptos AS c 
	       where c.IdEmpresa = ?oApp.Empresa
ENDTEXT

=sql(cmdSQL,'Cuentas')




GOTO TOP


DO WHILE  .NOT. EOF()
	SELECT SALDOS
	APPEND BLANK
	REPLACE IdConcepto WITH xsaldo.IdConcepto 
	REPLACE Importe WITH xsaldo.Importe
	REPLACE Orden WITH xsaldo.Orden 

	REPLACE Concepto WITH xsaldo.Concepto 
	XINTEGRADORA = xsaldo.IdPadre 
	
	DO WHILE .T. .AND.  .NOT. EMPTY(XINTEGRADORA)
		SELECT CUENTAS
		LOCATE FOR XINTEGRADORA=CUENTAS.Idconcepto
		IF FOUND()
			SELECT SALDOS
			GOTO TOP
*			SEEK XPROCESO+CUENTAS.CUENTA
			LOCATE FOR Saldos.IdConcepto=CUENTAS.Idconcepto
			IF  .NOT. FOUND()
				APPEND BLANK
				REPLACE IdConcepto WITH CUENTAS.IdConcepto 
				REPLACE Concepto WITH CUENTAS.Concepto
				REPLACE Orden WITH Cuentas.Orden	 
			ENDIF
			
 
			REPLACE Importe WITH Importe + xsaldo.Importe
			XINTEGRADORA = cuentas.IdPadre 
			
		ELSE
			EXIT
		ENDIF
		
		IF EMPTY(XINTEGRADORA)
			EXIT
		ENDIF
		
	ENDDO
	
	SELECT xsaldo
	SKIP
ENDDO
SELECT SALDOS
INDEX on Orden TAG Orden


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                  ~���    e  e                        |�   %   n        ;   �          �  U  �S h�� Saldos� � C��
�� � C��2�� � C��
�� � Y� � C���� G(� datos�	 M(� ��� �{ 	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, d.IdConcepto, Orden=ISNULL(Orden,''), Importe=SUM(d.Importe) �: �4 	FROM         ts_DepositoConceptoDet AS d INNER JOIN�k �e 	                      cn_conceptos AS c ON d.IdEmpresa = c.idempresa AND d.IdConcepto = c.idconcepto�( �" 	where d.IdEmpresa = ?oApp.Empresa�3 �- 	and d.Fecha between ?m.dFecha and ?m.hFecha �L �F 	group by c.concepto, c.Tipo, ISNULL(c.IdPadre,''), d.IdConcepto,Orden� �  � � ��C � � xSaldo� �� F�	 �	 M(� ��g �a 	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, c.IdConcepto,Orden=ISNULL(Orden,'')�  � 	FROM   cn_conceptos AS c �/ �) 	       where c.IdEmpresa = ?oApp.Empresa� � ��C � � Cuentas� �� #)� +�C+
���� F�  � � >� ���	 � �� >� ���	 � �� >� ���	 � �� >� ���	 � �� T�
 ��	 � �� +�a� C�
 �
	���� F� � -��
 � � �� %�C4��m� F�  � #)� -��  � � � �� %�C4
��E� � >� ��� � �� >� ��� � �� >� ��� � �� � >� ��� �	 � �� T�
 �� � �� �y� !� � %�C�
 ����� !� � � F�	 � H� � F�  � & �� ��� � U  SALDOS
 IDCONCEPTO CONCEPTO IDPADRE IMPORTE ORDEN DATOS CMDSQL SQL XSALDO XINTEGRADORA CUENTAS
  �  � U  SETEO Init,     �� BeforeOpenTablesY    ��1 2� � ���1�a A rr � q�A �U � q Q bq � q Q B� Q A C� A A � A A B r A A q � 4 q 2                       /     9   V  `  P    )   e                                                                            