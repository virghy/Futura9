  B)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP Deskjet F300 series
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
DUPLEX=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      B  *  winspool  HP Deskjet F300 series  USB001                       "pHP Deskjet F300 series           � �!C��  �4d   ,  ,  A4                                                           ����                DINU" D	�X��                            C                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        SMTJ     �H P   D e s k j e t   F 3 0 0   s e r i e s   InputBin FORMSOURCE RESDLL UniresDLL HPPreAnalysis False MSPreAnalysis False HPReportSymmetricMargins False HPMinimizeMargins False HPAlignMarginsForMD True Duplex NONE HPPrintPreview False HPBorderLessPhoto False HPOutputOrderReverse True HPOrientRotate180 False JobUI True PaperSize LETTER Orientation PORTRAIT HPDocPropResourceData hpzhl054.cab HPNUseDiffFirstPageChoice True HPPrintInGrayScale False PrintQuality FastDraft PrintQualityGroup PQGroup_3 HPColorMode COLOR_MODE HPPDLType PDL_PCL3 HPXMLFileUsed hpof3003.xml HPTilingSelection True HPMediaTypeTreeviewPopup True ColorMode Color24 TextAsBlack False MediaType PLAIN Resolution 300dpi PQDPI Installed HPMirrorPrint False HPAutoDuplexScaling True HPPrintOnBothSidesManually False HPManualDuplexDialogItems InstructionID_01_FACEUP-NOROTATE HPManualDuplexPageRotate UserRotate HPManualFeedOrientation FACEUP HPOutputBinOrientation FACEUP HPManualDuplexDialogModel Modal HPManualDuplexPageOrder OddPagesFirst HPMapManualFeedToTray1 False PSAlignmentFile HPZlA054 PSServicesOption LaunchToolBox HPConsumerCustomPaper HPCustom HPBornOnDate HPBOD HPInputColorSpace COLORSMART HPDriverData DriverData HPDisplayPrintingHelp RegPath ESPRITSupported True HPRESDLLName HPFRS054 HPRedEyeReduction Off HPDigitalImaging HPHomePrinting HPSmartFocus Off HPContrast On HPDigitalFlash On HPSharpness On HPSmoothing On HPJpegPngPassthrough True Halftone HT_PATSIZE_DEFAULT HPHTDLLName HPFIG054 HPMHDLLName HPFIE054 HPCRDCommand True HPSendUnitMeasureCommand TRUE HPSimplifiedUI True HPPosterPrintingOptionSUI False HPAutoEnhance True HPNWatermark True HPPhotoFix OFF                                                                                                                                  �  IUPH                                   d             C a r t a                            �
o  ������������                                                                                                          [ n i n g u n a ]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               [ n i n g u n a ]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A r i a l                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4   P              ���     ���                       d                 �?                                                                         P C   E S T A N C I A                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             < A u t o m � t i c o >                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     R� 4  4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                I m p r e s i � n   r � p i d a / e c o n � m i c a                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         d                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      I n t r o d u z c a   n u e v o   n o m b r e   d e   c o n f i g .   r � p i d a                                                                                                   x   B  x   B                  t   I  {   C                  z   E  u   D     �  futura.exe                                                                                                                                   C : \ A r c h i v o s   d e   p r o g r a m a \ F u t u r a   S o f t w a r e \ F u t u r a \ f u t u r a . e x e                                                                                                                                                                                                                                                                                                                                                                                                                                      IPACDRWDSGATCAPI;�F�4�E���8Q;K                 i�݅&�E�%�9�x�      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      ("Detalle de Imputaci�n de Orden de Pago"      Arial      oApp.nombreempresa      Arial      OP.nroorden      "@BL 99999"      Arial      "Nro."      Arial      OP.proveedor      Arial      "Proveedor"      Arial      OP.nrocheque      Arial      OP.banco      Arial      "Fecha: " ,OP.fecha      Arial      ""Vencimiento : ", OP.fechadiferida      Arial      &!empty(nvl(OP.fechadiferida,{ /  / }))      "Cheque"      Arial      
OP.Importe      "@B 999,999,999,999.99"      Arial      	"Importe"      Arial      "Cuenta"      Arial      "Descripci�n"      Arial      "Debe"      Arial      "Haber"      Arial      cuenta      Arial      descripcion      Arial      debe      "@Z 9,999,999,999"      Arial      haber      "@Z 9,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
Datetime()      Arial      dataenvironment      �Top = 178
Left = -11
Width = 760
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "cp_rordenpago"
DataSource = .NULL.
Name = "Dataenvironment"
     UPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
TEXT TO cmdSQL
	SELECT     a.fecha, RTRIM(ISNULL(a.idproveedor, '')) + '-' + a.nombre AS proveedor, a.nroorden, a.nrocheque, a.Detalle AS Concepto, b.facturaproveedor, 
                      a.idmoneda, b.valorizado, a.Importe, d.nombre AS banco, convert(char(10),ts_d.FechaDiferida,105) as FechaDiferida
FROM         dbo.ts_depositos_base ts_d RIGHT OUTER JOIN
                      dbo.cp_ordenpago a ON ts_d.iddeposito = a.iddeposito LEFT OUTER JOIN
                      dbo.cp_orddet_pago b ON a.IdOrdenPago = b.nroorden AND a.idempresa = b.idempresa LEFT OUTER JOIN
                      dbo.ts_cuentas d ON a.idcuenta = d.idcuenta LEFT OUTER JOIN
                      dbo.bs_bancos e ON d.idbanco = e.idbanco
	where a.idempresa=?oApp.Empresa and a.nroorden=?m.NroOrden
ENDTEXT
sql(cmdSQL,'OP')





TEXT TO cmdSQL
SELECT     asi.n�mero, asi.ejercicio, asiD.cuenta, rtrim(c.Descripci�n) + CASE WHEN ce.descripci�n IS NULL THEN '' ELSE '(' + asid.Centro + rtrim(ce.descripci�n) 
                      + ')' END AS Descripcion, asiD.debe, asiD.haber,IdDetalle,asi.IdAsiento
FROM         dbo.cp_ordenpago op INNER JOIN
                      dbo.ts_depositos_base dep ON op.iddeposito = dep.iddeposito INNER JOIN
                      dbo.cn_asientos asi ON dep.nroasiento = asi.idasiento INNER JOIN
                      dbo.cn_detalle asiD ON asi.idasiento = asiD.idasiento INNER JOIN
                      dbo.cn_cuentas c ON asiD.idempresa = c.IdEmpresa AND asiD.ejercicio = c.Ejercicio AND asiD.cuenta = c.Cuenta LEFT OUTER JOIN
                      dbo.centros ce ON asiD.idempresa = ce.idempresa AND asiD.centro = ce.centro
	where op.idempresa=?oApp.Empresa and op.nroorden=?m.NroOrden
ENDTEXT
sql(cmdSQL,'Detalle2')
TEXT TO cmdSQL
SELECT     asi.n�mero, asi.ejercicio, asiD.cuenta, rtrim(c.Descripci�n) + CASE WHEN ce.descripci�n IS NULL THEN '' ELSE '(' + asid.Centro + rtrim(ce.descripci�n) 
                      + ')' END AS Descripcion, asiD.debe, asiD.haber,IdDetalle
FROM         		  dbo.cn_asientos asi INNER JOIN
                      dbo.cn_detalle asiD ON asi.idasiento = asiD.idasiento INNER JOIN
                      dbo.cn_cuentas c ON asiD.idempresa = c.IdEmpresa AND asiD.ejercicio = c.Ejercicio AND asiD.cuenta = c.Cuenta LEFT OUTER JOIN
                      dbo.centros ce ON asiD.idempresa = ce.idempresa AND asiD.centro = ce.centro
	where asid.NroAsientoRef=?Detalle2.IdAsiento
ENDTEXT
sql(cmdSQL,'Detalle3')

*, op.iddeposito, dep.nroasiento

SELECT "" as Origen,cuenta,Descripcion,debe,haber ,IdDetalle;
 FROM Detalle2;
union;
SELECT "*" as Origen,ALLTRIM(cuenta)+"*" as Cuenta ,Descripcion,debe,haber ,IdDetalle;
 FROM Detalle3;
order BY IdDetalle,1;
INTO CURSOR Detallex

SELECT Detallex


ENDPROC
PROCEDURE Destroy
RELEASE detalle

ENDPROC
     J���    1  1                        �4   %   =      �  )   v          �  U  
  �  � U  SETEO�
 M(�  �� �� 	SELECT     a.fecha, RTRIM(ISNULL(a.idproveedor, '')) + '-' + a.nombre AS proveedor, a.nroorden, a.nrocheque, a.Detalle AS Concepto, b.facturaproveedor, �� ��                       a.idmoneda, b.valorizado, a.Importe, d.nombre AS banco, convert(char(10),ts_d.FechaDiferida,105) as FechaDiferida�> �8 FROM         dbo.ts_depositos_base ts_d RIGHT OUTER JOIN�` �Z                       dbo.cp_ordenpago a ON ts_d.iddeposito = a.iddeposito LEFT OUTER JOIN�| �v                       dbo.cp_orddet_pago b ON a.IdOrdenPago = b.nroorden AND a.idempresa = b.idempresa LEFT OUTER JOIN�W �Q                       dbo.ts_cuentas d ON a.idcuenta = d.idcuenta LEFT OUTER JOIN�D �>                       dbo.bs_bancos e ON d.idbanco = e.idbanco�A �; 	where a.idempresa=?oApp.Empresa and a.nroorden=?m.NroOrden� � ��C �  � OP� �� M(�  �� �� SELECT     asi.n�mero, asi.ejercicio, asiD.cuenta, rtrim(c.Descripci�n) + CASE WHEN ce.descripci�n IS NULL THEN '' ELSE '(' + asid.Centro + rtrim(ce.descripci�n) �c �]                       + ')' END AS Descripcion, asiD.debe, asiD.haber,IdDetalle,asi.IdAsiento�1 �+ FROM         dbo.cp_ordenpago op INNER JOIN�b �\                       dbo.ts_depositos_base dep ON op.iddeposito = dep.iddeposito INNER JOIN�\ �V                       dbo.cn_asientos asi ON dep.nroasiento = asi.idasiento INNER JOIN�\ �V                       dbo.cn_detalle asiD ON asi.idasiento = asiD.idasiento INNER JOIN�� ��                       dbo.cn_cuentas c ON asiD.idempresa = c.IdEmpresa AND asiD.ejercicio = c.Ejercicio AND asiD.cuenta = c.Cuenta LEFT OUTER JOIN�g �a                       dbo.centros ce ON asiD.idempresa = ce.idempresa AND asiD.centro = ce.centro�C �= 	where op.idempresa=?oApp.Empresa and op.nroorden=?m.NroOrden� � ��C �  � Detalle2� �� M(�  �� �� SELECT     asi.n�mero, asi.ejercicio, asiD.cuenta, rtrim(c.Descripci�n) + CASE WHEN ce.descripci�n IS NULL THEN '' ELSE '(' + asid.Centro + rtrim(ce.descripci�n) �U �O                       + ')' END AS Descripcion, asiD.debe, asiD.haber,IdDetalle�5 �/ FROM         		  dbo.cn_asientos asi INNER JOIN�\ �V                       dbo.cn_detalle asiD ON asi.idasiento = asiD.idasiento INNER JOIN�� ��                       dbo.cn_cuentas c ON asiD.idempresa = c.IdEmpresa AND asiD.ejercicio = c.Ejercicio AND asiD.cuenta = c.Cuenta LEFT OUTER JOIN�g �a                       dbo.centros ce ON asiD.idempresa = ce.idempresa AND asiD.centro = ce.centro�3 �- 	where asid.NroAsientoRef=?Detalle2.IdAsiento� � ��C �  � Detalle3� ��� o��@  � Detalle3�� *�Q� �C� �� *�Q� �� ��� ��� ��� �� Detalle2��  �Q� �� ��� ��� ��� ��� ���� ������� Detallex� F�
 � U  CMDSQL SQL ORIGEN CUENTA DESCRIPCION DEBE HABER	 IDDETALLE DETALLE2 DETALLE3 DETALLEX
  <�  � U  DETALLE BeforeOpenTables,     �� InitA     �� Destroy&    ��1 q 3 � �	���qAA 1� �
1!���	q1A �� �
QQ��	q1A �J	r 4 q 2                       &         A        '   9  J  ;    )   1                  