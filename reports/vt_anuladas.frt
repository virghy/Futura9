  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=EPSON LX-350 ESC/P
OUTPUT=LPT1:
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
      =  &  winspool  EPSON LX-350 ESC/P  LPT1:                       EPSON LX-350 ESC/P               � hC�  �4d   ,  ,  A4                                                            ����                DINU" T �2�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         SMTJ     E P S O N   L X - 3 5 0   E S C / P   InputBin MANUAL RESDLL UniresDLL Orientation PORTRAIT Resolution RESOLUTION_120X72 PrintSpeed PRINTER_SETTING_SPEED PaperSize A4 PrintDirection PRINTER_SETTING Halftone HT_PATSIZE_AUTO                                             ISPE                 Arial      Arial      Arial      Arial      Arial      "Control de Facturas Anuladas"      Arial      empresa             Arial      *iif(empty(m.sucursal),'Todos',descripci�n)             Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Cpbte."      Arial      "Nro."      Arial      vt_rdiariocontrol.idcomprobante             Arial      vt_rdiariocontrol.numero             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      idcomprobante      Arial      "Total"      Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
*!*	LOCAL strsql
*!*	SET DATABASE TO DATOS 
*!*	strsql = 'SELECT a.sucursal, a.fecha, a.idcomprobante, '+;
*!*			 ' a.numero, a.idcliente, a.exenta, '+;
*!*			 ' a.gravada, a.iva, b.razsocial, '+;
*!*			 ' c.descripcion, d.descripci�n, a.idmoneda '+;
*!*			 'FROM vt_factura a, vt_clientes b , vt_cpbt c, sucursal d '+;
*!*			 'WHERE a.idcliente = b.idcliente '+;
*!*			 '  AND a.idcomprobante = c.idcomprobante '+;
*!*			 '  AND a.sucursal = d.sucursal '+;
*!*			 '  AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha '+;
*!*			 '  AND a.sucursal = ?m.sucursal and a.anulado=0 '+;
*!*			 'ORDER BY a.sucursal, a.fecha, a.idcomprobante, '+;
*!*			 ' a.numero '

*!*	= sql(strsql,'vt_rdiariocontrol')
*!*	SELECT vt_rdiariocontrol

If Empty(m.sucursal)
	Store null To sucursal
ENDIF

TEXT TO cmdSQL noshow

SELECT     a.Sucursal, a.Fecha, a.IdComprobante, a.Numero, d.Descripci�n
FROM         dbo.vt_factura a LEFT OUTER JOIN
                      dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal
where
	a.IdEmpresa = ?oApp.Empresa  and                     
	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha 
	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) 
	and anulado = 1
	ORDER BY a.idcomprobante, 
	a.numero 

ENDTEXT

	
	sql(cmdSQL,'vt_rdiariocontrol')
	SELECT vt_rdiariocontrol
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
     |���    c  c                        �m   %   �      
     �          �  U  C %�C��  ��� � J���(�  � �	 M(� �� �  �N �H SELECT     a.Sucursal, a.Fecha, a.IdComprobante, a.Numero, d.Descripci�n�3 �- FROM         dbo.vt_factura a LEFT OUTER JOIN�c �]                       dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal� � where�< �6 	a.IdEmpresa = ?oApp.Empresa  and                     �/ �) 	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha �= �7 	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) � � 	and anulado = 1�! � 	ORDER BY a.idcomprobante, � �
 	a.numero � �  � �" ��C � � vt_rdiariocontrol� �� F� � U  SUCURSAL CMDSQL SQL VT_RDIARIOCONTROL
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1  � A � a �11� ���aa A #q 2 q 1                       G        n  v  +    )   c                  