  $�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\ELI\HP Deskjet 1000 J110 series
OUTPUT=USB002
ORIENTATION=0
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=0
      M  5  winspool  \\ELI\HP Deskjet 1000 J110 series  USB002                       	X\\ELI\HP Deskjet 1000 J110 ser   � �C�  �
od   X  X   Carta                                                                          DINU" 8tH��$E                                 o                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    8  SMTJ     (H P   D e s k j e t   1 0 0 0   J 1 1 0   s e r i e s   InputBin 7 RESDLL UniresDLL Locale Spanish_Paraguay hpPrinterProtocol pcl3gui hpImagingDll hpfime51.dll Orientation PORTRAIT ColorMode 24bpp HPGrayScale 0 hpSupportsREST 1 HPTextThreshold 24 HPGraphicThreshold 22 Halftone HT_PATSIZE_DEFAULT HPMechOffset 60 hpDPIInfo 0 Resolution 600x300dpi HPRlt 1 HPPagesToPrint 4_AllPages PaperSize A4 MediaType 0.1004_0_600x300 HPHideQualitySettings 0 HPMaxDpi 0_disabled hpSpeedMech 3                                                                          H  LPPH   Q�`Q�`                              X  X  X  ,     �  �  �  }  K   K   <   <   <   <          '           '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Tue Jul 10 10:35:31:579 2012                                                                    w�`K   K                w�`      Arial      cuenta      saldo      debe - haber      anterior      Arial      Arial      Arial      Arial      Arial      Arial      !"Libro Mayor (Resumen por Fecha)"      Arial      empresa             Arial      "Sucursal:"      Arial      
"Per�odo:"      Arial      1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      %iif(m.sucursal='%','Todos',NombreSuc)             Arial      "Centro de Costo:"      Arial      &iif(m.centro='%','Todos',NombreCentro)             Arial      "Rango:"      Arial      m.dcuenta + " al " + m.hcuenta             Arial      	"Detalle"      Arial      "Debe"      Arial      "Haber"      Arial      	"Fecha
"      Arial      
"Suc. N�m"      Arial      "Doc."      Arial      "Saldo"      Arial      cuenta             Arial      descripci�n             Arial      "Saldo Anterior"             Arial      	 anterior      "@Z 999,999,999,999"      Arial      dec <= 0      anterior      "999,999,999.99"             Arial      dec > 0       fecha      "@D"             Arial      sucursal             Arial      N�mero      "99999"             Arial      	documento             Arial      nota             Arial      debe      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 99,999,999,999"      Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 99,999,999,999"      Arial      dec <= 0      saldo      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Periodo"             Arial      debe      "@Z 999,999,999,999"             Arial      dec = 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec = 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Ejercicio"             Arial      Tdebe      "@Z 99,999,999,999"      Arial      dec = 0      Thaber      "@Z 99,999,999,999"      Arial      dec = 0      tDebe-THaber      "999,999,999,999"      Arial      dec=0      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      ~Top = 271
Left = 40
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     
uPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.sucursal)
     m.sucursal = '%'
ENDIF
IF EMPTY(m.centro)
     m.centro = '%'
ENDIF
IF EMPTY(m.dcuenta)
	m.dcuenta='0'
ENDIF

IF EMPTY(m.hcuenta)
	m.hcuenta='99999999999'
ENDIF
*!*	*!*	select det.cuenta, sum( det.debe ) as Tdebe, 
*!*	*!*		sum( det.haber ) as Thaber,
*!*	*!*		sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior
*!*	*!*	into #xSaldoEjercicio
*!*	*!*	from   cn_asientos asi, cn_detalle det
*!*	*!*	where  asi.idasiento = det.idasiento AND
*!*	*!*	(asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)
*!*	*!*		   AND asi.sucursal like rtrim(?m.sucursal)
*!*	*!*		   AND det.centro like rtrim(?m.centro)
*!*	*!*		   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND 
*!*	*!*		asi.idempresa = ?oApp.Empresa
*!*	*!*	group by cuenta       
*!*	*!*	order by cuenta

*!*	TEXT TO cmdSQL noshow



*!*	select  fecha, asi.n�mero,asi.cuenta, CuentaNombre as descripci�n, debe, haber,
*!*	      CASE when LEN(isnull(detalle,'')) > 0 then isnull(detalle,'') else  isnull(descripci�n,'') end as nota, sucursal, documento, 0 as dec,
*!*	      Tdebe,tHaber,Anterior,
*!*	       SucursalNombre as NombreSuc,
*!*	       centroNombre as NombreCentro,
*!*	       dbo.cn_CuentaPartida(asi.IdAsiento, asi.Cuenta) as CuentaPartida
*!*	from   cn_vasientos asi 
*!*	left join (select det.cuenta, sum( det.debe ) as Tdebe, 
*!*		sum( det.haber ) as Thaber,
*!*		sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior
*!*	from   cn_asientos asi, cn_detalle det
*!*	where  asi.idasiento = det.idasiento AND
*!*	(asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)
*!*		   AND asi.sucursal like rtrim(?m.sucursal)
*!*		   AND det.centro like rtrim(?m.centro)
*!*		   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND 
*!*		asi.idempresa = ?oApp.Empresa
*!*	group by cuenta) xSE on asi.cuenta=xSE.cuenta 
*!*	where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND
*!*		asi.sucursal like rtrim(?m.sucursal)  AND
*!*		(asi.cuenta between ?m.dCuenta and ?m.hCuenta) AND
*!*		asi.idempresa = ?oApp.Empresa AND
*!*		asi.centro like rtrim(?m.centro)		
*!*	order by asi.cuenta, asi.fecha, n�mero

*!*	ENDTEXT

 sql('exec cn_mayor_ResFec ?oApp.Empresa, ?oApp.InicioEjercicio,?oApp.FinalEjercicio,?m.dFecha,?m.hFecha,?m.dCuenta,?m.hCuenta,?m.sucursal,?m.centro','xMayor')

*= sql(cmdsql,'xMayor')
SELECT xmayor
linforme = "rmayor_ov"

IF LCDESTINO='A'
	exportar()
	RETURN .f.
ENDIF
	

ENDPROC
     ����    �  �                        t�   %         v     F          �  U  
  �  � U  SETEO� %�C��  ���! � T��  �� %�� � %�C�� ���F � T�� �� %�� � %�C�� ���k � T�� �� 0�� � %�C�� ���� � T�� �� 99999999999�� �� ��C�� exec cn_mayor_ResFec ?oApp.Empresa, ?oApp.InicioEjercicio,?oApp.FinalEjercicio,?m.dFecha,?m.hFecha,?m.dCuenta,?m.hCuenta,?m.sucursal,?m.centro� xMayor� �� F� � T� ��	 rmayor_ov�� %�� � A����
 ��C� �� B�-�� � U	  SUCURSAL CENTRO DCUENTA HCUENTA SQL XMAYOR LINFORME	 LCDESTINO EXPORTAR BeforeOpenTables,     �� InitA     ��1 q 3 A A A �A �A
s a"� q A 3                       &         A   j
      )   �                  