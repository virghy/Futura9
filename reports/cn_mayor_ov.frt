  !i                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      cuenta      saldo      debe - haber      anterior      Arial      Arial      Arial      Arial      Arial      Arial      "Libro Mayor"             Arial      empresa             Arial      "Sucursal:"      Arial      
"Per�odo:"      Arial      1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      %iif(m.sucursal='%','Todos',NombreSuc)             Arial      "Centro de Costo:"      Arial      &iif(m.centro='%','Todos',NombreCentro)             Arial      "Rango:"      Arial      m.dcuenta + " al " + m.hcuenta             Arial      "Concepto:"      Arial      *iif(isnull(m.idconcepto),'Todos',concepto)      Arial      	"Detalle"      Arial      "Debe"      Arial      "Haber"      Arial      	"Fecha
"      Arial      
"Suc. N�m"      Arial      "Doc."      Arial      "CC  Concepto"      Arial      "Saldo"      Arial      cuenta             Arial      descripci�n             Arial      "Saldo Anterior"             Arial      	 anterior      "@Z 999,999,999,999"      Arial      dec <= 0      anterior      "999,999,999.99"             Arial      dec > 0       fecha      "@D"      Arial      sucursal             Arial      N�mero      "99999"             Arial      	documento      Arial      Centro      Arial      nota             Arial      debe      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 99,999,999,999"      Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 99,999,999,999"      Arial      dec <= 0      saldo      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Periodo"             Arial      debe      "@Z 999,999,999,999"             Arial      dec = 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec = 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Ejercicio"             Arial      Tdebe      "@Z 99,999,999,999"      Arial      dec = 0      Thaber      "@Z 99,999,999,999"      Arial      dec = 0      tDebe-THaber      "999,999,999,999"      Arial      dec=0      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      ~Top = 271
Left = 40
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     
�PROCEDURE Init
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

IF EMPTY(m.idconcepto)
	m.idconcepto=.null.
ENDIF


*!*	select det.cuenta, sum( det.debe ) as Tdebe, 
*!*		sum( det.haber ) as Thaber,
*!*		sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior
*!*	into #xSaldoEjercicio
*!*	from   cn_asientos asi, cn_detalle det
*!*	where  asi.idasiento = det.idasiento AND
*!*	(asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)
*!*		   AND asi.sucursal like rtrim(?m.sucursal)
*!*		   AND det.centro like rtrim(?m.centro)
*!*		   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND 
*!*		asi.idempresa = ?oApp.Empresa
*!*	group by cuenta       
*!*	order by cuenta

TEXT TO cmdSQL noshow



select  fecha, asi.n�mero,asi.cuenta, CuentaNombre as descripci�n, debe, haber,
      CASE when LEN(isnull(detalle,'')) > 0 then isnull(detalle,'') else  isnull(descripci�n,'') end as nota, sucursal, documento, 0 as dec,
      Tdebe,tHaber,Anterior,
       SucursalNombre as NombreSuc,
       centroNombre as NombreCentro,
       dbo.cn_CuentaPartida(asi.IdAsiento, asi.Cuenta) as CuentaPartida,
       IdConcepto,Concepto,Centro
from   cn_vasientos asi 
left join (select det.cuenta, sum( det.debe ) as Tdebe, 
	sum( det.haber ) as Thaber,
	sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior
from   cn_asientos asi, cn_detalle det
where  asi.idasiento = det.idasiento AND
(asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)
	   AND asi.sucursal like rtrim(?m.sucursal)
	   AND det.centro like rtrim(?m.centro)
	   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND 
	asi.idempresa = ?oApp.Empresa
	and (det.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)
group by cuenta) xSE on asi.cuenta=xSE.cuenta 
where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND
	asi.sucursal like rtrim(?m.sucursal)  AND
	(asi.cuenta between ?m.dCuenta and ?m.hCuenta) AND
	asi.idempresa = ?oApp.Empresa AND
	asi.centro like rtrim(?m.centro)		
	and (asi.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)	
order by asi.cuenta, asi.fecha, n�mero

ENDTEXT

*= sql('exec cn_mayor ?oApp.Empresa, ?oApp.InicioEjercicio,?oApp.FinalEjercicio,?m.dFecha,?m.hFecha,?m.dCuenta,?m.hCuenta,?m.sucursal,?m.centro','xMayor')

= sql(cmdsql,'xMayor')
SELECT xmayor
linforme = "rmayor_ov"

IF LCDESTINO='A'
	exportar()
	RETURN .f.
ENDIF
	

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     	���    �  �                        �=   %   �      �  <   "          �  U  Q %�C��  ���! � T��  �� %�� � %�C�� ���F � T�� �� %�� � %�C�� ���k � T�� �� 0�� � %�C�� ���� � T�� �� 99999999999�� � %�C�� ���� � T�� ���� �	 M(� �� �  � �  � �  �U �O select  fecha, asi.n�mero,asi.cuenta, CuentaNombre as descripci�n, debe, haber,�� ��       CASE when LEN(isnull(detalle,'')) > 0 then isnull(detalle,'') else  isnull(descripci�n,'') end as nota, sucursal, documento, 0 as dec,�" �       Tdebe,tHaber,Anterior,�) �#        SucursalNombre as NombreSuc,�* �$        centroNombre as NombreCentro,�N �H        dbo.cn_CuentaPartida(asi.IdAsiento, asi.Cuenta) as CuentaPartida,�' �!        IdConcepto,Concepto,Centro� � from   cn_vasientos asi �> �8 left join (select det.cuenta, sum( det.debe ) as Tdebe, �" � 	sum( det.haber ) as Thaber,�x �r 	sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior�, �& from   cn_asientos asi, cn_detalle det�. �( where  asi.idasiento = det.idasiento AND�H �B (asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)�2 �, 	   AND asi.sucursal like rtrim(?m.sucursal)�. �( 	   AND det.centro like rtrim(?m.centro)�B �< 	   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND �$ � 	asi.idempresa = ?oApp.Empresa�D �> 	and (det.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)�4 �. group by cuenta) xSE on asi.cuenta=xSE.cuenta �< �6 where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND�0 �* 	asi.sucursal like rtrim(?m.sucursal)  AND�9 �3 	(asi.cuenta between ?m.dCuenta and ?m.hCuenta) AND�( �" 	asi.idempresa = ?oApp.Empresa AND�) �# 	asi.centro like rtrim(?m.centro)		�E �? 	and (asi.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)	�, �& order by asi.cuenta, asi.fecha, n�mero� �  � � ��C � � xMayor� �� F� � T� ��	 rmayor_ov�� %��	 � A��J�
 ��C�
 �� B�-�� � U  SUCURSAL CENTRO DCUENTA HCUENTA
 IDCONCEPTO CMDSQL SQL XMAYOR LINFORME	 LCDESTINO EXPORTAR
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 A A A �A � A  � a a a Q!	!���q��!����!�!AAA����Q�a A tq a"� q A 4 q 2                       w
     :   �
  �
  R    )   �                  