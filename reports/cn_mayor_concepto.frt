  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2970
PAPERWIDTH=2100
COLOR=2
      Arial      
Idconcepto      saldo      debe - haber      anterior      Arial      Arial      Arial      Arial      Arial      Arial      $"Detalle de Aplicacion de Conceptos"      Arial      empresa             Arial      "Sucursal:"      Arial      
"Per�odo:"      Arial      1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      %iif(m.sucursal='%','Todos',NombreSuc)             Arial      "Concepto:"      Arial      *iif(isnull(m.idconcepto),'Todos',concepto)      Arial      "Centro de Costo:"      Arial      &iif(m.centro='%','Todos',NombreCentro)             Arial      	"Detalle"      Arial      "Debe"      Arial      "Haber"      Arial      	"Fecha
"      Arial      
"Suc. N�m"      Arial      "Doc."      Arial      "CC"      Arial      "Saldo"      Arial      
IdConcepto      Arial      Concepto      Arial      "Saldo Anterior"             Arial      	 anterior      "@Z 999,999,999,999"      Arial      dec <= 0      anterior      "999,999,999.99"             Arial      dec > 0       fecha      "@D"             Arial      sucursal             Arial      N�mero      "99999"             Arial      	documento      Arial      Centro      Arial      7alltrim(Cuenta) + '-' + Alltrim(Descripci�n),', ' ,nota      Arial      debe      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 99,999,999,999"      Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 99,999,999,999"      Arial      dec <= 0      saldo      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Periodo"             Arial      debe      "@Z 999,999,999,999"             Arial      dec = 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec = 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Ejercicio"             Arial      Tdebe      "@Z 99,999,999,999"      Arial      dec = 0      Thaber      "@Z 99,999,999,999"      Arial      dec = 0      tDebe-THaber      "999,999,999,999"      Arial      dec=0      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      ~Top = 271
Left = 40
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     	�PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.sucursal)
     m.sucursal = '%'
ENDIF
IF EMPTY(m.centro)
     m.centro = '%'
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
       asi.IdConcepto,Concepto,Centro
from   cn_vasientos asi 
left join (select det.IdConcepto, sum( det.debe ) as Tdebe, 
	sum( det.haber ) as Thaber,
	sum(case when a.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior
from   cn_asientos a, cn_detalle det
where  a.idasiento = det.idasiento AND
(a.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)
	   AND a.sucursal like rtrim(?m.sucursal)
	   AND det.centro like rtrim(?m.centro)
	   and a.idempresa = ?oApp.Empresa
	   and (det.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)
group by IdConcepto) xSE on asi.IdConcepto=xSE.IdConcepto
where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND
	asi.sucursal like rtrim(?m.sucursal)  AND
	asi.idempresa = ?oApp.Empresa AND
	asi.centro like rtrim(?m.centro)		
	and (asi.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)	
order by Concepto,asi.fecha, asi.cuenta, n�mero

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
     5���                              �v   %   3      �  4   [          �  U  
  �  � U  SETEO� %�C��  ���! � T��  �� %�� � %�C�� ���F � T�� �� %�� � %�C�� ���h � T�� ���� �	 M(� �� �  � �  � �  �U �O select  fecha, asi.n�mero,asi.cuenta, CuentaNombre as descripci�n, debe, haber,�� ��       CASE when LEN(isnull(detalle,'')) > 0 then isnull(detalle,'') else  isnull(descripci�n,'') end as nota, sucursal, documento, 0 as dec,�" �       Tdebe,tHaber,Anterior,�) �#        SucursalNombre as NombreSuc,�* �$        centroNombre as NombreCentro,�N �H        dbo.cn_CuentaPartida(asi.IdAsiento, asi.Cuenta) as CuentaPartida,�+ �%        asi.IdConcepto,Concepto,Centro� � from   cn_vasientos asi �B �< left join (select det.IdConcepto, sum( det.debe ) as Tdebe, �" � 	sum( det.haber ) as Thaber,�v �p 	sum(case when a.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior�* �$ from   cn_asientos a, cn_detalle det�, �& where  a.idasiento = det.idasiento AND�F �@ (a.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)�0 �* 	   AND a.sucursal like rtrim(?m.sucursal)�. �( 	   AND det.centro like rtrim(?m.centro)�) �# 	   and a.idempresa = ?oApp.Empresa�G �A 	   and (det.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)�? �9 group by IdConcepto) xSE on asi.IdConcepto=xSE.IdConcepto�< �6 where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND�0 �* 	asi.sucursal like rtrim(?m.sucursal)  AND�( �" 	asi.idempresa = ?oApp.Empresa AND�) �# 	asi.centro like rtrim(?m.centro)		�E �? 	and (asi.idconcepto = ?m.idconcepto or ?m.IdConcepto is null)	�5 �/ order by Concepto,asi.fecha, asi.cuenta, n�mero� �  � � ��C � � xMayor� �� F� � T� ��	 rmayor_ov�� %�� � A����
 ��C� �� B�-�� � U	  SUCURSAL CENTRO
 IDCONCEPTO CMDSQL SQL XMAYOR LINFORME	 LCDESTINO EXPORTAR BeforeOpenTables,     �� InitA     ��1 q 3 A A � A  � a a a Q!	!�����!!a��a��q����QQa A tq a"� q A 3                       &         A   �	      )                     