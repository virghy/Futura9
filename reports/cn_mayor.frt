  O                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      cuenta      saldo      debe - haber      anterior      Arial      Arial      Arial      Arial      Arial      Arial      "Libro Mayor"             Arial      empresa             Arial      "Sucursal:"      Arial      
"Per�odo:"      Arial      1"Desde " + dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      %iif(m.sucursal='%','Todos',NombreSuc)             Arial      "Centro de Costo:"      Arial      &iif(m.centro='%','Todos',NombreCentro)             Arial      "Rango:"      Arial      m.dcuenta + " al " + m.hcuenta             Arial      "Cuenta Part."      Arial      	"Detalle"      Arial      "Debe"      Arial      "Haber"      Arial      	"Fecha
"      Arial      
"Suc. N�m"      Arial      "Doc."      Arial      "Saldo"      Arial      cuenta             Arial      descripci�n             Arial      "Saldo Anterior"             Arial      anterior      "999,999,999.99"             Arial      dec > 0      abs( anterior)      "@Z 999,999,999,999"             Arial      dec <= 0      0iif((anterior)<0,'C', iif((anterior) >0,'D',''))             Arial       fecha      "@D"             Arial      sucursal             Arial      N�mero      "99999"             Arial      	documento             Arial      cuentaPartida             Arial      nota             Arial      debe      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 99,999,999,999"      Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 99,999,999,999"      Arial      dec <= 0      saldo      "@Z 999,999,999.99"             Arial      dec > 0      
abs(saldo)      "@Z 999,999,999,999"             Arial      dec <= 0      *iif((Saldo)<0,'C', iif((Saldo) >0,'D',''))             Arial      "Saldos del Periodo"             Arial      debe      "@Z 999,999,999,999"             Arial      dec = 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec = 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999.99"             Arial      dec > 0      
abs(saldo)      "@Z 999,999,999,999"             Arial      dec <= 0      *iif((Saldo)<0,'C', iif((Saldo) >0,'D',''))             Arial      "Saldos del Ejercicio"             Arial      Tdebe      "@Z 99,999,999,999"      Arial      dec = 0      Thaber      "@Z 99,999,999,999"      Arial      dec = 0      abs(tDebe-THaber)      "999,999,999,999"             Arial      dec=0      8iif((tDebe-THaber)<0,'C', iif((tDebe-THaber) >0,'D',''))             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      ~Top = 271
Left = 40
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     UPROCEDURE BeforeOpenTables
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
	
TEXT TO cmdSQL noshow

select det.cuenta, sum( det.debe ) as Tdebe, 
	sum( det.haber ) as Thaber,
	sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior
into #xSaldoEjercicio
from   cn_asientos asi, cn_detalle det
where  asi.idasiento = det.idasiento AND
(asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)
	   AND asi.sucursal like rtrim(?m.sucursal)
	   AND det.centro like rtrim(?m.centro)
	   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND 
	asi.idempresa = ?oApp.Empresa
group by cuenta       
order by cuenta;

select asi.n�mero,asi.cuenta, fecha, CuentaNombre as descripci�n, debe, haber,
      CASE when LEN(isnull(detalle,'')) > 0 then isnull(detalle,'') else  isnull(descripci�n,'') end as nota, sucursal, documento, 0 as dec,
      Tdebe,tHaber,Anterior,
       SucursalNombre as NombreSuc,
       centroNombre as NombreCentro,
       dbo.cn_CuentaPartida(asi.IdAsiento, asi.Cuenta) as CuentaPartida
from   cn_vasientos asi 
left join #xSaldoEjercicio xSE on asi.cuenta=xSE.cuenta 
where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND
	asi.sucursal like rtrim(?m.sucursal)  AND
	(asi.cuenta between ?m.dCuenta and ?m.hCuenta) AND
	asi.idempresa = ?oApp.Empresa AND
	asi.centro like rtrim(?m.centro)		
order by asi.cuenta, asi.fecha, n�mero

ENDTEXT

*= sql('exec cn_mayor ?oApp.Empresa, ?oApp.InicioEjercicio,?oApp.FinalEjercicio,?m.dFecha,?m.hFecha,?m.dCuenta,?m.hCuenta,?m.sucursal,?m.centro','xMayor')

= sql(cmdsql,'xMayor')
SELECT xmayor
linforme = "rmayor"

ENDPROC
     !���                              �   %   !      �  3   I          �  U  
  �  � U  SETEO� %�C��  ���! � T��  �� %�� � %�C�� ���F � T�� �� %�� � %�C�� ���k � T�� �� 0�� � %�C�� ���� � T�� �� 99999999999�� �	 M(� �� �  �3 �- select det.cuenta, sum( det.debe ) as Tdebe, �" � 	sum( det.haber ) as Thaber,�x �r 	sum(case when asi.fecha  between  ?oApp.InicioEjercicio and ?m.dFecha - 1 then debe-haber else 0 end) as Anterior� � into #xSaldoEjercicio�, �& from   cn_asientos asi, cn_detalle det�. �( where  asi.idasiento = det.idasiento AND�H �B (asi.fecha between ?oApp.InicioEjercicio and ?oApp.FinalEjercicio)�2 �, 	   AND asi.sucursal like rtrim(?m.sucursal)�. �( 	   AND det.centro like rtrim(?m.centro)�B �< 	   AND  (det.cuenta between ?m.dCuenta and ?m.hCuenta) AND �$ � 	asi.idempresa = ?oApp.Empresa� � group by cuenta       � � order by cuenta;� �  �T �N select asi.n�mero,asi.cuenta, fecha, CuentaNombre as descripci�n, debe, haber,�� ��       CASE when LEN(isnull(detalle,'')) > 0 then isnull(detalle,'') else  isnull(descripci�n,'') end as nota, sucursal, documento, 0 as dec,�" �       Tdebe,tHaber,Anterior,�) �#        SucursalNombre as NombreSuc,�* �$        centroNombre as NombreCentro,�M �G        dbo.cn_CuentaPartida(asi.IdAsiento, asi.Cuenta) as CuentaPartida� � from   cn_vasientos asi �> �8 left join #xSaldoEjercicio xSE on asi.cuenta=xSE.cuenta �< �6 where  (asi.fecha between ?m.dFecha and ?m.hFecha) AND�0 �* 	asi.sucursal like rtrim(?m.sucursal)  AND�9 �3 	(asi.cuenta between ?m.dCuenta and ?m.hCuenta) AND�( �" 	asi.idempresa = ?oApp.Empresa AND�) �# 	asi.centro like rtrim(?m.centro)		�, �& order by asi.cuenta, asi.fecha, n�mero� �  � � ��C � � xMayor� �� F� � T� �� rmayor�� U  SUCURSAL CENTRO DCUENTA HCUENTA CMDSQL SQL XMAYOR LINFORME BeforeOpenTables,     �� InitA     ��1 q 3 A A A �A � a 1!�����!�!A�aa A!	!����������a A tq 12                       &         A   J      )                     