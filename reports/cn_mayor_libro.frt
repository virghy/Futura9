  "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      cuenta      saldo      debe - haber      anterior      Saldo1      debe - haber      Anterior      Arial      Arial      Arial      Arial      	"Detalle"      Arial      "Debe"      Arial      "Haber"      Arial      	"Fecha
"      Arial      
"Sucursal"      Arial      "Saldo"      Arial      cuenta             Arial      descripci�n             Arial      Saldo1      "@Z 999,999,999,999"      Arial      "Saldo Anterior"      Arial       fecha      "@D"      Arial      sucursal             Arial      nota             Arial      debe      "@Z 999,999,999.99"             Arial      dec > 0      debe      "@Z 99,999,999,999"      Arial      dec <= 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 99,999,999,999"      Arial      dec <= 0      saldo      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      "Saldos del Periodo"             Arial      debe      "@Z 999,999,999,999"             Arial      dec = 0      debe      "@Z 999,999,999.99"             Arial      dec > 0      haber      "@Z 999,999,999,999"             Arial      dec = 0      haber      "@Z 999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999.99"             Arial      dec > 0      saldo      "@Z 999,999,999,999"      Arial      dec <= 0      dataenvironment      ~Top = 271
Left = 40
Width = 520
Height = 200
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
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
 **sql('exec cn_mayor_ResFec ?oApp.Empresa, ?oApp.InicioEjercicio,?oApp.FinalEjercicio,?m.dFecha,?m.hFecha,?m.dCuenta,?m.hCuenta,?m.sucursal,?m.centro','xMayor')


TEXT TO cmdSQL
	Declare @Empresa char(3), @IE datetime, @FE datetime, @dFecha datetime, @hFecha datetime, @dCuenta char(9),
	  @hCuenta char(9), @sucursal char(2),@centro char(5) 
	SELECT @Empresa=?oApp.Empresa,  @IE=?oApp.InicioEjercicio,
				@FE=?oApp.FinalEjercicio,@dFecha=?m.dFecha,@hFecha=?m.hFecha,
				@dCuenta= ?m.dCuenta,@hCuenta=?m.hCuenta,@sucursal=?m.sucursal,@centro=?m.centro 

		-- Calcula el saldo del Ejercicio 			(VG)
		select det.cuenta, sum( det.debe ) as Tdebe, 
			sum( det.haber ) as Thaber,
			sum(case when asi.fecha  between  @IE and @dFecha - 1 then debe-haber else 0 end) as Anterior
		into #xSaldoEjercicio
		from   cn_asientos asi, cn_detalle det
		where  asi.idasiento = det.idasiento AND
		(asi.fecha between @IE and @FE)
			   AND asi.sucursal like rtrim(@sucursal)
			   AND det.centro like rtrim(@centro)
			   AND  (det.cuenta between @dcuenta and @hCuenta) AND 
			asi.idempresa = @empresa
		group by cuenta       
		order by cuenta;

		select '1' as n�mero,asi.cuenta, asi.fecha, asi.CuentaNombre as descripci�n, 
			sum(debe) as Debe, 0 as Haber,
		     'A varios'  as nota, 
		  	'' as sucursal, '' as documento, 0 as dec,
		      sum(Tdebe) as tdebe, sum(THaber)  as tHaber , Anterior as Anterior,
		      '' as NombreSuc,
		       '' as NombreCentro
		from   cn_vasientos asi 
		left join #xSaldoEjercicio xSE on asi.cuenta=xSE.cuenta 
		where  (asi.fecha between @dFecha and @hFecha) AND
			asi.sucursal like rtrim(@sucursal)  AND
			(asi.cuenta between @dCuenta and @hCuenta) AND
			asi.idempresa = @empresa AND
			asi.centro like rtrim(@centro)		
		group by asi.cuenta, asi.fecha, asi.CuentaNombre,Anterior
		having Sum(Debe)<>0
		union
		select '2' as n�mero,asi.cuenta, asi.fecha, asi.CuentaNombre as descripci�n, 
			0 as Debe, sum(haber) as Haber,
		     'Por varios'  as nota, 
		  	'' as sucursal, '' as documento, 0 as dec,
		      sum(Tdebe) as tdebe, sum(tHaber)as tHaber , Anterior as Anterior,
		      '' as NombreSuc,
		       '' as NombreCentro
		from   cn_vasientos asi 
		left join #xSaldoEjercicio xSE on asi.cuenta=xSE.cuenta 
		where  (asi.fecha between @dFecha and @hFecha) AND
			asi.sucursal like rtrim(@sucursal)  AND
			(asi.cuenta between @dCuenta and @hCuenta) AND
			asi.idempresa = @empresa AND
			asi.centro like rtrim(@centro)		
		group by asi.cuenta, asi.fecha, asi.CuentaNombre,Anterior
		having Sum(haber)<>0
		order by asi.cuenta, asi.fecha,n�mero
ENDTEXT


sql(cmdSQL,'xMayor')

*= sql(cmdsql,'xMayor')
SELECT xmayor
*!*	linforme = "rmayor_ov"

*!*	IF LCDESTINO='A'
*!*		exportar()
*!*		RETURN .f.
*!*	ENDIF
	

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        al   %   �      �  K   �          �  U  J %�C��  ���! � T��  �� %�� � %�C�� ���F � T�� �� %�� � %�C�� ���k � T�� �� 0�� � %�C�� ���� � T�� �� 99999999999�� � M(� �r �l 	Declare @Empresa char(3), @IE datetime, @FE datetime, @dFecha datetime, @hFecha datetime, @dCuenta char(9),�= �7 	  @hCuenta char(9), @sucursal char(2),@centro char(5) �A �; 	SELECT @Empresa=?oApp.Empresa,  @IE=?oApp.InicioEjercicio,�G �A 				@FE=?oApp.FinalEjercicio,@dFecha=?m.dFecha,@hFecha=?m.hFecha,�[ �U 				@dCuenta= ?m.dCuenta,@hCuenta=?m.hCuenta,@sucursal=?m.sucursal,@centro=?m.centro � �  �1 �+ 		-- Calcula el saldo del Ejercicio 			(VG)�5 �/ 		select det.cuenta, sum( det.debe ) as Tdebe, �$ � 			sum( det.haber ) as Thaber,�f �` 			sum(case when asi.fecha  between  @IE and @dFecha - 1 then debe-haber else 0 end) as Anterior� � 		into #xSaldoEjercicio�. �( 		from   cn_asientos asi, cn_detalle det�0 �* 		where  asi.idasiento = det.idasiento AND�' �! 		(asi.fecha between @IE and @FE)�2 �, 			   AND asi.sucursal like rtrim(@sucursal)�. �( 			   AND det.centro like rtrim(@centro)�@ �: 			   AND  (det.cuenta between @dcuenta and @hCuenta) AND �! � 			asi.idempresa = @empresa� � 		group by cuenta       � � 		order by cuenta;� �  �U �O 		select '1' as n�mero,asi.cuenta, asi.fecha, asi.CuentaNombre as descripci�n, �' �! 			sum(debe) as Debe, 0 as Haber,�" � 		     'A varios'  as nota, �5 �/ 		  	'' as sucursal, '' as documento, 0 as dec,�Q �K 		      sum(Tdebe) as tdebe, sum(THaber)  as tHaber , Anterior as Anterior,� � 		      '' as NombreSuc,�! � 		       '' as NombreCentro�  � 		from   cn_vasientos asi �@ �: 		left join #xSaldoEjercicio xSE on asi.cuenta=xSE.cuenta �: �4 		where  (asi.fecha between @dFecha and @hFecha) AND�0 �* 			asi.sucursal like rtrim(@sucursal)  AND�7 �1 			(asi.cuenta between @dCuenta and @hCuenta) AND�% � 			asi.idempresa = @empresa AND�) �# 			asi.centro like rtrim(@centro)		�A �; 		group by asi.cuenta, asi.fecha, asi.CuentaNombre,Anterior� � 		having Sum(Debe)<>0� � 		union�U �O 		select '2' as n�mero,asi.cuenta, asi.fecha, asi.CuentaNombre as descripci�n, �( �" 			0 as Debe, sum(haber) as Haber,�$ � 		     'Por varios'  as nota, �5 �/ 		  	'' as sucursal, '' as documento, 0 as dec,�O �I 		      sum(Tdebe) as tdebe, sum(tHaber)as tHaber , Anterior as Anterior,� � 		      '' as NombreSuc,�! � 		       '' as NombreCentro�  � 		from   cn_vasientos asi �@ �: 		left join #xSaldoEjercicio xSE on asi.cuenta=xSE.cuenta �: �4 		where  (asi.fecha between @dFecha and @hFecha) AND�0 �* 			asi.sucursal like rtrim(@sucursal)  AND�7 �1 			(asi.cuenta between @dCuenta and @hCuenta) AND�% � 			asi.idempresa = @empresa AND�) �# 			asi.centro like rtrim(@centro)		�A �; 		group by asi.cuenta, asi.fecha, asi.CuentaNombre,Anterior� � 		having Sum(haber)<>0�- �' 		order by asi.cuenta, asi.fecha,n�mero� � ��C � � xMayor� �� F� � U  SUCURSAL CENTRO DCUENTA HCUENTA CMDSQL SQL XMAYOR
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 A A A �A � !�q�a QAa��q!���a Qq!Q��qQ��� Q�AQ���qQ���A ss : q 2                       �     I   
    Z    )   �                  