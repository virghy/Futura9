  ^                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      saldos.IdCuenta      saldos.centro      estado       iif(substr(cuenta,1,1)='4',-1,1)      0      v2      Liif( saldos.mes1 = 0 or mes2 = 0 , 0, ((mes1+mes2)*100/mes1) - 100) * estado      0      v3      Eiif( saldos.mes2 = 0 or mes3 = 0 , 0, (mes3*100/mes2) - 100) * estado      0      v4      Eiif( saldos.mes3 = 0 or mes4 = 0 , 0, (mes4*100/mes3) - 100) * estado      0      v5      Eiif( saldos.mes4 = 0 or mes5 = 0 , 0, (mes5*100/mes4) - 100) * estado      0      v6      Eiif( saldos.mes5 = 0 or mes6 = 0 , 0, (mes6*100/mes5) - 100) * estado      0      meses1      saldos.mes1      0      meses2      saldos.mes1+saldos.mes2      0      meses3      #saldos.mes1+saldos.mes2+saldos.mes3      0      meses4      /saldos.mes1+saldos.mes2+saldos.mes3+saldos.mes4      0      meses5      ;saldos.mes1+saldos.mes2+saldos.mes3+saldos.mes4+saldos.mes5      0      meses6      Gsaldos.mes1+saldos.mes2+saldos.mes3+saldos.mes4+saldos.mes5+saldos.mes6      0      Arial      Arial      Arial      Arial      Arial      Arial      ,"Analisis de Presupuesto: ", cCuentas.Titulo      Arial      	m.empresa             Arial      Tiif(empty(m.sucursal),"Consolidado",m.sucursal + " - " +  Sucursal_base.descripci�n)             Arial      Hiif(empty(m.centro),"Todos",m.centro+ " - " +  Centros_base.descripci�n)      Arial      "Sucursal:"      Arial      	"Centro:"      Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      ,"Nivel de Cuentas: " + alltrim(str(m.nivel))             Arial      ?'Moneda: '+iif(m.tipoMoneda='L','Miles de Guaranies','Dolares')             Arial      "Cuenta"      Arial      	"Inicial"      "@J"             Arial      m.IncluirSaldo="S"      "Ene"      "@J"             Arial      "Feb"      "@J"             Arial      "Mar"      "@J"             Arial      "Abr"      "@J"             Arial      "May"      "@J"             Arial      "Jun"      "@J"             Arial      "Jul"      "@J"             Arial      "Ago"      "@J"             Arial      "Set"      "@J"             Arial      "Oct"      "@J"             Arial      "Nov"      "@J"             Arial      "Dic"      "@J"             Arial      "Total"      "@J"             Arial      "Acumulado"      "@J"             Arial      Saldos.IdCuenta,c2      Arial      !saldos.cuenta, saldos.descripci�n             Arial       round(saldos.inicial * estado,0)      "999,999,999"             Arial      m.IncluirSaldo="S"      round(saldos.mes1 * estado,0)      "999,999,999"             Arial      round((saldos.mes2) * estado,0)      "999,999,999"             Arial      meslimite >=2      round((saldos.mes3) * estado,0)      "999,999,999"             Arial      meslimite >=2      round((saldos.mes4) * estado,0)      "999,999,999"             Arial      meslimite >=3      round((saldos.mes5) * estado,0)      "999,999,999"             Arial      meslimite >=3      round((saldos.mes6)* estado,0)      "999,999,999"             Arial      meslimite >=4      round((saldos.mes7)* estado,0)      "999,999,999"             Arial      meslimite >=4      round((saldos.mes8) * estado,0)      "999,999,999"             Arial      meslimite >=5      round((saldos.mes9) * estado,0)      "999,999,999"             Arial      meslimite >=5       round((saldos.mes10) * estado,0)      "999,999,999"             Arial      meslimite >=6       round((saldos.mes11) * estado,0)      "999,999,999"             Arial      meslimite >=6       round((saldos.mes12) * estado,0)      "999,999,999"             Arial      meslimite >=6      ground(( mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0)      "9,999,999,999"             Arial      pround((inicial + mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0)      "9,999,999,999"             Arial      centros_base.descripci�n             Arial       round(saldos.inicial * estado,0)      "999,999,999"             Arial      m.IncluirSaldo="S"      round(saldos.mes1 * estado,0)      "999,999,999"             Arial      round((saldos.mes2) * estado,0)      "999,999,999"             Arial      meslimite >=2      round((saldos.mes3) * estado,0)      "999,999,999"             Arial      meslimite >=2      round((saldos.mes4) * estado,0)      "999,999,999"             Arial      meslimite >=3      round((saldos.mes5) * estado,0)      "999,999,999"             Arial      meslimite >=3      round((saldos.mes6)* estado,0)      "999,999,999"             Arial      meslimite >=4      round((saldos.mes7)* estado,0)      "999,999,999"             Arial      meslimite >=4      round((saldos.mes8) * estado,0)      "999,999,999"             Arial      meslimite >=5      round((saldos.mes9) * estado,0)      "999,999,999"             Arial      meslimite >=5       round((saldos.mes10) * estado,0)      "999,999,999"             Arial      meslimite >=6       round((saldos.mes11) * estado,0)      "999,999,999"             Arial      meslimite >=6       round((saldos.mes12) * estado,0)      "999,999,999"             Arial      meslimite >=6      fround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0)      "9,999,999,999"             Arial      pround((inicial + mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0)      "9,999,999,999"             Arial      'Total ',c2             Arial       round(saldos.inicial * estado,0)      "999,999,999"             Arial      m.IncluirSaldo="S"      round(saldos.mes1 * estado,0)      "999,999,999"             Arial      round((saldos.mes2) * estado,0)      "999,999,999"             Arial      meslimite >=2      round((saldos.mes3) * estado,0)      "999,999,999"             Arial      meslimite >=2      round((saldos.mes4) * estado,0)      "999,999,999"             Arial      meslimite >=3      round((saldos.mes5) * estado,0)      "999,999,999"             Arial      meslimite >=3      round((saldos.mes6)* estado,0)      "999,999,999"             Arial      meslimite >=4      round((saldos.mes7)* estado,0)      "999,999,999"             Arial      meslimite >=4      round((saldos.mes8) * estado,0)      "999,999,999"             Arial      meslimite >=5      round((saldos.mes9) * estado,0)      "999,999,999"             Arial      meslimite >=5       round((saldos.mes10) * estado,0)      "999,999,999"             Arial      meslimite >=6       round((saldos.mes11) * estado,0)      "999,999,999"             Arial      meslimite >=6       round((saldos.mes12) * estado,0)      "999,999,999"             Arial      meslimite >=6      fround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0)      "9,999,999,999"             Arial      pround((inicial + mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0)      "9,999,999,999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total Ingresos"             Arial      -iif(factor=1,round(saldos.mes1 * estado,0),0)      "999,999,999"             Arial      -iif(factor=1,round(saldos.mes2 * estado,0),0)      "999,999,999"             Arial      meslimite >=2      -iif(factor=1,round(saldos.mes3 * estado,0),0)      "999,999,999"             Arial      meslimite >=2      -iif(factor=1,round(saldos.mes4 * estado,0),0)      "999,999,999"             Arial      meslimite >=3      -iif(factor=1,round(saldos.mes5 * estado,0),0)      "999,999,999"             Arial      meslimite >=3      -iif(factor=1,round(saldos.mes6 * estado,0),0)      "999,999,999"             Arial      meslimite >=4      -iif(factor=1,round(saldos.mes7 * estado,0),0)      "999,999,999"             Arial      meslimite >=4      -iif(factor=1,round(saldos.mes8 * estado,0),0)      "999,999,999"             Arial      meslimite >=5      -iif(factor=1,round(saldos.mes9 * estado,0),0)      "999,999,999"             Arial      meslimite >=5      .iif(factor=1,round(saldos.mes10 * estado,0),0)      "999,999,999"             Arial      meslimite >=6      .iif(factor=1,round(saldos.mes11 * estado,0),0)      "999,999,999"             Arial      meslimite >=6      .iif(factor=1,round(saldos.mes12 * estado,0),0)      "999,999,999"             Arial      meslimite >=6      viif(factor=1,round((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0),0)      "9,999,999,999"             Arial      "Total Egresos"             Arial      .iif(factor=-1,round(saldos.mes1 * estado,0),0)      "999,999,999"             Arial      .iif(factor=-1,round(saldos.mes2 * estado,0),0)      "999,999,999"             Arial      meslimite >=2      .iif(factor=-1,round(saldos.mes3 * estado,0),0)      "999,999,999"             Arial      meslimite >=2      .iif(factor=-1,round(saldos.mes4 * estado,0),0)      "999,999,999"             Arial      meslimite >=3      .iif(factor=-1,round(saldos.mes5 * estado,0),0)      "999,999,999"             Arial      meslimite >=3      .iif(factor=-1,round(saldos.mes6 * estado,0),0)      "999,999,999"             Arial      meslimite >=4      .iif(factor=-1,round(saldos.mes7 * estado,0),0)      "999,999,999"             Arial      meslimite >=4      .iif(factor=-1,round(saldos.mes8 * estado,0),0)      "999,999,999"             Arial      meslimite >=5      .iif(factor=-1,round(saldos.mes9 * estado,0),0)      "999,999,999"             Arial      meslimite >=5      /iif(factor=-1,round(saldos.mes10 * estado,0),0)      "999,999,999"             Arial      meslimite >=6      /iif(factor=-1,round(saldos.mes11 * estado,0),0)      "999,999,999"             Arial      meslimite >=6      /iif(factor=-1,round(saldos.mes12 * estado,0),0)      "999,999,999"             Arial      meslimite >=6      wiif(factor=-1,round((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0),0)      "9,999,999,999"             Arial      'Saldo'             Arial      &round(saldos.mes1 * estado,0) * factor      "999,999,999"             Arial      (round((saldos.mes2) * estado,0) * factor      "999,999,999"             Arial      meslimite >=2      (round((saldos.mes3) * estado,0) * factor      "999,999,999"             Arial      meslimite >=2      (round((saldos.mes4) * estado,0) * factor      "999,999,999"             Arial      meslimite >=3      (round((saldos.mes5) * estado,0) * factor      "999,999,999"             Arial      meslimite >=3      'round((saldos.mes6)* estado,0) * factor      "999,999,999"             Arial      meslimite >=4      'round((saldos.mes7)* estado,0) * factor      "999,999,999"             Arial      meslimite >=4      (round((saldos.mes8) * estado,0) * factor      "999,999,999"             Arial      meslimite >=5      (round((saldos.mes9) * estado,0) * factor      "999,999,999"             Arial      meslimite >=5      )round((saldos.mes10) * estado,0) * factor      "999,999,999"             Arial      meslimite >=6      )round((saldos.mes11) * estado,0) * factor      "999,999,999"             Arial      meslimite >=6      )round((saldos.mes12) * estado,0) * factor      "999,999,999"             Arial      meslimite >=6      oround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 +mes7 +mes8 + mes9 + mes10 +  mes11 + mes12)* estado,0) * factor      "9,999,999,999"             Arial      dataenvironment      �Top = 188
Left = 40
Width = 792
Height = 419
InitialSelectedAlias = "saldos"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
PUBLIC cabeza[12]
PUBLIC meslimite
xproceso = SYS(2015)
SET DATABASE TO datos
SELECT centros_base
INDEX on centro TAG centro
meslimite = MONTH(m.hfecha)
IF m.tipomoneda = 'L'
     m.decimales = 0
ELSE
     m.decimales = 2
ENDIF
*m.idanalisis = 2
TEXT TO cmdSQL
	SELECT cuentas.cuenta, IdCuenta=z.Cuenta,factor, titulo 
	FROM cn_cuentas cuentas, cn_analisisdet z, cn_analisis a 
	WHERE a.idanalisis = z.idanalisis 
	AND cuentas.cuenta like LTRIM(RTRIM(reverse(LTRIM(STR(CONVERT(INT,(reverse(z.cuenta))))))))+'%' 
	AND a.idanalisis = ?m.idanalisis 
	and a.IdEmpresa=?oApp.Empresa
	and cuentas.IdEmpresa=?oApp.Empresa
	and cuentas.Ejercicio = ?oApp.Ejercicio

ENDTEXT
sql(cmdSQL,'cCuentas')
TEXT TO cmdSQL
	SELECT * 
	FROM cn_cuentas cuentas
	WHERE cuentas.IdEmpresa=?oApp.Empresa
	and cuentas.Ejercicio = ?oApp.Ejercicio

ENDTEXT
sql(cmdSQL,'Cuentas')

TEXT TO cmdSQL
	SELECT * 
	FROM cn_Asientos
	WHERE IdEmpresa=?oApp.Empresa
	and Ejercicio = ?oApp.Ejercicio

ENDTEXT
sql(cmdSQL,'asientos_base')

TEXT TO cmdSQL
	SELECT * 
	FROM cn_Detalle
	WHERE IdEmpresa=?oApp.Empresa
	and Ejercicio = ?oApp.Ejercicio

ENDTEXT

sql(cmdSQL,'detalle_base')

TEXT TO cmdSQL
	SELECT cuenta,SUM(Importe) 
	FROM cn_Presupuesto
	WHERE IdEmpresa=?oApp.Empresa
	and Ejercicio = ?oApp.Ejercicio
	and (Centro like ?m.Centro)

ENDTEXT

sql(cmdSQL,'Presupuesto')


*!*	SELECT cuentas.cuenta, idcuenta AS idcuenta, factor, titulo ;
*!*	FROM cuentas, cn_analisis_det z, cn_analisis_base a ;
*!*	WHERE a.idanalisis = z.idanalisis AND cuentas.cuenta = reverse(LTRIM(STR(VAL(reverse(z.idcuenta))))) AND a.idanalisis = m.idanalisis ;
*!*	INTO CURSOR cCuentas

SET ENGINEBEHAVIOR 70
SELECT ccuentas.idcuenta, c2.descripci�n AS c2, detalle.cuenta, centro, factor, ;
SUM(ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales)) AS debe, ;
SUM(ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales)) AS haber, ;
cuentas.integradora, cuentas.nivel, cuentas.descripci�n, ;
SUM(IIF(asientos.tipo = 'A' AND m.incluirsaldo = "S", ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS inicial, ;
SUM(IIF(MONTH(asientos.fecha) = 1 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes1, ;
SUM(IIF(MONTH(asientos.fecha) = 2 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes2, SUM(IIF(MONTH(asientos.fecha) = 3 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes3, SUM(IIF(MONTH(asientos.fecha) = 4 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes4, SUM(IIF(MONTH(asientos.fecha) = 5 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes5, ;
SUM(IIF(MONTH(asientos.fecha) = 6 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes6, SUM(IIF(MONTH(asientos.fecha) = 7 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes7, SUM(IIF(MONTH(asientos.fecha) = 8 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes8, SUM(IIF(MONTH(asientos.fecha) = 9 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes9, ;
SUM(IIF(MONTH(asientos.fecha) = 10 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes10, SUM(IIF(MONTH(asientos.fecha) = 11 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes11, SUM(IIF(MONTH(asientos.fecha) = 12 AND asientos.tipo <> 'A', ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales) - ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1000, cotizacion), m.decimales), $0.0000)) AS mes12 ;
FROM asientos_base asientos, detalle_base detalle, cuentas, cCuentas, cuentas c2 ;
WHERE asientos.tipo <> 'C' AND asientos.idasiento = detalle.idasiento AND detalle.cuenta = ccuentas.cuenta AND asientos.idempresa = oapp.empresa ;
AND ccuentas.cuenta = cuentas.cuenta AND ccuentas.Idcuenta= c2.cuenta AND detalle.centro = m.centro AND asientos.sucursal = m.sucursal ;
AND BETWEEN(asientos.fecha, m.dfecha, m.hfecha) ;
GROUP BY ccuentas.idcuenta, centro, detalle.cuenta ORDER BY factor DESC, ccuentas.idcuenta, centro, detalle.cuenta ;
INTO CURSOR saldos
SELECT saldos
SET RELATION TO centro INTO centros_base

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
      cursor     Comment = "centros_base.centro = m.centro and centros_Base.idempresa = oApp.Empresa"
Top = 164
Left = 159
Height = 92
Width = 96
Alias = "centros_base"
Order = ""
Database = ..\data\datos.dbc
CursorSource = "bs_centros"
Filter = "idEmpresa=oApp.Empresa"
Name = "Cursor10"
      cursor      �Top = 46
Left = 295
Height = 90
Width = 95
Alias = "sucursal_base"
Database = ..\data\datos.dbc
CursorSource = "bs_sucursal"
Filter = "sucursal_base.sucursal = m.sucursal"
Name = "Cursor2"
      cursor      �Top = 20
Left = 430
Height = 90
Width = 96
Alias = "vcentrocosto"
Database = ..\data\datos.dbc
CursorSource = "bs_centros"
Name = "Cursor3"
     ���                              v   %         �  A   )          �  U   7�  ���� 7� � T� �C��]�� G(� datos� F� � & �� ��� � T� �C�� H�� %��� � L��{ � T�� �� �� �� � T�� ���� � M(�	 �? �9 	SELECT cuentas.cuenta, IdCuenta=z.Cuenta,factor, titulo �@ �: 	FROM cn_cuentas cuentas, cn_analisisdet z, cn_analisis a �) �# 	WHERE a.idanalisis = z.idanalisis �g �a 	AND cuentas.cuenta like LTRIM(RTRIM(reverse(LTRIM(STR(CONVERT(INT,(reverse(z.cuenta))))))))+'%' �( �" 	AND a.idanalisis = ?m.idanalisis �$ � 	and a.IdEmpresa=?oApp.Empresa�* �$ 	and cuentas.IdEmpresa=?oApp.Empresa�. �( 	and cuentas.Ejercicio = ?oApp.Ejercicio� �  � � ��C �	 � cCuentas�
 �� M(�	 � �
 	SELECT * � � 	FROM cn_cuentas cuentas�, �& 	WHERE cuentas.IdEmpresa=?oApp.Empresa�. �( 	and cuentas.Ejercicio = ?oApp.Ejercicio� �  � � ��C �	 � Cuentas�
 �� M(�	 � �
 	SELECT * � � 	FROM cn_Asientos�$ � 	WHERE IdEmpresa=?oApp.Empresa�& �  	and Ejercicio = ?oApp.Ejercicio� �  � � ��C �	 � asientos_base�
 �� M(�	 � �
 	SELECT * � � 	FROM cn_Detalle�$ � 	WHERE IdEmpresa=?oApp.Empresa�& �  	and Ejercicio = ?oApp.Ejercicio� �  � � ��C �	 � detalle_base�
 �� M(�	 �" � 	SELECT cuenta,SUM(Importe) � � 	FROM cn_Presupuesto�$ � 	WHERE IdEmpresa=?oApp.Empresa�& �  	and Ejercicio = ?oApp.Ejercicio�" � 	and (Centro like ?m.Centro)� �  � � ��C �	 � Presupuesto�
 ��
 G���F���o� asientos_baseQ� � detalle_baseQ� � cuentas� cCuentas� cuentasQ� �� � ��� � �Q� �� � ��� ��� ��CCC� �}C�� � L� ��� � 6�� T���Q� �CCC� �}C�� � L� ��� � 6�� T���Q� �� � ��� � ��� � ��CC� � � A� �� � S	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q� �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q� �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q� �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q� �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�  �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�! �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�" �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�# �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�$ �CCC� � H�	� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�% �CCC� � H�
� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�& �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�' �CCC� � H�� � � � A	�R CC� �}C�� � L� ��� � 6�� TCC� �}C�� � L� ��� � 6�� T� �        6���Q�( ��� � � C� � �+ � �+ 	� � � � � 	� � �, �- �. 	� � � � � 	� � � � � 	� � � �� 	� � �/ ��/ 	� C� � ��0 �� �	���� � ��� ��� � ���� �<�� � ��� ��� � ���� saldos� F�1 � G-(�� ��� � U2  CABEZA	 MESLIMITE XPROCESO DATOS CENTROS_BASE CENTRO HFECHA
 TIPOMONEDA	 DECIMALES CMDSQL SQL CCUENTAS IDCUENTA C2 DESCRIPCI�N DETALLE CUENTA FACTOR DEBE
 COTIZACION HABER CUENTAS INTEGRADORA NIVEL ASIENTOS TIPO INCLUIRSALDO INICIAL FECHA MES1 MES2 MES3 MES4 MES5 MES6 MES7 MES8 MES9 MES10 MES11 MES12 ASIENTOS_BASE DETALLE_BASE	 IDASIENTO	 IDEMPRESA OAPP EMPRESA SUCURSAL DFECHA SALDOS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � q � q � A� � � A � ��q�A��a A �� ���a A �� qAaa A �� aAaa A �� !�Aa!a A �� �q � 3 q 2                       _     ?   �  �  \    )                     