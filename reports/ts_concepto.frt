  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      cp_rcompraconcepto.concepto      Arial      Arial      Arial      Arial      Arial      "Movimientos por Concepto"      Arial      empresa             Arial      "Cuenta"      Arial      :iif( Isnull(m.cuenta), 'Todos', nvl(CuentaEnt,CuentaSal) )      Arial      
"Per�odo:"      Arial      m.dfecha, "  al " ,m.hfecha      Arial      	"Fecha
"      Arial      "Cheque"      Arial      "Operacion"      Arial      "Referencia"      "@I"      Arial      "Cuenta"      Arial      	"Importe"      Arial      "Fecha Dif.
"      Arial      
"Concepto"      Arial      !SOLORESUMEN      cp_rcompraconcepto.concepto      Arial      !SOLORESUMEN      cp_rcompraconcepto.fecha      Arial      	NroCheque      Arial      NroOperacion      Arial      FechaDiferida      Arial      
Referencia      Arial      Importe      "999,999,999,999.99"      Arial      Nvl(CuentaEnt,CuentaSal)      Arial      Concepto      Arial      Importe      "99,999,999,999,999.99"      Arial      "Total"      Arial      !SOLORESUMEN      !SoloResumen      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Importe      "999,999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      �Top = 95
Left = 13
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraconcepto"
DataSource = .NULL.
Name = "Dataenvironment"
     BPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.IdConcepto)
	m.IdConcepto = null
ENDIF

IF EMPTY(m.cuenta)
	m.cuenta = null
ENDIF

TEXT TO cmdSQL

	SELECT     TOP 100 PERCENT a.idconcepto + '-' + c.concepto AS Concepto, a.fecha, a.nrooperacion, a.idcuenta, a.idcuenta_ent, a.FechaDiferida, 
	                      ISNULL(a.totalcheque, 0) + ISNULL(a.totalefectivo, 0) AS Importe, a.referencia, a.nroasiento, a.nroasiento2,
	                          (SELECT     TOP 1 NroCheque
	                            FROM          dbo.ts_detdepos_base td
	                            WHERE      a.IdDeposito = td.IdDeposito) AS NroCheque, cSal.Nombre AS CuentaSal, cEnt.Nombre AS CuentaEnt
	FROM         dbo.ts_depositos_base a INNER JOIN
	                      dbo.cn_conceptos c ON a.idempresa = c.idempresa AND a.idconcepto = c.idconcepto LEFT OUTER JOIN
	                      dbo.ts_cuentas cSal ON a.idcuenta = cSal.idcuenta AND a.idempresa = cSal.idempresa LEFT OUTER JOIN
	                      dbo.ts_cuentas cEnt ON a.idcuenta_ent = cEnt.idcuenta AND a.idempresa = cEnt.idempresa
	                      where a.IdEmpresa = ?oApp.Empresa
	                      and a.fecha between ?m.dFecha and ?m.hFecha 
	                     and  (a.IdConcepto = ?m.IdConcepto or ?m.IdConcepto is null)
	                     and (a.IdCuenta =?m.cuenta or a.IdCuenta_Ent=?m.Cuenta or ?m.cuenta is null)
	ORDER BY a.idconcepto + '-' + c.concepto, a.fecha, a.nrooperacion
ENDTEXT

IF sql(cmdSQL,'cp_rcompraconcepto')>0
	SELECT cp_rcompraconcepto
ENDIF

ENDPROC
     @���    '  '                        {W   %   j      �     �          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � M(� � �  �� �� 	SELECT     TOP 100 PERCENT a.idconcepto + '-' + c.concepto AS Concepto, a.fecha, a.nrooperacion, a.idcuenta, a.idcuenta_ent, a.FechaDiferida, �� �� 	                      ISNULL(a.totalcheque, 0) + ISNULL(a.totalefectivo, 0) AS Importe, a.referencia, a.nroasiento, a.nroasiento2,�< �6 	                          (SELECT     TOP 1 NroCheque�H �B 	                            FROM          dbo.ts_detdepos_base td�� �� 	                            WHERE      a.IdDeposito = td.IdDeposito) AS NroCheque, cSal.Nombre AS CuentaSal, cEnt.Nombre AS CuentaEnt�6 �0 	FROM         dbo.ts_depositos_base a INNER JOIN�| �v 	                      dbo.cn_conceptos c ON a.idempresa = c.idempresa AND a.idconcepto = c.idconcepto LEFT OUTER JOIN� �y 	                      dbo.ts_cuentas cSal ON a.idcuenta = cSal.idcuenta AND a.idempresa = cSal.idempresa LEFT OUTER JOIN�s �m 	                      dbo.ts_cuentas cEnt ON a.idcuenta_ent = cEnt.idcuenta AND a.idempresa = cEnt.idempresa�> �8 	                      where a.IdEmpresa = ?oApp.Empresa�I �C 	                      and a.fecha between ?m.dFecha and ?m.hFecha �X �R 	                     and  (a.IdConcepto = ?m.IdConcepto or ?m.IdConcepto is null)�h �b 	                     and (a.IdCuenta =?m.cuenta or a.IdCuenta_Ent=?m.Cuenta or ?m.cuenta is null)�H �B 	ORDER BY a.idconcepto + '-' + c.concepto, a.fecha, a.nrooperacion� �+ %�C � � cp_rcompraconcepto� � ���� F� � � U 
 IDCONCEPTO CUENTA CMDSQL SQL CP_RCOMPRACONCEPTO BeforeOpenTables,     �� InitA     ��1 q 3 � A � A � a Q	����a��1�����A �q A 2                       &         A   7      )   '                  