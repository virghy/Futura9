XPROCESO = SYS(2015)


SET STEP ON
CREATE CURSOR Saldos(IdConcepto c(10), Concepto c(50), IdPadre c(10), Importe Y, Orden C(20))

SET DATABASE TO datos
TEXT TO cmdSQL noshow
	SELECT     c.concepto, c.Tipo, ISNULL(c.IdPadre,'') IdPadre, d.IdConcepto, Orden=ISNULL(Orden,''), Importe=SUM(d.Importe) 
	FROM         ts_DepositoConceptoDet AS d INNER JOIN
	                      cn_conceptos AS c ON d.IdEmpresa = c.idempresa AND d.IdConcepto = c.idconcepto
	where d.IdEmpresa = ?oApp.Empresa
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
BROWSE

