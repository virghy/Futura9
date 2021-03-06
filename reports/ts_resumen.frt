  %]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      "cTesor"      	"cVentas"      
"cCompras"      Arial      Arial      Arial      Arial      Arial      Arial      ""Resumen Financiero al ",getdate()      Arial      empresa             Arial      5"Moneda: ",cMonedas.IdMoneda,' ',cMonedas.Descripcion      Arial      "Saldo Conciliado"      Arial      "Saldo Actual"      Arial      "Saldo a Vencer"      Arial      "Total"      Arial      "Tesorer�a:"      Arial      "Cuenta"      Arial      cTesor.NroCuenta,cTesor.Nombre      Arial      cTesor.SaldoConciliado      "999,999,999,999.99"      Arial      cTesor.SaldoActual      "999,999,999,999.99"      Arial      cTesor.SaldoFinal      "999,999,999,999.99"      Arial      &cTesor.SaldoActual + cTesor.SaldoFinal      "999,999,999,999.99"      Arial      cTesor.SaldoConciliado      "999,999,999,999.99"      Arial      cTesor.SaldoActual      "999,999,999,999.99"      Arial      cTesor.SaldoFinal      "999,999,999,999.99"      Arial      &cTesor.SaldoActual + cTesor.SaldoFinal      "999,999,999,999.99"      Arial      	"Totales"      Arial      "Cuentas a Cobrar:"      Arial      cVentas.SaldoActual      "999,999,999,999.99"      Arial      cVentas.SaldoFinal      "999,999,999,999.99"      Arial      (cVentas.SaldoActual + cVentas.SaldoFinal      "999,999,999,999.99"      Arial      cCompras.SaldoActual      "999,999,999,999.99"      Arial      cCompras.SaldoFinal      "999,999,999,999.99"      Arial      *cCompras.SaldoActual + cCompras.SaldoFinal      "999,999,999,999.99"      Arial      "Cuentas a Pagar:"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
TEXT TO cmdSQL
	SELECT  * FROM bs_Monedas ORDER BY IdMoneda                                            
ENDTEXT

=sql(cmdSQL,'cMonedas')
SELECT cMonedas

TEXT TO cmdSQL NOSHOW 
	SELECT     s.IdCuenta, 
	SUM(ISNULL(s.credito, 0) - ISNULL(s.debito, 0)) AS SaldoActual, 
	SUM(ISNULL(s.credito1, 0) - ISNULL(s.debito1, 0)) AS SaldoFinal, 
	SUM(ISNULL(s.credito2, 0) - ISNULL(s.debito2, 0)) AS SaldoConciliado, 
	
	c.nrocuenta, c.nombre, c.idbanco, c.idmoneda, Banco=b.descripcion, Tipo=t.Descripcion
	FROM         (SELECT     d.idcuenta AS IdCuenta, 
						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() then dt.importe else 0 end) AS debito, 
						SUM(case when isnull(d.FechaDiferida,d.Fecha)>getdate() then dt.importe else 0 end) AS debito1, 
						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() and dt.IdEstado='2' then dt.importe else 0 end) AS debito2, 
						
						$0 AS credito,
						$0 AS credito1,
						$0 as Credito2
						FROM         ts_detdepos_base AS dt INNER JOIN
											  ts_depositos_base AS d ON d.iddeposito = dt.iddeposito
						WHERE     (NOT (d.idcuenta IS NULL)) and d.IdEmpresa=?oApp.Empresa
						GROUP BY d.idcuenta
						union
						SELECT     d.idcuenta_ent AS IdCuenta, 
						$0 AS Debito,
						$0 AS Debito1,
						$0 AS Debito2,
						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() then dt.importe else 0 end) AS Credito, 
						SUM(case when isnull(d.FechaDiferida,d.Fecha)>getdate() then dt.importe else 0 end) AS Credito1,
						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() and dt.IdEstado='2' then dt.importe else 0 end) AS Credito2

						FROM         ts_detdepos_base AS dt INNER JOIN
											  ts_depositos_base AS d ON d.iddeposito = dt.iddeposito
						WHERE     (NOT (d.idcuenta_ent IS NULL)) and d.IdEmpresa=?oApp.Empresa
						GROUP BY d.idcuenta_ent) AS s INNER JOIN
	                      ts_cuentas AS c ON s.IdCuenta = c.idcuenta INNER JOIN
	                      bs_bancos AS b ON c.idbanco = b.idbanco
	                      left join ts_Tipo t on c.IdTipo = t.IdTipo 
	                      WHERE c.IdEmpresa=?oApp.Empresa
	GROUP BY s.IdCuenta, c.nrocuenta, c.nombre, t.Descripcion, c.idbanco, c.idmoneda, b.descripcion
	order by IdMoneda,t.Descripcion,Nombre

ENDTEXT

=sql(cmdSQL,'cTesor')
TEXT TO cmdSQL NOSHOW
	Select IdMoneda,
	sum(case when Vencimiento<=GetDate() then Saldo else 0 end) as SaldoActual,
	sum(case when Vencimiento>=GetDate() then Saldo else 0 end) as SaldoFinal
	from vt_Forma_Pago s 
	where s.IdEmpresa=?oApp.Empresa
	group by Idmoneda

ENDTEXT

=sql(cmdSQL,'cVentas')
TEXT TO cmdSQL NOSHOW
	Select IdMoneda,
	sum(case when Vencimiento<=GetDate() then Saldo else 0 end) as SaldoActual,
	sum(case when Vencimiento>=GetDate() then Saldo else 0 end) as SaldoFinal
	from cp_Forma_Pago s 
	where s.IdEmpresa=?oApp.Empresa
	group by Idmoneda
ENDTEXT

=sql(cmdSQL,'cCompras')

SELECT cCompras
INDEX on IdMoneda TAG Nro

SELECT cVentas
INDEX on IdMoneda TAG Nro

SELECT cTesor
INDEX on IdMoneda TAG Nro

SELECT cMonedas
SET RELATION TO IdMoneda INTO cVentas ADDITIVE  
SET RELATION TO IdMoneda INTO cCompras ADDITIVE  
SET RELATION TO IdMoneda INTO cTesor ADDITIVE  
ENDPROC
     A���    (  (                        @r   %   	      �  O   1          �  U  
  �  � U  SETEO� M(�  �^ �X 	SELECT  * FROM bs_Monedas ORDER BY IdMoneda                                            � � ��C �  � cMonedas� �� F� �	 M(�  �� � 	SELECT     s.IdCuenta, �G �A 	SUM(ISNULL(s.credito, 0) - ISNULL(s.debito, 0)) AS SaldoActual, �H �B 	SUM(ISNULL(s.credito1, 0) - ISNULL(s.debito1, 0)) AS SaldoFinal, �M �G 	SUM(ISNULL(s.credito2, 0) - ISNULL(s.debito2, 0)) AS SaldoConciliado, � � 	�\ �V 	c.nrocuenta, c.nombre, c.idbanco, c.idmoneda, Banco=b.descripcion, Tipo=t.Descripcion�8 �2 	FROM         (SELECT     d.idcuenta AS IdCuenta, �l �f 						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() then dt.importe else 0 end) AS debito, �l �f 						SUM(case when isnull(d.FechaDiferida,d.Fecha)>getdate() then dt.importe else 0 end) AS debito1, �� �{ 						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() and dt.IdEstado='2' then dt.importe else 0 end) AS debito2, � � 						� � 						$0 AS credito,� � 						$0 AS credito1,� � 						$0 as Credito2�: �4 						FROM         ts_detdepos_base AS dt INNER JOIN�I �C 											  ts_depositos_base AS d ON d.iddeposito = dt.iddeposito�N �H 						WHERE     (NOT (d.idcuenta IS NULL)) and d.IdEmpresa=?oApp.Empresa� � 						GROUP BY d.idcuenta� � 						union�3 �- 						SELECT     d.idcuenta_ent AS IdCuenta, � � 						$0 AS Debito,� � 						$0 AS Debito1,� � 						$0 AS Debito2,�m �g 						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() then dt.importe else 0 end) AS Credito, �l �f 						SUM(case when isnull(d.FechaDiferida,d.Fecha)>getdate() then dt.importe else 0 end) AS Credito1,�� �z 						SUM(case when isnull(d.FechaDiferida,d.Fecha)<=getdate() and dt.IdEstado='2' then dt.importe else 0 end) AS Credito2� �  �: �4 						FROM         ts_detdepos_base AS dt INNER JOIN�I �C 											  ts_depositos_base AS d ON d.iddeposito = dt.iddeposito�R �L 						WHERE     (NOT (d.idcuenta_ent IS NULL)) and d.IdEmpresa=?oApp.Empresa�4 �. 						GROUP BY d.idcuenta_ent) AS s INNER JOIN�R �L 	                      ts_cuentas AS c ON s.IdCuenta = c.idcuenta INNER JOIN�D �> 	                      bs_bancos AS b ON c.idbanco = b.idbanco�H �B 	                      left join ts_Tipo t on c.IdTipo = t.IdTipo �< �6 	                      WHERE c.IdEmpresa=?oApp.Empresa�f �` 	GROUP BY s.IdCuenta, c.nrocuenta, c.nombre, t.Descripcion, c.idbanco, c.idmoneda, b.descripcion�- �' 	order by IdMoneda,t.Descripcion,Nombre� �  � � ��C �  � cTesor� ��	 M(�  �� � 	Select IdMoneda,�R �L 	sum(case when Vencimiento<=GetDate() then Saldo else 0 end) as SaldoActual,�P �J 	sum(case when Vencimiento>=GetDate() then Saldo else 0 end) as SaldoFinal� � 	from vt_Forma_Pago s �& �  	where s.IdEmpresa=?oApp.Empresa� � 	group by Idmoneda� �  � � ��C �  � cVentas� ��	 M(�  �� � 	Select IdMoneda,�R �L 	sum(case when Vencimiento<=GetDate() then Saldo else 0 end) as SaldoActual,�P �J 	sum(case when Vencimiento>=GetDate() then Saldo else 0 end) as SaldoFinal� � 	from cp_Forma_Pago s �& �  	where s.IdEmpresa=?oApp.Empresa� � 	group by Idmoneda� � ��C �  � cCompras� �� F� � & �� ��� � F� � & �� ��� � F� � & �� ��� � F� � G-(�� ��� � G-(�� ��� � G-(�� ��� � U  CMDSQL SQL CMONEDAS CCOMPRAS IDMONEDA NRO CVENTAS CTESOR BeforeOpenTables,     �� InitA     ��1 q 3 � �A �q � �q��q ����� �������1�����a ��!A!A��a�a A r� q!�a�a A �� q!�a�A �r � r � r � r 1                       &         A         )   (                  