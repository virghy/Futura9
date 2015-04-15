  =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      idmoneda      Arial      Arial      Arial      Arial      Arial      "Saldo Consolidado de Cuentas"             Arial      empresa             Arial      "Nro.Cuenta"      Arial      	"Titular"      Arial      "Banco"      Arial      "Tipo Cuenta"      Arial      "Saldo"      Arial      
"Moneda
"      Arial      idmoneda             Arial      	nrocuenta             Arial      nombre             Arial      banco             Arial      idtipo             Arial      nvl(saldo,0)      "@Z 999,999,999,999.99"             Arial      nvl(saldo,0)      "@Z 999,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 81
Left = 180
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
*sql("exec ts_rconsolidado ?oApp.Empresa","rCheques")
TEXT TO cmdSQL noshow
SELECT     cuentas.idcuenta, cuentas.nrocuenta, cuentas.nombre, cuentas.idbanco, cuentas.idtipo, cuentas.idmoneda, 
t.descripcion AS tipo, b.descripcion AS banco,
sum(Debito-Credito) as Saldo
FROM         dbo.ts_cuentas cuentas INNER JOIN
                      dbo.ts_tipo t ON cuentas.idtipo = t.idtipo INNER JOIN
                      dbo.bs_bancos b ON cuentas.idbanco = b.idbanco
inner join (
SELECT IdCuenta =Ts_depositos.idcuenta_ent, 
sum(case when Ts_depositos.idcuenta_ent>0 then Ts_detdepos.importe else 0 end) AS debito,  
0 AS credito 
FROM ts_detdepos_base ts_detdepos   
INNER JOIN ts_depositos_base ts_depositos   
on  Ts_depositos.iddeposito = Ts_detdepos.iddeposito   
where isnull(Ts_depositos.fechaDiferida,Ts_depositos.fecha) <= ?m.hfecha 
and ts_depositos.idempresa = ?oApp.Empresa
group by Ts_depositos.idcuenta_ent
union 
SELECT IdCuenta =Ts_depositos.idcuenta, 
0 as Debito,
sum(case when Ts_depositos.idcuenta>0 then Ts_detdepos.importe else 0 end) AS Credito  
FROM ts_detdepos_base ts_detdepos   
INNER JOIN ts_depositos_base ts_depositos   
on  Ts_depositos.iddeposito = Ts_detdepos.iddeposito   
where isnull(Ts_depositos.fechaDiferida,Ts_depositos.fecha) <= ?m.hfecha 
and ts_depositos.idempresa = ?oApp.Empresa
group by Ts_depositos.idcuenta
) s on cuentas.IdCuenta = s.IdCuenta                      
where cuentas.idempresa= ?oApp.Empresa 
group by cuentas.idcuenta, cuentas.nrocuenta, cuentas.nombre, cuentas.idbanco, cuentas.idtipo, cuentas.idmoneda, 
t.descripcion, b.descripcion 
order by idmoneda,cuentas.nrocuenta,cuentas.idtipo
ENDTEXT
sql(cmdSQL,"rCheques")

SELECT rcheques

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     .���                              ��   %   F      �  '   n          �  U  �	 M(�  ��y �s SELECT     cuentas.idcuenta, cuentas.nrocuenta, cuentas.nombre, cuentas.idbanco, cuentas.idtipo, cuentas.idmoneda, �4 �. t.descripcion AS tipo, b.descripcion AS banco,�" � sum(Debito-Credito) as Saldo�4 �. FROM         dbo.ts_cuentas cuentas INNER JOIN�Q �K                       dbo.ts_tipo t ON cuentas.idtipo = t.idtipo INNER JOIN�J �D                       dbo.bs_bancos b ON cuentas.idbanco = b.idbanco� � inner join (�2 �, SELECT IdCuenta =Ts_depositos.idcuenta_ent, �a �[ sum(case when Ts_depositos.idcuenta_ent>0 then Ts_detdepos.importe else 0 end) AS debito,  � � 0 AS credito �* �$ FROM ts_detdepos_base ts_detdepos   �2 �, INNER JOIN ts_depositos_base ts_depositos   �= �7 on  Ts_depositos.iddeposito = Ts_detdepos.iddeposito   �O �I where isnull(Ts_depositos.fechaDiferida,Ts_depositos.fecha) <= ?m.hfecha �0 �* and ts_depositos.idempresa = ?oApp.Empresa�( �" group by Ts_depositos.idcuenta_ent� � union �. �( SELECT IdCuenta =Ts_depositos.idcuenta, � � 0 as Debito,�] �W sum(case when Ts_depositos.idcuenta>0 then Ts_detdepos.importe else 0 end) AS Credito  �* �$ FROM ts_detdepos_base ts_detdepos   �2 �, INNER JOIN ts_depositos_base ts_depositos   �= �7 on  Ts_depositos.iddeposito = Ts_detdepos.iddeposito   �O �I where isnull(Ts_depositos.fechaDiferida,Ts_depositos.fecha) <= ?m.hfecha �0 �* and ts_depositos.idempresa = ?oApp.Empresa�$ � group by Ts_depositos.idcuenta�@ �: ) s on cuentas.IdCuenta = s.IdCuenta                      �- �' where cuentas.idempresa= ?oApp.Empresa �w �q group by cuentas.idcuenta, cuentas.nrocuenta, cuentas.nombre, cuentas.idbanco, cuentas.idtipo, cuentas.idmoneda, �# � t.descripcion, b.descripcion �8 �2 order by idmoneda,cuentas.nrocuenta,cuentas.idtipo� � ��C �  � rCheques� �� F� � U  CMDSQL SQL RCHEQUES
  �  � U  SETEO Init,     �� BeforeOpenTables1    ��1 � �A!A�!!1�!���� �!��!��A�q1�A �r 3 q 2                       �     %   �    )    )                     