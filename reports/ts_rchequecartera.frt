  4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=1
COLOR=2
      Arial      ts_rchequecartera.idmoneda      ts_rchequecartera.fchcheque      Arial      Arial      Arial      Arial      Arial      "Cheques en Cartera"             Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Importe"      Arial      "Fecha Emisi�n"      Arial      	"Titular"      Arial      "Fecha Cobro"      Arial      "Tipo
"      Arial      "Banco"      Arial      "Nro."      Arial      "Moneda:
"      Arial      ts_rchequecartera.idmoneda             Arial      Moneda             Arial      ts_rchequecartera.fchcheque             Arial      	tipovalor             Arial      ts_rchequecartera.banco             Arial      ts_rchequecartera.fchemision             Arial      	NroCheque      Arial      ts_rchequecartera.importe      "@Z 999,999,999.99"             Arial      ts_rchequecartera.nombre             Arial      "Total fecha"             Arial      ts_rchequecartera.importe      "@Z 999,999,999.99"             Arial      "Total Moneda"             Arial      ts_rchequecartera.importe      "@Z 999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 243
Left = 129
Width = 520
Height = 252
InitialSelectedAlias = "ts_rchequecartera"
DataSource = .NULL.
Name = "Dataenvironment"
     	PROCEDURE BeforeOpenTables
DO seteo
ENDPROC
PROCEDURE Init



TEXT TO cmdSQL noshow
SELECT     b.descripcion AS Banco,a.Importe, a.IdMoneda, m.Descripcion AS Moneda, t.tipovalor, a.nrocheque, a.idbanco, a.fchemision, a.fchcheque, a.idestado, a.nombre
FROM         dbo.ts_valores_base a INNER JOIN
                      dbo.bs_Monedas m ON m.IdMoneda = a.idmoneda INNER JOIN
                      dbo.ts_tipovalor t ON t.idtipovalor = a.idtipovalor LEFT OUTER JOIN
                      dbo.bs_bancos b ON a.idbanco = b.idbanco
where fchCheque between ?m.dFecha and ?m.hFecha and isnull(idestado,1)=1 and a.IdEmpresa=?oApp.Empresa
order by a.IdMoneda, fchCheque                      
ENDTEXT

sql(cmdSQL,'ts_rchequecartera')
SELECT ts_rchequecartera
ENDPROC
     ����    �  �                        �   %         ^     @          �  U  
  �  � U  SETEO�	 M(�  ��� �� SELECT     b.descripcion AS Banco,a.Importe, a.IdMoneda, m.Descripcion AS Moneda, t.tipovalor, a.nrocheque, a.idbanco, a.fchemision, a.fchcheque, a.idestado, a.nombre�3 �- FROM         dbo.ts_valores_base a INNER JOIN�R �L                       dbo.bs_Monedas m ON m.IdMoneda = a.idmoneda INNER JOIN�_ �Y                       dbo.ts_tipovalor t ON t.idtipovalor = a.idtipovalor LEFT OUTER JOIN�D �>                       dbo.bs_bancos b ON a.idbanco = b.idbanco�l �f where fchCheque between ?m.dFecha and ?m.hFecha and isnull(idestado,1)=1 and a.IdEmpresa=?oApp.Empresa�: �4 order by a.IdMoneda, fchCheque                      � �" ��C �  � ts_rchequecartera� �� F� � U  CMDSQL SQL TS_RCHEQUECARTERA BeforeOpenTables,     �� InitA     ��1 q 2 � �
1!�A��A "q 1                       $         ?   �      )   �                  