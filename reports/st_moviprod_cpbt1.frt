  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      deposito      Cantidad      iif(Entrada=0,Salida,Entrada)      0      Arial      Arial      Arial      Arial      Arial      Arial      )"Movimientos por Comprobante Consolidado"      Arial      alltrim( empresa )             Arial      m.dfecha,'al', m.hfecha             Arial      
"Periodo:"      Arial      Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)      Arial      "Dep�sito:"      Arial      descripcion      Arial      
"Comprob:"      Arial      "
"      Arial      "Cantidad "      Arial      
"Producto"      Arial      &alltrim(Idproducto) + " - " + producto      Arial      cantidad      "@Z 999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.deposito)
	m.deposito=.null.
ELSE
	sql("Select Deposito from st_Depositos where IDEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito",'xDeposito')	
ENDIF

TEXT TO cmdSQL noshow
SELECT     m.IdEmpresa, m.IdProducto, m.IdDeposito, SUM(m.Entrada) as Entrada, SUM(m.Salida) as Salida,
			p.Descripcion as Producto, m.IdComprobante + c.Descripcion as Descripcion, c.Titulo as Comprob
FROM         dbo.st_MoviProducto(?oApp.Empresa,null,?m.Deposito,?m.dfecha,?m.hfecha) AS m INNER JOIN
                      st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN
                      st_cpbt_stk AS c ON m.IdEmpresa = c.IdEmpresa AND m.IdComprobante = c.Cpbt_Stk
where IdComprobante = ?m.cpbt_stk     
group by m.IdEmpresa, m.IdProducto, m.IdDeposito, p.Descripcion, m.IdComprobante + c.Descripcion, c.Titulo
order by m.IdProducto
ENDTEXT

sql(cmdSQL,'cMov')
SELECT cMov
	

ENDPROC
     ����    �  �                        �O   %          R     (          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_Depositos where IDEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xDeposito� �� �	 M(� ��m �g SELECT     m.IdEmpresa, m.IdProducto, m.IdDeposito, SUM(m.Entrada) as Entrada, SUM(m.Salida) as Salida,�g �a 			p.Descripcion as Producto, m.IdComprobante + c.Descripcion as Descripcion, c.Titulo as Comprob�j �d FROM         dbo.st_MoviProducto(?oApp.Empresa,null,?m.Deposito,?m.dfecha,?m.hfecha) AS m INNER JOIN�t �n                       st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN�j �d                       st_cpbt_stk AS c ON m.IdEmpresa = c.IdEmpresa AND m.IdComprobante = c.Cpbt_Stk�, �& where IdComprobante = ?m.cpbt_stk     �p �j group by m.IdEmpresa, m.IdProducto, m.IdDeposito, p.Descripcion, m.IdComprobante + c.Descripcion, c.Titulo� � order by m.IdProducto� � ��C � � cMov� �� F� � U  DEPOSITO SQL CMDSQL CMOV BeforeOpenTables,     �� InitA     ��1 q 3 � � QA � �q�A���A Rq 3                       &         A   �      )   �                  