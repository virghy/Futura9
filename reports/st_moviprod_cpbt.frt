  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      deposito      fecha      #comprob+ " " + alltrim(str(numero))      Cantidad      iif(Entrada=0,Salida,Entrada)      0      Arial      Arial      Arial      Arial      Arial      Arial      "Movimientos por Comprobante"      Arial      alltrim( empresa )             Arial      m.dfecha,'al', m.hfecha             Arial      
"Periodo:"      Arial      Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)      Arial      "Dep�sito:"      Arial      descripcion      Arial      
"Comprob:"      Arial      "
"      Arial      "Costo"      Arial      "Cantidad "      Arial      "Total"      Arial      "Fecha"      Arial      "Cpbte."      Arial      
"Producto"      Arial      "Referencia"      Arial      fecha             Arial      #comprob, " " , alltrim(str(numero))      Arial      &alltrim(Idproducto) + " - " + producto      Arial      
referencia             Arial      precio      "@Z 9,999,999.99"      Arial      cantidad      "@Z 999,999.99"      Arial      cantidad*precio      "999,999,999.99"      Arial      cantidad*precio      "999,999,999.99"      Arial      "Total"      Arial      "Total ", fecha      Arial      cantidad*precio      "999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      cantidad*precio      "999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      `Top = 32
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
SELECT     m.IdEmpresa, m.IdProducto, m.IdDeposito, m.Fecha, m.IdComprobante, m.Numero, m.Entrada, m.Salida, m.Precio, m.Costo_Pro, m.Referencia, 
                      p.Descripcion as Producto, c.Descripcion , c.Titulo as Comprob
FROM         dbo.st_MoviProducto(?oApp.Empresa,null,?m.Deposito,?m.dfecha,?m.hfecha) AS m INNER JOIN
                      st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN
                      st_cpbt_stk AS c ON m.IdEmpresa = c.IdEmpresa AND m.IdComprobante = c.Cpbt_Stk
where IdComprobante = ?m.cpbt_stk          
order by Fecha
ENDTEXT

sql(cmdSQL,'cMov')
SELECT cMov
	

ENDPROC
     n���    U  U                        U>   %   �      �     �          �  U  
  �  � U  SETEOJ %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_Depositos where IDEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xDeposito� �� �	 M(� ��� �� SELECT     m.IdEmpresa, m.IdProducto, m.IdDeposito, m.Fecha, m.IdComprobante, m.Numero, m.Entrada, m.Salida, m.Precio, m.Costo_Pro, m.Referencia, �Z �T                       p.Descripcion as Producto, c.Descripcion , c.Titulo as Comprob�j �d FROM         dbo.st_MoviProducto(?oApp.Empresa,null,?m.Deposito,?m.dfecha,?m.hfecha) AS m INNER JOIN�t �n                       st_Producto AS p ON m.IdEmpresa = p.IdEmpresa AND m.IdProducto = p.IdProducto INNER JOIN�j �d                       st_cpbt_stk AS c ON m.IdEmpresa = c.IdEmpresa AND m.IdComprobante = c.Cpbt_Stk�1 �+ where IdComprobante = ?m.cpbt_stk          � � order by Fecha� � ��C � � cMov� �� F� � U  DEPOSITO SQL CMDSQL CMOV BeforeOpenTables,     �� InitA     ��1 q 3 � � QA � �	��A�AA Rq 3                       &         A   �      )   U                  