  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Arial      Corte      Arial      Arial      Arial      Arial      Arial      Arial      Arial      B"..\..\casalatina\futura9\bitmaps\superior verdadeiro simbolo.jpg"      " ORDEN DE COBRO"      Arial      "N�"      Arial      NroOC      Arial      
FechaCobro      "@ZYS"      Arial      "Fecha Cobro:"      Arial      Fecha      "@EZYS"      Arial      Audit_Usuario      Arial      "Fecha Llamada:"      Arial      "Telemarketer"      Arial      IdCliente,' ', RazSocial      Arial      	"Cliente"      Arial      direccion, Barrio, Ciudad      Arial      "Direccion:"      Arial      telefono,' ',Celular      Arial      "Telefono:"      Arial      "Gastos Adm.
"      "@I"      Arial      
"TOTAL 
"      "@I"      Arial      "Factura
"      "@I"      Arial      	"Vence
"      "@I"      Arial      "Importe
"      "@I"      Arial      "Mora
"      "@I"      Arial      GastoAdm      "@Z 999,999,999.99"      Arial      Valorizado + IntMora + GastoAdm      "@Z 9,999,999,999.99"      Arial      Numero,'-',Cuota      Arial      Vence      Arial      
Valorizado      "9,999,999,999.99"      Arial      IntMora      "@Z 999,999,999.99"      Arial      	"TOTAL
"      Arial      Valorizado + IntMora + GastoAdm      "9,999,999,999.99"      Arial      "Notas o Comentarios"      Arial      Obs      Arial      #"Futura Software www.futura.com.py"      Arial      dataenvironment      aTop = 30
Left = -55
Width = 1016
Height = 634
DataSource = .NULL.
Name = "Dataenvironment"
     =PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


TEXT TO cmdSQL noshow
SELECT     o.NroOC, o.Fecha, o.IdCliente, o.IdCobrador, o.TotalFacturas, o.Obs, od.numero, od.IntMora, od.GastoAdm, od.Vence, p.Nombre, p.Apellido, cl.RazSocial, cl.Direccion, 
                      cl.Barrio, cl.Ciudad, cl.Telefono, cl.Celular, cl.CI, o.FechaPago, o.Audit_Usuario, od.cuota, od.Valorizado
FROM         vt_clientes AS cl INNER JOIN
                      Vt_OrdenCobro AS o INNER JOIN
                      vt_OrdenCobroDet AS od ON o.IdOC = od.IdOC ON cl.IdEmpresa = o.idempresa AND cl.IdCliente = o.IdCliente LEFT OUTER JOIN
                      BS_Personas AS p INNER JOIN
                      vt_cobradores AS cb ON p.IdPersona = cb.idpersona ON o.idempresa = cb.idempresa AND o.Sucursal = cb.idcobrador
where o.IdEmpresa=?oApp.Empresa and o.NroOC= ?m.NroOC                      
ENDTEXT

sql(cmdSQL,'rpedido')
SELECT rpedido
SCATTER MEMVAR memo
FOR i=1 TO 5-RECCOUNT()
	APPEND BLANK
	GATHER MEMVAR memo
	replace numero WITH 0,cuota WITH 0, Valorizado WITH 0, IntMora WITH 0, GastoAdm WITH 0, Linea WITH i + 1
ENDFOR 




SELECT *, '1' as Corte FROM force rPedido;
union ; 
SELECT *, '2' as Corte FROM force rPedido ORDER BY Corte,Linea INTO CURSOR rP 
SELECT rP


*Sum Importe To m.Total

ENDPROC
     ����    �  �                        <   %   �      4               �  U  
  �  � U  SETEO>	 M(�  ��� �� SELECT     o.NroOC, o.Fecha, o.IdCliente, o.IdCobrador, o.TotalFacturas, o.Obs, od.numero, od.IntMora, od.GastoAdm, od.Vence, p.Nombre, p.Apellido, cl.RazSocial, cl.Direccion, �� ��                       cl.Barrio, cl.Ciudad, cl.Telefono, cl.Celular, cl.CI, o.FechaPago, o.Audit_Usuario, od.cuota, od.Valorizado�/ �) FROM         vt_clientes AS cl INNER JOIN�9 �3                       Vt_OrdenCobro AS o INNER JOIN�� ��                       vt_OrdenCobroDet AS od ON o.IdOC = od.IdOC ON cl.IdEmpresa = o.idempresa AND cl.IdCliente = o.IdCliente LEFT OUTER JOIN�7 �1                       BS_Personas AS p INNER JOIN�� ��                       vt_cobradores AS cb ON p.IdPersona = cb.idpersona ON o.idempresa = cb.idempresa AND o.Sucursal = cb.idcobrador�Q �K where o.IdEmpresa=?oApp.Empresa and o.NroOC= ?m.NroOC                      � � ��C �  � rpedido� �� F� � ^�� �� ���(��CN���� � _��C >� ��� �� ��� �� ��� �� ��� �� ��� ��	 ��� ��� ��L o��  �� rPedido��� 2�Q�
 �� rPedido��� 1�Q�
 ���
 ���	 ���� rP� F� � U  CMDSQL SQL RPEDIDO I NUMERO CUOTA
 VALORIZADO INTMORA GASTOADM LINEA CORTE RP BeforeOpenTables,     �� InitA     ��1 q 2 � aq��1	q�A �q a �Q a 1A �q 5                       $         ?   2      )   �                  