  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      acum      importe      0      Arial      Arial      Arial      Arial      Arial      Arial      "Ranking de Ventas de Clientes"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "Sucursal:"      Arial      "
"      Arial      "IdCliente"      Arial      "Raz�n Social"      Arial      "Importe en
Moneda Local
"      "@I"      Arial      "Acumulado
"      Arial      " %
s/Total
"      "@I"      Arial      "% 
s/Acum.
"      "@I"      Arial      	idcliente             Arial      	razsocial             Arial      importe      "999,999,999,999.99"             Arial      acum      "999,999,999,999.99"             Arial      round(importe * 100 / total,2)      "999.99"             Arial      round(acum * 100 / total,2)      "999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
Release m.total
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
Public m.Total
If Empty(m.idcliente)
	m.idcliente= null
Endif

TEXT TO cmdSQL noshow
	SELECT    v.idcliente,c.razsocial, 
	sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Importe * cotizacion) as importe
	FROM         dbo.vt_factura v INNER JOIN dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura 
	inner join vt_clientes c on v.idcliente=c.idcliente and v.IdEmpresa= c.IdEmpresa 
	WHERE v.idempresa = ?oApp.Empresa
	AND v.fecha between  ?m.dfecha and  ?m.hfecha and (v.idcliente=?m.Idcliente or ?m.Idcliente is null)
	group by v.sucursal, v.idcliente,c.razsocial
	order by 3 desc


ENDTEXT

	
sql(cmdSQL,'rcliente_importe')



SELECT rcliente_importe

Sum Importe To m.Total

ENDPROC
     p���    W  W                        {   %   �      �     �          �  U   	 <��  � U  TOTAL
  �  � U  SETEO�	 7��  � %�C�� ���' � T�� ���� �	 M(� ��* �$ 	SELECT    v.idcliente,c.razsocial, �> �8 	sum(case when s.IdDeposito_Sal is not null then 1 else �l �f 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Importe * cotizacion) as importe�e �_ 	FROM         dbo.vt_factura v INNER JOIN dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura �X �R 	inner join vt_clientes c on v.idcliente=c.idcliente and v.IdEmpresa= c.IdEmpresa �( �" 	WHERE v.idempresa = ?oApp.Empresa�k �e 	AND v.fecha between  ?m.dfecha and  ?m.hfecha and (v.idcliente=?m.Idcliente or ?m.Idcliente is null)�3 �- 	group by v.sucursal, v.idcliente,c.razsocial� � 	order by 3 desc� �  � �  � �! ��C � � rcliente_importe� �� F� � K(��  �� �� U  TOTAL	 IDCLIENTE CMDSQL SQL RCLIENTE_IMPORTE IMPORTE Destroy,     �� BeforeOpenTablesC     �� InitX     ��1 � 2 q 2 � � A � ���Q���1aa a A t � 2                       "         I   Q         l   {      )   W                  