  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      
repartidor      Arial      Arial      Arial      Arial      Arial      Arial      "Pedidos por Repartidor"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "Nro. Pedido"      Arial      	"Cliente"      Arial      "Importe
"      "@I"      Arial      "Hora Program
"      "@I"      Arial      "Hora Entrega
"      "@I"      Arial      	"Firma
"      "@I"      Arial      "Repartidor:
"      Arial      
repartidor             Arial      "
"      Arial      	nropedido      "999,999,999"             Arial      Gcliente,"/"+Direccion," " +Referencia, " / " +Telefono, " / " + Celular      Arial      importe      "999,999,999"             Arial      Ytransform(day(FechaEntrega),"99"),"/",transform(Month(FechaEntrega),"99")," ",HoraEntrega      Arial      importe      "999,999,999"      Arial      
"Total:
"      "@I"      Arial      "Cobranzas:
"      "@I"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      ~Top = 32
Left = 177
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     wPROCEDURE Init
*Public m.Total
If Empty(m.idrepartidor)
	Store null To idrepartidor
endif
TEXT TO cmdSQL noshow
	select a.idpedido, a.FechaEntrega,a.HoraEntrega, a.nropedido, ISNULL(rtrim(a.idcliente)+'-','')+ a.razonsocial as cliente,a.importe,   
	rtrim(c.idrepartidor)+'-'+rtrim(g.nombre)+' '+g.apellido as repartidor,a.Direccion,a.Referencia,h.Telefono,h.Celular
	from vt_pedido a --inner join  vt_pedidodet b on a.idpedido=b.idpedido 
	left join vt_repartidor c on a.idrepartidor=c.idrepartidor and a.IdEmpresa=c.IdEmpresa
	left join bs_personas g on c.idpersona=g.idpersona
	left join vt_clientes h on a.idcliente=h.idcliente and a.IdEmpresa=h.IdEmpresa
	where a.idempresa=?oApp.empresa and a.FechaEntrega between ?m.dfecha and  ?m.hfecha 
	and (a.idrepartidor=?m.idrepartidor or ?m.idrepartidor is null) 
	and a.idestado<>'A'
	order by 7,2,3,4

ENDTEXT

	
	*sql('exec vt_pedido_repar ?m.idrepartidor,?oapp.empresa,?m.dFecha,?m.hFecha','rpedido_resumen')
	sql(cmdSQL,'rpedido_resumen')
	SELECT rpedido_resumen

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Destroy
*Release m.total
ENDPROC
     ����    �  �                        Gu   %   �      Q     #          �  U  r %�C��  ��� � J���(�  � �	 M(� ��� �� 	select a.idpedido, a.FechaEntrega,a.HoraEntrega, a.nropedido, ISNULL(rtrim(a.idcliente)+'-','')+ a.razonsocial as cliente,a.importe,   �{ �u 	rtrim(c.idrepartidor)+'-'+rtrim(g.nombre)+' '+g.apellido as repartidor,a.Direccion,a.Referencia,h.Telefono,h.Celular�N �H 	from vt_pedido a --inner join  vt_pedidodet b on a.idpedido=b.idpedido �] �W 	left join vt_repartidor c on a.idrepartidor=c.idrepartidor and a.IdEmpresa=c.IdEmpresa�9 �3 	left join bs_personas g on c.idpersona=g.idpersona�U �O 	left join vt_clientes h on a.idcliente=h.idcliente and a.IdEmpresa=h.IdEmpresa�[ �U 	where a.idempresa=?oApp.empresa and a.FechaEntrega between ?m.dfecha and  ?m.hfecha �G �A 	and (a.idrepartidor=?m.idrepartidor or ?m.idrepartidor is null) � � 	and a.idestado<>'A'� � 	order by 7,2,3,4� �  � �  ��C � � rpedido_resumen� �� F� � U  IDREPARTIDOR CMDSQL SQL RPEDIDO_RESUMEN
  �  � U  SETEO  U   Init,     �� BeforeOpenTables�    �� Destroy�    ��1 � A � �����Q�q�qa A q 3 q 2 2                               6  >        \  l      )   �                  