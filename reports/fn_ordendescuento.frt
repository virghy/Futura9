  V                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      empresa             Arial      "Orden de Descuento"      Arial      "Mes:"      Arial      m.mes      Arial      "A�o:"      Arial      m.A�o      Arial      
"Entidad:"      Arial      999      Arial      "Cedula"      Arial      "Apellido y Nombre"      Arial      
"Contrato"      Arial      	"Importe"      Arial      CI      Arial      rtrim(Apellido) + ' ' + Nombre      Arial      	NroPagare      "999,999,999"      Arial      Monto      "999,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      	"Importe"      Arial      Monto      "999,999,999,999"      Arial      
"Cantidad"      Arial      Monto      "999,999,999"      Arial      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init


TEXT TO cmdSQL NOSHOW 
	SELECT     cl.ci, cl.apellido, cl.nombre, cl.idcliente, cr.nropagare, ocd.monto,
	oc.mes,oc.a�o, oc.NroOrden
	FROM         fn_ordencobro_det AS ocd INNER JOIN
	                      fn_ordencobro AS oc ON ocd.idorden = oc.idorden INNER JOIN
	                      fn_clientes AS cl ON ocd.idcliente = cl.idcliente INNER JOIN
	                      fn_creditos AS cr ON ocd.idcredito = cr.idcredito
	where oc.IdAsociacion = ?m.Asociacion
	and oc.Mes = ?m.Mes
	and oc.A�o = ?m.A�o 
order by CI
ENDTEXT

=SQL(cmdSQL,'Eventos')
SELECT Eventos

ENDPROC
     g���    N  N                        '�   %   �      �     �          �  U  
  �  � U  SETEON	 M(�  ��W �Q 	SELECT     cl.ci, cl.apellido, cl.nombre, cl.idcliente, cr.nropagare, ocd.monto,�! � 	oc.mes,oc.a�o, oc.NroOrden�7 �1 	FROM         fn_ordencobro_det AS ocd INNER JOIN�W �Q 	                      fn_ordencobro AS oc ON ocd.idorden = oc.idorden INNER JOIN�Y �S 	                      fn_clientes AS cl ON ocd.idcliente = cl.idcliente INNER JOIN�N �H 	                      fn_creditos AS cr ON ocd.idcredito = cr.idcredito�, �& 	where oc.IdAsociacion = ?m.Asociacion� � 	and oc.Mes = ?m.Mes� � 	and oc.A�o = ?m.A�o � � order by CI� � ��C �  � Eventos� �� F� � U  CMDSQL SQL EVENTOS BeforeOpenTables,     �� InitA     ��1 q 3 � qqq�����A �q 2                       &         A   �      )   N                  