   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\FUTURA1\HP DeskJet 840C/841C/842C/843C  USB001                              �\\FUTURA1\HP DeskJet 840C/841C   � XC� 	 �
od   ,  ,  Letter                                                                          DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\FUTURA1\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=9
ASCII=9
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                 "Servicios Realizados"                                        Arial                          empresa                                                       Arial                          Arial                          "Fecha"                       Arial                          
"Per�odo:"                     &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                         Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          	"IdOrden"                      Arial                          "Estado"                       )iif(empty(m.idcliente),'Todos',razsocial)                                                      Arial                          Arial                          
"Cliente:"                     Arial                          "Descripcion"                  Arial                          "Total General"                Arial                          "Desc.Servicio"                idorden                                                       Arial                          fecha                                                         Arial                          tipo                                                          Arial                          des_ser                                                       Arial                          estado                                                        Arial                          descripcion                                                   Arial                          modelo                                                        Arial                          serie                                                         Arial                          tipo                                                          Arial                          pro_rep                                                       Arial                          Arial                          "Tipo"                        Arial                          "Probl.Reportados"            Arial                          "Tipo"                         Arial                          "Serie"                        Arial                          "Modelo"                       marca                                                         Arial                          Arial                          "Marca"                        cantidad                                                      Arial                          precio                                                        Arial                          impuesto                                                      Arial                          Arial                          "Cant"                         Arial                          "Precio"                       Arial                          
"Impuesto"                     Arial                          "Total"                        (cantidad*precio)+impuesto                                                                     Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                KLeft = -38
Top = 97
Width = 793
Height = 439
Name = "Dataenvironment"
                     �PROCEDURE BeforeOpenTables
*SET DATABASE TO DATOS 
Do SETEO

strsql='select a.idorden,convert(varchar(12),a.fecha_elaboracion,103)as fecha,a.idcliente, '+;
		'c.razsocial,a.tipo,a.idmoneda,a.cotizacion, '+ ;
		'a.problemas_reportados as pro_rep, a.descripcion_servicio as des_ser,a.estado,a.fecha_aprobacion, '+ ;
		'a.fecha_llamada,a.fecha_inicio_servicio,a.fecha_equipo_retirado, '+ ;
		'a.fecha_backup_instalado,a.fecha_paralizacion,a.fecha_reinicio_servicio, '+ ;
		'a.fecha_soluciones,b.idproducto, b.descripcion, b.modelo, b.serie,b.marca,b.cantidad, ' + ;
		'b.precio,b.impuesto,b.tipo,b.tipoproducto '+ ;
		'from os_ordenservicio a, os_detalleservicio b, vt_clientes c '+ ;
		'where a.idorden = b.idorden and a.fecha_elaboracion >= ?m.dfecha '+ ;
		'and a.fecha_elaboracion <= ?m.hfecha '+ ;
		'and a.idcliente=c.idcliente '+ ;
		IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')
		
=sql(strsql,'os_rser')

Select os_rser
*!*	brow
ENDPROC
                                     ���                              ��   %   �      �     �          �  U  ? �  �T� ��S select a.idorden,convert(varchar(12),a.fecha_elaboracion,103)as fecha,a.idcliente, �, c.razsocial,a.tipo,a.idmoneda,a.cotizacion, �b a.problemas_reportados as pro_rep, a.descripcion_servicio as des_ser,a.estado,a.fecha_aprobacion, �A a.fecha_llamada,a.fecha_inicio_servicio,a.fecha_equipo_retirado, �I a.fecha_backup_instalado,a.fecha_paralizacion,a.fecha_reinicio_servicio, �V a.fecha_soluciones,b.idproducto, b.descripcion, b.modelo, b.serie,b.marca,b.cantidad, �* b.precio,b.impuesto,b.tipo,b.tipoproducto �= from os_ordenservicio a, os_detalleservicio b, vt_clientes c �A where a.idorden = b.idorden and a.fecha_elaboracion >= ?m.dfecha �% and a.fecha_elaboracion <= ?m.hfecha � and a.idcliente=c.idcliente CC�� �
�& �   and a.idcliente = ?m.idcliente � �  6�� ��C � � os_rser� �� F� � U  SETEO STRSQL	 IDCLIENTE SQL OS_RSER BeforeOpenTables,     ��1 r m1�r 2                       �      )                     