  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      ZonaCom      Zona      m.Tzg      nvl(t1,0)+nvl(t2,0)      0      m.tzc      nvl(t1,0)+nvl(t2,0)      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      )"Resumen de Ventas por Clientes/Sucursal"      Arial      alltrim( empresa )             Arial      ,iif(isnull(m.Familia),'Todos',NombreFamilia)      Arial      
"Familia:"      Arial      "%
"      "@I"      Arial      	"Cliente"      Arial      "Representante
"      "@I"      Arial      	"Central"      "@I"      Arial      
"Sucursal"      "@I"      Arial      	"Total
"      "@I"      Arial      Zonacom      Arial      Zona      Arial      "
"      Arial      IdCliente,razSocial      Arial      Contacto      Arial      t1      "999,999,999,999"      Arial      t2      "999,999,999,999"      Arial      nvl(t1,0)+nvl(t2,0)      "999,999,999,999"      Arial      (nvl(t1,0)+nvl(t2,0))/m.TG*100      "999.99"      Arial      t1      "999,999,999,999"      Arial      t2      "999,999,999,999"      Arial      nvl(t1,0)+nvl(t2,0)      "999,999,999,999"      Arial      m.tzg/m.TG*100      "999.99"      Arial      "Total :",Zonacom      Arial      t1      "999,999,999,999"      Arial      t2      "999,999,999,999"      Arial      nvl(t1,0)+nvl(t2,0)      "999,999,999,999"      Arial      m.tzc/m.TG*100      "999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      t1      "999,999,999,999"      Arial      t2      "999,999,999,999"      Arial      nvl(t1,0)+nvl(t2,0)      "999,999,999,999"      Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
Release m.TG
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
PUBLIC m.TG

IF EMPTY(m.ZonaCom )
	m.ZonaCom = null
ENDIF

IF EMPTY(m.Familia )
	m.Familia = null
ENDIF
	

TEXT TO cmdSQL
select 	ISNULL(c.IdZonaCom,'99') +' '+ ISNULL(zc.descripcion,'Otros') ZonaCom,
		ISNULL(c.IdZonaGeog,'99') +' '+ ISNULL(z.descripcion,'Otros') Zona, b.idcliente, 
		c.razsocial, b.IdMoneda, c.NomContacto Contacto,
		f.Descripcion NombreFamilia,
		sum(case when b.Sucursal ='01' then  m.Importe else 0 end) T1,
		sum(case when b.Sucursal ='02' then  m.Importe else 0 end) T2
		from vt_factura b inner join vt_clientes c on  b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa 
		inner join sucursal d on b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa 
		left join vt_zonageog z on c.IdEmpresa=z.idempresa and c.IdZonaGeog = z.idzonageog
		left join vt_zonacom zc on c.IdEmpresa=zc.idempresa and c.IdZonaCom = zc.IdZonaCom
		left join st_Movimiento_Det m on b.idfactura = m.idfactura
		left join st_Producto p on m.idempresa=p.IdEmpresa and m.IdProducto = p.IdProducto
		left join st_Familia f on p.IdEmpresa = f.IdEmpresa and p.Familia = f.IdFamilia  
		where	b.IdEmpresa = ?oApp.Empresa
		and b.fecha BETWEEN ?m.dfecha and ?m.hfecha
		and (ISNULL(c.IdZonaCom,'99')=?m.ZonaCom or ?m.ZonaCom is null)
		and (p.Familia=?m.Familia or ?m.Familia is null)
		group by ISNULL(c.IdZonaCom,'99') +' '+ ISNULL(zc.descripcion,'Otros'), ISNULL(c.IdZonaGeog,'99') +' '+ ISNULL(z.descripcion,'Otros'),b.idcliente,c.NomContacto, 
		c.razsocial,b.IdMoneda,f.Descripcion 
		order by B.IdMoneda,ISNULL(c.IdZonaCom,'99') +' '+ ISNULL(zc.descripcion,'Otros'),
		ISNULL(c.IdZonaGeog,'99') +' '+ ISNULL(z.descripcion,'Otros'), b.idcliente

ENDTEXT

sql(cmdSQL,'Clientes')
SELECT Clientes
	
SUM NVL(t1,0)+NVL(t2,0) TO m.TG
	
*!*	endi
*Sum Importe To m.Total

ENDPROC
     ����                              ��   %   �        (   �          �  U   	 <��  � U  TG
  �  � U  SETEO�	 7��  � %�C�� ���' � T�� ���� � %�C�� ���I � T�� ���� � M(� �T �N select 	ISNULL(c.IdZonaCom,'99') +' '+ ISNULL(zc.descripcion,'Otros') ZonaCom,�Y �S 		ISNULL(c.IdZonaGeog,'99') +' '+ ISNULL(z.descripcion,'Otros') Zona, b.idcliente, �8 �2 		c.razsocial, b.IdMoneda, c.NomContacto Contacto,�$ � 		f.Descripcion NombreFamilia,�F �@ 		sum(case when b.Sucursal ='01' then  m.Importe else 0 end) T1,�E �? 		sum(case when b.Sucursal ='02' then  m.Importe else 0 end) T2�o �i 		from vt_factura b inner join vt_clientes c on  b.idcliente = c.idcliente and b.IdEmpresa = c.IdEmpresa �W �Q 		inner join sucursal d on b.sucursal = d.sucursal and b.IdEmpresa = d.IdEmpresa �Z �T 		left join vt_zonageog z on c.IdEmpresa=z.idempresa and c.IdZonaGeog = z.idzonageog�Z �T 		left join vt_zonacom zc on c.IdEmpresa=zc.idempresa and c.IdZonaCom = zc.IdZonaCom�B �< 		left join st_Movimiento_Det m on b.idfactura = m.idfactura�Z �T 		left join st_Producto p on m.idempresa=p.IdEmpresa and m.IdProducto = p.IdProducto�Y �S 		left join st_Familia f on p.IdEmpresa = f.IdEmpresa and p.Familia = f.IdFamilia  �) �# 		where	b.IdEmpresa = ?oApp.Empresa�3 �- 		and b.fecha BETWEEN ?m.dfecha and ?m.hfecha�G �A 		and (ISNULL(c.IdZonaCom,'99')=?m.ZonaCom or ?m.ZonaCom is null)�8 �2 		and (p.Familia=?m.Familia or ?m.Familia is null)�� �� 		group by ISNULL(c.IdZonaCom,'99') +' '+ ISNULL(zc.descripcion,'Otros'), ISNULL(c.IdZonaGeog,'99') +' '+ ISNULL(z.descripcion,'Otros'),b.idcliente,c.NomContacto, �- �' 		c.razsocial,b.IdMoneda,f.Descripcion �Z �T 		order by B.IdMoneda,ISNULL(c.IdZonaCom,'99') +' '+ ISNULL(zc.descripcion,'Otros'),�R �L 		ISNULL(c.IdZonaGeog,'99') +' '+ ISNULL(z.descripcion,'Otros'), b.idcliente� �  � � ��C � � Clientes� �� F� � K(��  �C� � �C� � ��� U  TG ZONACOM FAMILIA CMDSQL SQL CLIENTES T1 T2 Destroy,     �� BeforeOpenTables@     �� InitU     ��1 � 2 q 2 � � A � A � A��AaQ�q��!���1q��
��!a A �q �5                                F   N         i   {      )                     