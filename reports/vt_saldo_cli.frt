  n                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      	DesMoneda      	IdCliente      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Saldos de Clientes "             Arial      empresa             Arial      )iif(empty(m.sucursal),'Todos',m.sucursal)             Arial      
"Sucursal"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      &dtoc(m.dvence)+ " al " +dtoc(m.hVence)             Arial      "Per�odo Ventas:"      Arial      "Per�odo Vto.:"      Arial      	"Cliente"      Arial      
"Telefono"      Arial      
"Contacto"      Arial      	"Importe"      Arial      "Saldo"      Arial      	"Factura"             Arial      !soloresumen      "Fecha"             Arial      !soloresumen      "Vence"             Arial      !soloresumen      "Descripcion"      Arial      !soloresumen      	DesMoneda      Arial      !SoloResumen      IdCliente,razsocial             Arial      !SoloResumen      4telefono,"("+alltrim(NomContacto)+")", " ",Direccion      Arial      !SoloResumen      5allt(IdComprobante) + " - " + allt( str(nrocomprob) )             Arial      ttod( fecha )             Arial      ttod( Vencimiento )             Arial      Importe      "99,999,999,999.99"      Arial      saldo      "99,999,999,999.99"             Arial      descripcion      Arial      IdCliente,razsocial             Arial      SoloResumen      "Total"      Arial      saldo      "99,999,999,999.99"      Arial      "Total",DesMoneda      Arial      saldo      "99,999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      Top = 114
Left = 162
Width = 381
Height = 301
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
      PROCEDURE Init
Local sSQL

*--- agar 04/10/05

IF EMPTY(m.IdCliente)
	m.IdCliente = null
ENDIF
IF EMPTY(m.Sucursal)
	m.sucursal=null
ENDIF
	
	

*!*	sSQL = "select c.IdCliente, c.RazSocial, " + ;
*!*	              "f.IdComprobante, " + ;
*!*	              "f.Numero as NroComprob, " + ;
*!*	              "f.Fecha,fp.Vencimiento, " + ;
*!*	              "mo.Descripcion as DesMoneda, " + ;
*!*	              "sum( fp.Saldo ) as Saldo " + ;             
*!*	        "from vt_Forma_Pago fp, vt_factura f, vt_Clientes c, bs_monedas mo " + ;
*!*	       "where fp.IdFactura = f.IdFactura and fp.IdEmpresa = f.IdEmpresa " + ;
*!*	          "and f.IdCliente = c.IdCliente and f.IdEmpresa = c.IdEmpresa " + ;
*!*	          "and fp.IdMoneda = mo.IdMoneda " + ;
*!*	          " and f.IdEmpresa = ?oApp.Empresa " +;       
*!*	          "and ( '" + m.Sucursal + "' = '' or f.Sucursal = '" + m.Sucursal + "' ) " + ;
*!*	          "and f.fecha between ?m.dfecha and ?m.hfecha " + ;
*!*	          "and fp.vencimiento between ?m.dvence and ?m.hvence " + ;
*!*	          "and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente) " + ;
*!*	        "group by c.IdCliente, c.RazSocial, f.IdComprobante, f.Numero, f.Fecha, fp.Vencimiento, mo.Descripcion " + ;
*!*	        " Having sum(fp.Saldo) <> 0 " + ;
*!*	        "order by 1, 2 ,3 ,4"

TEXT TO cmdSQL noshow
select c.IdCliente, c.RazSocial,c.Telefono,c.NomContacto,c.Direccion,  
              f.IdComprobante,  
              f.Numero as NroComprob,  
              f.Fecha,fp.Vencimiento,  
              mo.Descripcion as DesMoneda,  
              sum( fp.Saldo ) as Saldo,SUM(Importe) as Importe,
              Descripcion=(Select top 1 Descripcion from st_movimiento_Det m where m.IdFactura = f.IdFactura order by IdDetalle)               
--              Descripcion=dbo.vt_Descripcion(f.IdFactura)               
        from vt_Forma_Pago fp, vt_factura f, vt_Clientes c, bs_monedas mo  
       where fp.IdFactura = f.IdFactura and fp.IdEmpresa = f.IdEmpresa  
          and f.IdCliente = c.IdCliente and f.IdEmpresa = c.IdEmpresa  
          and fp.IdMoneda = mo.IdMoneda  
           and f.IdEmpresa = ?oApp.Empresa       
          and ( ?m.Sucursal is null or f.Sucursal = ?m.Sucursal )  
          and f.fecha between ?m.dfecha and ?m.hfecha  
          and fp.vencimiento between ?m.dvence and ?m.hvence  
          and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  
        group by mo.Descripcion  ,c.IdCliente, c.RazSocial,f.IdFactura, f.IdComprobante, f.Numero, f.Fecha, fp.Vencimiento,c.Telefono,c.NomContacto,c.Direccion
         Having sum(fp.Saldo) <> 0  
        order by mo.Descripcion ,1, f.fecha


ENDTEXT

sql( cmdSQL, 'xxSaldoCli' )

Select xxSaldoCli



ENDPROC
PROCEDURE BeforeOpenTables
DO seteo
ENDPROC
     x���    _  _                        t   %   �        &   �          �  U   ��  � %�C�� ���% � T�� ���� � %�C�� ���G � T�� ���� �	 M(� ��M �G select c.IdCliente, c.RazSocial,c.Telefono,c.NomContacto,c.Direccion,  �& �                f.IdComprobante,  �- �'               f.Numero as NroComprob,  �- �'               f.Fecha,fp.Vencimiento,  �2 �,               mo.Descripcion as DesMoneda,  �E �?               sum( fp.Saldo ) as Saldo,SUM(Importe) as Importe,�� ��               Descripcion=(Select top 1 Descripcion from st_movimiento_Det m where m.IdFactura = f.IdFactura order by IdDetalle)               �P �J --              Descripcion=dbo.vt_Descripcion(f.IdFactura)               �Q �K         from vt_Forma_Pago fp, vt_factura f, vt_Clientes c, bs_monedas mo  �N �H        where fp.IdFactura = f.IdFactura and fp.IdEmpresa = f.IdEmpresa  �M �G           and f.IdCliente = c.IdCliente and f.IdEmpresa = c.IdEmpresa  �/ �)           and fp.IdMoneda = mo.IdMoneda  �7 �1            and f.IdEmpresa = ?oApp.Empresa       �I �C           and ( ?m.Sucursal is null or f.Sucursal = ?m.Sucursal )  �= �7           and f.fecha between ?m.dfecha and ?m.hfecha  �D �>           and fp.vencimiento between ?m.dvence and ?m.hvence  �K �E           and ( ?m.IdCliente is null or f.IdCliente = ?m.IdCliente)  �� ��         group by mo.Descripcion  ,c.IdCliente, c.RazSocial,f.IdFactura, f.IdComprobante, f.Numero, f.Fecha, fp.Vencimiento,c.Telefono,c.NomContacto,c.Direccion�* �$          Having sum(fp.Saldo) <> 0  �1 �+         order by mo.Descripcion ,1, f.fecha� �  � �  � � ��C � �
 xxSaldoCli� �� F� � U  SSQL	 IDCLIENTE SUCURSAL CMDSQL SQL
 XXSALDOCLI
  �  � U  SETEO Init,     �� BeforeOpenTables}    ��1 q � A � A `� �a��!QQ	���q��A�Q
�a a A �r 5 q 1                       �
     $       B    )   _                  