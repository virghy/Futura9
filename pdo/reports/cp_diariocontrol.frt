     !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=3
COLLATE=1
                 idmoneda                       idcomprobante                  cp_rdiariocontrol.exenta                                      Arial                          "999,999,999"                  cp_rdiariocontrol.gravada                                     Arial                          "999,999,999"                  cp_rdiariocontrol.iva                                         Arial                          "999,999,999"                  "Control de Compras y Gastos"                                                                  Arial                          empresa                                                       Arial                          Arial                          "Fecha"                       Arial                          	"Exentas"                      Arial                          
"Gravadas"                     Arial                          
"Per�odo:"                     &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                         Arial                          cp_rdiariocontrol.fecha                                       Arial                          cp_rdiariocontrol.idproveedor                                                                  Arial                          Arial                          "Iva"                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          	"Cliente"                      Arial                          "Nro."                         *iif(empty(m.sucursal),'Todos',descripci�n)                                                     Arial                          cp_rdiariocontrol.razon                                       Arial                          Ncp_rdiariocontrol.exenta +  cp_rdiariocontrol.gravada +  cp_rdiariocontrol.iva                                                  Arial                          "999,999,999,999"              cp_rdiariocontrol.idfactura                                                                    Arial                          Arial                          
"Sucursal"                     cp_rdiariocontrol.exenta                                      Arial                          "99,999,999,999"               cp_rdiariocontrol.gravada                                     Arial                          "99,999,999,999"               cp_rdiariocontrol.iva                                         Arial                          "99,999,999,999"               Ncp_rdiariocontrol.exenta +  cp_rdiariocontrol.gravada +  cp_rdiariocontrol.iva                                                  Arial                          "99,999,999,999,999"           cp_rdiariocontrol.exenta                                      Arial                          "999,999,999,999"              cp_rdiariocontrol.gravada                                     Arial                          "999,999,999,999"              cp_rdiariocontrol.iva                                         Arial                          "999,999,999"                  Ncp_rdiariocontrol.exenta +  cp_rdiariocontrol.gravada +  cp_rdiariocontrol.iva                                                  Arial                          "999,999,999,999"              Arial                          "Total"                        Arial                          "Total General"                Arial                          "Cpbte."                       ?'Comprobrante: (' + alltrim(idcomprobante) + ') ' + comprobante                                                                 Arial                          ("Total "+  cp_rdiariocontrol.comprobante                                                       Arial                          ''Moneda : (' + alltrim(idmoneda) + ') '                                                        Arial                          cp_rdiariocontrol.exenta                                      Arial                          "99,999,999,999"               cp_rdiariocontrol.gravada                                     Arial                          "99,999,999,999"               cp_rdiariocontrol.iva                                         Arial                          "99,999,999,999"               Ncp_rdiariocontrol.exenta +  cp_rdiariocontrol.gravada +  cp_rdiariocontrol.iva                                                  Arial                          "99,999,999,999,999"           "Total "+  idmoneda                                           Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                xLeft = 158
Top = 175
Width = 759
Height = 448
InitialSelectedAlias = "cp_rdiariocontrol"
Name = "Dataenvironment"
         �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
LOCAL strsql
*SET DATABASE TO DATOS 
*!*	m.sucursal = '01'
*!*	m.idcomprob ='EC'
*!*	m.dfecha = DATE()-30
*!*	m.hfecha = DATE()
strsql = 'SELECT Cp_factura.idcomprobante, Cp_factura.idfactura, Cp_factura.facturaproveedor, '+;
	  'Cp_factura.idproveedor, Cp_factura.fecha, Cp_factura.sucursal, '+;
	  'Cp_factura.exenta, Cp_factura.gravada, Cp_factura.iva, Cp_factura.idmoneda, '+;
	  'Cp_proveedor.razon, st_comprobante.descripcion comprobante, sucursal.descripci�n '+;
	 'FROM st_comprobante, cp_factura, cp_proveedor, sucursal '+;
	 'WHERE Cp_factura.idproveedor = Cp_proveedor.idproveedor '+;
	 'AND Cp_factura.idcomprobante = st_comprobante.idcomprobante '+;
	 IIF( !EMPTY(m.sucursal),'AND Cp_factura.sucursal = ?m.sucursal ','')+;
 	 IIF( !EMPTY(m.idcomprob),'AND Cp_factura.idcomprobante = ?m.idcomprob ','')+;
 	 'AND cp_factura.sucursal = sucursal.sucursal '+;
	 'AND Cp_factura.fecha BETWEEN ?m.dfecha AND ?m.hfecha '+;
	 'ORDER BY cp_factura.idmoneda, Cp_factura.sucursal, Cp_factura.idcomprobante, '+;
	 'Cp_factura.fecha, Cp_factura.idfactura'

	=sql(strsql,'cp_rdiariocontrol')
	
	SELECT cp_rdiariocontrol
ENDPROC
                                   ����    �  �                        K�   %         L  	   :          �  U  
  �  � U  SETEO� ��  �eT�  ��T SELECT Cp_factura.idcomprobante, Cp_factura.idfactura, Cp_factura.facturaproveedor, �? Cp_factura.idproveedor, Cp_factura.fecha, Cp_factura.sucursal, �L Cp_factura.exenta, Cp_factura.gravada, Cp_factura.iva, Cp_factura.idmoneda, �Q Cp_proveedor.razon, st_comprobante.descripcion comprobante, sucursal.descripci�n �8 FROM st_comprobante, cp_factura, cp_proveedor, sucursal �8 WHERE Cp_factura.idproveedor = Cp_proveedor.idproveedor �< AND Cp_factura.idcomprobante = st_comprobante.idcomprobante CC�� �
�, �& AND Cp_factura.sucursal = ?m.sucursal � �  6CC�� �
�2 �, AND Cp_factura.idcomprobante = ?m.idcomprob � �  6�, AND cp_factura.sucursal = sucursal.sucursal �5 AND Cp_factura.fecha BETWEEN ?m.dfecha AND ?m.hfecha �M ORDER BY cp_factura.idmoneda, Cp_factura.sucursal, Cp_factura.idcomprobante, �& Cp_factura.fecha, Cp_factura.idfactura��" ��C �  � cp_rdiariocontrol� �� F� � U  STRSQL SUCURSAL	 IDCOMPROB SQL CP_RDIARIOCONTROL BeforeOpenTables,     �� InitA     ��1 q 3 q Q6"r 1                       &         A   �      )   �                  