  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      sucursal      Comprobante      Arial      Arial      Arial      Arial      empresa             Arial      #"Detalle de Ventas por Comprobante"             Arial      nombreMes(m.Mes)      Arial      m.A�o      "9999"      Arial      1IIF(m.ordenfactura='F','Fecha','Nro Comprobante')             Arial      "MES:
"             Arial      "A�O:
"      Arial      
"Orden:
"      Arial      "Facturas/Notas
"      Arial      
"Clientes"      Arial      "Valor Ventas"      Arial      "Retenciones
"      "@I"      Arial      "Numero"      Arial      " Fecha"      Arial      $"Razon Social / Apellidos / Nombres"      Arial      " RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      cn_rivaventas.sucursal             Arial      "Sucursal:"      Arial      "Comprobante: ", Comprobante      Arial      cn_rivaventas.numero      Arial      cn_rivaventas.fecha      Arial      Nombre      Arial      cn_rivaventas.ruc      Arial      len( ALLTRIM(ruc) ) > 8      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"       cn_rivacompra.iva5      Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      Total      "999,999,999,999"      Arial      "Total: ", Comprobante      Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      Total      "999,999,999,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total General"             Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      Total      "999,999,999,999"      Arial      dataenvironment      �Top = 104
Left = 2
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
SET TEXTMERGE ON 
IF m.ordenfactura='F'
	m.orden = 'c.Fecha, c.Numero'
ELSE
	m.orden = 'c.Numero, c.Fecha'
ENDIF
IF EMPTY(m.vt_cpbt)
	m.vt_cpbt=null
ENDIF
	
		      

TEXT TO cmdSQL noshow
	exec dbo.vt_rLibroIVA ?oApp.Empresa,?m.dFecha,?m.hFecha, 'V',?m.Mes,?m.A�o,?m.Sucursal,?m.OrdenFactura,?m.vt_cpbt
ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     r���    Y  Y                        ��   %   �            �          �  U  5 G` � %���  � F��: �  T�� �� c.Fecha, c.Numero�� �b �  T�� �� c.Numero, c.Fecha�� � %�C�� ���� � T�� ���� �	 M(� ��x �r 	exec dbo.vt_rLibroIVA ?oApp.Empresa,?m.dFecha,?m.hFecha, 'V',?m.Mes,?m.A�o,?m.Sucursal,?m.OrdenFactura,?m.vt_cpbt� � ��C � � cn_rIvaVentas� �� F� � U  ORDENFACTURA ORDEN VT_CPBT CMDSQL SQL CN_RIVAVENTAS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 a A� A � A � �A �r 3 q 2                       �        �  �      )   Y                  