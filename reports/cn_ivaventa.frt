  h                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      sucursal      Comprobante      Arial      Arial      Arial      Arial      empresa             Arial      "Libro IVA Venta - Ley 125"      Arial      NombreMes(m.Mes)      Arial      m.a�o      "9999"      Arial      1IIF(m.ordenfactura='F','Fecha','Nro Comprobante')             Arial      "MES:
"             Arial      "A�O:
"      Arial      
"Orden:
"      Arial      "Facturas/Notas
"      Arial      
"Clientes"      Arial      "Valor Ventas"      Arial      "Retenciones
"      "@I"      Arial      "Numero"      Arial      " Fecha"      Arial      $"Razon Social / Apellidos / Nombres"      Arial      " RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      cn_rivaventas.sucursal             Arial      "Sucursal:"      Arial      "Comprobante: ", Comprobante      Arial      cn_rivaventas.numero      "@R"      Arial      cn_rivaventas.fecha      "@E"      Arial      Nombre      Arial      cn_rivaventas.ruc      Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"       cn_rivacompra.iva5      Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      "Total: ", Comprobante      Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total General"             Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      dataenvironment      �Top = 104
Left = 2
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
SET TEXTMERGE ON 
*!*	IF m.ordenfactura='F'
*!*	*	m.orden = 'c.Fecha, c.Numero'
*!*	ELSE
*!*	*	m.orden = 'c.Numero, c.Fecha'
*!*	ENDIF

IF EMPTY(m.vt_cpbt)
	m.vt_cpbt=null
ENDIF

IF EMPTY(m.Sucursal)
	m.sucursal=null
ENDIF
	

TEXT TO cmdSQL noshow
	exec dbo.cn_rLibroIVA ?oApp.Empresa,?m.dFecha,?m.hFecha, 'V',?m.Mes,?m.A�o,?m.Sucursal,?m.OrdenFactura,?m.vt_cpbt
ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas



ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     %���                                  %   k      �     �          �  U  �  G` � %�C��  ���$ � T��  ���� � %�C�� ���F � T�� ���� �	 M(� ��x �r 	exec dbo.cn_rLibroIVA ?oApp.Empresa,?m.dFecha,?m.hFecha, 'V',?m.Mes,?m.A�o,?m.Sucursal,?m.OrdenFactura,?m.vt_cpbt� � ��C � � cn_rIvaVentas� �� F� � U  VT_CPBT SUCURSAL CMDSQL SQL CN_RIVAVENTAS
  �  � U  SETEO Init,     �� BeforeOpenTablesV    ��1 a � A � A � �A �r 5 q 2                       �        �        )                     