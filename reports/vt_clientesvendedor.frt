  m                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=1
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Arial      
Supervisor      Vendedor      Arial      Arial      Arial      Arial      Arial      Arial      ""Listado de Vendedor por Clientes"             Arial      alltrim( empresa )             Arial      	"Cliente"      Arial      	"Cliente"      Arial      "Direccion
"      "@I"      Arial      
"Telefono"      "@I"      Arial      "Fch. Alta"      "@I"      Arial      "Ult. Movim.
"      "@I"      Arial      
"Activo
"      "@I"      Arial      "Supervisor:
"      Arial      
Supervisor             Arial      "
"      Arial      "Vendedor:
"      Arial      Vendedor             Arial      	IdCliente             Arial      	razSocial             Arial      	Direccion             Arial      telefono             Arial      
Fecha_Alta      "@D"             Arial      	UltimoMov      "@D"             Arial      iif(Activo,'Si','No')             Arial      'Total Vendedor'             Arial      	IdCliente      "99999"             Arial      'Total Supervisor'             Arial      	IdCliente      "99999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      'Total General'             Arial      	IdCliente             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
*Release m.total
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
*Public m.Total

If Empty(m.IdSupervisor)
	m.IdSupervisor = null
endif

If Empty(m.IdVendedor)
	m.IdVendedor= null
ENDIF

If Empty(m.ClienteActivo)
	m.ClienteActivo= null
ENDIF


	
sql('exec vt_ClientesVendedor ?oapp.empresa, ?m.IdSupervisor, ?m.IdVendedor, ?m.ClienteActivo','Clientes')
	
SELECT Clientes
	
	
*!*	endi
*Sum Importe To m.Total

ENDPROC
     ?���    &  &                        �   %   d      �     �          �  U    U  
  �  � U  SETEO�  %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� � %�C�� ���b � T�� ���� �p ��C�X exec vt_ClientesVendedor ?oapp.empresa, ?m.IdSupervisor, ?m.IdVendedor, ?m.ClienteActivo� Clientes� �� F� � U  IDSUPERVISOR
 IDVENDEDOR CLIENTEACTIVO SQL CLIENTES Destroy,     �� BeforeOpenTables3     �� InitH     ��1 3 q 2 � A � A � A r 6                       #         J   R         m   �      )   &                  