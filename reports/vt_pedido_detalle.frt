  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 )ORIENTATION=0
PAPERSIZE=32767
COLOR=2
      Arial      idpedido      Arial      Arial      Arial      Arial      Arial      Arial      "Estado de Pedidos Detallado"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "
"      Arial      	"Cliente"      Arial      "Repartidor
"      Arial      " Estado
"      "@I"      Arial      	"Fecha
"      "@I"      Arial      "Negocio
"      "@I"      Arial      "Receptor
"      "@I"      Arial      "Pedido"      Arial      "Cantidad
"      "@I"      Arial      
"Precio
"      "@I"      Arial      	"Total
"      "@I"      Arial      "Descripci�n
"      "@I"      Arial      	nropedido      "9,999,999"             Arial      cliente             Arial      fechapedido             Arial      negocio             Arial      receptor             Arial      
repartidor             Arial      )iif(idestado='P','Pendiente','Entregado')             Arial      descripcion             Arial      cantidad      	"999,999"             Arial      precio      	"999,999"             Arial      cantidad*precio      "99,999,999"             Arial      cantidad*precio      "99,999,999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      
"Total:
"      Arial      cantidad*precio      "99,999,999"             Arial      dataenvironment      ~Top = 32
Left = 177
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     wPROCEDURE Init
Public m.Total
If Empty(m.idcliente)
	m.idcliente= null
Endif
If Empty(m.idnegocio)
	m.idnegocio=null
endif
sql('exec vt_pedido_deta ?m.idcliente,?oapp.empresa,?m.dFecha,?m.hFecha,?m.idnegocio','rpedido')
SELECT rpedido

*Sum Importe To m.Total

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Destroy
Release m.total
ENDPROC
     ���                              ��   %   D      �     }          �  U  � 	 7��  � %�C�� ���' � T�� ���� � %�C�� ���I � T�� ���� �f ��C�O exec vt_pedido_deta ?m.idcliente,?oapp.empresa,?m.dFecha,?m.hFecha,?m.idnegocio� rpedido� �� F� � U  TOTAL	 IDCLIENTE	 IDNEGOCIO SQL RPEDIDO
  �  � U  SETEO 	 <��  � U  TOTAL Init,     �� BeforeOpenTables    �� Destroy-    ��1 � � A � A aq 5 q 2 � 1                               7  ?        ]  l      )                     