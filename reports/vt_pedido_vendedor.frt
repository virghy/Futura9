  
�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      
idvendedor      Arial      Arial      Arial      Arial      Arial      Arial      "Pedidos por Vendedor"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "
"      Arial      
"Vendedor"      Arial      "Nro.Comanda"      Arial      	"Cliente"      Arial      "FechaPedido"      Arial      "FechaEntrega"      Arial      "Importe
"      "@I"      Arial      
"Estado
"      "@I"      Arial      	"Negocio"      Arial      vendedor             Arial      	nropedido      	"999,999"             Arial      cliente             Arial      negocio             Arial      
horapedido             Arial      horaentrega             Arial      importe      "9,999,999"             Arial      )iif(idestado='P','Pendiente','Entregado')             Arial      "TotalVendedor:
"      "@I"      Arial      importe      "9,999,999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      importe      "999,999,999"             Arial      
"Total:
"      "@I"      Arial      dataenvironment      Top = 177
Left = 174
Width = 381
Height = 355
Visible = .F.
TabStop = .F.
DataSource = .NULL.
Name = "Dataenvironment"
     >PROCEDURE Init
*Set Step On
If Empty(m.idvendedor)
	Store null To m.idvendedor
endif
sql('exec vt_pedido_vendedor ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idvendedor','rpedido_vendedor')
SELECT rpedido_vendedor

ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Destroy
*Release m.total
ENDPROC
     ����    �  �                        ��   %         O  
   ;          �  U  �  %�C��  ��� � J���(��  � �g ��C�G exec vt_pedido_vendedor ?oapp.empresa,?m.dFecha,?m.hFecha,?m.idvendedor� rpedido_vendedor� �� F� � U 
 IDVENDEDOR SQL RPEDIDO_VENDEDOR
  �  � U  SETEO  U   Init,     �� BeforeOpenTables�     �� Destroy�     ��1 � A qq 3 q 2 2                       �         �     
   	   #  3      )   �                  