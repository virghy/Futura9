  
�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      acum      importe      0      Arial      Arial      Arial      Arial      Arial      Arial      )"Ranking de Nro. de Pedidos por Clientes"             Arial      alltrim( empresa )             Arial      m.dfecha, ' al ' ,m.hfecha             Arial      
"Periodo:"      Arial      "Sucursal:"      Arial      "
"      Arial      "Veces"      Arial      "IdCliente"      Arial      "Raz�n Social"      Arial      "Importe en
Moneda Local
"      "@I"      Arial      "Acumulado
"      Arial      " %
s/Total
"      "@I"      Arial      "% 
s/Acum.
"      "@I"      Arial      	idcliente             Arial      	razsocial             Arial      veces      "9,999,999"             Arial      importe      "999,999,999,999.99"             Arial      acum      "999,999,999,999.99"             Arial      round(importe * 100 / total,2)      "999.99"             Arial      round(acum * 100 / total,2)      "999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     OPROCEDURE Destroy
Release m.total
ENDPROC
PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init
Public m.Total
If Empty(m.idcliente)
	m.idcliente= null
Endif
	
sql('exec vt_cliente_veces ?m.idcliente,?oapp.empresa,?m.dFecha,?m.hFecha','rcliente_importe')
SELECT rcliente_importe

Sum Importe To m.Total

ENDPROC
     ���    �  �                        C�   %   6      �     o          �  U   	 <��  � U  TOTAL
  �  � U  SETEO� 	 7��  � %�C�� ���' � T�� ���� �d ��C�D exec vt_cliente_veces ?m.idcliente,?oapp.empresa,?m.dFecha,?m.hFecha� rcliente_importe� �� F� � K(��  �� �� U  TOTAL	 IDCLIENTE SQL RCLIENTE_IMPORTE IMPORTE Destroy,     �� BeforeOpenTablesC     �� InitX     ��1 � 2 q 2 � � A Bq � 2                       "         I   Q         l   D      )   �                  