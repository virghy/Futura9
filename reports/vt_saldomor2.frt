  T                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\futura1\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
      T  <  winspool  \\futura1\HP DeskJet 840C/841C/842C/843C  USB001                       �\\futura1\HP DeskJet 840C/841C   � XC�  �
od   ,  ,  Letter                                                                          DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        $   �$               $   �$         Arial      sucursal      sucursal+idcliente      sucursal+idcliente+moneda      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Listados de Clientes Morosos"             Arial      empresa             Arial      
"Sucursal"      Arial      )iif(empty(m.sucursal),'Todos',m.sucursal)             Arial      "Per�odo Fecha  a Cobrar:"      Arial      per�odo             Arial      	peror�gen             Arial      "Per�odo Ventas:"      Arial      "  Cliente"      Arial      "Nombre"      Arial      
"Tel�fono"      Arial      "Comprobante"      Arial      "Fecha Comp.
"      Arial      " Vencimiento"      Arial      " Ultimo Venc."      Arial      "En mora
"      Arial      	"Saldo
"      Arial      
"Moneda
"      Arial      alltrim( idcliente )             Arial      nombre             Arial      telefono             Arial      comprobante             Arial      enmora      "999,999,999.99"             Arial      saldo      "999,999,999.99"             Arial      moneda             Arial      primervencimiento             Arial      �ltimovencimiento             Arial      moneda             Arial      "Saldo cliente en:"      Arial      enmora      "999,999,999.99"             Arial      saldo      "999,999,999,999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      date()             Arial      time()             Arial      dataenvironment      ILeft = 10
Top = 6
Width = 381
Height = 380
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init

sql('exec vt_climoroso ?oapp.empresa,?m.sucursal,?m.dFecha,?m.hFecha, ;
?m.dvence, ?m.hvence, ?m.idcliente','rclimoroso')
SELECT rclimoroso



ENDPROC
     ����    j  j                           %   �                      �  U  
  �  � U  SETEO� } ��C�c exec vt_climoroso ?oapp.empresa,?m.sucursal,?m.dFecha,?m.hFecha, ?m.dvence, ?m.hvence, ?m.idcliente�
 rclimoroso�  �� F� � U  SQL
 RCLIMOROSO BeforeOpenTables,     �� InitA     ��1 q 2 �q 4                       $         ?   �       )   j                  