  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=9
SCALE=100
ASCII=100
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S� 	 �4d   ,  ,  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             D�M      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial      
idproducto      deposito      saldos      entrada - salida      ,iif(isnull(saldo_anterior),0,saldo_anterior)      Arial      Arial      Arial      Arial      Arial      Arial      %"Movimiento de Productos por Cliente"             Arial      alltrim( empresa )             Arial      m.dfecha,'al', m.hfecha             Arial      
"Periodo:"      Arial      Jiif(empty(m.deposito),'Consolidado',m.deposito+" - " + rmoviprod.deposito)             Arial      "Dep�sito:"      Arial      &alltrim(IdCliente) + " - " + RazSocial             Arial      
"Cliente:"      Arial      "
"      Arial      "Fecha"      Arial      "Cpbte."      Arial      "Referencia"      Arial      	"Precio "      Arial      "Costo"      Arial      
"Entrada "      Arial      "Salida"      Arial      	"Saldo
"      Arial      "Saldo anterior"      Arial      saldo_anterior      "9,999,999,999.99"             Arial      )alltrim(idproducto) + " - " + descripcion             Arial      "Producto:"      Arial      fecha             Arial      
referencia             Arial      '((comprob))+ALLTRIM(str(nvl(numero,0)))             Arial      IIF(RegimenTurismo,'RT','')             Arial      rmoviprod.precio      "999,999,999.99"             Arial      rmoviprod.costo_pro      "9,999,999.99"             Arial      rmoviprod.entrada      "@Z 9,999,999.99"             Arial      rmoviprod.salida      "@Z 9,999,999.99"             Arial      saldos      "9,999,999.99"             Arial      entrada      "@Z 9,999,999.99"             Arial      salida      "@Z 9,999,999.99"             Arial      saldos      "9,999,999.99"             Arial      	"Totales"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      KLeft = 177
Top = 32
Width = 381
Height = 355
Name = "Dataenvironment"
     ?PROCEDURE Init

If Empty(deposito)
	Messagebox('Ingrese Dep�sito',0,'Futura')
	Return .f.
ELSE


	sql('exec st_moviprodCli ?dFecha, ?hFecha,?oapp.empresa,?producto,?deposito, ?m.IdCliente','rmoviprod')

	Select rmoviprod
	*brow
*set
endi
*brow

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        �   %          ^     H          �  U  �  %�C�  ���> �( ��C� Ingrese Dep�sito� � Futura�x�� B�-�� �� �m ��C�T exec st_moviprodCli ?dFecha, ?hFecha,?oapp.empresa,?producto,?deposito, ?m.IdCliente�	 rmoviprod� �� F� � � U  DEPOSITO SQL	 RMOVIPROD
  �  � U  SETEO Init,     �� BeforeOpenTables    ��1 � �q � �r C 4 q 2                            	   *  4      )   �                  