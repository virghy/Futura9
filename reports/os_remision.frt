  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\FUTURA1\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=9
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=2
COLLATE=1
      T  <  winspool  \\FUTURA1\HP DeskJet 840C/841C/842C/843C  USB001                       �\\FUTURA1\HP DeskJet 840C/841C   � XC� 	 �
od   ,  ,  Letter                                                                          DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        $   �$               $   �$         Arial      Arial      Arial      Arial      Arial      "Remisi�n de Servicio"             Arial      empresa             Arial      )iif(empty(m.idcliente),'Todos',razsocial)             Arial      
"Cliente:"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"IdOrden"      Arial      "Tipo
"      Arial      	"Fecha
"      Arial      "Cliente
"      Arial      "Descuento"      Arial      "Exenta"      Arial      	"Gravada"      Arial      "Iva"      Arial      "Total"      Arial      ordenservicio             Arial      
idremision             Arial      fecha             Arial      	razsocial             Arial      	descuento      "9,999,999,999"             Arial      exenta      "9,999,999,999"             Arial      gravada      "9,999,999,999"             Arial      iva      "99,999,999,999"             Arial      exenta+gravada+iva-descuento      "999,999,999,999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total General"      Arial      dataenvironment      KLeft = -38
Top = 97
Width = 793
Height = 439
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
*SET DATABASE TO DATOS 
Do SETEO

strsql='select a.ordenservicio,a.idremision,convert(varchar(10),a.fecha,103)as fecha, '+;
		'a.idcliente,b.razsocial,a.descuento,a.exenta,a.gravada,a.iva, a.idmoneda  '+;
		'from os_remision a, vt_clientes b '+;
		'where a.fecha >= ?m.dfecha '+ ;
		'and a.fecha <= ?m.hfecha '+ ;
		'and a.idcliente=b.idcliente '+ ;
		IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')
		
=sql(strsql,'os_rremi')

Select os_rremi

ENDPROC
     m���    T  T                        �   %   �           �          �  U  � �  �bT� ��N select a.ordenservicio,a.idremision,convert(varchar(10),a.fecha,103)as fecha, �J a.idcliente,b.razsocial,a.descuento,a.exenta,a.gravada,a.iva, a.idmoneda  �" from os_remision a, vt_clientes b � where a.fecha >= ?m.dfecha � and a.fecha <= ?m.hfecha � and a.idcliente=b.idcliente CC�� �
�& �   and a.idcliente = ?m.idcliente � �  6�� ��C � � os_rremi� �� F� � U  SETEO STRSQL	 IDCLIENTE SQL OS_RREMI BeforeOpenTables,     ��1 r (�r 2                       �      )   T                  