  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\FUTURA1\HP DeskJet 840C/841C/842C/843C
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
COLLATE=0
      T  <  winspool  \\FUTURA1\HP DeskJet 840C/841C/842C/843C  USB001                       �\\FUTURA1\HP DeskJet 840C/841C   � XC�  �
od   ,  ,   Letter                                                                          DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        $   �$               $   �$         Arial      familia      
idproducto      Arial      Arial      Arial      Arial      Arial      Arial      "Ingredientes por Producto"             Arial      empresa             Arial      "Producto:"      Arial      *iif(isnull(m.producto),'Todos',m.producto)             Arial      0iif(isnull(m.familiaprod),'Todos',m.familiaprod)             Arial      
"Familia:"      Arial      %iif(isnull(m.familiaprod),'',familia)             Arial      "Familia
"      Arial      "Ingredientes"      Arial      
"Cantidad"      Arial      "Unid.Medida"      Arial      "Plato"      Arial      familia             Arial      "rtrim(idproducto)+'-'+rtrim(plato)             Arial      cantidad      	"999,999"             Arial      +rtrim(idingrediente)+'-'+rtrim(ingrediente)             Arial      cant      "999,999.999"             Arial      unidad             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      oLeft = 202
Top = 250
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
Name = "Dataenvironment"
     PROCEDURE Init
If Empty(m.producto)
	m.producto=null
Endif

If Empty(m.familiaprod)
	m.familiaprod=null
Endif

sql("exec prd_ingreprodu ?m.familiaprod,?m.producto, ?oapp.empresa","ringre")
SELECT ringre
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        �r   %   
      J     2          �  U  �  %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� �S ��C�= exec prd_ingreprodu ?m.familiaprod,?m.producto, ?oapp.empresa� ringre� �� F� � U  PRODUCTO FAMILIAPROD SQL RINGRE
  �  � U  SETEO Init,     �� BeforeOpenTables�     ��1 � A � A 2q 2 q 2                       �      
   �         )   �                  