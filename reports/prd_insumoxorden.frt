  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
TTOPTION=1
COLLATE=0
      T  <  winspool  \\FUTURA1\HP DeskJet 840C/841C/842C/843C  USB001                       �\\FUTURA1\HP DeskJet 840C/841C   � XC� 	 �
od   ,  ,   Letter                                                                          DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        $   �$               $   �$         Arial      nroorden      plato      Arial      Arial      Arial      Arial      Arial      !"Insumos por Orden de Producci�n"             Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      cantidadbase      "@ 99"             Arial      cantidad      	"@ 9,999"             Arial      "Plato"      Arial      
"CantBase"      Arial      	"CantPed"      Arial      "Cant.Ingred"      Arial      
"Unid.Med"      Arial      "CostoUnit"      Arial      "CostoBase"      Arial      "CantProdu"      Arial      
"CostoPed"      Arial      "NroOrden:
"      Arial      nroorden             Arial      "Estado"      Arial      Liif(idestado='P','Pendiente',iif(idestado='E','En Peoduccion','Finalizado'))             Arial      plato             Arial       alltrim(idingre)+'-'+descripcion             Arial      
cantingred      "99.999"             Arial      unidad             Arial      	costounit      "@ 99,999.99"             Arial      costo      "@ 999,999"             Arial       iif(isnull(cantprod),0,cantprod)      	"@ 9,999"             Arial      (costo/cantidadbase)*cantidad      "@ 999,999"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      oLeft = 202
Top = 250
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
Name = "Dataenvironment"
      �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
sql("exec prd_InsumoxOrden ?oapp.empresa,?dFecha, ?hFecha","rInsumoxOrden")
SELECT rInsumoxOrden
ENDPROC
     Z���    A  A                        ��   %   �       �      �           �  U  
  �  � U  SETEO[ Q ��C�4 exec prd_InsumoxOrden ?oapp.empresa,?dFecha, ?hFecha� rInsumoxOrden�  �� F� � U  SQL RINSUMOXORDEN BeforeOpenTables,     �� InitA     ��1 q 3 q 1                       &         A   �       )   A                  