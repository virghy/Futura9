  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=TOSHIBA e-STUDIO451c PS3
OUTPUT=192.168.0.17
ORIENTATION=0
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
DUPLEX=1
YRESOLUTION=300
TTOPTION=1
COLLATE=0
      J  ,  winspool  TOSHIBA e-STUDIO451c PS3  192.168.0.17                       �TOSHIBA e-STUDIO451c PS3         � S�� 	 �
od   ,  ,   Letter                                                                            PRIV�0                                                                                       '''  '        @                                  \K hC                             x�d�      �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @  SMTJ     0T O S H I B A   e - S T U D I O 4 5 1 c   P S 3   Resolution 600dpi PageSize Letter PageRegion  InputSlot Auto MediaType Plain Duplex None Collate True OutputBin Bin2 Stapling Off HolePunch Off PrintMode Normal DINDigit1 0 DINDigit2 0 DINDigit3 0 DINDigit4 0 DINDigit5 0 DeptCode False DCDigit1 0 DCDigit2 0 DCDigit3 0 DCDigit4 0 DCDigit5 0 ColorResType ColorLowGeneral DistinguishThinLines True BlackOverPrint True PureBlackGray BlackGrayAuto TonerSave False BlankPage False Smoothing True                                                                            Arial      descripcion      Arial      Arial      Arial      Arial      Arial      *"Insumos por Orden de Producci�n(Resumen)"             Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      "Ingrediente
"      Arial      "Cant.Ingred"      Arial      
"Unid.Med"      Arial      "CostoUnit"      Arial      "CostoBase"      Arial      "CantProdu"      Arial      
"CostoPed"      Arial       alltrim(idingre)+'-'+descripcion             Arial      
cantingred      "999,999.99"      Arial      unidad             Arial      	costounit      "999,999.99"      Arial      costo      "99,999,999"      Arial       iif(isnull(cantprod),0,cantprod)      "9,999,999"      Arial      (costo/cantidadbase)*cantidad      "999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
      �PROCEDURE Init
sql("exec prd_InsuxOrdenResu ?oapp.empresa,?dFecha, ?hFecha","rInsumoxOrden")
SELECT rInsumoxOrden
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     \���    C  C                        ��   %   �       �      �           �  U  ] S ��C�6 exec prd_InsuxOrdenResu ?oapp.empresa,?dFecha, ?hFecha� rInsumoxOrden�  �� F� � U  SQL RINSUMOXORDEN
  �  � U  SETEO Init,     �� BeforeOpenTables�     ��1 1q 2 q 2                       s         �   �       )   C                  