  D                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=0
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=120
COLOR=2
YRESOLUTION=144
TTOPTION=3
COLLATE=0
      T    winspool  PrimoPDF  PrimoPort:                  1)  USB006                       �PrimoPDF -STUDIO451c PS3         � lS� 	 �
od   x   �    Letter                                                                            PRIV�0                                                                                       '''  '        � l                                  \K hC                             �{��      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       �   SMTJ     � P r i m o P D F   Resolution 600dpi PageSize Letter PageRegion  LeadingEdge  InputSlot OnlyOne                                          ne Collate True OutputBin Bin2 Stapling Off HolePunch Off PrintMode Normal DINDigit1 0 DINDigit2 0 DINDigit3 0 DINDigit4 0 DINDigit5 0 DeptCode False DCDigit1 0 DCDigit2 0 DCDigit3 0 DCDigit4 0 DCDigit5 0 ColorResType ColorLowGeneral DistinguishThinLines True BlackOverPrint True PureBlackGray BlackGrayAuto TonerSave False BlankPage False Smoothing True                                                                            Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Saldos de Stock por C�digo"             Arial      alltrim( empresa )             Arial      m.hfecha,m.idlista      Arial      "Hasta Fecha:"      Arial      Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)      Arial      "Dep�sito:"      Arial      "
"      Arial      "Existencia
"      Arial      "Precio Venta
"      Arial      "Precio Costo
"      Arial      "Costo Total
"      Arial      
"Producto"      Arial      "Descripci�n"      Arial       Idproducto,Descripcion, Catalogo      Arial      Cantidad      "999,999.99"      Arial      unidad      Arial      precio      "999,999,999.99"      Arial      
costo_prom      "999,999,999.99"      Arial      Cantidad*costo_prom      "999,999,999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      	"Total
"      Arial      Cantidad      "999,999.99"      Arial      precio*Cantidad      "999,999,999.99"      Arial      Cantidad*costo_prom      "999,999,999,999.99"      Arial      Cantidad*costo_prom      "999,999,999,999.99"      Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     bPROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
IF EMPTY(m.Deposito)
      m.deposito = null
ELSE
      sql("Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito",'xdeposito')     
ENDIF
 
IF EMPTY(m.hproducto)
      m.hproducto= 'ZZZ'
ENDIF

IF m.OrdenProducto='P'
	m.Orden='p.IdProducto'
ELSE
	m.Orden='p.Descripcion'
ENDIF
	
	 
 
TEXT TO cmdSQL NOSHOW TEXTMERGE 
SELECT p.IdProducto,Descripcion,Catalogo,Unidad,Cantidad,Costo_Prom=<<m.TipoCosto>>,
		v.Precio 
      from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s
		ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto
           left join vt_Precios v ON p.IdProducto =v.IdProducto 
           and p.IdEmpresa = v.IdEmpresa and
			v.IdLista = ?m.IdLista
      where p.IdEmpresa=?oApp.Empresa                
            and p.IdProducto between ?m.dproducto and ?m.hProducto
            and p.AfectaStock = 1
            and Cantidad <> 0
            order by <<m.Orden>>
ENDTEXT
 
sql(cmdSQL,'saldos')
SELECT saldos
ENDPROC
     K���    2  2                        ;�   %   o      �  !   �          �  U  
  �  � U  SETEO� %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� � %��� � P��� � T�� �� p.IdProducto�� �� T�� �� p.Descripcion�� �
 M(� `��Z �T SELECT p.IdProducto,Descripcion,Catalogo,Unidad,Cantidad,Costo_Prom=<<m.TipoCosto>>,� � 		v.Precio �g �a       from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�B �< 		ON p.IdEmpresa=s.IdEmpresa AND p.IdProducto = s.IdProducto�F �@            left join vt_Precios v ON p.IdProducto =v.IdProducto �2 �,            and p.IdEmpresa = v.IdEmpresa and� � 			v.IdLista = ?m.IdLista�; �5       where p.IdEmpresa=?oApp.Empresa                �H �B             and p.IdProducto between ?m.dproducto and ?m.hProducto�' �!             and p.AfectaStock = 1�# �             and Cantidad <> 0�& �              order by <<m.Orden>>� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO ORDENPRODUCTO ORDEN CMDSQL SALDOS BeforeOpenTables,     �� InitA     ��1 q 3 � � QA !A B�� �A � �q!a!���q1aA rq 1                       &         A   W      )   2                  