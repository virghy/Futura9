  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\tierra2\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=3
COLLATE=0
      T  <  winspool  \\tierra2\HP DeskJet 840C/841C/842C/843C  USB001                       \\tierra2\HP DeskJet 840C/841C   � pC� 	 �
od   ,  ,   Letter                                                                          DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize LETTER Resolution r300x300 PM PlainEconoMono MediaType STANDARD Photo1200Mode Off ColorMode Mono PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                       $   �$               $   �$         Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Informe de Stock Minimo"      Arial      alltrim( empresa )             Arial      m.hfecha             Arial      "Hasta Fecha:"      Arial      Kiif(isnull(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)      Arial      "Dep�sito:"      Arial      "
"      Arial      
"Minimo
"      Arial      "Existencia
"      Arial      "Requerido
"      Arial      "Ult. Costo"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      "Ult. Compra"      Arial      
Idproducto      Arial      descripcion             Arial      unidad      Arial      
Fecha_�lti      "@D"      Arial      	Ult_Costo      "99,999,999.99"      Arial      
Stock_Mini      "999,999.99"      Arial      Cantidad      "999,999.99"      Arial      abs(Cantidad - Stock_Mini)      "999,999.99"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
IF EMPTY(m.Deposito)
	m.deposito = null
ELSE
	sql("Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito",'xdeposito')	
ENDIF

IF EMPTY(m.hproducto)
	m.hproducto= 'ZZZ'
ENDIF


TEXT TO cmdSQL noshow
SELECT p.IdProducto,Descripcion,Unidad,SUM(Cantidad) as Cantidad, Stock_mini, Fecha_�lti, Ult_Costo
	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s
			ON p.IdEmpresa=s.IdEmpresa AND 
				p.IdProducto = s.IdProducto 
	where p.IdEmpresa=?oApp.Empresa			
		and p.IdProducto between ?m.dproducto and ?m.hProducto
		and p.AfectaStock = 1
--		and Cantidad <> 0
		and ISNULL(Stock_mini,0)>0
		group by p.IdProducto, Descripcion, Unidad, Stock_mini, Fecha_�lti, Ult_Costo
		having ISNULL(Stock_mini,0)>SUM(Cantidad)
		order by p.IdProducto
ENDTEXT

sql(cmdSQL,'saldos')
SELECT saldos
	 
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        \   %   �      @               �  U  q %�C��  ��� � T��  ���� �� �u ��C�\ Select Deposito from st_depositos where IdEmpresa=?oApp.Empresa and IdDeposito = ?m.Deposito�	 xdeposito� �� � %�C�� ���� � T�� �� ZZZ�� �	 M(� ��i �c SELECT p.IdProducto,Descripcion,Unidad,SUM(Cantidad) as Cantidad, Stock_mini, Fecha_�lti, Ult_Costo�b �\ 	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,?m.deposito,?m.hfecha) s�( �" 			ON p.IdEmpresa=s.IdEmpresa AND �& �  				p.IdProducto = s.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto� � 		and p.AfectaStock = 1� � --		and Cantidad <> 0�" � 		and ISNULL(Stock_mini,0)>0�U �O 		group by p.IdProducto, Descripcion, Unidad, Stock_mini, Fecha_�lti, Ult_Costo�1 �+ 		having ISNULL(Stock_mini,0)>SUM(Cantidad)� � 		order by p.IdProducto� � ��C � � saldos� �� F� � U  DEPOSITO SQL	 HPRODUCTO CMDSQL SALDOS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � � QA !A � �!�a����!Q�A rq 3 q 2                       �        �  �       )   �                  