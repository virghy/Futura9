  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=PrimoPDF
OUTPUT=PrimoPort:
ORIENTATION=1
PAPERSIZE=9
SCALE=100
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=600
COLOR=2
YRESOLUTION=600
TTOPTION=3
COLLATE=1
      8    winspool  PrimoPDF  PrimoPort:                       `PrimoPDF                        � �S� 	 �4d   X  X  A4                                                                                PRIV�0                                                                                       '''  '          �                                  P4 (�                             D�M      � �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial      t      alen(gaDep,1)      0      Arial      Arial      Arial      Arial      Arial      Arial      "Saldos de Stock por Deposito"             Arial      alltrim( empresa )             Arial      m.hfecha             Arial      "Hasta Fecha:"      Arial      "
"      Arial      
"Producto"      Arial      "Descripci�n"      Arial      
gaDep(1,2)             Arial      alen(gaDep,1)>0      "iif(alen(gaDep,1)>1,gaDep(2,2),'')             Arial      alen(gaDep,1)>1      "iif(alen(gaDep,1)>2,gaDep(3,2),'')             Arial      alen(gaDep,1)>2      "iif(alen(gaDep,1)>3,gaDep(4,2),'')             Arial      alen(gaDep,1)>3      "iif(alen(gaDep,1)>4,gaDep(5,2),'')             Arial      alen(gaDep,1)>4      "iif(alen(gaDep,1)>5,gaDep(6,2),'')             Arial      alen(gaDep,1)>5      "iif(alen(gaDep,1)>6,gaDep(7,2),'')             Arial      alen(gaDep,1)>6      "iif(alen(gaDep,1)>7,gaDep(8,2),'')             Arial      alen(gaDep,1)>7      producto             Arial      descripcion             Arial      unidad             Arial      D1      "9,999,999.99"             Arial      alen(gaDep,1)>0      iif(t>1,D2,0)      "9,999,999.99"             Arial      alen(gaDep,1)>1      iif(t>2,D3,0)      "9,999,999.99"             Arial      alen(gaDep,1)>2      iif(t>3,D4,0)      "9,999,999.99"             Arial      alen(gaDep,1)>3      iif(t>4,D5,0)      "9,999,999.99"             Arial      alen(gaDep,1)>4      iif(t>5,D6,0)      "9,999,999.99"             Arial      alen(gaDep,1)>5      iif(t>6,D7,0)      "9,999,999.99"             Arial      alen(gaDep,1)>6      iif(t>7,D8,0)      "9,999,999.99"             Arial      alen(gaDep,1)>7      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      dataenvironment      `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
RELEASE gadep

ENDPROC
PROCEDURE Init
SET NULLDISPLAY TO ''

sql("Select IdDeposito,Deposito from st_depositos where IdEmpresa=?oApp.Empresa ",'xdeposito')	
COPY TO ARRAY adep FIELDS iddeposito, deposito

IF EMPTY(m.hproducto)
	m.hproducto= 'ZZZ'
ENDIF


TEXT TO cmdSQL noshow
SELECT Producto=p.IdProducto,Descripcion,Unidad,Cantidad,IdDeposito
	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,null,?m.hfecha) s
			ON p.IdEmpresa=s.IdEmpresa AND 
				p.IdProducto = s.IdProducto 
	where p.IdEmpresa=?oApp.Empresa			
		and p.IdProducto between ?m.dproducto and ?m.hProducto
		and Cantidad <> 0
		and p.AfectaStock = 1
		order by p.IdProducto
ENDTEXT

sql(cmdSQL,'saldo')

cmdsql = " sum(iif(ss.IdDeposito=aDep(1,1),Cantidad,$0.0000)) as d1 "

FOR i = 2 TO ALEN(adep, 1)
     cmdsql = cmdsql + "," + " sum(iif(ss.IdDeposito=aDep(" + ALLTRIM(STR(i)) + ",1),0)) as d" + ALLTRIM(STR(i))
ENDFOR
SELECT Ss.producto, ss.descripcion, ss.unidad, &cmdSQL ;
FROM  Saldo ss ;
group by 1, 2,3;
order by 1 into cursor saldos
PUBLIC gadep(ALEN(adep, 1), 2)
ACOPY(adep, gadep)
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     h���    O  O                        ��   %   o      �     �          �  U  
  <�  � U  GADEP� G�(��  ��d ��C�K Select IdDeposito,Deposito from st_depositos where IdEmpresa=?oApp.Empresa �	 xdeposito�  �� (� � � � %�C�� ���� � T�� �� ZZZ�� �	 M(� ��I �C SELECT Producto=p.IdProducto,Descripcion,Unidad,Cantidad,IdDeposito�[ �U 	from st_Producto p inner join dbo.st_SaldoStock(?oApp.Empresa,null,null,?m.hfecha) s�( �" 			ON p.IdEmpresa=s.IdEmpresa AND �& �  				p.IdProducto = s.IdProducto �) �# 	where p.IdEmpresa=?oApp.Empresa			�> �8 		and p.IdProducto between ?m.dproducto and ?m.hProducto� � 		and Cantidad <> 0� � 		and p.AfectaStock = 1� � 		order by p.IdProducto� � ��C � � saldo�  ��G T� ��:  sum(iif(ss.IdDeposito=aDep(1,1),Cantidad,$0.0000)) as d1 �� �� ���(�C�� ����,�R T� �� � ,�  sum(iif(ss.IdDeposito=aDep(CC� Z�� ,1),0)) as dCC� Z��� ��y SELECT Ss.producto, ss.descripcion, ss.unidad, &cmdSQL  FROM  Saldo ss  group by 1, 2,3 order by 1 into cursor saldos
 7� �C�� ������� ��C�� �� ��� U  SQL ADEP
 IDDEPOSITO DEPOSITO	 HPRODUCTO CMDSQL I GADEP
  �  � U  SETEO Destroy,     �� InitA     �� BeforeOpenTablesZ    ��1 q 3 � B!A � ���a�����A br�!A ��!2 q 2                       "         =   o        �  �  *    )   O                  