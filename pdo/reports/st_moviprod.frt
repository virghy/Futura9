  H   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           `\\futura5\HP DeskJet 840C/841C   � XC� 	 �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                                                                                                                             Arial                                                         
idproducto                                                    "Movimiento de Productos"                                                                                                   Arial                                                         "Producto:"                                                   Arial                                                         )alltrim(idproducto) + " - " + descripcion                                                                                   Arial                                                         ""                                                           Arial                                                         "Saldo"                                                      Arial                                                         rmoviprod.entrada                                             "@Z 9,999,999.99"                                             Arial                                                         "Fecha"                                                       Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
"Periodo:"                                                    Arial                                                         "Dep�sito:"                                                   Arial                                                         m.dfecha,'al', m.hfecha                                                                                                     Arial                                                         Kiif(empty(m.deposito),'Consolidado',m.deposito+" - " + rmoviprod1.deposito)                                                   ""                                                            Arial                                                         "Cpbte."                                                      Arial                                                         "Referencia"                                                  Arial                                                         
"Entrada "                                                    Arial                                                         "Salida"                                                      Arial                                                         rmoviprod.salida                                              "@Z 9,999,999.99"                                             Arial                                                         saldos                                                        "9,999,999.99"                                                                                                              Arial                                                         fecha                                                                                                                       Arial                                                         
referencia                                                    Arial                                                         ;alltrim(((IdComprobante))) +'-'+alltrim(str(nvl(numero,0)))                                                                   Arial                                                         saldo_anterior                                                "999,999.99"                                                  Arial                                                         "Saldo anterior"                                              Arial                                                         entrada                                                       "@Z 9,999,999.99"                                                                                                           Arial                                                         salida                                                        "@Z 9,999,999.99"                                                                                                           Arial                                                         saldos                                                        "9,999,999.99"                                                                                                              Arial                                                         	"Totales"                                                     Arial                                                         rmoviprod.precio                                              "99,999,999.99"                                               Arial                                                         	"Precio "                                                     Arial                                                         "Costo"                                                       Arial                                                         rmoviprod.costo_pro                                           "9,999,999.99"                                                                                                              Arial                                                         saldos                                                        entrada - salida                                              ,iif(isnull(saldo_anterior),0,saldo_anterior)                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             8PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init

If Empty(m.deposito)
	Messagebox('Ingrese Dep�sito',0,'Futura')
	Return .f.
else
*	sql('exec st_moviprod ?m.dFecha, ?m.hFecha,?oapp.empresa,?m.producto,?m.deposito','rmoviprod')
	TEXT TO cmdSQL noshow
		Declare @dFecha1 datetime
		Set @dFecha1=dateadd(dd,-1,?m.dfecha) 



		SELECT ss.*, isnull(saldo_anterior.Cantidad,0)as saldo_anterior, st_producto.descripcion, st_producto.unidad 
		FROM dbo.st_MoviProducto(?oApp.Empresa,?m.Producto, ?m.Deposito,?m.dFecha,?m.hFecha  )ss left join 	
		dbo.st_SaldoStock(?oApp.Empresa,?m.Producto, ?m.Deposito,@dFecha1)   SALDO_ANTERIOR ON ss.idproducto = SALDO_ANTERIOR.idproducto 
		left JOIN st_producto 
			ON ss.idproducto = st_producto.idproducto and ss.IdEmpresa = st_producto.IdEmpresa
		ORDER BY ss.idproducto, fecha, entrada DESC,  numero 
		
		SELECT deposito 
		FROM st_depositos WHERE IdEmpresa = ?oApp.Empresa and IdDeposito = ?m.deposito  


	ENDTEXT
	sql(cmdSQL,'rmoviprod')
	Select rmoviprod
*	brow
*set
ENDIF 
*brow


ENDPROC
     ����    �  �                        ܩ   %   �      K               �  U  
  �  � U  SETEO� %�C��  ���@ �( ��C� Ingrese Dep�sito� � Futura�x�� B�-�� �{�	 M(� ��! � 		Declare @dFecha1 datetime�. �( 		Set @dFecha1=dateadd(dd,-1,?m.dfecha) � �  � �  � �  �u �o 		SELECT ss.*, isnull(saldo_anterior.Cantidad,0)as saldo_anterior, st_producto.descripcion, st_producto.unidad �l �f 		FROM dbo.st_MoviProducto(?oApp.Empresa,?m.Producto, ?m.Deposito,?m.dFecha,?m.hFecha  )ss left join 	�� �� 		dbo.st_SaldoStock(?oApp.Empresa,?m.Producto, ?m.Deposito,@dFecha1)   SALDO_ANTERIOR ON ss.idproducto = SALDO_ANTERIOR.idproducto � � 		left JOIN st_producto �[ �U 			ON ss.idproducto = st_producto.idproducto and ss.IdEmpresa = st_producto.IdEmpresa�= �7 		ORDER BY ss.idproducto, fecha, entrada DESC,  numero � � 		� � 		SELECT deposito �X �R 		FROM st_depositos WHERE IdEmpresa = ?oApp.Empresa and IdDeposito = ?m.deposito  � �  � �  � � ��C � �	 rmoviprod� �� F� � � U  DEPOSITO CMDSQL SQL	 RMOVIPROD BeforeOpenTables,     �� InitA     ��1 q 3 �q � � �a a a Q������ ��a a A �q C 4                       &         A   -      )   �                                                                                   �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                                         T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           `\\futura5\HP DeskJet 840C/841C   � XC� 	 �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                                                                                                                             Arial                                                         
idproducto                                                    "Movimiento de Productos"                                                                                                   Arial                                                         "Producto:"                                                   Arial                                                         )alltrim(idproducto) + " - " + descripcion                                                                                   Arial                                                         ""                                                           Arial                                                         "Saldo"                                                      Arial                                                         rmoviprod.entrada                                             "@Z 9,999,999.99"                                             Arial                                                         "Fecha"                                                       Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         
"Periodo:"                                                    Arial                                                         "Dep�sito:"                                                   Arial                                                         m.dfecha,'al', m.hfecha                                                                                                     Arial                                                         Kiif(empty(m.deposito),'Consolidado',m.deposito+" - " + rmoviprod1.deposito)                                                   ""                                                            Arial                                                         "Cpbte."                                                      Arial                                                         "Referencia"                                                  Arial                                                         
"Entrada "                                                    Arial                                                         "Salida"                                                      Arial                                                         rmoviprod.salida                                              "@Z 9,999,999.99"                                             Arial                                                         saldos                                                        "9,999,999.99"                                                                                                              Arial                                                         fecha                                                                                                                       Arial                                                         
referencia                                                    Arial                                                         ;alltrim(((IdComprobante))) +'-'+alltrim(str(nvl(numero,0)))                                                                   Arial                                                         saldo_anterior                                                "999,999.99"                                                  Arial                                                         "Saldo anterior"                                              Arial                                                         entrada                                                       "@Z 9,999,999.99"                                                                                                           Arial                                                         salida                                                        "@Z 9,999,999.99"                                                                                                           Arial                                                         saldos                                                        "9,999,999.99"                                                                                                              Arial                                                         	"Totales"                                                     Arial                                                         rmoviprod.precio                                              "99,999,999.99"                                               Arial                                                         	"Precio "                                                     Arial                                                         "Costo"                                                       Arial                                                         rmoviprod.costo_pro                                           "9,999,999.99"                                                                                                              Arial                                                         saldos                                                        entrada - salida                                              ,iif(isnull(saldo_anterior),0,saldo_anterior)                  Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 32
Left = 177
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             �PROCEDURE Init

If Empty(m.deposito)
	Messagebox('Ingrese Dep�sito',0,'Futura')
	Return .f.
else
*	sql('exec st_moviprod ?m.dFecha, ?m.hFecha,?oapp.empresa,?m.producto,?m.deposito','rmoviprod')
	TEXT TO cmdSQL noshow
		Declare @dFecha1 datetime
		Set @dFecha1=dateadd(dd,-1,?m.dfecha) 



		SELECT ss.*, isnull(saldo_anterior.Cantidad,0)as saldo_anterior, st_producto.descripcion, st_producto.unidad 
		FROM dbo.st_MoviProducto(?oApp.Empresa,?m.Producto, ?m.Deposito,?m.dFecha,?m.hFecha  )ss left join 	
		dbo.st_SaldoStock(?oApp.Empresa,?m.Producto, ?m.Deposito,@dFecha1)   SALDO_ANTERIOR ON ss.idproducto = SALDO_ANTERIOR.idproducto 
		left JOIN st_producto 
			ON ss.idproducto = st_producto.idproducto and ss.IdEmpresa = st_producto.IdEmpresa
		ORDER BY ss.idproducto, fecha, entrada DESC,  numero 

	ENDTEXT
	sql(cmdSQL,'rmoviprod')
	Select rmoviprod
*	brow
*set
ENDIF 
*brow


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
                                                   7���                              3   %   k      �     �          �  U   %�C��  ���@ �( ��C� Ingrese Dep�sito� � Futura�x�� B�-�� ���	 M(� ��! � 		Declare @dFecha1 datetime�. �( 		Set @dFecha1=dateadd(dd,-1,?m.dfecha) � �  � �  � �  �u �o 		SELECT ss.*, isnull(saldo_anterior.Cantidad,0)as saldo_anterior, st_producto.descripcion, st_producto.unidad �l �f 		FROM dbo.st_MoviProducto(?oApp.Empresa,?m.Producto, ?m.Deposito,?m.dFecha,?m.hFecha  )ss left join 	�� �� 		dbo.st_SaldoStock(?oApp.Empresa,?m.Producto, ?m.Deposito,@dFecha1)   SALDO_ANTERIOR ON ss.idproducto = SALDO_ANTERIOR.idproducto � � 		left JOIN st_producto �[ �U 			ON ss.idproducto = st_producto.idproducto and ss.IdEmpresa = st_producto.IdEmpresa�= �7 		ORDER BY ss.idproducto, fecha, entrada DESC,  numero � �  � � ��C � �	 rmoviprod� �� F� � � U  DEPOSITO CMDSQL SQL	 RMOVIPROD
  �  � U  SETEO Init,     �� BeforeOpenTablesV    ��1 �q � � �a a a Q�����a A �q C 5 q 2                       �        �  �      )                      