  "�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=hp LaserJet 1150
OUTPUT=hpLaserJet1150
ORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2970
PAPERWIDTH=2100
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=-3
COLOR=2
DUPLEX=1
TTOPTION=3
COLLATE=0
      D  $  winspool  hp LaserJet 1150  hpLaserJet1150                        hp LaserJet 1150                !@� d߀ 	 �4d   ��                                                                                         B�e�               �� �� ��         4  �  d  	                                                                                                                                   A r i a l                                                       ��� H   �      B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   L a s e r J e t   1 1 5 0     1 1 5 0 , L o c a l O n l y , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Arial      contador      0      0      Arial      Arial      Arial      Arial      Arial      Arial      &"Stock Valorizado por Precio de Venta"             Arial      alltrim( empresa )             Arial      m.hfecha             Arial      "Hasta Fecha:"      Arial      Jiif(empty(m.deposito),'Consolidado',m.deposito+" - " + xdeposito.deposito)             Arial      "Dep�sito:"      Arial      "
"      Arial      "Precio ",clista.descripcion             Arial      "Descripci�n"      Arial      
"Producto"      Arial      "Existencia
"      Arial      	"Total
"      Arial      descripcion             Arial      producto             Arial      round( entrada-salida,2)      "9,999,999.99"             Arial      unidad             Arial      precio      "9,999,999.99"             Arial      !round( entrada-salida,2) * precio      "999,999,999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total General (",contador,")"             Arial      !round( entrada-salida,2) * precio      "99,999,999,999.99"             Arial      dataenvironment      KLeft = 177
Top = 32
Width = 381
Height = 355
Name = "Dataenvironment"
     
�PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SET NULLDISPLAY TO ''
SELECT fa_detfactu.producto, ;
	SUM(IIF(fa_detfactu.deposito_ent = m.deposito AND  NOT EMPTY(fa_detfactu.deposito_ent), fa_detfactu.cantidad, $0.0000)) AS entrada, ;
	SUM(IIF(fa_detfactu.deposito = m.deposito AND  NOT EMPTY(fa_detfactu.deposito), fa_detfactu.cantidad, $0.0000)) AS salida ;
FROM datos!fa_detfactu INNER JOIN datos!st_movimiento_base ON fa_detfactu.idfactura = st_movimiento_base.idmovimiento ;
WHERE st_movimiento_base.idempresa = oapp.empresa ;
	AND fa_detfactu.idempresa = oapp.empresa ;
	AND st_movimiento_base.fecha <= m.hfecha AND BETWEEN(fa_detfactu.producto, m.dproducto, m.hproducto) ;
GROUP BY fa_detfactu.producto ;
INTO CURSOR SALDO_STOCK ;
UNION ;
SELECT fa_detfactu.producto, ;
	SUM(IIF(fa_detfactu.deposito_ent = m.deposito AND  NOT EMPTY(fa_detfactu.deposito_ent), fa_detfactu.cantidad, $0.0000)) AS entrada, ;
	SUM(IIF(fa_detfactu.deposito = m.deposito AND  NOT EMPTY(fa_detfactu.deposito), fa_detfactu.cantidad, $0.0000)) AS salida ;
FROM datos!fa_detfactu INNER JOIN datos!vt_factura ON fa_detfactu.idfactura = vt_factura.idfactura ;
WHERE vt_factura.idempresa = oapp.empresa AND ;
	fa_detfactu.idempresa = oapp.empresa AND ;
	vt_factura.fecha <= m.hfecha AND BETWEEN(fa_detfactu.producto, m.dproducto, m.hproducto) ;
GROUP BY fa_detfactu.producto ;
UNION ;
SELECT fa_detfactu.producto, ;
SUM(IIF(fa_detfactu.deposito_ent = m.deposito AND  NOT EMPTY(fa_detfactu.deposito_ent), fa_detfactu.cantidad, $0.0000)) AS entrada, ;
SUM(IIF(fa_detfactu.deposito = m.deposito AND  NOT EMPTY(fa_detfactu.deposito), fa_detfactu.cantidad, $0.0000)) AS salida ;
FROM datos!fa_detfactu INNER JOIN datos!cp_factura ON fa_detfactu.idfactura = cp_factura.idfactura ;
WHERE cp_factura.idempresa = oapp.empresa ;
AND fa_detfactu.idempresa = oapp.empresa ;
AND cp_factura.fecha <= m.hfecha AND BETWEEN(fa_detfactu.producto, m.dproducto, m.hproducto) ;
GROUP BY fa_detfactu.producto

SELECT deposito ;
FROM datos!st_deposito ;
WHERE m.deposito = iddeposito ;
INTO CURSOR xdeposito

Select descripcion, ;
idLista ;
from datos!vt_listaprecio_base ;
where idLista = m.listaprecio AND idempresa = oApp.Empresa ;
order by 1 into cursor cLista


SELECT ss.producto, st_producto.descripcion, st_producto.unidad, ;
SUM(ss.entrada) AS entrada, SUM(ss.salida) AS salida, p.precio ;
FROM saldo_stock ss INNER JOIN datos!st_producto ON (ss.producto = st_producto.idproducto AND st_producto.idempresa = oapp.empresa) ;
LEFT JOIN vt_precios_base p ON ss.producto = p.IdProducto AND p.IdLista = ?m.listaPrecio;
GROUP BY 1 HAVING entrada <> salida ;
ORDER BY 2,1 INTO CURSOR saldos

SELECT Saldos
MESSAGEBOX(STR(RECCOUNT()))
ENDPROC
     J���    1  1                        X   %   �      �     �          �  U  
  �  � U  SETEO� G�(��  ��Do�� � datos!fa_detfactu��� datos!vt_factura ��  � � � ���  � ��CC�  � �� �
 C�  � �
	�	 �  � � �        6���Q� �CC�  � �� �
 C�  � �
	�	 �  � � �        6���Q� ��� � � � � �  � � � 	� � � �� 	� C�  � �� �� �	����  � ��� � datos!fa_detfactu��� datos!cp_factura ��  � � � ���  � ��CC�  � �� �
 C�  � �
	�	 �  � � �        6���Q� �CC�  � �� �
 C�  � �
	�	 �  � � �        6���Q� ��� � � � � �  � � � 	� � � �� 	� C�  � �� �� �	����  � �� datos!fa_detfactu��� datos!st_movimiento_base ��  � �	 �
 ���  � ��CC�  � �� �
 C�  � �
	�	 �  � � �        6���Q� �CC�  � �� �
 C�  � �
	�	 �  � � �        6���Q� ���	 � � � � �  � � � 	� �	 � �� 	� C�  � �� �� �	����  � ���� SALDO_STOCK�8 o� datos!st_deposito�� ����� � ����	 xdeposito�W o� datos!vt_listaprecio_base�� ��� ���� �� � � � � 	�������� cLista�� o� saldo_stockQ� ��� datos!st_producto �� � � �  � � � � � 	�X�� vt_precios_baseQ�  �� � � �  � � � �� 	��� � ��� � ��� � ��C� � ���Q� �C� � ���Q� �� � �������� � ����������� saldos� F�" � ��CCCNZ�x�� U#  FA_DETFACTU PRODUCTO DEPOSITO_ENT DEPOSITO CANTIDAD ENTRADA SALIDA DATOS	 IDFACTURA ST_MOVIMIENTO_BASE IDMOVIMIENTO	 IDEMPRESA OAPP EMPRESA FECHA HFECHA	 DPRODUCTO	 HPRODUCTO SALDO_STOCK
 VT_FACTURA
 CP_FACTURA
 IDDEPOSITO	 XDEPOSITO DESCRIPCION IDLISTA LISTAPRECIO CLISTA SS ST_PRODUCTO UNIDAD P PRECIO
 IDPRODUCTO VT_PRECIOS_BASE SALDOS BeforeOpenTables,     �� InitA     ��1 q 3 � �A4�vr � 1                       &         A   �
      )   1                  