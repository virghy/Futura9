  '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Epson LQ-1070+ ESC/P 2
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=9
ASCII=9
COPIES=1
DEFAULTSOURCE=15
PRINTQUALITY=360
COLOR=1
YRESOLUTION=360
TTOPTION=2
COLLATE=0
      A  *   winspool  Epson LQ-1070+ ESC/P 2 eLPT1: 50                     Epson LQ-1070+ ESC/P 2           � 4C� 	 �4d   h  h   A4                                                            ����                DINU"   4  "��H                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          �ںh p   L a s e r J e t   1 1 5 0     1 1 5 0 , L o c a l O n l y , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Courier New      "A"      
PrecioProd      precio      0      ParcialFactura      PrecioProd * Cantidad      0      TotalFactura      Exenta + Gravada + Iva      0      Courier New      Krtrim(PC.P_SETUP)+rtrim(Pc.P_10CPI)+rtrim(Pc.P_COMPRESS)+replicate('-',120)             Courier New      artrim(Pc.P_BOLDON)+SPACE(20) + left(" "+space(50),50) +SPACE(30)+ Comprobante+rtrim(Pc.P_BOLDOFF)             Courier New      Xrtrim(pc.p_superon) + space(100) + TRANSFORM(NROCOMPROB,'999999') + rtrim(pc.p_superoff)             Courier New      replicate('-',120)             Courier New      �SPACE(5) + "Lugar y Fecha:  Asuncion, " +  TRANSFORM(DAY(FECHA)) + " de "+ TRANSFORM(CMONTH(FECHA))  + " de " + TRANSFORM(year(FECHA))             Courier New      `CPCONVERT(1252,850,SPACE(5) + "Se�or(es) : " + LEFT(RAZSOCIAL,45) + SPACE(30) +  "RUC : " + RUC)             Courier New      kCPCONVERT(1252,850,SPACE(5) + "Direcci�n : " + LEFT(DIRECCION,45)+ SPACE(30) + "Cond. de Venta : " + NOTAS)             Courier New      SPACE(5) + "Vendedor :"             Courier New      replicate('-',120)             Courier New      tSPACE(5) +"Art./Codigo   Cantidad   Clase de Mercaderias/Servicios               Precio Unit.   Exentas    Gravadas"             Courier New      DETALLE1 + DETALLE2             Courier New      ESPACE(66)+"       Parcial: " +TRANSFORM(ParcialFactura,'999,999,999')             Courier New      :SPACE(70)+"Descuento : " +TRANSFORM(ImpDesc,'999,999,999')             Courier New      REPLICATE("-",120)             Courier New      ]"Sub-Totales :" +space(71) + TRANSFORM(Exenta,'999,999,999')+TRANSFORM(Gravada,'999,999,999')             Courier New      6"IVA 10%" +space(84) +TRANSFORM(Iva,'999,999,999,999')             Courier New      "Total a Pagar:"             Courier New      [LEFT(NUMERAL(TotalFactura)+SPACE(90),90)+SPACE(1)+TRANSFORM(TotalFactura,'999,999,999,999')             Courier New      REPLICATE("-",120) + pc.p_ff             Courier New      dataenvironment      LLeft = 154
Top = 263
Width = 520
Height = 200
Name = "Dataenvironment"
     �PROCEDURE Destroy
RELEASE cr_ff, cr_lf, cr_tab, cr_space
ENDPROC
PROCEDURE Init
LCIDFACTURA='V2107'
SET SAFETY OFF

_ASCIIROWS = 33
_asciicols = 132
SELECT vt_factura.nrocomprob,  ;
       vt_factura.idcliente,  ;
       vt_factura.idcondicion,  ;
       vt_factura.fecha,  ;
       vt_factura.sucursal,  ;
       vt_factura.impdesc,  ;
       vt_factura.descuento,  ;
       vt_factura.exenta,  ;
       vt_factura.gravada,  ;
       vt_factura.iva,  ;
       vt_factura.idvendedor,  ;
       fa_detfactu.producto AS prod,  ;
       st_producto_base.descripcion AS DESC,  ;
       fa_detfactu.precio,  ;
       fa_detfactu.cantidad,  ;
       vt_clientes_base.razsocial,  ;
       vt_factura.idcomprob,  ;
       vt_condicion_base.descripcion  ;
       AS condicion,  ;
       vt_clientes_base.direccion,  ;
       vt_clientes_base.ruc,  ;
       fa_detfactu.iva AS  ;
       iva_prod,  ;
       fa_detfactu.real,  ;
       vt_cpbt_vta_base.descripcion AS comprobante,;
       Notas,;
       SPACE(7)+LEFT(PRODUCTO,10)+;
			SPACE(4)+TRANSFORM(CANTIDAD,'9999')+;
			SPACE(3)+CPCONVERT(1252,850,LEFT(st_producto_base.DESCRIPCION,40))+;
			SPACE(2)+TRANSFORM(fa_detfactu.precio,'999,999,999') AS DETALLE1, ;
			SPACE(3)+IIF(fa_detfactu.iva=0,TRANSFORM(fa_detfactu.precio*CANTIDAD,'999,999,999'),SPACE(11))+;
			IIF(fa_detfactu.iva>0,TRANSFORM(fa_detfactu.precio*CANTIDAD,'999,999,999'),SPACE(11)) AS DETALLE2,;
			IdDetalle;
       FROM datos!vt_factura, ;
       datos!fa_detfactu, ;
       datos!st_producto_base, ;
       datos!vt_clientes_base, ;
       datos!vt_condicion_base, ;
       datos!vt_cpbt_vta_base, ;
       datos!bs_monedas ;
       WHERE  ;
       vt_factura.idfactura =  ;
       fa_detfactu.idfactura AND  ;
       fa_detfactu.producto =  ;
       st_producto_base.idproducto  ;
       AND vt_factura.idcliente =  ;
       vt_clientes_base.idcliente  ;
       AND vt_factura.idcondicion =  ;
       vt_condicion_base.idcondicion  ;
       AND vt_factura.idcomprob =  ;
       vt_cpbt_vta_base.idcomprobante  ;
       AND vt_factura.idmoneda =  ;
       bs_monedas.idmoneda AND  ;
       (vt_factura.idempresa =  ;
       oapp.empresa AND  ;
       fa_detfactu.idempresa =  ;
       oapp.empresa AND  ;
       vt_factura.idfactura =  ;
       ?m.LCIDFACTURA AND  ;
       st_producto_base.idempresa =  ;
       oapp.empresa AND  ;
       vt_clientes_base.idempresa =  ;
       oapp.empresa AND  ;
       vt_condicion_base.idempresa =  ;
       oapp.empresa AND  ;
       vt_cpbt_vta_base.idempresa =  ;
       oapp.empresa) ;
       ORDER BY IdDetalle ;
       INTO CURSOR  vt_rFactura READWRITE 
SELECT * FROM p_codes WHERE  ;
         p_name == "Epson LX"  ;
         INTO CURSOR pc
SELECT vt_rfactura

lnTotal =13
lnLineas = RECCOUNT()
SCATTER FIELDS ImpDesc,Descuento,Exenta,Gravada,Iva MEMVAR 
FOR i=lnLineas TO lntotal
	APPEND BLANK
	GATHER MEMVAR fields ImpDesc,Descuento,Exenta,Gravada,IVa
ENDFOR


ENDPROC
     d���    K  K                        yr   %   �      �     �          �  U    <�  � � � � U  CR_FF CR_LF CR_TAB CR_SPACE+ T�  �� V2107�� G.� T�?��!�� T�>�����]o� datos!vt_factura� datos!fa_detfactu� datos!st_producto_base� datos!vt_clientes_base� datos!vt_condicion_base� datos!vt_cpbt_vta_base� datos!bs_monedas�� � ��� � ��� � ��� � ��� � ��� � ��� � ��� �	 ��� �
 ��� � ��� � ��� � �Q� �� � �Q� �� � ��� � ��� � ��� � ��� � �Q� �� � ��� � ��� � �Q� �� � ��� � �Q� ��  ��C�XC� �
=C�XC� � 9999_C�XC���RC� � �(=�9C�XC� � � 999,999,999_�Q�! �C�XC� � � � C� � � � 999,999,999_� C�X6C� � � � C� � � � 999,999,999_� C�X6�Q�" ��# ���� �% � �% � � � � �& 	� � � � � 	� � � � � 	� � � � �' 	� � �( �) �( 	�t � �* �+ �, � � �* �+ �, 	� � �% ��  	� � �* �+ �, 	� � �* �+ �, 	� � �* �+ �, 	� � �* �+ �, 		����# ���� vt_rFactura��) o� p_codes����/ � Epson LX���� pc� F�- � T�1 ���� T�2 �CN�� ^�� � �	 �
 � � ��3 ��2 �(��1 ��$� � _�� � �	 �
 � � �� U4  LCIDFACTURA
 VT_FACTURA
 NROCOMPROB	 IDCLIENTE IDCONDICION FECHA SUCURSAL IMPDESC	 DESCUENTO EXENTA GRAVADA IVA
 IDVENDEDOR FA_DETFACTU PRODUCTO PROD ST_PRODUCTO_BASE DESCRIPCION DESC PRECIO CANTIDAD VT_CLIENTES_BASE	 RAZSOCIAL	 IDCOMPROB VT_CONDICION_BASE	 CONDICION	 DIRECCION RUC IVA_PROD REAL VT_CPBT_VTA_BASE COMPROBANTE NOTAS DETALLE1 DETALLE2	 IDDETALLE DATOS	 IDFACTURA
 IDPRODUCTO IDCOMPROBANTE IDMONEDA
 BS_MONEDAS	 IDEMPRESA OAPP EMPRESA VT_RFACTURA P_CODES P_NAME PC LNTOTAL LNLINEAS I Destroy,     �� Initf     ��1 12 !a � � @�5�q � � �qQ �A 3                       9         T   �      )   K                  