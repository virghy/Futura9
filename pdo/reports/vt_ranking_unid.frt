  8   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC� 	 �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Arial                                                         ,"Ranking de Ventas en Unidades por Sucursal"                                                                                Arial                                                         
"Producto"                                                    Arial                                                         rranking.idproducto                                                                                                         Arial                                                         rranking.descripcion                                          Arial                                                         ""                                                           Arial                                                         "Unidades"                                                   Arial                                                         rranking.cantidad                                             "9,999,999.99"                                                Arial                                                         "Descripci�n"                                                 Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         "Periodo::"                                                   Arial                                                         "Sucursal:"                                                   Arial                                                         m.dfecha, ' al ' ,m.hfecha                                                                                                  Arial                                                         Kiif(isnull(m.Sucursal),'Consolidado',m.sucursal+" - " + rranking1.sucursal)                                                                                                                 Arial                                                         rranking.unidad                                                                                                             Arial                                                         "Acumulado"                                                  Arial                                                         " %s/Total"                                                 "@I"                                                          Arial                                                         "% s/Acum."                                                 "@I"                                                          Arial                                                         cantidad                                                      "9,999,999.99"                                                Arial                                                         Eiif(rranking.cantidad>0,round(rranking.cantidad * 100 / m.total,2),0)                                                         "999.99"                                                      Arial                                                         6iif(rranking.cantidad>0,round(acum * 100 / total,2),0)        
"9,999.99"                                                    Arial                                                         "Unid.Medida"                                                Arial                                                         
"Importe"                                                    Arial                                                         importe                                                       "9,999,999,999.99"                                            Arial                                                         acum                                                          cantidad                                                      0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE Init
DO seteo
Public m.Total
If Empty(m.sucursal)
	m.sucursal= null
ENDIF

TEXT TO cmdSQL NOSHOW 
	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,
	sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad* s.precio * cotizacion) as importe,
			sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad
	FROM         dbo.vt_factura v INNER JOIN
	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN
	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto
	WHERE v.idempresa = ?oApp.empresa
	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)
	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal
	order by 6 desc,1

	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa

ENDTEXT


	
sql(cmdSQL,'rranking')
SELECT rranking

Sum Cantidad To m.Total

ENDPROC
           ����    �  �                        >!   %         x     :          �  U  
  �  � U  SETEO
  <�  � U  TOTALn �  �	 7�� � %�C�� ���. � T�� ���� �	 M(� ��D �> 	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,�> �8 	sum(case when s.IdDeposito_Sal is not null then 1 else �x �r 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad* s.precio * cotizacion) as importe,�@ �: 			sum(case when s.IdDeposito_Sal is not null then 1 else �a �[ 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad�/ �) 	FROM         dbo.vt_factura v INNER JOIN�\ �V 	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN�k �e 	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto�( �" 	WHERE v.idempresa = ?oApp.empresa�g �a 	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)�@ �: 	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal� � 	order by 6 desc,1� �  �y �s 	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa� �  � � ��C � � rranking� �� F� � K(�� �� �� U  SETEO TOTAL SUCURSAL CMDSQL SQL RRANKING CANTIDAD BeforeOpenTables,     �� DestroyA     �� InitV     ��1 q 3 q 3 q � � A � A������q�a �a A �q � 2                       &         D   S         n   �  	    )   �                                                                                      �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                                                           \\futura5\HP DeskJet 840C/841C   � pC� 	 �4d   ,  ,  A4                                                                              DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize A4 Resolution r300x300 PM PlainEconoColor MediaType STANDARD Photo1200Mode Off ColorMode Color4 PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                        $   �$               $   �$                                                     Arial                                                         ,"Ranking de Ventas en Unidades por Sucursal"                                                                                Arial                                                         
"Producto"                                                    Arial                                                         rranking.idproducto                                                                                                         Arial                                                         rranking.descripcion                                          Arial                                                         ""                                                           Arial                                                         "Unidades"                                                   Arial                                                         rranking.cantidad                                             "9,999,999.99"                                                Arial                                                         "Descripci�n"                                                 Arial                                                         alltrim( empresa )                                                                                                          Arial                                                         
datetime()                                                                                                                  Arial                                                         "P�g. " + str( _pageno,3 )                                                                                                  Arial                                                         "Periodo::"                                                   Arial                                                         "Sucursal:"                                                   Arial                                                         m.dfecha, ' al ' ,m.hfecha                                                                                                  Arial                                                         Kiif(isnull(m.Sucursal),'Consolidado',m.sucursal+" - " + rranking1.sucursal)                                                                                                                 Arial                                                         rranking.unidad                                                                                                             Arial                                                         "Acumulado"                                                  Arial                                                         " %s/Total"                                                 "@I"                                                          Arial                                                         "% s/Acum."                                                 "@I"                                                          Arial                                                         cantidad                                                      "9,999,999.99"                                                Arial                                                         Eiif(rranking.cantidad>0,round(rranking.cantidad * 100 / m.total,2),0)                                                         "999.99"                                                      Arial                                                         6iif(rranking.cantidad>0,round(acum * 100 / total,2),0)        
"9,999.99"                                                    Arial                                                         "Unid.Medida"                                                Arial                                                         
"Importe"                                                    Arial                                                         importe                                                       "9,999,999,999.99"                                            Arial                                                         acum                                                          cantidad                                                      0                                                             Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               `Top = 75
Left = 208
Width = 381
Height = 355
DataSource = .NULL.
Name = "Dataenvironment"
                             �PROCEDURE Init
DO seteo
Public m.Total
If Empty(m.sucursal)
	m.sucursal= null
ENDIF

TEXT TO cmdSQL NOSHOW 
	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,
	sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad* s.precio * cotizacion) as importe,
			sum(case when s.IdDeposito_Sal is not null then 1 else 
		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad
	FROM         dbo.vt_factura v INNER JOIN
	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN
	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto
	WHERE v.idempresa = ?oApp.empresa
	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)
	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal
	order by 6 desc,1

	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa

ENDTEXT


	
sql(cmdSQL,'rranking')
SELECT rranking

Sum Cantidad To m.Total

ENDPROC
PROCEDURE Destroy
RELEASE total

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
           ����    �  �                        >!   %         x     :          �  U  n �  �	 7�� � %�C�� ���. � T�� ���� �	 M(� ��D �> 	SELECT     s.IdProducto, p.Descripcion, p.Unidad, v.sucursal,�> �8 	sum(case when s.IdDeposito_Sal is not null then 1 else �x �r 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad* s.precio * cotizacion) as importe,�@ �: 			sum(case when s.IdDeposito_Sal is not null then 1 else �a �[ 		(case when s.IdDeposito_Ent is not null then -1 else 0 end) end * s.Cantidad) as Cantidad�/ �) 	FROM         dbo.vt_factura v INNER JOIN�\ �V 	                      dbo.st_movimiento_Det s ON v.IdFactura = s.IdFactura INNER JOIN�k �e 	                      dbo.st_Producto p ON s.IdEmpresa = p.IdEmpresa AND s.IdProducto = p.IdProducto�( �" 	WHERE v.idempresa = ?oApp.empresa�g �a 	AND v.fecha between ?m.dfecha and  ?m.hfecha and (v.sucursal=?m.sucursal or ?m.sucursal is null)�@ �: 	group by s.IdProducto, p.Descripcion, p.Unidad,v.sucursal� � 	order by 6 desc,1� �  �y �s 	Select  s.descripci�n as sucursal  from sucursal  s where s.sucursal = ?m.sucursal and s.IdEmpresa = ?oApp.empresa� �  � � ��C � � rranking� �� F� � K(�� �� �� U  SETEO TOTAL SUCURSAL CMDSQL SQL RRANKING CANTIDAD
  <�  � U  TOTAL
  �  � U  SETEO Init,     �� Destroy�    �� BeforeOpenTables�    ��1 q � � A � A������q�a �a A �q � 3 q 3 q 2                       �        �  �  "      �  �  &    )   �                                                                                