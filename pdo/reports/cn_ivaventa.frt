  �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          B  *   winspool  hp deskjet 840c series  USB001  TS001                                         	hp deskjet 840c series          !@� h߀      d   ��                                                                                         B�e�                                   4  �  d  	                                n                                                                                                                                                                                         �  B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   d e s k j e t   8 4 0 c   s e r i e s   e r i e s , L o c a l O n l y , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           �DRIVER=winspool
DEVICE=hp deskjet 840c series
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=5
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=-3
COLOR=2
DUPLEX=1
TTOPTION=3
COLLATE=0
                   sucursal                       
datetime()                                                    Arial                          "P�g. " + str( _pageno,3 )                                                                     Arial                          Arial                          "Sucursal:"                    Arial                          " Fecha"                       cn_rivaventas.fechacomprobante                                                                 Arial                          cn_rivaventas.sucursal                                        Arial                          cn_rivaventas.comprobante                                     Arial                          Arial                          "Numero"                       cn_rivaventas.razsocial                                       Arial                          Arial                          
"Clientes"                     empresa                                                       Arial                          cn_rivaventas.ruc              Arial                          len( ALLTRIM(ruc) ) > 8        Arial                          " RUC"                         "Total General"                                               Arial                          Arial                          "Comprob/Tickets"             "LIBRO DE VENTAS LEY 125/91"                                                                   Arial                          Arial                                                         "MES:"                        Arial                          "A�O:"                        Arial                          $"Razon Social / Apellidos / Nombres"                            Arial                          "Valor Ventas"                 "@I"                           Arial                          "Retenciones"                 Arial                          "Dia"                          &cmonth(cn_rivaventas.fechacomprobante)                                                         Arial                          $year(cn_rivaventas.fechacomprobante)                                                           Arial                          "9999"                         Arial                          "Facturas/Notas"              Arial                          "Del"                          Arial                          "Al"                           cn_rivaventas.min                                             Arial                          "@Z 999999"                    cn_rivaventas.max                                             Arial                          "@Z 999999"                    #day(cn_rivaventas.fechacomprobante)                                                            Arial                          "99"                           	retencion                                                     Arial                          "999,999,999"                  	retencion                                                     Arial                          "999,999,999,999"              	gravadas5                                                     Arial                          "999,999,999,999"              Arial                          "Gravadas 5%"                  iva10                                                         Arial                          "999,999,999"                  Arial                          	"Exentas"                      -gravadas5 + gravadas10+ exentas+ iva5 + iva10                                                  Arial                          "999,999,999,999"              Arial                          "Total"                        exentas                                                       Arial                          "999,999,999,999"              Arial                          "    Iva 10%"                  	gravadas5                                                     Arial                          "999,999,999,999"              iva10                                                         Arial                          "999,999,999"                  -gravadas5 + gravadas10+ exentas+ iva5 + iva10                                                  Arial                          "999,999,999,999"              exentas                                                       Arial                          "999,999,999,999"              iva5                            cn_rivacompra.iva5            Arial                          "999,999,999"                  Arial                          "    Iva 5%"                   iva5                                                          Arial                          "999,999,999"                  
gravadas10                                                    Arial                          "999,999,999,999"              Arial                          "Gravadas 10%"                 
gravadas10                                                    Arial                          "999,999,999,999"              Arial                          	"Orden:"                      1IIF(m.ordenfactura='F','Fecha','Nro Comprobante')                                              Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                rLeft = 2
Top = 104
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
Name = "Dataenvironment"
               
�PROCEDURE Init
SET TEXTMERGE ON 
IF m.ordenfactura='F'
	m.orden = '3, 2'
ELSE
	m.orden = '2, 3'
ENDIF
		      

TEXT TO cmdSQL noshow
SELECT cn_iva.sucursal, cn_iva.comprobante,
  cn_iva.fechacomprobante, vt_clientes.razsocial,
  vt_clientes.ruc, MIN(d.comprobante) AS min,
  MAX(d.comprobante) AS max, 
  cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion ,
    sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    
   sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  
     sum(d.exentas) as Exentas, 
     sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    
     sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10    
 FROM vt_clientes,
   cn_iva left JOIN cn_iva_detalle d
   ON  cn_iva.idiva = d.idiva 
 WHERE cn_iva.c�digo = vt_clientes.idcliente
   AND (cn_iva.idempresa = ?oApp.empresa
   AND cn_iva.sucursal = ?m.sucursal
   AND MONTH(cn_iva.fechacomprobante) = ?m.mes
   AND YEAR(cn_iva.fechacomprobante) = ?m.a�o
   AND cn_iva.tipoiva = 'V'
   AND vt_clientes.idempresa = ?oApp.empresa 
   and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha)
GROUP BY cn_iva.sucursal, cn_iva.comprobante,
  cn_iva.fechacomprobante, cn_iva.c�digo, cn_iva.gravadas,
  cn_iva.exentas, cn_iva.iva, vt_clientes.razsocial,
  vt_clientes.ruc,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta
 UNION
 SELECT cn_iva.sucursal, cn_iva.comprobante,
  cn_iva.fechacomprobante,SPACE(45) AS rassocial, SPACE(15) AS ruc,
  MIN(d.comprobante) AS min,
  MAX(d.comprobante) AS max, cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion,
      sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    
   sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  
     sum(d.exentas) as Exentas, 
     sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    
     sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10    
 
 FROM  cn_iva INNER JOIN cn_iva_detalle d
   ON  cn_iva.idiva = d.idiva
 WHERE cn_iva.idempresa = ?oApp.empresa
   AND cn_iva.sucursal = ?m.sucursal
   AND MONTH(cn_iva.fechacomprobante) = ?m.mes
   AND YEAR(cn_iva.fechacomprobante) = ?m.a�o
   AND cn_iva.tipoiva = 'T'
   and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha
 GROUP BY cn_iva.sucursal, cn_iva.comprobante,
  cn_iva.fechacomprobante, cn_iva.c�digo, cn_iva.gravadas,
  cn_iva.exentas, cn_iva.iva,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta
 ORDER BY <<m.orden>>

ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
      m���    T  T                        ɞ   %   U      �  ?   }          �  U  �
 G` � %���  � F��- � T�� �� 3, 2�� �H � T�� �� 2, 3�� �	 M(� ��1 �+ SELECT cn_iva.sucursal, cn_iva.comprobante,�7 �1   cn_iva.fechacomprobante, vt_clientes.razsocial,�3 �-   vt_clientes.ruc, MIN(d.comprobante) AS min,�# �   MAX(d.comprobante) AS max, �F �@   cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion ,�[ �U     sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    �Z �T    sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  �& �       sum(d.exentas) as Exentas, �S �M      sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    �T �N      sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10    � �  FROM vt_clientes,�* �$    cn_iva left JOIN cn_iva_detalle d�$ �    ON  cn_iva.idiva = d.idiva �2 �,  WHERE cn_iva.c�digo = vt_clientes.idcliente�. �(    AND (cn_iva.idempresa = ?oApp.empresa�* �$    AND cn_iva.sucursal = ?m.sucursal�4 �.    AND MONTH(cn_iva.fechacomprobante) = ?m.mes�3 �-    AND YEAR(cn_iva.fechacomprobante) = ?m.a�o�! �    AND cn_iva.tipoiva = 'V'�3 �-    AND vt_clientes.idempresa = ?oApp.empresa �G �A    and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha)�3 �- GROUP BY cn_iva.sucursal, cn_iva.comprobante,�@ �:   cn_iva.fechacomprobante, cn_iva.c�digo, cn_iva.gravadas,�: �4   cn_iva.exentas, cn_iva.iva, vt_clientes.razsocial,�G �A   vt_clientes.ruc,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta� �  UNION�2 �,  SELECT cn_iva.sucursal, cn_iva.comprobante,�I �C   cn_iva.fechacomprobante,SPACE(45) AS rassocial, SPACE(15) AS ruc,�" �   MIN(d.comprobante) AS min,�` �Z   MAX(d.comprobante) AS max, cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion,�] �W       sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    �Z �T    sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  �& �       sum(d.exentas) as Exentas, �S �M      sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    �T �N      sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10    � �  �/ �)  FROM  cn_iva INNER JOIN cn_iva_detalle d�# �    ON  cn_iva.idiva = d.idiva�- �'  WHERE cn_iva.idempresa = ?oApp.empresa�* �$    AND cn_iva.sucursal = ?m.sucursal�4 �.    AND MONTH(cn_iva.fechacomprobante) = ?m.mes�3 �-    AND YEAR(cn_iva.fechacomprobante) = ?m.a�o�! �    AND cn_iva.tipoiva = 'T'�F �@    and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha�4 �.  GROUP BY cn_iva.sucursal, cn_iva.comprobante,�@ �:   cn_iva.fechacomprobante, cn_iva.c�digo, cn_iva.gravadas,�R �L   cn_iva.exentas, cn_iva.iva,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta� �  ORDER BY <<m.orden>>� �  � � ��C � � cn_rIvaVentas� �� F� � U  ORDENFACTURA ORDEN CMDSQL SQL CN_RIVAVENTAS
  �  � U  SETEO Init,     �� BeforeOpenTables@    ��1 a A1� 1A � q11a��a1A��A!��A11q1�q� !�!��a1Aq �1��A1aA!�a A �r 3 q 2                       n
     =   �
  �
  E    )   T                  �B q "� 