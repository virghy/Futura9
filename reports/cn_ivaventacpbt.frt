  ;�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      sucursal      ComprobanteDesc      m.TipoOperacion      iif(TipoOperacion='D',1,-1)      0      Arial      Arial      Arial      Arial      Arial      empresa             Arial      #"Detalle de Ventas por Comprobante"             Arial      &cmonth(cn_rivaventas.fechacomprobante)             Arial      $year(cn_rivaventas.fechacomprobante)      "9999"             Arial      1IIF(m.ordenfactura='F','Fecha','Nro Comprobante')             Arial      .IIF(m.CondicionIVA='T','Todos',m.CondicionIVA)      Arial      "MES:
"             Arial      "A�O:
"      Arial      
"Orden:
"      Arial      "Condicion Venta:
"      Arial      "Comprob/Tickets
"      Arial      "Facturas/Notas
"      Arial      
"Clientes"      Arial      "Valor Ventas"      Arial      "Retenciones
"      "@I"      Arial      "Dia"      Arial      "Del"      Arial      "Al"      Arial      "Cond."      Arial      "Numero"      Arial      " Fecha"      Arial      ""Razon Social /Apellidos /Nombres"      Arial      " RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      cn_rivaventas.sucursal             Arial      "Sucursal:"      Arial       "Comprobante: ", ComprobanteDesc             Arial      #day(cn_rivaventas.fechacomprobante)      "99"             Arial      cn_rivaventas.min      "@Z 999999"             Arial      cn_rivaventas.max      "@Z 999999"             Arial      cn_rivaventas.fechacomprobante      "@D"      Arial      cn_rivaventas.razsocial             Arial      cn_rivaventas.ruc      Arial      	gravadas5      "999,999,999,999"      Arial      iva5      "999,999,999"       cn_rivacompra.iva5      Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      	retencion      "999,999,999"             Arial      IdCondicion      Arial      cn_rivaventas.comprobante             Arial      "Total: ", ComprobanteDesc             Arial      	gravadas5      "999,999,999,999"             Arial      iva5      "999,999,999"             Arial      
gravadas10      "999,999,999,999"             Arial      iva10      "999,999,999"             Arial      exentas      "999,999,999,999"             Arial      -gravadas5 + gravadas10+ exentas+ iva5 + iva10      "999,999,999,999"             Arial      	retencion      "999,999,999,999"             Arial      "Total: ", Sucursal      Arial      "Total Facturas "      Arial      "iif(m.TipoOperacion=1,gravadas5,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=1,iva5,0)      "999,999,999"      Arial      #iif(m.TipoOperacion=1,gravadas10,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=1,iva10,0)      "999,999,999"      Arial       iif(m.TipoOperacion=1,exentas,0)      "999,999,999,999"      Arial      Hiif(m.TipoOperacion=1,(gravadas5 + gravadas10+ exentas+ iva5 + iva10),0)      "999,999,999,999"      Arial      "iif(m.TipoOperacion=1,retencion,0)      "999,999,999,999"      Arial      "Total Creditos "      Arial      #iif(m.TipoOperacion=-1,gravadas5,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=-1,iva5,0)      "999,999,999"      Arial      $iif(m.TipoOperacion=-1,gravadas10,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=-1,iva10,0)      "999,999,999"      Arial      !iif(m.TipoOperacion=-1,exentas,0)      "999,999,999,999"      Arial      Iiif(m.TipoOperacion=-1,(gravadas5 + gravadas10+ exentas+ iva5 + iva10),0)      "999,999,999,999"      Arial      #iif(m.TipoOperacion=-1,retencion,0)      "999,999,999,999"      Arial      "Saldo "      Arial      gravadas5 * m.TipoOperacion      "999,999,999,999"      Arial      iva5* m.TipoOperacion      "999,999,999"      Arial      gravadas10 * m.TipoOperacion      "999,999,999,999"      Arial      iva10* m.TipoOperacion      "999,999,999"      Arial      exentas* m.TipoOperacion      "999,999,999,999"      Arial      @(gravadas5 + gravadas10+ exentas+ iva5 + iva10)* m.TipoOperacion      "999,999,999,999"      Arial      retencion* m.TipoOperacion      "999,999,999,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      "Total General"             Arial      "Total Facturas "      Arial      "iif(m.TipoOperacion=1,gravadas5,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=1,iva5,0)      "999,999,999"      Arial      #iif(m.TipoOperacion=1,gravadas10,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=1,iva10,0)      "999,999,999"      Arial       iif(m.TipoOperacion=1,exentas,0)      "999,999,999,999"      Arial      Hiif(m.TipoOperacion=1,(gravadas5 + gravadas10+ exentas+ iva5 + iva10),0)      "999,999,999,999"      Arial      "iif(m.TipoOperacion=1,retencion,0)      "999,999,999,999"      Arial      "Total Creditos "      Arial      #iif(m.TipoOperacion=-1,gravadas5,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=-1,iva5,0)      "999,999,999"      Arial      $iif(m.TipoOperacion=-1,gravadas10,0)      "999,999,999,999"      Arial      iif(m.TipoOperacion=-1,iva10,0)      "999,999,999"      Arial      !iif(m.TipoOperacion=-1,exentas,0)      "999,999,999,999"      Arial      Iiif(m.TipoOperacion=-1,(gravadas5 + gravadas10+ exentas+ iva5 + iva10),0)      "999,999,999,999"      Arial      #iif(m.TipoOperacion=-1,retencion,0)      "999,999,999,999"      Arial      "Saldo "      Arial      gravadas5 * m.TipoOperacion      "999,999,999,999"      Arial      iva5* m.TipoOperacion      "999,999,999"      Arial      gravadas10 * m.TipoOperacion      "999,999,999,999"      Arial      iva10* m.TipoOperacion      "999,999,999"      Arial      exentas* m.TipoOperacion      "999,999,999,999"      Arial      @(gravadas5 + gravadas10+ exentas+ iva5 + iva10)* m.TipoOperacion      "999,999,999,999"      Arial      retencion* m.TipoOperacion      "999,999,999,999"      Arial      dataenvironment      �Top = 104
Left = 2
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     mPROCEDURE Init
SET TEXTMERGE ON 
IF m.ordenfactura='F'
	m.orden = '1,5,3, 2'
ELSE
	m.orden = '1,5,2, 3'
ENDIF
IF EMPTY(m.vt_cpbt)
	m.vt_cpbt=null
ENDIF

IF EMPTY(m.sucursal)
	m.sucursal=null
ENDIF


TEXT TO cmdSQL noshow
SELECT Sucursal = cn_iva.sucursal + ' ' + s.Descripci�n, cn_iva.comprobante,
  cn_iva.fechacomprobante, vt_clientes.razsocial, TipoComprobante +'-'+ cp.Descripcion as ComprobanteDesc, cp.Tipo as TipoOperacion, 
  vt_clientes.ruc, 
  SPACE(15) AS min,
  SPACE(15) AS max, 
  cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion ,
    sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    
   sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  
     sum(d.exentas) as Exentas, 
     sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    
     sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10,
     cn_IVa.IdCondicion    
 FROM vt_clientes,
   cn_iva left JOIN cn_iva_detalle d
   ON  cn_iva.idiva = d.idiva 
    left join vt_cpbt cp on cn_iva.TipoComprobante = cp.IdComprobante and cn_iva.idempresa=cp.idempresa
    left join Sucursal s on cn_IVA.Sucursal = s.Sucursal and cn_Iva.Idempresa= s.IdEmpresa
 WHERE cn_iva.c�digo = vt_clientes.idcliente
   AND (cn_iva.idempresa = ?oApp.empresa
   AND (cn_iva.sucursal = ?m.sucursal or ?m.sucursal is null)
   AND MONTH(cn_iva.fechacomprobante) = ?m.mes
   AND YEAR(cn_iva.fechacomprobante) = ?m.a�o
   AND cn_iva.tipoiva = 'V'
   AND vt_clientes.idempresa = ?oApp.empresa 
   and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha)
   and (cn_iva.TipoComprobante = ?m.vt_cpbt or ?m.vt_cpbt is null)
   and (?m.CondicionIVA='T' or ?m.CondicionIVA=cn_Iva.IdCondicion)           
GROUP BY cn_iva.sucursal, s.Descripci�n, cn_iva.comprobante,
  cn_iva.fechacomprobante, cn_iva.c�digo, cn_iva.gravadas,
  cn_iva.exentas, cn_iva.iva, vt_clientes.razsocial,
  vt_clientes.ruc,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta,
  TipoComprobante, cp.Descripcion, cp.Tipo,cn_IVa.IdCondicion
 UNION
 SELECT Sucursal = cn_iva.sucursal + ' ' + s.Descripci�n, SPACE(2) as comprobante,
  cn_iva.fechacomprobante,SPACE(45) AS rassocial, 'Ticket' as ComprobanteDesc,'D' as TipoOperacion, SPACE(15) AS ruc,
  CN_IVA.comprobante AS min,
  CN_iva.comprobante1 AS max, cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion,
      sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    
   sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  
     sum(d.exentas) as Exentas, 
     sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    
     sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10,
     IdCondicion='  '    
  FROM  cn_iva INNER JOIN cn_iva_detalle d
   ON  cn_iva.idiva = d.idiva
   left join Sucursal s on cn_IVA.Sucursal = s.Sucursal and cn_Iva.Idempresa= s.IdEmpresa
 WHERE cn_iva.idempresa = ?oApp.empresa
   AND (cn_iva.sucursal = ?m.sucursal or ?m.sucursal is null)
   AND MONTH(cn_iva.fechacomprobante) = ?m.mes
   AND YEAR(cn_iva.fechacomprobante) = ?m.a�o
   AND cn_iva.tipoiva = 'T'
   and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha
 GROUP BY cn_iva.sucursal,s.Descripci�n, cn_iva.comprobante,
  cn_iva.fechacomprobante,cn_iva.fecha,cn_iva.comprobante1, cn_iva.c�digo, cn_iva.gravadas,
  cn_iva.exentas, cn_iva.iva,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta
 ORDER BY <<m.orden>>

ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo
SET CENTURY OFF 

ENDPROC
     c���    J  J                        �V   %   -      �  N   U          �  U  � G` � %���  � F��1 � T�� �� 1,5,3, 2�� �P � T�� �� 1,5,2, 3�� � %�C�� ���r � T�� ���� � %�C�� ���� � T�� ���� �	 M(� ��R �L SELECT Sucursal = cn_iva.sucursal + ' ' + s.Descripci�n, cn_iva.comprobante,�� ��   cn_iva.fechacomprobante, vt_clientes.razsocial, TipoComprobante +'-'+ cp.Descripcion as ComprobanteDesc, cp.Tipo as TipoOperacion, � �   vt_clientes.ruc, � �   SPACE(15) AS min,� �   SPACE(15) AS max, �F �@   cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion ,�[ �U     sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    �Z �T    sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  �& �       sum(d.exentas) as Exentas, �S �M      sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    �Q �K      sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10,�! �      cn_IVa.IdCondicion    � �  FROM vt_clientes,�* �$    cn_iva left JOIN cn_iva_detalle d�$ �    ON  cn_iva.idiva = d.idiva �m �g     left join vt_cpbt cp on cn_iva.TipoComprobante = cp.IdComprobante and cn_iva.idempresa=cp.idempresa�` �Z     left join Sucursal s on cn_IVA.Sucursal = s.Sucursal and cn_Iva.Idempresa= s.IdEmpresa�2 �,  WHERE cn_iva.c�digo = vt_clientes.idcliente�. �(    AND (cn_iva.idempresa = ?oApp.empresa�C �=    AND (cn_iva.sucursal = ?m.sucursal or ?m.sucursal is null)�4 �.    AND MONTH(cn_iva.fechacomprobante) = ?m.mes�3 �-    AND YEAR(cn_iva.fechacomprobante) = ?m.a�o�! �    AND cn_iva.tipoiva = 'V'�3 �-    AND vt_clientes.idempresa = ?oApp.empresa �G �A    and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha)�H �B    and (cn_iva.TipoComprobante = ?m.vt_cpbt or ?m.vt_cpbt is null)�S �M    and (?m.CondicionIVA='T' or ?m.CondicionIVA=cn_Iva.IdCondicion)           �B �< GROUP BY cn_iva.sucursal, s.Descripci�n, cn_iva.comprobante,�@ �:   cn_iva.fechacomprobante, cn_iva.c�digo, cn_iva.gravadas,�: �4   cn_iva.exentas, cn_iva.iva, vt_clientes.razsocial,�H �B   vt_clientes.ruc,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta,�C �=   TipoComprobante, cp.Descripcion, cp.Tipo,cn_IVa.IdCondicion� �  UNION�X �R  SELECT Sucursal = cn_iva.sucursal + ' ' + s.Descripci�n, SPACE(2) as comprobante,�{ �u   cn_iva.fechacomprobante,SPACE(45) AS rassocial, 'Ticket' as ComprobanteDesc,'D' as TipoOperacion, SPACE(15) AS ruc,�" �   CN_IVA.comprobante AS min,�a �[   CN_iva.comprobante1 AS max, cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta as Retencion,�] �W       sum(CASE WHEN porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    �Z �T    sum(CASE WHEN porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  �& �       sum(d.exentas) as Exentas, �S �M      sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    �Q �K      sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10,� �      IdCondicion='  '    �0 �*   FROM  cn_iva INNER JOIN cn_iva_detalle d�# �    ON  cn_iva.idiva = d.idiva�_ �Y    left join Sucursal s on cn_IVA.Sucursal = s.Sucursal and cn_Iva.Idempresa= s.IdEmpresa�- �'  WHERE cn_iva.idempresa = ?oApp.empresa�C �=    AND (cn_iva.sucursal = ?m.sucursal or ?m.sucursal is null)�4 �.    AND MONTH(cn_iva.fechacomprobante) = ?m.mes�3 �-    AND YEAR(cn_iva.fechacomprobante) = ?m.a�o�! �    AND cn_iva.tipoiva = 'T'�F �@    and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha�B �<  GROUP BY cn_iva.sucursal,s.Descripci�n, cn_iva.comprobante,�a �[   cn_iva.fechacomprobante,cn_iva.fecha,cn_iva.comprobante1, cn_iva.c�digo, cn_iva.gravadas,�R �L   cn_iva.exentas, cn_iva.iva,cn_IVa.RetencionesIva + cn_IVa.RetencionesRenta� �  ORDER BY <<m.orden>>� �  � � ��C � � cn_rIvaVentas� �� F� � U  ORDENFACTURA ORDEN VT_CPBT SUCURSAL CMDSQL SQL CN_RIVAVENTAS  �  � G� U  SETEO Init,     �� BeforeOpenTables    ��1 a Aq� qA � A � A � !����a��a1��A�!�1A11q�1!��1� ��!��a1�1��1A1a!!�a A �r 3 q a 2                            K   F  b  T    )   J                  