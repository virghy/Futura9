  ,g                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
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
      B  *   winspool  hp deskjet 840c series  USB001  TS001                	hp deskjet 840c series          !@� h߀      d   ��                                                                                         B�e�                                   4  �  d  	                                n                                                                                                                                                                                         �  B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   d e s k j e t   8 4 0 c   s e r i e s   e r i e s , L o c a l O n l y , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Arial      sucursal      Arial      Arial      Arial      Arial      empresa             Arial      "Libro de Iva Compra"             Arial      &CMONTH(cn_rivacompra.fechacomprobante)             Arial      $year(cn_rivacompra.fechacomprobante)      "9999"             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "MES:
"             Arial      "A�O:
"      Arial      "Documento
"      Arial       " Proveedor de Bienes/Servicios"      Arial      "Valor de Compras / Servicios"      Arial      !"Importaciones
Base Imponible
"      "@I"      Arial      "Dia"      Arial      "Numero"      Arial      " Fecha"      Arial      $"Razon Social / Apellidos / Nombres"      Arial      "RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      cn_rivacompra.sucursal             Arial      "Sucursal:"      Arial      cn_rivacompra.comprobante             Arial      cn_rivacompra.fechacomprobante             Arial      cn_rivacompra.razon             Arial      ruc             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial       cn_rivacompra.iva5      "999,999,999"       cn_rivacompra.iva5      Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      
"Directos"             Arial      'iif(Tipo='D',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='D',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='D',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='D',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='D',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='D',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='D',Imponible,0)      "999,999,999,999"             Arial      "Indirectos"             Arial      'iif(Tipo='I',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='I',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='I',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='I',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='I',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='I',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='I',Imponible,0)      "999,999,999,999"             Arial      "Importaciones"             Arial      'iif(Tipo='M',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='M',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='M',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='M',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='M',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='M',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='M',Imponible,0)      "999,999,999,999"             Arial      "Total General"             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial      cn_rivacompra.iva5      "999,999,999"             Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      dataenvironment      rLeft = 1
Top = 220
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
Name = "Dataenvironment"
     /PROCEDURE Init


TEXT TO cmdSQL noshow

SELECT cn_iva.sucursal, cn_iva.comprobante,    
  cn_iva.fechacomprobante, cp_proveedor.razon,    
   Cp_proveedor.ruc, cn_iva.c�digo, cn_Iva.IdIva, Tipo,
   sum(CASE WHEN CN_IVA.TIPO <> 'M' and porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    
   sum(CASE WHEN CN_IVA.TIPO <> 'M' and porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  
	  sum(CASE WHEN CN_IVA.TIPO = 'M' THEN d.gravada else 0 end) as imponible,    
     sum(d.exentas) as Exentas, 
     sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    
     sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10    
     FROM  cn_iva inner join cn_iva_Detalle d on cn_Iva.IdIva = d.IdIva    
      INNER JOIN cp_proveedor   
     ON  cn_iva.c�digo = cp_proveedor.idproveedor    
      WHERE cn_iva.idempresa = ?oApp.Empresa  
      AND cp_proveedor.idempresa = ?oApp.Empresa    
       AND cn_iva.sucursal = ?m.sucursal    
        AND MONTH(cn_iva.fechacomprobante) = ?m.mes  
        AND YEAR(cn_iva.fechacomprobante) = ?m.a�o  
        and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha
      AND cn_iva.tipoiva = 'C'  
       group by cn_iva.sucursal, cn_iva.comprobante,    
  cn_iva.fechacomprobante, cp_proveedor.razon,    
   Cp_proveedor.ruc, cn_iva.c�digo, cn_Iva.IdIva, Tipo    
      ORDER BY cn_iva.fechacomprobante, cn_iva.comprobante  

ENDTEXT


sql(cmdsql,'cn_rivacompra')
SELECT cn_rivacompra

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo


ENDPROC
     \���    C  C                        ��   %   �      �  !   �          �  U  	 M(�  �� �  �5 �/ SELECT cn_iva.sucursal, cn_iva.comprobante,    �8 �2   cn_iva.fechacomprobante, cp_proveedor.razon,    �= �7    Cp_proveedor.ruc, cn_iva.c�digo, cn_Iva.IdIva, Tipo,�q �k    sum(CASE WHEN CN_IVA.TIPO <> 'M' and porcentaje=5 THEN ISNULL(d.gravada,0) else 0 end) as gravadas5,    �q �k    sum(CASE WHEN CN_IVA.TIPO <> 'M' and porcentaje=10 THEN ISNULL(d.gravada,0) else 0 end) as gravadas10,  �U �O 	  sum(CASE WHEN CN_IVA.TIPO = 'M' THEN d.gravada else 0 end) as imponible,    �& �       sum(d.exentas) as Exentas, �S �M      sum(case when porcentaje=5 then ISNULL(d.iva,0) else 0 end) as Iva5,    �T �N      sum(case when porcentaje=10 then ISNULL(d.iva,0) else 0 end) as Iva10    �Q �K      FROM  cn_iva inner join cn_iva_Detalle d on cn_Iva.IdIva = d.IdIva    �& �        INNER JOIN cp_proveedor   �; �5      ON  cn_iva.c�digo = cp_proveedor.idproveedor    �4 �.       WHERE cn_iva.idempresa = ?oApp.Empresa  �: �4       AND cp_proveedor.idempresa = ?oApp.Empresa    �2 �,        AND cn_iva.sucursal = ?m.sucursal    �; �5         AND MONTH(cn_iva.fechacomprobante) = ?m.mes  �: �4         AND YEAR(cn_iva.fechacomprobante) = ?m.a�o  �K �E         and   cn_iva.fechacomprobante between ?m.dFecha and ?m.hFecha�& �        AND cn_iva.tipoiva = 'C'  �> �8        group by cn_iva.sucursal, cn_iva.comprobante,    �8 �2   cn_iva.fechacomprobante, cp_proveedor.razon,    �@ �:    Cp_proveedor.ruc, cn_iva.c�digo, cn_Iva.IdIva, Tipo    �B �<       ORDER BY cn_iva.fechacomprobante, cn_iva.comprobante  � �  � � ��C �  � cn_rivacompra� �� F� � U  CMDSQL SQL CN_RIVACOMPRA
  �  � U  SETEO Init,     �� BeforeOpenTablesk    ��1 � a Q��Qa1Aa�A�!���a��!a A �q 3 q 3                       �          $  %    )   C                  