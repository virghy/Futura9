  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      sucursal      Arial      Arial      Arial      Arial      empresa             Arial      "Libro de IVA Compras"      Arial      NombreMes(m.Mes)      Arial      m.a�o      "9999"      Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "MES:
"             Arial      "A�O:
"      Arial      "Documento
"      Arial       " Proveedor de Bienes/Servicios"      Arial      "Valor de Compras / Servicios"      Arial      !"Importaciones
Base Imponible
"      "@I"      Arial      "Dia"      Arial      "Numero"      Arial      " Fecha"      Arial      $"Razon Social / Apellidos / Nombres"      Arial      "RUC"      Arial      "Gravadas 5%"      Arial      "    Iva 5%"      Arial      "Gravadas 10%"      Arial      "    Iva 10%"      Arial      	"Exentas"      Arial      "Total"      Arial      cn_rivacompra.sucursal             Arial      "Sucursal:"      Arial      Numero      Arial      cn_rivacompra.fecha      "@YS"      Arial      Nombre      Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial       cn_rivacompra.iva5      "999,999,999"       cn_rivacompra.iva5      Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      ruc             Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      
"Directos"             Arial      'iif(Tipo='D',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='D',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='D',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='D',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='D',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='D',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='D',Imponible,0)      "999,999,999,999"             Arial      "Indirectos"             Arial      'iif(Tipo='I',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='I',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='I',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='I',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='I',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='I',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='I',Imponible,0)      "999,999,999,999"             Arial      "Importaciones"             Arial      'iif(Tipo='M',cn_rivacompra.gravadas5,0)      "999,999,999,999"             Arial      "iif(Tipo='M',cn_rivacompra.iva5,0)      "999,999,999"             Arial      (iif(Tipo='M',cn_rivacompra.gravadas10,0)      "999,999,999,999"             Arial      #iif(Tipo='M',cn_rivacompra.iva10,0)      "999,999,999"             Arial      %iif(Tipo='M',cn_rivacompra.exentas,0)      "999,999,999,999"             Arial      siif(Tipo='M',cn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible,0)      "999,999,999,999"             Arial      iif(Tipo='M',Imponible,0)      "999,999,999,999"             Arial      "Total General"             Arial      cn_rivacompra.gravadas5      "999,999,999,999"             Arial      cn_rivacompra.iva5      "999,999,999"             Arial      cn_rivacompra.gravadas10      "999,999,999,999"             Arial      cn_rivacompra.iva10      "999,999,999"             Arial      cn_rivacompra.exentas      "999,999,999,999"             Arial      ccn_rivacompra.gravadas5 + gravadas10+ cn_rivacompra.exentas+ cn_rivacompra.iva5 + iva10 + imponible      "999,999,999,999"             Arial      	Imponible      "999,999,999,999"             Arial      dataenvironment      �Top = 220
Left = 1
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     @PROCEDURE Init
IF EMPTY(m.sucursal)
	m.sucursal=null
ENDIF



TEXT TO cmdSQL noshow
	exec dbo.vt_rLibroIVA ?oApp.Empresa,?m.dFecha,?m.hFecha, 'C',?m.Mes,?m.A�o,?m.Sucursal,'F',null
ENDTEXT



sql(cmdsql,'cn_rivacompra')
SELECT cn_rivacompra


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo


ENDPROC
     ����    �  �                        	�   %   (      h     P          �  U  �  %�C��  ��� � T��  ���� �	 M(� ��f �` 	exec dbo.vt_rLibroIVA ?oApp.Empresa,?m.dFecha,?m.hFecha, 'C',?m.Mes,?m.A�o,?m.Sucursal,'F',null� � ��C � � cn_rivacompra� �� F� � U  SUCURSAL CMDSQL SQL CN_RIVACOMPRA
  �  � U  SETEO Init,     �� BeforeOpenTables    ��1 � A � aA �q 4 q 3                            
   )  5      )   �                  