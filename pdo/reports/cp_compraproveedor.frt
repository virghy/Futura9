   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                 cp_rcompraproveedor.idproveedor                                 @ cp_rcompraproveedor.exenta +  cp_rcompraproveedor.gravada + Iva                                 : cp_rcompraproveedor.exenta +  cp_rcompraproveedor.gravada      Arial                          "9,999,999,999.99"              "Compras y Gastos por Proveedor"                                                               Arial                          Empresa                                                       Arial                          Arial                          "Fecha"                       Arial                          
"Per�odo:"                     &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                         Arial                          cp_rcompraproveedor.fecha                                     Arial                          cp_rcompraproveedor.idproveedor                                                                Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          "Proveedor"                    Arial                          "Nro."                         -iif(empty(m.sucursal),'Todos','Casa Central')                                                  Arial                          cp_rcompraproveedor.razon                                     Arial                          	IdFactura                                                     Arial                          Arial                          
"Sucursal"                     ?cp_rcompraproveedor.exenta  + cp_rcompraproveedor.gravada + Iva                                                                 Arial                          "99,999,999,999,999.99"        >cp_rcompraproveedor.gravada + cp_rcompraproveedor.exenta + Iva                                                                  Arial                          "999,999,999,999.99"           Arial                          "Total"                        Arial                          "Total General"                Arial                          "Cpbte."                       cp_rcompraproveedor.idcomprob                                                                  Arial                          "@!"                           U"Total ("+  alltrim(cp_rcompraproveedor.idproveedor)+") "+  cp_rcompraproveedor.razon                                           Arial                          "@I"                           Arial                          "FacturaProveedor"            $cp_rcompraproveedor.facturaproveedor                                                           Arial                          
referencia                                                    Arial                          Arial                          "Referencia"                   Arial                          Arial                          Arial                          Arial                          dataenvironment                {Left = -102
Top = 161
Width = 759
Height = 448
InitialSelectedAlias = "cp_rcompraproveedor"
Name = "Dataenvironment"
      �PROCEDURE Init
LOCAL strsql

strsql= 'SELECT c.idfactura, a.idcomprob,a.comprobante des_comp, c.facturaproveedor, ' +;
	'c.idproveedor, c.fecha, Referencia, '+ ;
	'c.sucursal, Isnull(c.exenta,0) as exenta, Isnull(c.gravada,0) as gravada, ' +;
	'Isnull(c.iva,0)as iva , c.idmoneda, b.razon '+ ;
	'FROM cp_comprobante a, cp_proveedor b, cp_factura c '+ ;
	'WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.idEmpresa = b.IdEmpresa '+ ;
	'AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa '+ ; 
	'AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha  '+ ; 
	IIF(!EMPTY(m.idproveedor),'AND (c.idproveedor = ?m.idproveedor )','') + ; 
	IIF(!EMPTY(m.sucursal),'AND c.sucursal = ?m.sucursal  ','') + ; 
	'ORDER BY c.idproveedor, c.fecha, c.idfactura,  '+ ; 
	'c.idcomprobante ' 
	
 =sql(strsql,'cp_rcompraproveedor')
 SELECT cp_rcompraproveedor

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
         '���                              A   %   }      �     �          �  U  � ��  ��T�  ��L SELECT c.idfactura, a.idcomprob,a.comprobante des_comp, c.facturaproveedor, �$ c.idproveedor, c.fecha, Referencia, �J c.sucursal, Isnull(c.exenta,0) as exenta, Isnull(c.gravada,0) as gravada, �, Isnull(c.iva,0)as iva , c.idmoneda, b.razon �4 FROM cp_comprobante a, cp_proveedor b, cp_factura c �a WHERE c.idempresa=?oapp.empresa and c.idproveedor = b.idproveedor  and c.idEmpresa = b.IdEmpresa �A AND c.idcomprobante = a.idcomprob  and c.IdEmpresa = a.IdEmpresa �- AND c.fecha BETWEEN ?m.dfecha AND ?m.hfecha  CC�� �
�+ �% AND (c.idproveedor = ?m.idproveedor )� �  6CC�� �
�$ � AND c.sucursal = ?m.sucursal  � �  6�/ ORDER BY c.idproveedor, c.fecha, c.idfactura,  � c.idcomprobante ��$ ��C �  � cp_rcompraproveedor� �� F� � U  STRSQL IDPROVEEDOR SUCURSAL SQL CP_RCOMPRAPROVEEDOR
  �  � U  SETEO Init,     �� BeforeOpenTablesh    ��1 q �,Bq 3 q 2                       u        �  �      )                     