   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                              �\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,  A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                     �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=3
COLLATE=1
                 idmoneda                       "Orden de Pagos"                                              Arial                          empresa                                                       Arial                          Arial                          	"Factura"                      Arial                          "Proveedor"                   Arial                          "Fecha"                        Arial                          	"Importe"                      Arial                          "Numero"                       nroorden                                                      Arial                          cp_lisordpag.razon                                            Arial                          cp_lisordpag.facturaproveedor                                                                  Arial                          cp_lisordpag.fecha                                            Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          	"Codigo"                      cp_lisordpag.idproveedor                                      Arial                          cp_lisordpag.importe                                          Arial                          "999,999,999,999.99"           cp_lisordpag.importe                                          Arial                          "999,999,999,999.99"           Arial                          "Total"                       cp_lisordpag.idmoneda                                         Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                rLeft = 41
Top = 120
Width = 520
Height = 200
InitialSelectedAlias = "cp_lisordpag"
Name = "Dataenvironment"
                PROCEDURE Init
LOCAL strsql
SET DATABASE TO DATOS 

strsql = 'SELECT Cp_orddet_pago.idcomprob, Cp_orddet_pago.nrocomprob,'+;
'  Cp_orddet_pago.facturaproveedor, Cp_orddet_pago.cuota,'+;
'  Cp_orddet_pago.importe,cp_orddet_pago.idmoneda, Cp_ordenpago.nroorden,'+;
'  Cp_ordenpago.sucursal, Cp_orddet_pago.idproveedor,c.razon, Cp_ordenpago.nombre,'+;
'  Cp_ordenpago.nrocheque, cp_ordenpago.fecha'+;
' FROM cp_orddet_pago ,'+;
'  cp_ordenpago,cp_proveedor c '+;
' WHERE cp_ordenpago.idempresa= '+"'"+oapp.empresa +"'"+' and Cp_ordenpago.nroorden = Cp_orddet_pago.nroorden'+;
' and Cp_ordenpago.fecha BETWEEN ?m.dfecha AND ?m.hfecha and cp_orddet_pago.idproveedor= '+;
' c.idproveedor '

 = sql(strsql,'cp_lisordpag')
SELECT cp_lisordpag
*brow
ENDPROC
                     ����    n  n                        ��   %   	      %               �  U  � ��  � G(� DATOS�lT�  ��; SELECT Cp_orddet_pago.idcomprob, Cp_orddet_pago.nrocomprob,�8   Cp_orddet_pago.facturaproveedor, Cp_orddet_pago.cuota,�H   Cp_orddet_pago.importe,cp_orddet_pago.idmoneda, Cp_ordenpago.nroorden,�Q   Cp_ordenpago.sucursal, Cp_orddet_pago.idproveedor,c.razon, Cp_ordenpago.nombre,�,   Cp_ordenpago.nrocheque, cp_ordenpago.fecha�  FROM cp_orddet_pago ,�   cp_ordenpago,cp_proveedor c �  WHERE cp_ordenpago.idempresa= � '� � � '�4  and Cp_ordenpago.nroorden = Cp_orddet_pago.nroorden�X  and Cp_ordenpago.fecha BETWEEN ?m.dfecha AND ?m.hfecha and cp_orddet_pago.idproveedor= �  c.idproveedor �� ��C �  � cp_lisordpag� �� F� � U  STRSQL DATOS OAPP EMPRESA SQL CP_LISORDPAG Init,     ��1 q � �&�q 2                       �      )   n                  