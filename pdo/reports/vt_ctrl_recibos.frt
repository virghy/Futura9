   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          R  ,  winspool \\ASUADM_LNFS1\HP_SIST3_PS 10.129.4.100:HP_SIST3                               P\\ASUADM_LNFS1\HP_SIST3_PS       � � W   �
od   X       Letter                                                                                PRIV�                                                                                       '''  '        ]B �                                                                                     �DRIVER=winspool
DEVICE=\\ASUADM_LNFS1\HP_SIST3_PS
OUTPUT=10.129.4.100:HP_SIST3
ORIENTATION=0
PAPERSIZE=1
SCALE=100
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=600
DUPLEX=1
TTOPTION=3
        vt_recibos.idcliente           vt_recibos.cuota                                              Arial                          "999,999,999"                  vt_recibos.importe                                            Arial                          "999,999,999"                  ,"Control de Emisi�n de Recibos por Clientes"                                                   Arial                          empresa                                                       Arial                          Arial                          "Fecha"                       Arial                          "Cuota"                        Arial                          
"Per�odo:"                     &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)                                                         Arial                          vt_recibos.fecha                                              Arial                          vt_recibos.idcliente                                          Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          Arial                          	"Cliente"                      Arial                          "Nro."                         7iif(empty(m.sucursal),'Todos',vt_recibos.desc_sucursal)                                        Arial                          vt_recibos.razsocial                                          Arial                          numero                                                        Arial                          Arial                          
"Sucursal"                     Arial                          	"Importe"                      Arial                          "Total General"                Arial                          "Cpbte."                       tip_fact                                                      Arial                          9"Total "+  vt_recibos.idcliente+" "+ vt_recibos.razsocial                                      Arial                          vt_recibos.importe                                            Arial                          "999,999,999"                  vt_recibos.importe                                            Arial                          "999,999,999"                  Arial                          	"Recibo"                      tip_reci                                                      Arial                          num_reci                                                      Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                xLeft = -20
Top = 161
Width = 759
Height = 448
InitialSelectedAlias = "vt_rdiariocontrol"
Name = "Dataenvironment"
         \PROCEDURE Init
LOCAL strsql 
SET DATABASE TO DATOS 
strsql = 'SELECT  a.idpago,a.tip_reci, a.num_recibo, '+;
		 ' a.fecha, a.idcliente, '+;
		 ' a.sucursal, a.idmoneda, '+;
		 ' c.razsocial, d.tip_fact, b.numero, b.cuota, '+;
		 ' SUM(b.importe_pag) AS importe '+;
		 'FROM vt_clientes c, vt_condicion d, '+;
		 '  vt_pagos a INNER JOIN vt_det_pagos  b '+;
		 '  ON  a.idpago = b.idpago '+;
		 'WHERE a.idcliente = c.idcliente '+;
		 '  AND a. = ?m.idrecibo '+;
		 '  AND a.num_recibo = ?m.idrecibo '+;
		 '  AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha) '+;
		 'GROUP BY a.idpago,a.tip_reci, a.num_recibo, '+;
		 ' a.fecha, a.idcliente, '+;
		 ' a.sucursal, a.idmoneda, '+;
		 ' c.razsocial, d.tip_fact, b.numero, b.cuota '+;
		 'ORDER BY a.fecha, tip_rec, '+;
		 ' a.num_recibo '
= sql(strsql,'vt_recibos')
SELECT vt_recibos

ENDPROC
                            ����    �  �                        �   %          >     .          �  U  � ��  � G(� DATOS��T�  ��+ SELECT  a.idpago,a.tip_reci, a.num_recibo, �  a.fecha, a.idcliente, �  a.sucursal, a.idmoneda, �-  c.razsocial, d.tip_fact, b.numero, b.cuota, �  SUM(b.importe_pag) AS importe �$ FROM vt_clientes c, vt_condicion d, �(   vt_pagos a INNER JOIN vt_det_pagos  b �   ON  a.idpago = b.idpago �  WHERE a.idcliente = c.idcliente �   AND a. = ?m.idrecibo �!   AND a.num_recibo = ?m.idrecibo �/   AND a.fecha BETWEEN ?m.dfecha AND ?m.hfecha) �, GROUP BY a.idpago,a.tip_reci, a.num_recibo, �  a.fecha, a.idcliente, �  a.sucursal, a.idmoneda, �,  c.razsocial, d.tip_fact, b.numero, b.cuota � ORDER BY a.fecha, tip_rec, �  a.num_recibo �� ��C �  �
 vt_recibos� �� F� � U  STRSQL DATOS SQL
 VT_RECIBOS Init,     ��1 q � a)�q 2                       Q      )   �                  � � �) �* �+ 	� �