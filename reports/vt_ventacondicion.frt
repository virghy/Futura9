  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      sucursal      Arial      Arial      Arial      Arial      !"Resumen de Ventas por Condici�n"      Arial      empresa             Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Fecha
"      Arial      	"Contado"      Arial      	"Credito"      Arial      "Total"      Arial      ?"Sucursal : "+   vt_rventadeposito.sucursal + '  '+ descripci�n             Arial      vt_rVentaDeposito.fecha             Arial      Contado      "999,999,999,999.99"      Arial      Credito      "999,999,999,999.99"      Arial      TotalFactura      "999,999,999,999.99"      Arial      Contado      "999,999,999,999.99"      Arial      Credito      "999,999,999,999.99"      Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total Sucursal"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Contado      "999,999,999,999.99"      Arial      Credito      "999,999,999,999.99"      Arial      TotalFactura      "999,999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      �Top = 142
Left = -61
Width = 759
Height = 448
InitialSelectedAlias = "vt_rventadeposito"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SET DATABASE TO DATOS 
IF EMPTY(m.Sucursal)
	m.Sucursal=null
ENDIF
	

TEXT TO cmdSQL noshow
	select a.fecha, 
	a.sucursal, d.Descripci�n, 
	SUM(a.TotalFactura) as totalFactura, 
	SUM(case when cn.Plazo=0 then TotalFactura else 0 end) as Contado,
	SUM(case when cn.Plazo>0 then TotalFactura else 0 end) as Credito
	from vt_factura a inner join  sucursal d on a.sucursal = d.sucursal and a.idempresa=d.IdEmpresa 
	inner join vt_Condicion cn on a.IdEmpresa=cn.IdEmpresa and a.IdCondicion=cn.Idcondicion
	where a.IdEmpresa = ?oApp.Empresa 
	AND a.fecha between ?m.dfecha  and ?m.hfecha 
	and (a.sucursal = ?m.sucursal or ?m.Sucursal is null)
	group by a.fecha, 
	a.sucursal, d.Descripci�n
	order by a.sucursal,a.fecha


ENDTEXT


*IIF(!EMPTY(m.idcliente ),' and a.idcliente = ?m.idcliente ','')+;


= sql(cmdSQL ,'vt_rVentaDeposito')
SELECT vt_rVentaDeposito

ENDPROC
     j���    Q  Q                        	p   %   �      �     �          �  U  
  �  � U  SETEO$ G(� DATOS� %�C�� ���, � T�� ���� �	 M(� �� � 	select a.fecha, �" � 	a.sucursal, d.Descripci�n, �, �& 	SUM(a.TotalFactura) as totalFactura, �I �C 	SUM(case when cn.Plazo=0 then TotalFactura else 0 end) as Contado,�H �B 	SUM(case when cn.Plazo>0 then TotalFactura else 0 end) as Credito�g �a 	from vt_factura a inner join  sucursal d on a.sucursal = d.sucursal and a.idempresa=d.IdEmpresa �^ �X 	inner join vt_Condicion cn on a.IdEmpresa=cn.IdEmpresa and a.IdCondicion=cn.Idcondicion�) �# 	where a.IdEmpresa = ?oApp.Empresa �4 �. 	AND a.fecha between ?m.dfecha  and ?m.hfecha �< �6 	and (a.sucursal = ?m.sucursal or ?m.Sucursal is null)� � 	group by a.fecha, �  � 	a.sucursal, d.Descripci�n�" � 	order by a.sucursal,a.fecha� �  � �  � �" ��C � � vt_rVentaDeposito� �� F� � U  DATOS SUCURSAL CMDSQL SQL VT_RVENTADEPOSITO BeforeOpenTables,     �� InitA     ��1 q 3 � � A � q!���q��A��!a a A &q 2                       &         A   �      )   Q                  