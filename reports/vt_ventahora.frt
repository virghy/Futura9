  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=1
COLOR=2
      Arial      Sucursal      Arial      Arial      Arial      Arial      Arial      "Reporte de Ventas por hora"      Arial      empresa             Arial      'iif(empty(m.sucursal),'Todos',Sucursal)      Arial      
"Sucursal"      Arial      
"Per�odo:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      	"Domingo"      Arial      "Lunes"      Arial      "Martes"      Arial      "Miercoles"      Arial      "Jueves"      Arial      	"Viernes"      Arial      "Sabado"      Arial      "Total"      Arial      "Hora
"      Arial      Sucursal      Arial      Hora+':00 - '+Hora +':59'      Arial      d1      "999,999,999.99"      Arial      d2      "999,999,999.99"      Arial      d3      "999,999,999.99"      Arial      d4      "999,999,999.99"      Arial      d5      "999,999,999.99"      Arial      d6      "999,999,999.99"      Arial      d7      "999,999,999.99"      Arial      Total      "999,999,999.99"      Arial      d1      "99,999,999,999.99"      Arial      d2      "99,999,999,999.99"      Arial      d3      "99,999,999,999.99"      Arial      d4      "99,999,999,999.99"      Arial      d5      "99,999,999,999.99"      Arial      d6      "99,999,999,999.99"      Arial      d7      "99,999,999,999.99"      Arial      Total      "99,999,999,999.99"      Arial      "Total Sucursal"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      d1      "999,999,999,999.99"      Arial      d2      "999,999,999,999.99"      Arial      d3      "999,999,999,999.99"      Arial      d4      "999,999,999,999.99"      Arial      d5      "999,999,999,999.99"      Arial      d6      "999,999,999,999.99"      Arial      d7      "999,999,999,999.99"      Arial      Total      "999,999,999,999.99"      Arial      "Total General"      Arial      dataenvironment      �Top = 62
Left = 4
Width = 759
Height = 448
Visible = .F.
TabStop = .F.
InitialSelectedAlias = "vt_rdiariocontrol"
DataSource = .NULL.
Name = "Dataenvironment"
     3PROCEDURE BeforeOpenTables
Do seteo
ENDPROC
PROCEDURE Init


If Empty(m.sucursal)
	Store null To sucursal
ENDIF

TEXT TO cmdSQL noshow

SET DATEFIRST 7

SELECT     a.IdMoneda+' - '+a.Sucursal + '-'+d.Descripci�n as Sucursal, LEFT(Hora,2) as Hora, 
SUM(case when datepart(dw,fecha)=1 then TotalFactura else 0 end) as D1,
SUM(case when datepart(dw,fecha)=2 then TotalFactura else 0 end) as D2,
SUM(case when datepart(dw,fecha)=3 then TotalFactura else 0 end) as D3,
SUM(case when datepart(dw,fecha)=4 then TotalFactura else 0 end) as D4,
SUM(case when datepart(dw,fecha)=5 then TotalFactura else 0 end) as D5,
SUM(case when datepart(dw,fecha)=6 then TotalFactura else 0 end) as D6,
SUM(case when datepart(dw,fecha)=7 then TotalFactura else 0 end) as D7,

SUM(ISNULL(a.Exenta,0) + ISNULL(a.Gravada,0) + ISNULL(a.Iva,0)) as Total

FROM         dbo.vt_factura a LEFT OUTER JOIN
                      dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal
where
	a.IdEmpresa = ?oApp.Empresa  and                     
	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha 
	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) 
group by  a.IdMoneda+' - '+a.Sucursal + '-'+d.Descripci�n , LEFT(Hora,2)
ORDER BY 1,2

ENDTEXT

	
	sql(cmdSQL,'vt_rdiariocontrol')
	SELECT vt_rdiariocontrol
ENDPROC
     O���    6  6                        �   %   q      �  "   �          �  U  
  �  � U  SETEO %�C��  ��� � J���(�  � �	 M(� �� �  � � SET DATEFIRST 7� �  �d �^ SELECT     a.IdMoneda+' - '+a.Sucursal + '-'+d.Descripci�n as Sucursal, LEFT(Hora,2) as Hora, �M �G SUM(case when datepart(dw,fecha)=1 then TotalFactura else 0 end) as D1,�M �G SUM(case when datepart(dw,fecha)=2 then TotalFactura else 0 end) as D2,�M �G SUM(case when datepart(dw,fecha)=3 then TotalFactura else 0 end) as D3,�M �G SUM(case when datepart(dw,fecha)=4 then TotalFactura else 0 end) as D4,�M �G SUM(case when datepart(dw,fecha)=5 then TotalFactura else 0 end) as D5,�M �G SUM(case when datepart(dw,fecha)=6 then TotalFactura else 0 end) as D6,�M �G SUM(case when datepart(dw,fecha)=7 then TotalFactura else 0 end) as D7,� �  �N �H SUM(ISNULL(a.Exenta,0) + ISNULL(a.Gravada,0) + ISNULL(a.Iva,0)) as Total� �  �3 �- FROM         dbo.vt_factura a LEFT OUTER JOIN�c �]                       dbo.sucursal d ON a.IdEmpresa = d.IdEmpresa AND a.Sucursal = d.Sucursal� � where�< �6 	a.IdEmpresa = ?oApp.Empresa  and                     �/ �) 	a.fecha BETWEEN ?m.dfecha AND ?m.hfecha �= �7 	AND (a.sucursal = ?m.sucursal or ?m.Sucursal is null) �N �H group by  a.IdMoneda+' - '+a.Sucursal + '-'+d.Descripci�n , LEFT(Hora,2)� � ORDER BY 1,2� �  � �" ��C � � vt_rdiariocontrol� �� F� � U  SUCURSAL CMDSQL SQL VT_RDIARIOCONTROL BeforeOpenTables,     �� InitA     ��1 q 2 � A � a Qa A�������a �a 11� ����!a A #q 1                       $         ?   (      )   6                  