  -�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=1
COLOR=2
      Arial      
IdVendedor      m.total      miif(AT(ALLTRIM(IdLista),cConfig.arancel)>0 AND AT(ALLTRIM(IdVendedor),cConfig.profesional)>0,0,Importe) * .45      0      
m.Comision      biif(AT(ALLTRIM(IdLista),cConfig.arancel)>0 AND AT(ALLTRIM(IDVendedor),cConfig.profesional)>0,0,45)      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Planilla de Honorarios"      Arial      oApp.Nombreempresa      Arial      m.dfecha, ' al ', m.hFecha      Arial      
"Per�odo:"      Arial      "Paciente
"      Arial      "Fecha"      Arial      "Referencia"      Arial      "Pagado"      Arial      "%"      Arial      
"Comisi�n"      Arial      "Pendiente"      Arial      /"Profesional: ", IdVendedor,' ', NombreVendedor      Arial      AIdCliente,' ',alltrim(RazSocial), ' - ',Tratamiento, 'P: '+ Pieza      Arial      IdCliente=IdPaciente      oIdPaciente,alltrim(Paciente), '(', IdCliente,' ',alltrim(RazSocial), ') - ',alltrim(Tratamiento), 'P: ' + Pieza      Arial      IdCliente<>IdPaciente      Importe      "9,999,999,999"      Arial      Comision      Arial      m.Total      "9,999,999,999"      Arial      Saldo      "9,999,999,999"      Arial      Fecha      "@D"      Arial      Tipo,'-', alltrim(str(numero))      Arial      "Total Profesional: "      Arial      Importe      "9,999,999,999"      Arial      Saldo      "9,999,999,999"      Arial      m.total      "9,999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     lPROCEDURE Init
IF EMPTY(m.IDProfesional)
	m.IdProfesional = null
ENDIF

IF SQL("Select convert(varchar(200),dbo.LeerConstante(?oApp.Empresa,'ODT_ARANCEL')) as Arancel,convert(varchar(200),dbo.LeerConstante(?oApp.Empresa,'ODT_PROFESIONAL')) as Profesional", 'cConfig')>0
	IF RECCOUNT('cConfig')>0
*		THISFORM.Arancel=ALLTRIM(cConfig.Arancel)	
*		THISFORM.profesional=ALLTRIM(cConfig.Profesional)	
	ENDIF
ENDIF
	

TEXT TO cmdSQL noshow
	SELECT    tr.IdProfesional as IdVendedor,Origen='C',  v.NombreVendedor, p.fecha, p.idcliente, c.RazSocial, 
	ROUND((tr.Importe/TotalFactura)*pd.importe,0) as Importe, Saldo=0,p.Tip_Reci Tipo, num_recibo as numero, pr.Descripcion as Tratamiento,
	c1.RazSocial as Paciente,tr.IdCliente as IdPaciente, tr.Pieza,f.IdLista
	FROM         vt_pagos AS p INNER JOIN
	                      vt_det_pagos AS pd ON p.idpago = pd.idpago INNER JOIN
	                      vt_factura AS f ON pd.idfactura = f.IdFactura INNER JOIN
	                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente 
	                      left join odt_Tratamiento tr on f.IdFactura = tr.IdFactura
	                      inner join st_Producto pr on tr.IdEmpresa=pr.IdEmpresa and tr.IdTratamiento= pr.IdProducto                     
	                      left join vt_clientes AS c1 ON tr.IdEmpresa = c1.IdEmpresa AND tr.IdCliente = c1.IdCliente
						  INNER JOIN vvt_Vendedores AS v ON tr.IdEmpresa = v.IdEmpresa AND tr.IdProfesional= v.IdVendedor	                      
	where p.IdEmpresa = ?oApp.Empresa
		and p.Fecha between ?m.dFecha and ?m.hFecha
		and (tr.IdProfesional = ?m.IdProfesional or ?m.IdProfesional is null)
	union
	SELECT    tr.IdProfesional as IdVendedor,  Origen='C', v.NombreVendedor, Fecha=f.Fecha, f.IdCliente, c.RazSocial, 
	tr.Importe,Saldo=0,f.IdComprobante as Tipo,f.Numero, pr.Descripcion as Tratamiento,
	c1.RazSocial as Paciente,tr.IdCliente as IdPaciente,tr.Pieza,f.IdLista
	FROM         vt_factura AS f INNER JOIN
	                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente 
	                      left join odt_Tratamiento tr on f.IdFactura = tr.IdFactura
	                      inner join st_Producto pr on tr.IdEmpresa=pr.IdEmpresa and tr.IdTratamiento= pr.IdProducto                     	                      
	                      left join vt_clientes AS c1 ON tr.IdEmpresa = c1.IdEmpresa AND tr.IdCliente = c1.IdCliente
	                      left join vt_Condicion cn on f.IdEmpresa=cn.IdEmpresa and f.Idcondicion=cn.Idcondicion 
						  INNER JOIN vvt_Vendedores AS v ON tr.IdEmpresa = v.IdEmpresa AND tr.IdProfesional= v.IdVendedor	                      	                      
	where f.IdEmpresa = ?oApp.Empresa 
		and f.Fecha between ?m.dFecha and ?m.hFecha 
		and (tr.IdProfesional = ?m.IdProfesional or ?m.IdProfesional is null) 
		and cn.Plazo=0 
	union
	SELECT    tr.IdProfesional as IdVendedor,  Origen='S', v.NombreVendedor, Fecha=null, f.IdCliente, c.RazSocial, 
	Importe=0,SUM(ROUND((tr.Importe/TotalFactura)*(s.Saldo),0)) as Saldo,f.IdComprobante as Tipo,f.Numero, pr.Descripcion as Tratamiento,
	c1.RazSocial as Paciente,tr.IdCliente as IdPaciente,tr.Pieza,f.IdLista
	FROM         vt_factura AS f 
						  inner join vt_FOrma_pago s on f.IdFactura=s.IdFactura
	                      inner join vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente 
	                      left join odt_Tratamiento tr on f.IdFactura = tr.IdFactura
	                      inner join st_Producto pr on tr.IdEmpresa=pr.IdEmpresa and tr.IdTratamiento= pr.IdProducto                     	                      
	                      left join vt_clientes AS c1 ON tr.IdEmpresa = c1.IdEmpresa AND tr.IdCliente = c1.IdCliente
						  INNER JOIN vvt_Vendedores AS v ON tr.IdEmpresa = v.IdEmpresa AND tr.IdProfesional= v.IdVendedor	                      	                      
	where f.IdEmpresa = ?oApp.Empresa
		and (tr.IdProfesional = ?m.IdProfesional or ?m.IdProfesional is null)
		and TotalFactura<>0             	                              
	group by tr.IdProfesional, f.IdCliente, c.RazSocial, v.NombreVendedor, f.IdComprobante, f.Numero,pr.Descripcion,c1.RazSocial,tr.IdCliente,tr.Pieza,f.IdLista
	having sum(s.Saldo)<>0
	order by  1,2,3,4   

ENDTEXT

sql(cmdSQL,'cComision')
SELECT cComision


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        V=   %   �      F  >   �          �  U  6 %�C��  ��� � T��  ���� �� %�Cٮ Select convert(varchar(200),dbo.LeerConstante(?oApp.Empresa,'ODT_ARANCEL')) as Arancel,convert(varchar(200),dbo.LeerConstante(?oApp.Empresa,'ODT_PROFESIONAL')) as Profesional� cConfig� � ��� %�C� cConfigN� ��	� � �	 M(� ��r �l 	SELECT    tr.IdProfesional as IdVendedor,Origen='C',  v.NombreVendedor, p.fecha, p.idcliente, c.RazSocial, �� �� 	ROUND((tr.Importe/TotalFactura)*pd.importe,0) as Importe, Saldo=0,p.Tip_Reci Tipo, num_recibo as numero, pr.Descripcion as Tratamiento,�N �H 	c1.RazSocial as Paciente,tr.IdCliente as IdPaciente, tr.Pieza,f.IdLista�, �& 	FROM         vt_pagos AS p INNER JOIN�R �L 	                      vt_det_pagos AS pd ON p.idpago = pd.idpago INNER JOIN�U �O 	                      vt_factura AS f ON pd.idfactura = f.IdFactura INNER JOIN�i �c 	                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente �W �Q 	                      left join odt_Tratamiento tr on f.IdFactura = tr.IdFactura�� �� 	                      inner join st_Producto pr on tr.IdEmpresa=pr.IdEmpresa and tr.IdTratamiento= pr.IdProducto                     �w �q 	                      left join vt_clientes AS c1 ON tr.IdEmpresa = c1.IdEmpresa AND tr.IdCliente = c1.IdCliente�� �~ 						  INNER JOIN vvt_Vendedores AS v ON tr.IdEmpresa = v.IdEmpresa AND tr.IdProfesional= v.IdVendedor	                      �( �" 	where p.IdEmpresa = ?oApp.Empresa�3 �- 		and p.Fecha between ?m.dFecha and ?m.hFecha�M �G 		and (tr.IdProfesional = ?m.IdProfesional or ?m.IdProfesional is null)� � 	union�y �s 	SELECT    tr.IdProfesional as IdVendedor,  Origen='C', v.NombreVendedor, Fecha=f.Fecha, f.IdCliente, c.RazSocial, �Z �T 	tr.Importe,Saldo=0,f.IdComprobante as Tipo,f.Numero, pr.Descripcion as Tratamiento,�M �G 	c1.RazSocial as Paciente,tr.IdCliente as IdPaciente,tr.Pieza,f.IdLista�. �( 	FROM         vt_factura AS f INNER JOIN�i �c 	                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente �W �Q 	                      left join odt_Tratamiento tr on f.IdFactura = tr.IdFactura�� �� 	                      inner join st_Producto pr on tr.IdEmpresa=pr.IdEmpresa and tr.IdTratamiento= pr.IdProducto                     	                      �w �q 	                      left join vt_clientes AS c1 ON tr.IdEmpresa = c1.IdEmpresa AND tr.IdCliente = c1.IdCliente�t �n 	                      left join vt_Condicion cn on f.IdEmpresa=cn.IdEmpresa and f.Idcondicion=cn.Idcondicion �� �� 						  INNER JOIN vvt_Vendedores AS v ON tr.IdEmpresa = v.IdEmpresa AND tr.IdProfesional= v.IdVendedor	                      	                      �) �# 	where f.IdEmpresa = ?oApp.Empresa �4 �. 		and f.Fecha between ?m.dFecha and ?m.hFecha �N �H 		and (tr.IdProfesional = ?m.IdProfesional or ?m.IdProfesional is null) � � 		and cn.Plazo=0 � � 	union�v �p 	SELECT    tr.IdProfesional as IdVendedor,  Origen='S', v.NombreVendedor, Fecha=null, f.IdCliente, c.RazSocial, �� �� 	Importe=0,SUM(ROUND((tr.Importe/TotalFactura)*(s.Saldo),0)) as Saldo,f.IdComprobante as Tipo,f.Numero, pr.Descripcion as Tratamiento,�M �G 	c1.RazSocial as Paciente,tr.IdCliente as IdPaciente,tr.Pieza,f.IdLista�$ � 	FROM         vt_factura AS f �C �= 						  inner join vt_FOrma_pago s on f.IdFactura=s.IdFactura�t �n 	                      inner join vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente �W �Q 	                      left join odt_Tratamiento tr on f.IdFactura = tr.IdFactura�� �� 	                      inner join st_Producto pr on tr.IdEmpresa=pr.IdEmpresa and tr.IdTratamiento= pr.IdProducto                     	                      �w �q 	                      left join vt_clientes AS c1 ON tr.IdEmpresa = c1.IdEmpresa AND tr.IdCliente = c1.IdCliente�� �� 						  INNER JOIN vvt_Vendedores AS v ON tr.IdEmpresa = v.IdEmpresa AND tr.IdProfesional= v.IdVendedor	                      	                      �( �" 	where f.IdEmpresa = ?oApp.Empresa�M �G 		and (tr.IdProfesional = ?m.IdProfesional or ?m.IdProfesional is null)�G �A 		and TotalFactura<>0             	                              �� �� 	group by tr.IdProfesional, f.IdCliente, c.RazSocial, v.NombreVendedor, f.IdComprobante, f.Numero,pr.Descripcion,c1.RazSocial,tr.IdCliente,tr.Pieza,f.IdLista� � 	having sum(s.Saldo)<>0� � 	order by  1,2,3,4   � �  � � ��C � �	 cComision� �� F� � U  IDPROFESIONAL SQL CMDSQL	 CCOMISION
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � A ��C A � !���!Q�q�qA�1�� �����q1
qA�	�A�q� a��A1Aq1
q�	��q1
��a A �q 4 q 2                       0     <   W  a  E    )   �                  