  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Arial      IdLista      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Nuevos Pacientes por Arancel"      Arial      oApp.Nombreempresa      Arial      m.dfecha, ' al ', m.hFecha      Arial      
"Per�odo:"      Arial      "Paciente
"      Arial      "1ra. Consulta
"      Arial      "Importe del Periodo
"      Arial      "Arancel: ", IdLista, Arancel      Arial      IdCliente,' ', Cliente      Arial      Fecha      Arial      Importe      "@Z 999,999,999"      Arial      Fecha      "9999"      Arial      Importe      "@Z 99,999,999,999"      Arial      "Total Arancel
"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Fecha      "9999"      Arial      Importe      "@Z 99,999,999,999"      Arial      "Total General
"      Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE Init
IF EMPTY(m.Arancel)
	m.Arancel=null
ENDIF
	

TEXT TO cmdSQL noshow
	SELECT     c.RazSocial AS Cliente, l.Descripcion AS Arancel,
	t.IdLista,t.IdCliente,t.Fecha,
	Sum(Importe) as Importe
	FROM         (Select IdEmpresa,IdLista,IdCliente,Min(Fecha) as Fecha
	from odt_Tratamiento                       
	group by Idempresa,IdLista,IdCliente) t
		inner join vt_clientes AS c on t.IdEmpresa=c.IdEmpresa and t.IdCliente=c.IdCliente 
		inner join vt_ListaPrecio AS l ON t.IdEmpresa = l.IdEmpresa AND t.IdLista = l.IdLista
		inner join odt_Tratamiento tr on t.IDEmpresa=tr.IdEmpresa and t.IdCliente= tr.IdCliente
	where t.IdEmpresa=?oApp.Empresa
	and t.Fecha between ?m.dFecha and ?m.hFecha
	and tr.Fecha between ?m.dFecha and ?m.hFecha
	and (t.IdLista = ?m.Arancel or ?m.Arancel is null)
	group by c.RazSocial, l.Descripcion,
	t.IdLista,t.IdCliente,t.Fecha		
	order by IdLista,Fecha,IdCliente
			
ENDTEXT

sql(cmdSQL,'cComision')
SELECT cComision


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���    �  �                        ȍ   %   F      �     n          �  U  � %�C��  ��� � T��  ���� �	 M(� ��C �= 	SELECT     c.RazSocial AS Cliente, l.Descripcion AS Arancel,�% � 	t.IdLista,t.IdCliente,t.Fecha,� � 	Sum(Importe) as Importe�K �E 	FROM         (Select IdEmpresa,IdLista,IdCliente,Min(Fecha) as Fecha�2 �, 	from odt_Tratamiento                       �. �( 	group by Idempresa,IdLista,IdCliente) t�[ �U 		inner join vt_clientes AS c on t.IdEmpresa=c.IdEmpresa and t.IdCliente=c.IdCliente �] �W 		inner join vt_ListaPrecio AS l ON t.IdEmpresa = l.IdEmpresa AND t.IdLista = l.IdLista�_ �Y 		inner join odt_Tratamiento tr on t.IDEmpresa=tr.IdEmpresa and t.IdCliente= tr.IdCliente�& �  	where t.IdEmpresa=?oApp.Empresa�2 �, 	and t.Fecha between ?m.dFecha and ?m.hFecha�3 �- 	and tr.Fecha between ?m.dFecha and ?m.hFecha�9 �3 	and (t.IdLista = ?m.Arancel or ?m.Arancel is null)�+ �% 	group by c.RazSocial, l.Descripcion,�& �  	t.IdLista,t.IdCliente,t.Fecha		�' �! 	order by IdLista,Fecha,IdCliente�	 � 			� � ��C � �	 cComision� �� F� � U  ARANCEL CMDSQL SQL	 CCOMISION
  �  � U  SETEO Init,     �� BeforeOpenTables1    ��1 � A � 1Q��!����a!1��aq� A �q 4 q 2                       �                 )   �                  