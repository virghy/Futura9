  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Courier New      NroContrato      	m.Activos      IIF(IdEstado='A',1,0)      0      m.Inactivos      IIF(IdEstado='I',1,0)      0      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      #"Detalle de Afiliados por Contrato"      Arial      (IIF(m.EstadoAF ='T','Todos',m.EstadoAF )      Arial      	"Estado:"      Arial      	"Importe"      Arial      "Afiliados"      Arial      "C.I."      Arial      "Fecha Ingreso"      Arial      "Fecha Baja"      Arial      "Estado"      Arial      Importe      "999,999,999"      Arial      !NroContrato,nvl(Nombre,RazSocial)      Arial      
"Contrato"      Arial      IdEstado      "@D"      Arial      IdAfiliado, ' ', NombreAfiliado      Arial      CI      Arial      	FechaInsc      "@D"      Arial      	FechaBaja      "@D"      Arial      IIF(IdEstado='A',1,0)      "999,999,999"      Arial      IIF(IdEstado='I',1,0)      "999,999,999"      Arial      	"Activos"      Arial      "Inactivos"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      IIF(IdEstado='A',1,0)      "999,999,999"      Arial      IIF(IdEstado='I',1,0)      "999,999,999"      Arial      	"Activos"      Arial      "Inactivos"      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO SETEO

IF EMPTY(m.NroContrato)
	m.NroContrato= null
ENDIF

TEXT TO CMDSQL NOSHOW
	SELECT     a.IdAfiliado, c.NroContrato, c.IdCliente, c.nombre, cl.RazSocial, a.FechaInsc, a.Nombre AS NombreAfiliado, a.CI, a.fechaBaja, a.IdEstado, c.Importe
	FROM         sas_Contrato AS c INNER JOIN
	                      vt_clientes AS cl ON c.IdEmpresa = cl.IdEmpresa AND c.IdCliente = cl.IdCliente INNER JOIN
	                      sas_afiliados AS a ON c.IdContrato = a.IdContrato
					where c.Idempresa=?oApp.Empresa and (c.NroContrato = ?m.Nrocontrato or ?m.Nrocontrato is null)
						and (a.IdEstado=?m.EstadoAF or ?m.EstadoAF='T')
	order by c.NroContrato 
ENDTEXT


sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
     ����    �  �                        �h   %         ?               �  U  � �  � %�C�� ���% � T�� ���� �	 M(� ��� �� 	SELECT     a.IdAfiliado, c.NroContrato, c.IdCliente, c.nombre, cl.RazSocial, a.FechaInsc, a.Nombre AS NombreAfiliado, a.CI, a.fechaBaja, a.IdEstado, c.Importe�0 �* 	FROM         sas_Contrato AS c INNER JOIN�v �p 	                      vt_clientes AS cl ON c.IdEmpresa = cl.IdEmpresa AND c.IdCliente = cl.IdCliente INNER JOIN�N �H 	                      sas_afiliados AS a ON c.IdContrato = a.IdContrato�i �c 					where c.Idempresa=?oApp.Empresa and (c.NroContrato = ?m.Nrocontrato or ?m.Nrocontrato is null)�; �5 						and (a.IdEstado=?m.EstadoAF or ?m.EstadoAF='T')� � 	order by c.NroContrato � � ��C � � consulta� �� F� � U  SETEO NROCONTRATO CMDSQL SQL CONSULTA Init,     ��1 q � A � Q
a����A �q 1                       �      )   �                  