  N                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      IdProveedor      Arial      Arial      Arial      Arial      Arial      Arial      ""Ordenes de Trabajo por Proveedor"      Arial      oApp.Nombreempresa      Arial      m.dFecha," al ",m.hFecha      Arial      
"Per�odo:"      Arial      "Fecha Entrega"      Arial      "Fecha"      Arial      "Nro"      Arial      	"Trabajo"      Arial      
"Paciente"      Arial      "Pendiente"      Arial      "Realizado"      Arial      )"Proveedor: ", IdProveedor, " ",Proveedor      Arial      Fecha      Arial      Nro      "9999"      Arial      Trabajo, " ", OBs      Arial      IdPaciente, " ", Paciente      Arial      IIf(Estado='P',Importe,0)      "999,999,999"      Arial      IIf(Estado='R',Importe,0)      "999,999,999"      Arial      FechaEntrega      Arial      IIf(Estado='P',Importe,0)      "999,999,999"      Arial      IIf(Estado='R',Importe,0)      "999,999,999"      Arial      Importe      "999,999,999"      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      IIf(Estado='P',Importe,0)      "999,999,999"      Arial      IIf(Estado='R',Importe,0)      "999,999,999"      Arial      Importe      "999,999,999"      Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo


IF EMPTY(m.IdProveedor)
	m.IdProveedor=null
ENDIF

	
TEXT TO cmdSQL NOSHOW 
	SELECT     ot.IdProveedor, ot.IdPaciente, ot.Fecha, ot.Trabajo, ot.Estado, ot.Obs, ot.FechaEntrega, ot.Nro, ot.Importe, p.Razon AS Proveedor, c.RazSocial AS Paciente
		FROM         odt_OrdenTrabajo AS ot LEFT OUTER JOIN
                      vt_clientes AS c ON ot.IdEmpresa = c.IdEmpresa AND ot.IdPaciente = c.IdCliente LEFT OUTER JOIN
                      cp_proveedor AS p ON ot.IdEmpresa = p.IdEmpresa AND ot.IdProveedor = p.IdProveedor
        where ot.IdEmpresa=?oApp.Empresa
        and ot.Fecha between ?m.dFecha and ?m.hFecha
        and (ot.Estado = ?m.EstadoOT or ?m.EstadoOT='T')
        and (ot.IdProveedor = ?m.IdProveedor or ?m.IdProveedor is null)
		order by ot.IdProveedor,ot.Estado,ot.Fecha,Nro
ENDTEXT
sql(cmdSQL,'cFechas')
SELECT cFechas


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    j  j                        ��   %   �           �          �  U  P �  � %�C�� ���% � T�� ���� �	 M(� ��� �� 	SELECT     ot.IdProveedor, ot.IdPaciente, ot.Fecha, ot.Trabajo, ot.Estado, ot.Obs, ot.FechaEntrega, ot.Nro, ot.Importe, p.Razon AS Proveedor, c.RazSocial AS Paciente�; �5 		FROM         odt_OrdenTrabajo AS ot LEFT OUTER JOIN�z �t                       vt_clientes AS c ON ot.IdEmpresa = c.IdEmpresa AND ot.IdPaciente = c.IdCliente LEFT OUTER JOIN�n �h                       cp_proveedor AS p ON ot.IdEmpresa = p.IdEmpresa AND ot.IdProveedor = p.IdProveedor�. �(         where ot.IdEmpresa=?oApp.Empresa�: �4         and ot.Fecha between ?m.dFecha and ?m.hFecha�> �8         and (ot.Estado = ?m.EstadoOT or ?m.EstadoOT='T')�M �G         and (ot.IdProveedor = ?m.IdProveedor or ?m.IdProveedor is null)�6 �0 		order by ot.IdProveedor,ot.Estado,ot.Fecha,Nro� � ��C � � cFechas� �� F� � U  SETEO IDPROVEEDOR CMDSQL SQL CFECHAS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 q � A � �
�������aA �q 4 q 2                       v        �  �      )   j                  