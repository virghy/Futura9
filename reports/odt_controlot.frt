  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=9
COLOR=2
      Arial      Arial      Arial      Arial      Arial      Arial      Arial      "Control de Ordenes de Trabajo"      Arial      oApp.Nombreempresa      Arial      m.dFecha," al ",m.hFecha      Arial      
"Per�odo:"      Arial      "Fecha"      Arial      "Nro"      Arial      "Proveedor
"      Arial      	"Trabajo"      Arial      
"Paciente"      Arial      	"Importe"      Arial      "Estado"      Arial      "Fecha Entrega"      Arial      Fecha      Arial      Nro      "9999"      Arial      IdProveedor, " ",Proveedor      Arial      Trabajo, " ", OBs      Arial      IdPaciente, " ", Paciente      Arial      Importe      "999,999,999"      Arial      'IIf(Estado='P',"Pendiente","Realizado")      Arial      FechaEntrega      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      Importe      "999,999,999"      Arial      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
DO seteo
TEXT TO cmdSQL NOSHOW 
	SELECT     ot.IdProveedor, ot.IdPaciente, ot.Fecha, ot.Trabajo, ot.Estado, ot.Obs, ot.FechaEntrega, ot.Nro, ot.Importe, p.Razon AS Proveedor, c.RazSocial AS Paciente
		FROM         odt_OrdenTrabajo AS ot LEFT OUTER JOIN
                      vt_clientes AS c ON ot.IdEmpresa = c.IdEmpresa AND ot.IdPaciente = c.IdCliente LEFT OUTER JOIN
                      cp_proveedor AS p ON ot.IdEmpresa = p.IdEmpresa AND ot.IdProveedor = p.IdProveedor
        where ot.IdEmpresa=?oApp.Empresa
        and ot.Fecha between ?m.dFecha and ?m.hFecha
        and (ot.Estado = ?m.EstadoOT or ?m.EstadoOT='T')
		order by ot.Fecha,Nro
ENDTEXT
sql(cmdSQL,'cFechas')
SELECT cFechas


ENDPROC
     ����    �  �                        7�   %   *      t     R          �  U  
  �  � U  SETEO� �  �	 M(� ��� �� 	SELECT     ot.IdProveedor, ot.IdPaciente, ot.Fecha, ot.Trabajo, ot.Estado, ot.Obs, ot.FechaEntrega, ot.Nro, ot.Importe, p.Razon AS Proveedor, c.RazSocial AS Paciente�; �5 		FROM         odt_OrdenTrabajo AS ot LEFT OUTER JOIN�z �t                       vt_clientes AS c ON ot.IdEmpresa = c.IdEmpresa AND ot.IdPaciente = c.IdCliente LEFT OUTER JOIN�n �h                       cp_proveedor AS p ON ot.IdEmpresa = p.IdEmpresa AND ot.IdProveedor = p.IdProveedor�. �(         where ot.IdEmpresa=?oApp.Empresa�: �4         and ot.Fecha between ?m.dFecha and ?m.hFecha�> �8         and (ot.Estado = ?m.EstadoOT or ?m.EstadoOT='T')� � 		order by ot.Fecha,Nro� � ��C � � cFechas� �� F� � U  SETEO CMDSQL SQL CFECHAS BeforeOpenTables,     �� InitA     ��1 q 3 q � �
�������A �q 3                       &         A         )   �                  