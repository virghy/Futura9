  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=1
      Courier New      NroContrato      Arial      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      ."Detalle de Servicios realizados por Contrato"      Arial      m.dfecha, ' al ' , m.hfecha      Arial      	"Periodo"      Arial      	"Fecha
"      Arial      "N� Solic.
"      Arial      "Paciente
"      Arial      
"Servicio"      Arial      "Formulario"      Arial      	"Importe"      Arial      "Destino Final"      Arial      "Autorizado por"      Arial      "Contrato: ",NroContrato,Nombre      Arial      FechaAgenda      "@D"      Arial      NroSolicitud      Arial      alltrim(NroSeguro)," ",Paciente      Arial      descripcion, 'Aut: ' + Autoriza      Arial      NroFS      Arial      precio      "999,999,999"      Arial      Destino      Arial      Autoriza      Arial      precio      "999,999,999,999"      Arial      "Total"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      precio      "999,999,999,999"      Arial      "Total General"      Arial      dataenvironment      aTop = 120
Left = 315
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO SETEO

IF EMPTY(m.NroContrato)
	m.NroContrato= null
ENDIF

TEXT TO CMDSQL NOSHOW
SELECT     s.fechaAgenda, s.NroSolicitud, s.Paciente, c.NroContrato, c.nombre, s.Motivo, m.IdProducto, m.Cantidad, m.Precio, pr.Descripcion, s.NroSeguro, s.Autoriza, 
                      s.NroFS
FROM         sas_Servicios AS s INNER JOIN
                      sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN
                      st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN
                      st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto
	WHERE  s.idempresa = ?oApp.Empresa 
			and (s.NroContrato = ?m.NroContrato or ?m.NroContrato is null)
			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha
ORDER BY c.Nombre, s.NroContrato, s.fechaAgenda
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA
ENDPROC
     ����    }  }                        O�   %   �      4               �  U  � �  � %�C�� ���% � T�� ���� �	 M(� ��� �� SELECT     s.fechaAgenda, s.NroSolicitud, s.Paciente, c.NroContrato, c.nombre, s.Motivo, m.IdProducto, m.Cantidad, m.Precio, pr.Descripcion, s.NroSeguro, s.Autoriza, �# �                       s.NroFS�0 �* FROM         sas_Servicios AS s INNER JOIN�w �q                       sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN�\ �V                       st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN�l �f                       st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto�* �$ 	WHERE  s.idempresa = ?oApp.Empresa �G �A 			and (s.NroContrato = ?m.NroContrato or ?m.NroContrato is null)�f �` 			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha�5 �/ ORDER BY c.Nombre, s.NroContrato, s.fechaAgenda� � ��C � � consulta� �� F� � U  SETEO NROCONTRATO CMDSQL SQL CONSULTA Init,     ��1 q � A � �
1q���qaQA �q 1                       �      )   }                  