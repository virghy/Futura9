                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Courier New      NroContrato      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      )"Resumen General de Servicios realizados"      Arial      m.dFecha,' al ', m.hFecha      Arial      	"Periodo"      Arial      "Servicios"      Arial      "Realizado"      Arial      
"Incluido"      Arial      "Precio"      Arial      	"Importe"      Arial      NroContrato,Nombre      Arial      
"Contrato"      Arial      IdProducto,Descripcion      Arial      Cantidad      "999,999,999"      Arial      Incluido      "999,999,999"      Arial      Precio      "999,999,999"      Arial      $(Cantidad - nvl(Incluido,0))* Precio      "999,999,999"      Arial      Cantidad      "999,999,999"      Arial      $(Cantidad - nvl(Incluido,0))* Precio      "999,999,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      Cantidad      "999,999,999
999,999,999"      Arial      $(Cantidad - nvl(Incluido,0))* Precio      "999,999,999
999,999,999"      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     }PROCEDURE Init
DO SETEO

IF EMPTY(m.NroContrato)
	m.NroContrato= null
ENDIF

TEXT TO CMDSQL NOSHOW
SELECT     c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion, sum(m.Cantidad) as Cantidad,cb.Cantidad as Incluido, cb.Precio as Precio
FROM         sas_Servicios AS s INNER JOIN
                      sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN
                      st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN
                      st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto
                      LEFT JOIN SAS_COBERTURA cb on c.IdContrato = cb.Idcontrato and c.IdEmpresa=cb.IdEmpresa
					  and m.IdProducto = cb.IdProducto 	
	WHERE  s.idempresa = ?oApp.Empresa 
			and (s.NroContrato = ?m.NroContrato or ?m.NroContrato is null)
			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha
group by c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion,cb.Cantidad, cb.Precio
order by c.Nombre,c.NroContrato,IdProducto
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA

ENDPROC
     Q���    8  8                        �x   %   �      �     �          �  U  Z �  � %�C�� ���% � T�� ���� �	 M(� ��� �� SELECT     c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion, sum(m.Cantidad) as Cantidad,cb.Cantidad as Incluido, cb.Precio as Precio�0 �* FROM         sas_Servicios AS s INNER JOIN�w �q                       sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN�\ �V                       st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN�l �f                       st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto�s �m                       LEFT JOIN SAS_COBERTURA cb on c.IdContrato = cb.Idcontrato and c.IdEmpresa=cb.IdEmpresa�/ �) 					  and m.IdProducto = cb.IdProducto 	�* �$ 	WHERE  s.idempresa = ?oApp.Empresa �G �A 			and (s.NroContrato = ?m.NroContrato or ?m.NroContrato is null)�f �` 			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha�Z �T group by c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion,cb.Cantidad, cb.Precio�0 �* order by c.Nombre,c.NroContrato,IdProducto� � ��C � � consulta� �� F� � U  SETEO NROCONTRATO CMDSQL SQL CONSULTA Init,     ��1 q � A � �q��1��qa�A �q 2                       r      )   8                  