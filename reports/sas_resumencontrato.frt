  t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2794
PAPERWIDTH=2159
COLOR=2
      Courier New      
IdProducto      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      !"Resumen de Contratos x Servicio"      Arial      m.dFecha,' al ', m.hFecha      Arial      	"Periodo"      Arial      "Servicios"      Arial      
"Contrato"      Arial      "Realizado"      Arial      
"Incluido"      Arial      "Precio"      Arial      	"Importe"      Arial      IdProducto,Descripcion      Arial      !SOLORESUMEN      NroContrato,Nombre      Arial      Cantidad      "999,999,999"      Arial      Incluido      "999,999,999"      Arial      Precio      "999,999,999"      Arial      $(Cantidad - nvl(Incluido,0))* Precio      "999,999,999"      Arial      IdProducto,Descripcion      Arial      SOLORESUMEN      Cantidad      "999,999,999"      Arial      $(Cantidad - nvl(Incluido,0))* Precio      "999,999,999"      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      Cantidad      "999,999,999,999
999,999,999"      Arial      $(Cantidad - nvl(Incluido,0))* Precio      "999,999,999,999
999,999,999"      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     pPROCEDURE Init
DO SETEO

IF EMPTY(m.Producto)
	m.Producto= null
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
			and (m.IdProducto = ?m.Producto or ?m.Producto is null)
			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha
group by c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion,cb.Cantidad, cb.Precio
order by IdProducto,c.Nombre,c.NroContrato
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA

ENDPROC
     G���    .  .                        �8   %   �      �     �          �  U  S �  � %�C�� ���% � T�� ���� �	 M(� ��� �� SELECT     c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion, sum(m.Cantidad) as Cantidad,cb.Cantidad as Incluido, cb.Precio as Precio�0 �* FROM         sas_Servicios AS s INNER JOIN�w �q                       sas_Contrato AS c ON s.idEmpresa = c.IdEmpresa AND s.NroContrato = c.NroContrato INNER JOIN�\ �V                       st_movimiento_Det AS m ON s.IdRemision = m.IdRemision INNER JOIN�l �f                       st_Producto AS pr ON m.IdEmpresa = pr.IdEmpresa AND m.IdProducto = pr.IdProducto�s �m                       LEFT JOIN SAS_COBERTURA cb on c.IdContrato = cb.Idcontrato and c.IdEmpresa=cb.IdEmpresa�/ �) 					  and m.IdProducto = cb.IdProducto 	�* �$ 	WHERE  s.idempresa = ?oApp.Empresa �@ �: 			and (m.IdProducto = ?m.Producto or ?m.Producto is null)�f �` 			and convert(datetime,CONVERT(VARCHAR (10),s.fechaAgenda,105)) between ?m.dfecha and ?m.hfecha�Z �T group by c.NroContrato, c.nombre, m.IdProducto,pr.Descripcion,cb.Cantidad, cb.Precio�0 �* order by IdProducto,c.Nombre,c.NroContrato� � ��C � � consulta� �� F� � U  SETEO PRODUCTO CMDSQL SQL CONSULTA Init,     ��1 q � A � �q��1��a�A �q 2                       e      )   .                  