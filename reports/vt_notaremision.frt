  W                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'ORIENTATION=1
PAPERSIZE=120
COLOR=1
      Courier New      
IdRemision      chr(27) + chr(48)      Courier New      Courier New      Courier New      �"234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"      Courier New      3      Courier New      4      Courier New      5      Courier New      6      Courier New      7      Courier New      NroRemision      D      Courier New      8      Courier New      9      Courier New      0      Courier New      1      Courier New      2      Courier New      RUC      D      Courier New      Cliente      D      Courier New      3      Courier New      4      Courier New      5      Courier New      6      Courier New      7      Courier New      8      Courier New      9      Courier New      0      Courier New      1      Courier New      2      Courier New      3      Courier New      4      Courier New      
IdProducto      D      Courier New      Cantidad      "99999"      D      Courier New      
cantidad>0      Producto      D      Courier New      dataenvironment      `Top = 100
Left = 28
Width = 624
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     $PROCEDURE Init
DO seteo
_ASCIICOLS = 145
_asciirows = 45
TEXT TO cmdSQL noshow
SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, 
                      det.Imprime, c.Direccion, c.Telefono, c.Ruc, det.Descripcion AS Producto, det.Iva, det.real, r.NroRemision
FROM         os_remision AS r INNER JOIN
                      st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN
                      vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa
                      where r.IdRemision = ?m.IdRemision 
ENDTEXT
sql(cmdSQL,'cFactura')

SELECT cFactura

m.nro = RECCOUNT()
DO WHILE m.nro<30
	APPEND BLANK
	m.nro= m.nro + 1
ENDDO 

ENDPROC
     ����    �  �                        �   %   8      n     F          �  U  � �  � T�>����� T�?��-��	 M(� ��� �� SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, �� ��                       det.Imprime, c.Direccion, c.Telefono, c.Ruc, det.Descripcion AS Producto, det.Iva, det.real, r.NroRemision�. �( FROM         os_remision AS r INNER JOIN�` �Z                       st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN�g �a                       vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa�? �9                       where r.IdRemision = ?m.IdRemision � � ��C � � cFactura� �� F� � T�� �CN�� +��� ����� � T�� ��� ��� � U  SETEO CMDSQL SQL CFACTURA NRO Init,     ��1 q � � � �	a�q�A �r � AQ QA 2                             )   �                  