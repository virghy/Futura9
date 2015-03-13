***SQL
*lPARAMETERS m.IdFactura
TEXT TO cmdSQL noshow
SELECT     c.RazSocial AS Cliente, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, det.GravadaIncluido, det.RegimenTurismo, 
                      det.Imprime, c.Direccion, c.Telefono, c.Ruc, det.Descripcion AS Producto, det.Iva, det.real, r.NroRemision
FROM         os_remision AS r INNER JOIN
                      st_movimiento_Det AS det ON r.idRemision = det.IdRemision INNER JOIN
                      vt_clientes AS c ON r.idcliente = c.IdCliente AND r.idempresa = c.IdEmpresa
                      where r.IdRemision = ?m.IdRemision 
ENDTEXT
sql(cmdSQL,'cFactura')

**Creamos para imprimir descripciones de prod. largas
CREATE CURSOR CampoMemo(Producto m)
APPEND BLANK IN CampoMemo
SET MEMOWIDTH TO 40		&&Longitud de la Descripcion

SELECT cFactura

***Impresion
SET DEVICE TO FILE c:\factura
fpos=5
imprimir(0,0,CHR(27)+CHR(48))
Imprimir(fpos,90,"")
Imprimir(PROW()+5,0,"")

Imprimir(PROW()+1,25,Cliente)
Imprimir(PROW(),95,RUC)
mcolumna=100

SCAN
	replace CampoMemo.Producto WITH ALLTRIM(cFactura.Producto) IN CampoMemo
	Imprimir(PROW()+28,5,IdProducto,'XXXXXXXXXX')
	Imprimir(PROW(),17,Cantidad,'999999')
	Imprimir(PROW(),35,MLINE(CampoMemo.Producto,1))

*	Imprimir(PROW(),35,Producto,REPLICATE('X',40))

* RECORRIDO

	FOR i=2 TO MEMLINES(CampoMemo.producto) 
		Imprimir(PROW()+1,35,MLINE(CampoMemo.producto,i))
	ENDFOR 

ENDSCAN

imprimir(PROW()+1,0,CHR(12))
 
SET DEVICE TO SCREEN 


COPY FILE c:\factura.txt TO lpt1
