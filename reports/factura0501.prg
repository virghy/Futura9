***SQL
*lPARAMETERS m.IdFactura
TEXT TO cmdSQL noshow
SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, Fecha=convert(char(10),v.Fecha,105), v.IdVendedor, 
                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, 
                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, c.Direccion, c.Telefono, c.Ruc, cond.Plazo, det.Descripcion AS Producto, det.Iva, 
                      det.[real], TotalFactura
FROM         dbo.vt_clientes c RIGHT OUTER JOIN
                      dbo.vt_factura v INNER JOIN
                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente LEFT OUTER JOIN
                      dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN
                      dbo.BS_Personas p INNER JOIN
                      dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor
                      where v.IdFactura = ?m.IdFActura 
ENDTEXT
sql(cmdSQL,'cFactura')

**Creamos para imprimir descripciones de prod. largas
CREATE CURSOR CampoMemo(Producto m)
APPEND BLANK IN CampoMemo
SET MEMOWIDTH TO 40		&&Longitud de la Descripcion


SELECT cFactura

PUBLIC mLetras
*m.Dec=0

*Set Step On 
mTotal=TotalFactura
mLetras= ALLTRIM(numeral(int(TotalFactura)))
IF m.dec > 0
	mletras = "DOLARES " + mletras 
	mLetras = mLetras + ' CON ' + SUBSTR(STR(TotalFactura - INT(TotalFactura),3,2),2) + '/100'
	cMascara='@Z 999,999,999.99'
ELSE 
	mletras = "GUARANIES " + mletras 
		cMascara='@Z 999,999,999,999'
ENDIF

***Impresion
SET DEVICE TO FILE c:\factura
fpos=5
imprimir(0,0,CHR(27)+CHR(48))
Imprimir(fpos,90,Numero)
Imprimir(PROW()+5,0,"")
IF plazo > 0
	Imprimir(PROW()+1,125,"XX")
	ELSE
	Imprimir(PROW()+2,125,"")
ENDIF

Imprimir(PROW()+1,24,Fecha)

IF plazo = 0
	Imprimir(PROW()+2,125,"XX")
	ELSE
	Imprimir(PROW(),125,"")
ENDIF

Imprimir(PROW()+3,10,RUC)
Imprimir(PROW()+2,30,Cliente)
Imprimir(PROW()+2,15,Direccion, REPLICATE("X",60))
Imprimir(PROW(),100,Telefono)
Imprimir(PROW()+3,0,"")
mIva0=0
mIva5=0
mIva10=0
mcolumna=5
*SET STEP ON

SCAN
	m.Precio = NVL(real,precio)
	replace CampoMemo.Producto WITH ALLTRIM(cFactura.Producto) IN CampoMemo

	Imprimir(PROW()+1,3,IdProducto,'XXXXXXXXXX')
	Imprimir(PROW(),12,Cantidad,'999999')
*	Imprimir(PROW(),25,Producto,REPLICATE('X',40))
*	Imprimir(PROW(),25,Producto)
	Imprimir(PROW(),25,MLINE(CampoMemo.producto,1))
	Imprimir(PROW(),mcolumna+62,m.Precio,cMascara)
	
	IF Iva=0
		Imprimir(PROW(),mcolumna+80,m.Precio*Cantidad,cMascara)
		mIva0=mIva0 + m.Precio*Cantidad	
	ENDIF
	
	IF Iva=5
		Imprimir(PROW(),mcolumna+95,m.Precio*Cantidad,cMascara)
		mIva5=mIva5 + m.Precio*Cantidad	
	ENDIF
	
	IF Iva=10
		Imprimir(PROW(),mcolumna+110,m.Precio*Cantidad,cMascara)
		 mIva10=mIva10 + m.Precio*Cantidad	
	ENDIF
	
	*RECORRIDO

	FOR i=2 TO MEMLINES(CampoMemo.producto) 
		Imprimir(PROW()+1,25,MLINE(CampoMemo.producto,i))
	ENDFOR 

ENDSCAN


Imprimir(fpos + 54,mcolumna+80,mIva0,cMascara)
Imprimir(PROW(),mcolumna+95,m.mIva5,cMascara)
Imprimir(PROW(),mcolumna+110,m.mIva10,cMascara)

Imprimir(PROW()+2,25,LEFT(m.mletras,75))
Imprimir(PROW(),mcolumna+110,mTotal,cMascara)
Imprimir(PROW()+2,20,substr(m.mletras,76,75))

Imprimir(PROW()+2,25,round(mIva5*5/105,m.dec),cMascara)
Imprimir(PROW(),55,round(mIva10*10/110,m.dec),cMascara)
Imprimir(PROW(),90,round(mIva10*10/110,m.dec) + round(mIva5*5/105,m.dec),cMascara)
imprimir(PROW()+1,0,CHR(12))
 
SET DEVICE TO SCREEN 

COPY FILE c:\factura.txt TO lpt1
