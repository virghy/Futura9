***SQL
lPARAMETERS m.IdFactura
TEXT TO cmdSQL noshow
SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, c.RazSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, 
                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Precio, det.Ult_Costo AS CostoUnitario, 
                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, left(c.Direccion,30) as Direccion, c.Telefono, c.Ruc, cond.Plazo, det.Descripcion AS Producto, det.Iva, 
                      det.[real], TotalFactura,v.Descuento,v.ImpDesc
FROM         dbo.vt_clientes c RIGHT OUTER JOIN
                      dbo.vt_factura v INNER JOIN
                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente LEFT OUTER JOIN
                      dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN
                      dbo.BS_Personas p INNER JOIN
                      dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor
                      where v.IdFactura = ?m.IdFActura 
ENDTEXT
sql(cmdSQL,'cFactura')
SELECT cFactura

PUBLIC mLetras
m.Dec=0


*Set Step On 
mTotal=TotalFactura
mLetras= ALLTRIM(numeral(int(TotalFactura)))
IF m.dec > 0
	mletras = "DOLARES " + mletras 
	mLetras = mLetras + ' CON ' + SUBSTR(STR(TotalFactura - INT(TotalFactura),3,2),2) + '/100'
	cMascara='@Z 999,999,999.99'
ELSE 
	mletras = mletras 
		cMascara='@Z 999,999,999'
ENDIF



***Impresion
SET DEVICE TO FILE c:\factura.txt
fpos=1

Imprimir(fpos,51,Numero)

Imprimir(PROW()+3,8,DAY(Fecha),'99')
Imprimir(PROW(),17,NombreMes(month(Fecha)))
Imprimir(PROW(),28,year(Fecha))



IF plazo > 0
	Imprimir(PROW(),62,"X")
ENDIF
IF plazo = 0
	Imprimir(PROW(),57,"X")
ENDIF

Imprimir(PROW()+1,12,Cliente)
Imprimir(PROW(),45,RUC)
Imprimir(PROW()+1,12,CHRTRAN(Direccion,CHR(13)+CHR(10),''))
Imprimir(PROW(),51,Telefono)
Imprimir(PROW()+3,0,"")

mIva0=0
mIva5=0
mIva10=0

SCAN
m.Precio = NVL(real,precio)
	Imprimir(PROW()+1,6,ALLTRIM(IdProducto),'XXXX')
	Imprimir(PROW(),8,Cantidad,'99999.9')
	Imprimir(PROW(),18,ALLTRIM(Producto),REPLICATE('X',40))
	Imprimir(PROW(),33,m.Precio,cMascara)
	
	IF Iva=0
		Imprimir(PROW(),38,m.Precio*Cantidad,cMascara)
		mIva0=mIva0 + m.Precio*Cantidad	
	ENDIF
	
	IF Iva=5
		Imprimir(PROW(),48,m.Precio*Cantidad,cMascara)
		mIva5=mIva5 + m.Precio*Cantidad	
	ENDIF
	
	IF Iva=10
		Imprimir(PROW(),56,m.Precio*Cantidad,cMascara)
		 mIva10=mIva10 + m.Precio*Cantidad	
	ENDIF
ENDSCAN


Imprimir(fpos + 17,38,mIva0,cMascara)
Imprimir(PROW(),48,m.mIva5,cMascara)
Imprimir(PROW(),56,m.mIva10,cMascara)

Imprimir(PROW()+1,20,LEFT(m.mletras,120))
Imprimir(PROW(),56,mTotal,cMascara)

*Imprimir(PROW()+1,20,substr(m.mletras,76,75))

Imprimir(PROW()+1,18,round(mIva5*5/105,m.dec),cMascara)
Imprimir(PROW(),30,round(mIva10*10/110,m.dec),cMascara)
Imprimir(PROW()+5,0,"")
SET DEVICE TO SCREEN 

COPY FILE c:\factura.txt TO lpt1
