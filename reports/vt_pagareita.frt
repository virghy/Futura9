                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Arial      cFact.Cuota      Arial      Courier New      Courier New      cFact.Importe      "999,999,999,999"      Courier New      8alltrim(str(cFact.cuota)),'/', alltrim(str(TotalCuotas))      Courier New      cFact.Vencimiento      "@E"      Courier New      day(cFact.Vencimiento)      Courier New      #nombremes(month(cFact.Vencimiento))      Courier New      year(cFact.Vencimiento)      Courier New      numeral(cFact.Importe), ".-"      Courier New      cFact.Nombre      Courier New      cFact.Direccion      Courier New      cFact.Ciudad      Courier New      	cFact.RUC      Courier New      "cFact.Telefono, " ", cFact.Celular      Courier New      cf.CuerpoPagare      Courier New      <FJ>      dataenvironment      �Top = 250
Left = 202
Width = 519
Height = 254
InitialSelectedAlias = "rcheques"
DataSource = .NULL.
Name = "Dataenvironment"
     
 PROCEDURE Destroy
RELEASE m.TotalCuotas
ENDPROC
PROCEDURE Init
DO seteo
PUBLIC m.TotalCuotas
IF m.OpcionPagare=1
	TEXT TO cmdSQL NOSHOW 
		SELECT   f.IdMoneda,  f.Fecha, f.TotalFactura-ISNULL(f.Anticipo,0) AS Importe, c.RazSocial as Nombre, c.Ruc, c.Direccion, cd.Descripcion as Ciudad, c.Telefono, c.Celular, e.raz�n as RazonSocial,
		 e.domicilio, e.localidad,m.Descripcion AS Moneda, Cuota=1,f.Fecha as Vencimiento
		FROM         vt_factura AS f INNER JOIN
		                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente INNER JOIN
		                      empresa AS e ON f.IdEmpresa = e.idempresa INNER JOIN
	                      bs_Monedas AS m ON f.IdMoneda = m.IdMoneda
				LEFT JOIN BS_CIUDADES CD ON C.IdCiudad = cd.IdCiudad	                         
		                      where f.IdFactura=?m.IdFactura
		                     
	ENDTEXT
ELSE
	TEXT TO cmdSQL NOSHOW 
		SELECT   f.IdMoneda,  f.Fecha , s.Importe AS Importe, c.RazSocial as Nombre, c.Ruc, c.Direccion, cd.Descripcion as Ciudad, c.Telefono, c.Celular, e.raz�n as RazonSocial,
		 e.domicilio, e.localidad,m.Descripcion AS Moneda,s.Cuota,s.Vencimiento
		FROM         vt_factura AS f INNER JOIN
		                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente INNER JOIN
		                      empresa AS e ON f.IdEmpresa = e.idempresa INNER JOIN
	                      bs_Monedas AS m ON f.IdMoneda = m.IdMoneda   
	                      inner join vt_Forma_Pago s on f.IdFactura=s.IdFactura
	                      LEFT JOIN BS_CIUDADES CD ON C.IdCiudad = cd.IdCiudad
		                      where f.IdFactura=?m.IdFactura and s.Cuota<>0
		                      order by s.Vencimiento
	ENDTEXT

ENDIF

sql(cmdSQL,'cFact')
m.TotalCuotas= RECCOUNT('cFact')

=sql("Select dbo.LeerConstante(?oApp.Empresa,'VT_cuerpoPagare') as CuerpoPagare", 'cF')

=SQL("Select IdProducto,Descripcion,Cantidad from st_Movimiento_Det where IdFactura = ?m.IdFactura",'cDet')

SQL("Select convert(char(20),dbo.LeerConstante(?oApp.Empresa,'VT_CONCEPTO_INTERES')) as IDTASA", 'cConfig')


m.Detalle=""
SELECT cDet

SCAN 
	IF cConfig.IdTasa=IdProducto
		LOOP 
	ENDIF 	
	m.Detalle=m.Detalle + TRANSFORM(Cantidad,'999') + "(" + numeral(Cantidad) + ") " + CHR(9) + CHR(9) + CHR(9)+ CHR(9) + LEFT(Descripcion,50) + CHR(13)
ENDSCAN



SELECT cf
replace CuerpoPagare WITH STRTRAN(CuerpoPagare, "<<Contenido>>",m.Detalle)

SELECT cFact
GO TOP 









ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ���    �
  �
                        �&   %   �	      �
  3    
          �  U   	 <��  � U  TOTALCUOTAS� �  �	 7�� � %��� ���@�	 M(� ��� �� 		SELECT   f.IdMoneda,  f.Fecha, f.TotalFactura-ISNULL(f.Anticipo,0) AS Importe, c.RazSocial as Nombre, c.Ruc, c.Direccion, cd.Descripcion as Ciudad, c.Telefono, c.Celular, e.raz�n as RazonSocial,�Y �S 		 e.domicilio, e.localidad,m.Descripcion AS Moneda, Cuota=1,f.Fecha as Vencimiento�/ �) 		FROM         vt_factura AS f INNER JOIN�t �n 		                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente INNER JOIN�R �L 		                      empresa AS e ON f.IdEmpresa = e.idempresa INNER JOIN�G �A 	                      bs_Monedas AS m ON f.IdMoneda = m.IdMoneda�X �R 				LEFT JOIN BS_CIUDADES CD ON C.IdCiudad = cd.IdCiudad	                         �< �6 		                      where f.IdFactura=?m.IdFactura� � 		                     � � ���	 M(� ��� �� 		SELECT   f.IdMoneda,  f.Fecha , s.Importe AS Importe, c.RazSocial as Nombre, c.Ruc, c.Direccion, cd.Descripcion as Ciudad, c.Telefono, c.Celular, e.raz�n as RazonSocial,�O �I 		 e.domicilio, e.localidad,m.Descripcion AS Moneda,s.Cuota,s.Vencimiento�/ �) 		FROM         vt_factura AS f INNER JOIN�t �n 		                      vt_clientes AS c ON f.IdEmpresa = c.IdEmpresa AND f.IdCliente = c.IdCliente INNER JOIN�R �L 		                      empresa AS e ON f.IdEmpresa = e.idempresa INNER JOIN�J �D 	                      bs_Monedas AS m ON f.IdMoneda = m.IdMoneda   �R �L 	                      inner join vt_Forma_Pago s on f.IdFactura=s.IdFactura�Q �K 	                      LEFT JOIN BS_CIUDADES CD ON C.IdCiudad = cd.IdCiudad�K �E 		                      where f.IdFactura=?m.IdFactura and s.Cuota<>0�4 �. 		                      order by s.Vencimiento� � � ��C � � cFact� �� T�� �C� cFactN��[ ��C�I Select dbo.LeerConstante(?oApp.Empresa,'VT_cuerpoPagare') as CuerpoPagare� cF� ��p ��C�\ Select IdProducto,Descripcion,Cantidad from st_Movimiento_Det where IdFactura = ?m.IdFactura� cDet� ��p ��C�Y Select convert(char(20),dbo.LeerConstante(?oApp.Empresa,'VT_CONCEPTO_INTERES')) as IDTASA� cConfig� �� T�� ��  �� F� � ~��� %�� � �	 ��W� .� �X T�� ��� C�
 � 999_� (C �
 � � ) C�	 C�	 C�	 C�	 C� �2=C� �� � F� �$ >� ��C� � <<Contenido>>�� ��� F� � #)� U  SETEO TOTALCUOTAS OPCIONPAGARE CMDSQL SQL DETALLE CDET CCONFIG IDTASA
 IDPRODUCTO CANTIDAD NUMERAL DESCRIPCION CF CUERPOPAGARE CFACT
  �  � U  SETEO Destroy,     �� InitI     �� BeforeOpenTables�	    ��1 � 2 q � 1� ���A!q���A � � ��A!�!�AA B ba�� q � AA A �A t Ar Q ; q 2                       (         C   �	     1   
  
  H    )   �
                  