   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Arial                          B  *   winspool  hp deskjet 840c series  USB001  TS001                                         hp deskjet 840c series          !@� h߀ 	     d   ��        4 (210 x 297 mm)                                                                 B�e�                                   4  �  d  	                                n                                                                                                                                                                                         �  B�e��ں                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �ںh p   d e s k j e t   8 4 0 c   s e r i e s   e r i e s , L o c a l O n l y , D r v C o n v e r t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           �DRIVER=winspool
DEVICE=hp deskjet 840c series
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=-3
COLOR=2
DUPLEX=1
TTOPTION=3
COLLATE=0
                   cpbt                           cpbt                                                          Arial                          empresa                                                       Arial                          ?"Control de Correlatividad de Cpbt. de Venta (Nros. faltantes)"                                                                 Arial                          Arial                          "Comprobante/Nro"              Nro                                                           Arial                          
datetime()                                                    Arial                          'P�g. '+alltrim(Str(_pageno))                                                                  Arial                          ""Periodo: ", dFecha,' al ', hFecha                                                             Arial                          "Rango: ", dNro, ' al ' , hNro                                                                 Arial                          empresa                                                       Arial                          Arial                          Arial                          Arial                          Arial                          dataenvironment                rLeft = 2
Top = 104
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
Name = "Dataenvironment"
               �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SET TEXTMERGE ON 

IF EMPTY(m.vt_cpbt)
	m.vt_cpbt=null
ENDIF
	
		      

TEXT TO cmdSQL noshow

Declare @dNro int, @hNro int, @dFecha datetime,@hFecha datetime,
	@Cpbt char(3), @Empresa char(3)

Select @dNro= ?m.dNro,@hNro =?m.hNro, @cpbt=?m.vt_cpbt, 
	@Empresa=?oApp.Empresa,
	@dFecha =?m.dFecha, @hFecha = ?m.hFecha

Declare @Nros table(Nro int)

IF @dNro = 0 AND NOT @cpbt is null
begin
		SELECT @dNro = MIN(Comprobante) from cn_Iva where TipoComprobante = @cpbt
			and TipoIva ='V' and IdEmpresa=@Empresa 
			and FechaComprobante between @dFecha and @hFecha
	
END 

IF @hNro = 0 AND NOT @cpbt is null
begin
		SELECT @hNro = MAX(Comprobante) from cn_Iva where TipoComprobante = @cpbt
			and TipoIva ='V' and IdEmpresa=@Empresa 
			and FechaComprobante between @dFecha and @hFecha
	
END 



while @dNro <=@hNro
Begin
	Insert @Nros
	Select @dNro
	Set @dNro= @dNro + 1
end

Select IdComprobante + '-' + Descripcion as Cpbt, a.* from @Nros a ,vt_cpbt 
where not exists(Select * from cn_Iva where (TipoComprobante = IdComprobante or TipoComprobante is null)
		and Comprobante = Nro and TipoIva ='V' and IdEmpresa=@Empresa 
		and FechaComprobante between @dFecha and @hFecha)
and (vt_cpbt.IdComprobante = @cpbt or @cpbt is null) 
and vt_cpbt.IdEmpresa = @Empresa
order by 1,2
ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
              9���                                �-   %   3      �  6   [          �  U  
  �  � U  SETEO� G` � %�C��  ���$ � T��  ���� �	 M(� �� �  �F �@ Declare @dNro int, @hNro int, @dFecha datetime,@hFecha datetime,�& �  	@Cpbt char(3), @Empresa char(3)� �  �> �8 Select @dNro= ?m.dNro,@hNro =?m.hNro, @cpbt=?m.vt_cpbt, � � 	@Empresa=?oApp.Empresa,�. �( 	@dFecha =?m.dFecha, @hFecha = ?m.hFecha� �  �" � Declare @Nros table(Nro int)� �  �( �" IF @dNro = 0 AND NOT @cpbt is null� � begin�Q �K 		SELECT @dNro = MIN(Comprobante) from cn_Iva where TipoComprobante = @cpbt�1 �+ 			and TipoIva ='V' and IdEmpresa=@Empresa �9 �3 			and FechaComprobante between @dFecha and @hFecha� � 	�
 � END � �  �( �" IF @hNro = 0 AND NOT @cpbt is null� � begin�Q �K 		SELECT @hNro = MAX(Comprobante) from cn_Iva where TipoComprobante = @cpbt�1 �+ 			and TipoIva ='V' and IdEmpresa=@Empresa �9 �3 			and FechaComprobante between @dFecha and @hFecha� � 	�
 � END � �  � �  � �  � � while @dNro <=@hNro� � Begin� � 	Insert @Nros� � 	Select @dNro� � 	Set @dNro= @dNro + 1�	 � end� �  �R �L Select IdComprobante + '-' + Descripcion as Cpbt, a.* from @Nros a ,vt_cpbt �n �h where not exists(Select * from cn_Iva where (TipoComprobante = IdComprobante or TipoComprobante is null)�F �@ 		and Comprobante = Nro and TipoIva ='V' and IdEmpresa=@Empresa �9 �3 		and FechaComprobante between @dFecha and @hFecha)�; �5 and (vt_cpbt.IdComprobante = @cpbt or @cpbt is null) �& �  and vt_cpbt.IdEmpresa = @Empresa� � order by 1,2� � ��C � � cn_rIvaVentas� �� F� � U  VT_CPBT CMDSQL SQL CN_RIVAVENTAS BeforeOpenTables,     �� InitA     ��1 q 3 a � A � a aaa ���a !a �� �q � a �� �q � a a a �� 11�� a !�a��a!A �r 2                       &         A   �      )                      �  � U  SETEO Init,