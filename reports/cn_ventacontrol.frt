  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\tierra2\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=0
PAPERSIZE=9
ASCII=0
COPIES=1
DEFAULTSOURCE=7
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=3
COLLATE=0
      T  <  winspool  \\tierra2\HP DeskJet 840C/841C/842C/843C  USB001                       \\tierra2\HP DeskJet 840C/841C   � pC� 	 �
od   ,  ,   Letter                                                                          DINU" L$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     SMTJ      H P   D e s k J e t   8 4 0 C / 8 4 1 C / 8 4 2 C / 8 4 3 C   InputBin FORMSOURCE RESDLL UniresDLL PSAlignmentFile HPF880AL PSHelpFile HPFDJ200 Orientation PORTRAIT PaperSize LETTER Resolution r300x300 PM PlainEconoMono MediaType STANDARD Photo1200Mode Off ColorMode Mono PQ Econo HPHTDLLName HPFIMG50 HPHTBrightness HPHTSliderMiddle HPHTIntensity HPHTSliderMiddle HPHTColorTemp HPHTSliderMiddle HPHTVividness HPHTSliderMiddle HPHTInkLevels HPHTSliderMiddle                                                       $   �$               $   �$         Arial      cpbt      Arial      Arial      Arial      Arial      empresa             Arial      empresa             Arial      ?"Control de Correlatividad de Cpbt. de Venta (Nros. faltantes)"             Arial      ""Periodo: ", dFecha,' al ', hFecha             Arial      "Rango: ", dNro, ' al ' , hNro             Arial      "Comprobante/Nro"      Arial      cpbt             Arial      Nro             Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      dataenvironment      �Top = 104
Left = 2
Width = 520
Height = 200
InitialSelectedAlias = "cn_rivacompra"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init
SET TEXTMERGE ON 

IF EMPTY(m.vt_cpbt)
	m.vt_cpbt=null
ENDIF
	
		      

TEXT TO cmdSQL noshow

Declare @dNro bigint, @hNro bigint, @dFecha datetime,@hFecha datetime,
	@Cpbt char(3), @Empresa char(3)

Select @dNro= ?m.dNro,@hNro =?m.hNro, @cpbt=?m.vt_cpbt, 
	@Empresa=?oApp.Empresa,
	@dFecha =?m.dFecha, @hFecha = ?m.hFecha

Declare @Nros table(Nro bigint)

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
		and replace(replace(Comprobante,'-',''),' ','') = Nro and TipoIva ='V' and IdEmpresa=@Empresa 		
		and FechaComprobante between @dFecha and @hFecha)
and (vt_cpbt.IdComprobante = @cpbt or @cpbt is null) 
and vt_cpbt.IdEmpresa = @Empresa
order by 1,2
ENDTEXT



sql(cmdSQL,'cn_rIvaVentas')

SELECT cn_rivaventas

ENDPROC
     d���    K  K                        yr   %   ^      �  6   �          �  U  
  �  � U  SETEO� G` � %�C��  ���$ � T��  ���� �	 M(� �� �  �L �F Declare @dNro bigint, @hNro bigint, @dFecha datetime,@hFecha datetime,�& �  	@Cpbt char(3), @Empresa char(3)� �  �> �8 Select @dNro= ?m.dNro,@hNro =?m.hNro, @cpbt=?m.vt_cpbt, � � 	@Empresa=?oApp.Empresa,�. �( 	@dFecha =?m.dFecha, @hFecha = ?m.hFecha� �  �% � Declare @Nros table(Nro bigint)� �  �( �" IF @dNro = 0 AND NOT @cpbt is null� � begin�Q �K 		SELECT @dNro = MIN(Comprobante) from cn_Iva where TipoComprobante = @cpbt�1 �+ 			and TipoIva ='V' and IdEmpresa=@Empresa �9 �3 			and FechaComprobante between @dFecha and @hFecha� � 	�
 � END � �  �( �" IF @hNro = 0 AND NOT @cpbt is null� � begin�Q �K 		SELECT @hNro = MAX(Comprobante) from cn_Iva where TipoComprobante = @cpbt�1 �+ 			and TipoIva ='V' and IdEmpresa=@Empresa �9 �3 			and FechaComprobante between @dFecha and @hFecha� � 	�
 � END � �  � �  � �  � � while @dNro <=@hNro� � Begin� � 	Insert @Nros� � 	Select @dNro� � 	Set @dNro= @dNro + 1�	 � end� �  �R �L Select IdComprobante + '-' + Descripcion as Cpbt, a.* from @Nros a ,vt_cpbt �n �h where not exists(Select * from cn_Iva where (TipoComprobante = IdComprobante or TipoComprobante is null)�h �b 		and replace(replace(Comprobante,'-',''),' ','') = Nro and TipoIva ='V' and IdEmpresa=@Empresa 		�9 �3 		and FechaComprobante between @dFecha and @hFecha)�; �5 and (vt_cpbt.IdComprobante = @cpbt or @cpbt is null) �& �  and vt_cpbt.IdEmpresa = @Empresa� � order by 1,2� � ��C � � cn_rIvaVentas� �� F� � U  VT_CPBT CMDSQL SQL CN_RIVAVENTAS BeforeOpenTables,     �� InitA     ��1 q 3 a � A � a �aa ���a Qa �� �q � a �� �q � a a a �� 11�� a !����a!A �r 2                       &         A   �      )   K                  