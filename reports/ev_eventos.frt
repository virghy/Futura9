  T                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=Enviar a OneNote 2007
OUTPUT=Send To Microsoft OneNote Port:
ORIENTATION=1
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
      Z  )  winspool  Enviar a OneNote 2007  Send To Microsoft OneNote Port:                       ,Enviar a OneNote 2007            � � /        d   ,  ,   Letter                                                                                wpno               �          �                                                                                                                     Arial      Arial      Arial      Arial      Arial      Arial      	"Eventos"      Arial      empresa             Arial      
"Periodo:"      Arial      m.dFecha,m.hFecha      Arial      "Tipo:"      Arial      0iif(isnull(m.TipoEvento),'Todos',cOpcion.Opcion)      Arial      "Sucursal:"      Arial      (iif(isnull(m.Sucursal),'Todos',Sucursal)      Arial      "Nro"      Arial      	"Fecha
"      Arial      "Nombre"      Arial      "Basico"      Arial      "Adicional"      Arial      "IVA"      Arial      "Total"      Arial      "Costo"      Arial      
"Utilidad"      Arial      "Estado 
"      Arial      NroEvento,'-',IdSucursal      Arial      Fecha      Arial      	Agasajado      Arial      ImporteBasico      "999,999,999"      Arial      
Excedentes      "999,999,999"      Arial      IVa      "@Z 999,999,999"      Arial      TotalGeneral      "999,999,999"      Arial      
CostoTotal      "999,999,999"      Arial      Utilidad      "999,999,999"      Arial      Estado      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total"             Arial      Iva      "999,999,999,999"      Arial      TotalGeneral      "999,999,999,999"      Arial      
CostoTotal      "999,999,999,999"      Arial      Utilidad      "999,999,999,999"      Arial      
"Promedio"      Arial      	NroEvento      Arial      TotalGeneral      "999,999,999,999"      Arial      
CostoTotal      "999,999,999,999"      Arial      Utilidad      "999,999,999,999"      Arial      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
IF EMPTY(m.sucursal)
	m.sucursal=null
ENDIF
	
IF empty(m.TipoEvento)
	m.TipoEvento=null
ELSE
	=SQL('Select Opcion from ev_Opciones where IdOpcion=?m.TipoEvento	and IdEmpresa=?oApp.Empresa','cOpcion')
ENDIF 	

TEXT TO cmdSQL NOSHOW 
SELECT     e.NroEvento, e.Fecha, e.Agasajado, e.IdCliente, e.TotalGeneral,  Excedentes,
                      e.ImporteBasico, CostoTotal, e.Iva, Utilidad, Estado, e.Sucursal + ' ' + s.Descripci�n as Sucursal,e.Sucursal as IdSucursal
FROM         dbo.ev_Eventos e left join sucursal s on e.IdEmpresa=s.IdEmpresa and e.Sucursal=s.Sucursal
                      where e.IdEmpresa=?oApp.Empresa and (e.IdOpcion=?m.TipoEvento or ?m.TipoEvento is null)
                      and e.Fecha between ?m.dFecha and ?m.hFecha 
                      and (e.Sucursal = ?m.Sucursal or ?m.Sucursal is null)
ORDER BY e.Fecha, e.NroEvento
ENDTEXT
=SQL(cmdSQL,'Eventos')
SELECT Eventos

ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     ����    �  �                        \   %   �      @               �  U  y %�C��  ��� � T��  ���� � %�C�� ���@ � T�� ���� �� �n ��C�W Select Opcion from ev_Opciones where IdOpcion=?m.TipoEvento	and IdEmpresa=?oApp.Empresa� cOpcion� �� �	 M(� ��] �W SELECT     e.NroEvento, e.Fecha, e.Agasajado, e.IdCliente, e.TotalGeneral,  Excedentes,�� ��                       e.ImporteBasico, CostoTotal, e.Iva, Utilidad, Estado, e.Sucursal + ' ' + s.Descripci�n as Sucursal,e.Sucursal as IdSucursal�m �g FROM         dbo.ev_Eventos e left join sucursal s on e.IdEmpresa=s.IdEmpresa and e.Sucursal=s.Sucursal�s �m                       where e.IdEmpresa=?oApp.Empresa and (e.IdOpcion=?m.TipoEvento or ?m.TipoEvento is null)�H �B                       and e.Fecha between ?m.dFecha and ?m.hFecha �Q �K                       and (e.Sucursal = ?m.Sucursal or ?m.Sucursal is null)�# � ORDER BY e.Fecha, e.NroEvento� � ��C � � Eventos� �� F� � U  SUCURSAL
 TIPOEVENTO SQL CMDSQL EVENTOS
  �  � U  SETEO Init,     �� BeforeOpenTables�    ��1 � A � � �A � �q	�1�1A �q 3 q 2                       �        �  �      )   �                  