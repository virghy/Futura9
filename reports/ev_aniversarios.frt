  N                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=\\futura5\HP DeskJet 840C/841C/842C/843C
OUTPUT=USB001
ORIENTATION=1
PAPERSIZE=1
ASCII=0
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=2
YRESOLUTION=300
TTOPTION=1
COLLATE=0
      T  <  winspool  \\futura5\HP DeskJet 840C/841C/842C/843C  USB001                       `\\futura5\HP DeskJet 840C/841C   � XC�  �4d   ,  ,   A4                                                                              DINU"   4$ WVK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $   �$               $   �$                                                                                                                     Arial      Arial      Arial      Arial      Arial      Arial      "Control de Aniversarios"      Arial      empresa             Arial      
"Periodo:"      Arial      m.dFecha,m.hFecha      Arial      	"Fecha
"      Arial      "Nro"      Arial      "Cumplea�ero"      Arial      "Edad"      Arial      "Cliente/Conyuge"      Arial      "Direccion/Telefono/fax"      Arial      "Email"      Arial      Fecha      Arial      	NroEvento      Arial      	Agasajado      Arial      Edad      Arial      RazSocial,"-"+NomContacto      Arial      (Direccion," - T. " + telefono," F."+ fax      Arial      email      Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total"      Arial      fecha      Arial      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
PROCEDURE Init

TEXT TO cmdSQL NOSHOW 
SELECT     e.Fecha, e.NroEvento, e.Agasajado, e.Edad, c.RazSocial, c.NomContacto, c.Direccion, c.Telefono, c.Fax, c.Email
FROM         dbo.ev_Eventos e INNER JOIN
                      dbo.vt_clientes c ON e.IdEmpresa = c.IdEmpresa AND e.IdCliente = c.IdCliente
           where fecha between ?m.dFecha and ?m.hFecha           
ORDER BY e.Fecha
ENDTEXT
=SQL(cmdSQL,'Eventos')
SELECT Eventos

ENDPROC
     ����    �  �                        ��   %   �      >     $          �  U  
  �  � U  SETEO�	 M(�  �� �y SELECT     e.Fecha, e.NroEvento, e.Agasajado, e.Edad, c.RazSocial, c.NomContacto, c.Direccion, c.Telefono, c.Fax, c.Email�. �( FROM         dbo.ev_Eventos e INNER JOIN�h �b                       dbo.vt_clientes c ON e.IdEmpresa = c.IdEmpresa AND e.IdCliente = c.IdCliente�G �A            where fecha between ?m.dFecha and ?m.hFecha           � � ORDER BY e.Fecha� � ��C �  � Eventos� �� F� � U  CMDSQL SQL EVENTOS BeforeOpenTables,     �� InitA     ��1 q 3 � ���qaA �q 2                       &         A   �      )   �                  