
SET SYSMENU OFF
SET CONSOLE OFF
IF TYPE('oApp') = 'O'
     DO principal.mpr
     lcmenu = ALLTRIM(oapp.sysmenu) +  ;
              ".mpr"
     DO (lcmenu)
     RETURN
ENDIF
DECLARE INTEGER  ;
        GetPrivateProfileString  ;
        IN Win32API AS GetPrivStr  ;
        STRING, STRING, STRING,  ;
        STRING @, INTEGER,  ;
        STRING
DECLARE INTEGER  ;
        WritePrivateProfileString  ;
        IN Win32API AS  ;
        WritePrivStr STRING,  ;
        STRING, STRING, STRING
DECLARE INTEGER RegOpenKeyEx IN  ;
        Win32API INTEGER, STRING  ;
        @, INTEGER, INTEGER,  ;
        INTEGER @
DECLARE INTEGER RegQueryValueEx  ;
        IN Win32API INTEGER,  ;
        STRING, INTEGER, INTEGER  ;
        @, STRING @, INTEGER @
DECLARE INTEGER RegCloseKey IN  ;
        Win32API INTEGER
DECLARE INTEGER GetProfileString  ;
        IN Win32API AS GetProStr  ;
        STRING, STRING, STRING,  ;
        STRING @, INTEGER
        
DECLARE INTEGER ShellExecute IN shell32.dll ; 
INTEGER hndWin, ; 
STRING cAction, ; 
STRING cFileName, ; 
STRING cParams, ; 
STRING cDir, ; 
INTEGER nShowWin
      
        
CLEAR
IF SET('TALK') = 'ON'
     SET TALK OFF
     PUBLIC gcoldtalk
     gcoldtalk = 'ON'
ELSE
     PUBLIC gcoldtalk
     gcoldtalk = 'OFF'
ENDIF
PUBLIC gcolddir, gcoldpath,  ;
       gcoldclasslib, gcoldescape,  ;
       gttrade, gcsistema
gcoldescape = SET('ESCAPE')
gcolddir = FULLPATH(CURDIR())
gcoldpath = SET('PATH')
gcoldclasslib = SET('CLASSLIB')
gttrade = .T.
gcsistema = ''
IF setpath()
     IF TYPE('oApp') = "O"
          gcsistema = oapp.sistema
          IF oapp.cleanup()
               oapp.cleanup2
               CLEAR EVENTS
          ENDIF
     ENDIF
     DO CASE
     	  	
          CASE SYSMETRIC(1) = 800
               _SCREEN.picture = "BITMAPS\FONDO800.JPG"
          CASE SYSMETRIC(1) =  ;
               1024
               _SCREEN.picture = "BITMAPS\FONDO1024.JPG"
          OTHERWISE
               _SCREEN.picture = "BITMAPS\TAPIZ.JPG"
     ENDCASE
     IF FILE("FONDO.JPG")
     	_SCREEN.picture = "FONDO.JPG"
     ENDIF
     	
     	
     
     _SCREEN.icon = "BITMAPS\default.ico"
     PUBLIC oapp

     oapp = CREATEOBJECT("Tastrade")
     IF TYPE('oApp') = "O"
          RELEASE gcolddir,  ;
                  gcoldpath,  ;
                  gcoldclasslib,  ;
                  gcoldtalk,  ;
                  gcoldescape
          oapp.do()
     ENDIF
ENDIF
CLEAR DLLS
RELEASE ALL EXTENDED
CLEAR ALL
IF WEXIST("Administrador de Proyectos")
     ACTIVATE WINDOW  "Administrador de Proyectos"
ENDIF
ENDPROC
*
PROCEDURE SetPath
LOCAL lcsys16, lcprogram
lcsys16 = SYS(16)
lcprogram = SUBSTR(lcsys16,AT(":", lcsys16) - 1)
CD LEFT(lcprogram, RAT("\", lcprogram))
IF RIGHT(lcprogram, 3) = "FXP"
     CD ..
ENDIF

*SET PATH TO PROGS, FORMS, LIBS,  MENUS, data, OTHER,  REPORTS, INCLUDE, HELP, BITMAPS, \\venus\sistemas\futura9\data
IF _VFP.StartMode=0
 
     SET PATH TO PROGS, FORMS, LIBS,  MENUS, data, OTHER,  REPORTS, INCLUDE, HELP, BITMAPS, GDIX &&, \\tierra2\sistemas\futura9\data
         SET CLASSLIB TO MAIN, TSGEN
ELSE

     SET PATH TO PROGS, FORMS, LIBS,  MENUS, data, OTHER,  REPORTS, INCLUDE, HELP, BITMAPS
         SET CLASSLIB TO MAIN, TSGEN
ENDIF
ENDPROC
