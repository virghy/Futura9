DEFINE CLASS SocketWrapper As Custom
  * Based on Anatoliy Mogylevets SMTP code at http://fox.wikis.com/wc.dll?Wiki~SendSmtpEmail
  #DEFINE SMTP_PORT    25
  #DEFINE http_PORT    80
  #DEFINE AF_INET       2
  #DEFINE SOCK_STREAM   1
  #DEFINE IPPROTO_TCP   6
  #DEFINE SOCKET_ERROR -1
  #DEFINE FD_READ       1
  
    host     = ""
    IP       = ""
    Port     = 80
    hSocket  = 0
    cIn      = ''
    WaitForRead = 0

  PROCEDURE Init()
      THIS.decl
    IF WSAStartup(0x202, Repli(Chr(0),512)) <> 0
    * unable to initialize Winsock on this computer
        RETURN .F.
    ENDIF
    RETURN .T.
  ENDPROC

  PROCEDURE Destroy
    = WSACleanup()
  ENDPROC
  
  PROCEDURE Host_Assign( vNewVal )
    if empty(vNewVal)
      THIS.IP = ''
    else
      THIS.IP = THIS.GetIP(vNewVal)
    endif
    if not empty(THIS.IP)
      THIS.Host = vNewVal
    else
      THIS.Host = ''
    endif
  ENDPROC

  PROTECTED FUNCTION GetIP( pcHost )
  #DEFINE HOSTENT_SIZE 16
      LOCAL nStruct, nSize, cBuffer, nAddr, cIP
      nStruct = gethostbyname(pcHost)
      IF nStruct = 0
          RETURN ""
      ENDIF
      cBuffer = Repli(Chr(0), HOSTENT_SIZE)
      cIP = Repli(Chr(0), 4)
      = CopyMemory(@cBuffer, nStruct, HOSTENT_SIZE)
      = CopyMemory(@cIP, THIS.buf2dword(SUBS(cBuffer,13,4)),4)
      = CopyMemory(@cIP, THIS.buf2dword(cIP),4)
  RETURN inet_ntoa(THIS.buf2dword(cIP))
  ENDFUNC

  PROTECTED FUNCTION Connect
    LOCAL cBuffer, cPort, cHost, lResult
    THIS.hSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    IF THIS.hSocket = SOCKET_ERROR
        RETURN .F.
    ENDIF
    
    cPort = THIS.num2word(htons(THIS.Port))
    nHost = inet_addr(THIS.IP)
    cHost = THIS.num2dword(nHost)
    cBuffer = THIS.num2word(AF_INET) + cPort + cHost + Repli(Chr(0),8)
    lResult = (ws_connect(THIS.hSocket, @cBuffer, Len(cBuffer))=0)
  RETURN lResult

  FUNCTION httpGet( pcServer, pcUrl )
    LOCAL lResult
    THIS.Host = pcServer
    IF THIS.Connect()
        THIS.snd("GET "+pcURL+" http/1.0"+crlf)
        THIS.snd("Accept: */*"+crlf)
        THIS.snd("Accept-Language: en-us"+crlf)
        THIS.snd("Accept-Encoding: gzip, deflate"+crlf)
        THIS.snd("User-Agent: Mozilla/4.0"+crlf)
        THIS.snd("Host: "+pcServer+crlf)
        THIS.snd("Pragma: no-cache"+crlf)
        THIS.snd(crlf,.t.) && End of headers
        *info=url encoded string
        lResult = .T.
    ELSE
        lResult = .F.
    ENDIF
    THIS.Disconnect()
  ENDFUNC
  
  FUNCTION httpPost( pcServer, pcUrl, pcData, poFdbk )
    LOCAL lResult, lnLen, lnComplete, lcRemain, lcSnd
    THIS.Host = pcServer
    IF THIS.Connect()
        THIS.snd("POST "+pcURL+" http/1.0"+crlf)
        THIS.snd("Content-Type:application/x-www-form-urlencoded"+crlf)
        THIS.snd("Accept: */*"+crlf)
        THIS.snd("Accept-Language: en-us"+crlf)
        THIS.snd("Accept-Encoding: gzip, deflate"+crlf)
        THIS.snd("User-Agent: Mozilla/4.0"+crlf)
        THIS.snd("Host: "+pcServer+crlf)
        lnLen = len(pcData)
        THIS.snd("Content-Length: "+tran(lnLen)+crlf)
        THIS.snd("Pragma: no-cache"+crlf)
        THIS.snd(crlf) && End of headers
        * If we have a valid feedback object, provide feedback
        if vartype(poFdbk)='O' and PEMStatus(poFdbk,'Feedback',5) ;
           and upper(PEMStatus(poFdbk,'Feedback',3))='METHOD'
          lcRemain   = pcData
          lnComplete = 0
          poFdbk.Feedback( 0 )
          do while len(lcRemain)>0
            lcSnd      = LEFT( lcRemain, 100 )
            lcRemain   = SUBSTR( lcRemain, 101 )
            THIS.snd(lcSnd)
            lnComplete = lnComplete+len(lcSnd)
            poFdbk.Feedback( lnComplete/lnLen*100 )
          enddo
          THIS.snd('',.t.) && get a response, too.
        else && no feedback object, just send one big chunk.
          THIS.snd(pcData,.t.) && get a response, too.
        endif
        lResult = .T.
    ELSE
        lResult = .F.
    ENDIF
    THIS.Disconnect()
  ENDFUNC
  
  FUNCTION SendMail( pcSender, pcRecipient, pcSubject, pcBody )
    LOCAL lResult
    IF THIS.Connect()
        THIS.snd("HELO", .T.)
        THIS.snd("MAIL FROM:<" + pcSender + ">", .T.)
        THIS.snd("RCPT TO:<" + pcRecipient + ">", .T.)
        THIS.snd("DATA", .T.)
        THIS.snd("From: " + pcSender)
        THIS.snd("To: " + pcRecipient)
        THIS.snd("Subject: " + pcSubject)
        THIS.snd("")
        THIS.snd(pcBody)
        THIS.snd(".", .T.)
        THIS.snd("QUIT", .T.)
        lResult = .T.
    ELSE
        = MessageB("Unable to connect to [" + THIS.Host +;
            "] on port " + LTRIM(STR(SMTP_PORT)) + ". ",;
            48, " Connection error")
        lResult = .F.
    ENDIF
    THIS.Disconnect()
  RETURN lResult
  ENDFUNC

  Function URLencode
  LPARAMETER pcInStr
  *  ' encode Percent signs
  *  '        Double Quotes
  *  '        CarriageReturn / LineFeeds
  
  LOCAL lcOut, lnI
    * StrTran is WAY faster than building the string in memory
    lcOut = StrTran(pcInStr, [%], '%25' )
    lcOut = StrTran(lcOut,   [+], '%2B' )
    lcOut = StrTran(lcOut,   [ ], '+'   )
    for lnI = 0 to 31
      lcOut = StrTran( lcOut, chr(lnI), '%' + Right( Transform(lnI,'@0'), 2 ) )
    endfor
    for lnI = 127 to 255
      lcOut = StrTran( lcOut, chr(lnI), '%' + Right( Transform(lnI,'@0'), 2 ) )
    endfor

    RETURN lcOut
  
*!*  LOCAL lcIn, lcOut, lnI, lnCh
*!*        lcIn = StrTran(pcInStr, [%], '%25' )
*!*        lcIn = StrTran(lcIn,    [+], '%2B' )
*!*        lcIn = StrTran(lcIn,    [ ], '+'   )
*!*        lcIn = StrTran(lcIn,    ["], '%22' )
*!*        lcIn = StrTran(lcIn,    [,], '%2C' )
*!*        lcIn = StrTran(lcIn,    ['], '%27' )
*!*        lcIn = StrTran(lcIn,    [=], '%3D' )
*!*        lcIn = StrTran(lcIn,    [&], '%26' )
*!*        lcIn = StrTran(lcIn,    [`], '%60' )
*!*        lcOut = ''
*!*        for lnI = 1 to len(lcIn)
*!*          lcCh = Substr(lcIn,lnI,1)
*!*          lnCh = Asc(lcCh)
*!*          if not between( lnCh, 33, 126 )
*!*            lcCh = '%' + Right( Transform(lnCh,'@0'), 2 )
*!*          endif
*!*          lcOut = lcOut + lcCh
*!*        endfor
*!*        RETURN lcOut
  ENDFUNC && UrlEncode
  
  FUNCTION Disconnect
    if THIS.hSocket<>SOCKET_ERROR
      = closesocket(THIS.hSocket)
    endif
    THIS.hSocket = SOCKET_ERROR
  ENDFUNC

  FUNCTION snd(cData, lResponse)
    LOCAL cBuffer, nResult, cResponse
    cBuffer = cData && + CrLf
    nResult = send(THIS.hSocket, @cBuffer, Len(cBuffer), 0)
    IF nResult = SOCKET_ERROR
        RETURN .F.
    ENDIF
    IF Not lResponse
        RETURN .T.
    ENDIF

    LOCAL hEventRead, nWait, cRead
    DO WHILE .T.
        * creating event, linking it to the socket and wait
        hEventRead = WSACreateEvent()
        = WSAEventSelect(THIS.hSocket, hEventRead, FD_READ)

        * 1000 milliseconds can be not enough
        THIS.WaitForRead = WSAWaitForMultipleEvents(1, @hEventRead, 0, 2000, 0)
        = WSACloseEvent(hEventRead)

        IF THIS.WaitForRead <> 0 && error or timeout
            EXIT
        ENDIF
        
        * reading data from connected socket
        THIS.cIn = THIS.cIn+THIS.Rd()
    ENDDO
  RETURN .T.
  ENDFUNC

  PROTECTED FUNCTION Rd
  #DEFINE READ_SIZE 16384
    LOCAL cRecv, nRecv, nFlags
    cRecv = Repli(Chr(0), READ_SIZE)
    nFlags = 0
    nRecv = recv(THIS.hSocket, @cRecv, READ_SIZE, nFlags)
    RETURN Iif(nRecv<=0, "", LEFT(cRecv, nRecv))
  ENDFUNC

  PROCEDURE decl
    DECLARE INTEGER gethostbyname IN ws2_32 STRING host
    DECLARE STRING inet_ntoa IN ws2_32 INTEGER in_addr
    DECLARE INTEGER socket IN ws2_32 INTEGER af, INTEGER tp, INTEGER pt
    DECLARE INTEGER closesocket IN ws2_32 INTEGER s
    DECLARE INTEGER WSACreateEvent IN ws2_32
    DECLARE INTEGER WSACloseEvent IN ws2_32 INTEGER hEvent
    DECLARE GetSystemTime IN kernel32 STRING @lpSystemTime
    DECLARE INTEGER inet_addr IN ws2_32 STRING cp
    DECLARE INTEGER htons IN ws2_32 INTEGER hostshort
    DECLARE INTEGER WSAStartup IN ws2_32 INTEGER wVerRq, STRING lpWSAData
    DECLARE INTEGER WSACleanup IN ws2_32

    DECLARE INTEGER connect IN ws2_32 AS ws_connect ;
        INTEGER s, STRING @sname, INTEGER namelen

    DECLARE INTEGER send IN ws2_32;
        INTEGER s, STRING @buf, INTEGER buflen, INTEGER flags

    DECLARE INTEGER recv IN ws2_32;
        INTEGER s, STRING @buf, INTEGER buflen, INTEGER flags

    DECLARE INTEGER WSAEventSelect IN ws2_32;
        INTEGER s, INTEGER hEventObject, INTEGER lNetworkEvents

    DECLARE INTEGER WSAWaitForMultipleEvents IN ws2_32;
        INTEGER cEvents, INTEGER @lphEvents, INTEGER fWaitAll,;
        INTEGER dwTimeout, INTEGER fAlertable

    DECLARE RtlMoveMemory IN kernel32 As CopyMemory;
        STRING @Dest, INTEGER Src, INTEGER nLength
  ENDPROC

  FUNCTION buf2dword(lcBuffer)
    RETURN Asc(SUBSTR(lcBuffer, 1,1)) + ;
        BitLShift(Asc(SUBSTR(lcBuffer, 2,1)), 8) +;
        BitLShift(Asc(SUBSTR(lcBuffer, 3,1)), 16) +;
        BitLShift(Asc(SUBSTR(lcBuffer, 4,1)), 24)
  ENDFUNC
  
  FUNCTION num2dword(lnValue)
  #DEFINE m0 256
  #DEFINE m1 65536
  #DEFINE m2 16777216
      IF lnValue < 0
          lnValue = 0x100000000 + lnValue
      ENDIF
      LOCAL b0, b1, b2, b3
      b3 = Int(lnValue/m2)
      b2 = Int((lnValue - b3*m2)/m1)
      b1 = Int((lnValue - b3*m2 - b2*m1)/m0)
      b0 = Mod(lnValue, m0)
  RETURN Chr(b0)+Chr(b1)+Chr(b2)+Chr(b3)
  ENDFUNC
  
  FUNCTION num2word(lnValue)
    RETURN Chr(MOD(m.lnValue,256)) + CHR(INT(m.lnValue/256))
  ENDFUNC
ENDDEFINE