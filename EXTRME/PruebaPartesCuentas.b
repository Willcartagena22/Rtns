*-----------------------------------------------------------------------------
* <Rating>468</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PruebaPartesCuentas
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.COMPANY
    $INSERT I_F.EB.SLV.COMISIONES.TARJETAS
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PREPROCESS
    GOSUB CALL.J



    RETURN

INIT:
    TEXTO.ARCHIVO = 'ENTRA A INIT'
    GOSUB ESCRIBIR.ARCHIVO

    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    FN_SLVPA 		= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA 		= ''
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''



    STR.ERR = "/-1/"
    ID.PARAM.OFS ='OFS.COM.VISA.DEB'
    TRANS.ID = ''
    CURRENCY = 'USD'
    R.FT = ''
    CANT.REQ =''
    ID.FT=''
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
    STR.ERR = "/-1/"
    CURRENCY = 'USD'
    DATA.EXTRME=''
    PARAMETRO='ACC.VISA.DEB.FISC'

    RETURN

OPENFILE:
    CALL OPF(FN.FT,F.FT)
    CALL OPF(FN.SCT,F.SCT)
    CALL OPF(FN.ACC,F.ACC)
    RETURN


CALL.J:

;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
    THIS.METHOD.CLT= "getDataExtreme"
    CALLJ.ARGUMENTS.CLT = "CobroComisiones"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    DATA.EXTRME = CALLJ.RESPONSE.CLT

;*DATA.EXTRME ="1~10000000151503~USD1406600030001~1.15~AC~REV LIMITE CLASICA~2017-03-09 15:26:51~2017-03-09 15:26:51~A~2017-03-09 15:26:51~C~0|"

    Y.COUNT.FILE = DCOUNT(DATA.EXTRME,'|')-1

    FOR I=1 TO Y.COUNT.FILE
        YBLOQUE.1 = FIELD(DATA.EXTRME,'|',I)
        Y.CAMPOS.B1 = CHANGE(YBLOQUE.1,'~',VM)

        IDOFS = FIELD( Y.CAMPOS.B1,VM,1)
        IDOFS= CHANGE((IDOFS),'"','')
        CUENTADEBITO  = FIELD( Y.CAMPOS.B1,VM,2)
        CUENTACREDITO = FIELD( Y.CAMPOS.B1,VM,3)
        MONTO = FIELD( Y.CAMPOS.B1,VM,4)
        TIPOTRANSACTION = FIELD( Y.CAMPOS.B1,VM,5)
        ORDENBANCO = FIELD( Y.CAMPOS.B1,VM,6)
        FECHACREDITO = FIELD( Y.CAMPOS.B1,VM,7)
        FECHADEBITO = FIELD( Y.CAMPOS.B1,VM,8)
        ESTADO = FIELD( Y.CAMPOS.B1,VM,9)
        FECHACOB = FIELD( Y.CAMPOS.B1,VM,10)
        TIPOTARJETA = FIELD( Y.CAMPOS.B1,VM,11)
        IVA= CHANGE((IVA),'"','')
        IVA = FIELD( Y.CAMPOS.B1,VM,12)

        GOSUB PROCESS
    NEXT I

PREPROCESS:

*** <desc>Barrido de FT pendientes </desc>
    STMT.SCT = "SELECT ":FN.SCT:" STATUS EQ '0' AND CARD.TYPE EQ 'D'"
;*Aplicando el Statement
    CALL EB.READLIST(STMT.SCT, SCT.LIST,'',NO.OF.SCT,SCT.ERR)
    FOR I=1 TO NO.OF.SCT


        CUENTADEBITOPRE=AR.SCT<EB.SLV16.ACC.DEBITO>
        VAL=LEFT(CUENTADEBITOPRE,2)

        IF VAL NE 'US' AND VAL NE 'PL' THEN

            ;*Validando saldo

            CALL F.READ(FN.ACC, CUENTADEBITOPRE,RECORD.ACC,F.ACC,ERR.ACC)
            WORKING.BALANCE = RECORD.ACC<AC.WORKING.BALANCE>


            MONTOPRE=AR.SCT<EB.SLV16.AMOUNT>
            MONTOCONIVA=MONTOPRE*1.13
            MONTOCONIVA=DROUND(MONTOCONIVA,2)

            IF WORKING.BALANCE GT MONTOCONIVA  THEN
                R.FT = ''
                CALL F.READ(FN.SCT,SCT.LIST<I>,AR.SCT,F.SCT,ERR.SCT)
                R.FT<FT.DEBIT.ACCT.NO>    =  AR.SCT<EB.SLV16.ACC.DEBITO>
                R.FT<FT.CREDIT.ACCT.NO>   =  AR.SCT<EB.SLV16.ACC.CREDITO>
                R.FT<FT.DEBIT.CURRENCY>   =  'USD'
                R.FT<FT.DEBIT.AMOUNT>     =  AR.SCT<EB.SLV16.AMOUNT>
                R.FT<FT.ORDERING.BANK>    =  AR.SCT<EB.SLV16.ORDERING.BANK>
                R.FT<FT.TRANSACTION.TYPE> =  AR.SCT<EB.SLV16.TYPE.TRANSACTION>

                CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
                R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = SCT.LIST<I>

                CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
                FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
                    AR.SCT<EB.SLV16.STATUS>='0'
                    CALL F.WRITE (FN.SCT,SCT.LIST<I>,AR.SCT)
                END
                ELSE
                AR.SCT<EB.SLV16.STATUS>='1'
                AR.SCT<EB.SLV16.ID.OFS> =	FIELD(FIELD(Y.OUT,'<request>', 2),'//1',1)

                ;*Campos Auditoria
                AR.SCT<EB.SLV16.INPUTTER> = OPERATOR
                AR.SCT<EB.SLV16.AUTHORISER>= OPERATOR
                AR.SCT<EB.SLV16.CURR.NO> += 1
                X = OCONV(DATE(),"D-")
                V$TIMEDATE = TIMEDATE()
                V$TIMEDATE = V$TIMEDATE[1,5]
                CONVERT ":" TO "" IN V$TIMEDATE
                X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
                AR.SCT<EB.SLV16.DATE.TIME>		   = X
                AR.SCT<EB.SLV16.RECORD.STATUS>	   = 'AUTH'
                CALL F.WRITE (FN.SCT,SCT.LIST<I>,AR.SCT)
                CALL JOURNAL.UPDATE(FN.SCT)
                
                GOSUB IVAPROCESS

            END

        END



    END



    NEXT I



    RETURN


PROCESS:
    VAL=LEFT(CUENTADEBITO,2)

    IF VAL NE 'US' AND VAL NE 'PL' THEN
        ;*Validando saldo
        CALL F.READ(FN.ACC, CUENTADEBITO,RECORD.ACC,F.ACC,ERR.ACC)
        WORKING.BALANCE = RECORD.ACC<AC.WORKING.BALANCE>


        MONTOCONIVA=MONTO*1.13

        IF WORKING.BALANCE EQ '' THEN
            WORKING.BALANCE = '0.00'
        END

        IF WORKING.BALANCE GT MONTOCONIVA  THEN

            FT=''
            CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)
            R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO
            R.FT<FT.CREDIT.ACCT.NO>   =  CUENTACREDITO
            R.FT<FT.DEBIT.CURRENCY>   =  CURRENCY
            R.FT<FT.DEBIT.AMOUNT>     =  MONTO
            R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
            R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION
            ;*Enviando campo local que se utilizara como identificador
            CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
            R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = IDOFS
            ;*Llamada de rutina para aplicar OFS
            CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)

            FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
                Y.MSG=FIELD(FIELD(Y.OUT,'/-1/', 2),'</request>',1)
                R.SCT.STATUS='0'
                FT=''
                GOSUB WRIT
            END
            ELSE
            STMT.FT = "SELECT ":FN.FT:" LF.IDOFS.CARD EQ '":IDOFS:"'"

            CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)

        

            FT=FIELD(FT.LIST,FM,1)


            R.SCT.STATUS='1'
            GOSUB WRIT
            GOSUB IVAPROCESS

            END
        TEXTO.ARCHIVO = 'PROCESS'
        GOSUB ESCRIBIR.ARCHIVO


        END ;*IF WORKING.BALANCE GT MONTOCONIVA
        
	    ELSE
	    R.SCT.STATUS='0'
	    GOSUB WRIT

        END
        
    END

    ELSE

    FT=''
    CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)
    R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO
    R.FT<FT.CREDIT.ACCT.NO>   =  CUENTACREDITO
    R.FT<FT.DEBIT.CURRENCY>   =  CURRENCY
    R.FT<FT.DEBIT.AMOUNT>     =  MONTO
    R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
    R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION
;*Enviando campo local que se utilizara como identificador
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
    R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = IDOFS
;*Llamada de rutina para aplicar OFS
    CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)

    FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
        Y.MSG=FIELD(FIELD(Y.OUT,'/-1/', 2),'</request>',1)
        R.SCT.STATUS='0'
        FT=''
        GOSUB WRIT
    END
    ELSE
    STMT.FT = "SELECT ":FN.FT:" LF.IDOFS.CARD EQ '":IDOFS:"'"

    CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)

;*---------------------------------------------------ESCRIBIR UN FOR PARA RECORRER LAS ID DE LAS FT

    FT=FIELD(FT.LIST,FM,1)


    R.SCT.STATUS='1'
    GOSUB WRIT

    END



    END


    RETURN
*** </region>

IVAPROCESS:

    CALL F.READ(FN_SLVPA, PARAMETRO, R.TABLE.PA, F_SLVPA, F.ERR.PA)
    CUENTAIVA = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
    MONTOIVA=MONTO*0.13
    MONTOIVA=DROUND(MONTOIVA,2)

    FT=''
    CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)
    R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO
    R.FT<FT.CREDIT.ACCT.NO>   =  CUENTAIVA
    R.FT<FT.DEBIT.CURRENCY>   =  CURRENCY
    R.FT<FT.DEBIT.AMOUNT>     =  MONTOIVA
    R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
    R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION
;*Enviando campo local que se utilizara como identificador
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
    R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = IDOFS
;*Llamada de rutina para aplicar OFS
    CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)

    FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
        Y.MSG=FIELD(FIELD(Y.OUT,'/-1/', 2),'</request>',1)
        R.SCT.STATUS='0'
        FT=''
        
    END
    ELSE
    STMT.FT = "SELECT ":FN.FT:" LF.IDOFS.CARD EQ '":IDOFS:"'"

    CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)

;*---------------------------------------------------ESCRIBIR UN FOR PARA RECORRER LAS ID DE LAS FT

    FT=FIELD(FT.LIST,FM,1)


    R.SCT.STATUS='1'
    

    END
    TEXTO.ARCHIVO = 'IVAPROCESS'
    GOSUB ESCRIBIR.ARCHIVO




    RETURN

*** <region name= WRIT>
WRIT:
*** <desc>Persistir en la App Local </desc>
    R.SCT<EB.SLV16.ID.OFS> =FT
    R.SCT<EB.SLV16.ACC.DEBITO> = CUENTADEBITO
    R.SCT<EB.SLV16.ACC.CREDITO> = CUENTACREDITO
    R.SCT<EB.SLV16.AMOUNT>=MONTO
    R.SCT<EB.SLV16.TYPE.TRANSACTION> = TIPOTRANSACTION
    R.SCT<EB.SLV16.ORDERING.BANK> =  ORDENBANCO
    R.SCT<EB.SLV16.CREDIT.DATE> = TODAY
    R.SCT<EB.SLV16.DEBIT.DATE> = TODAY
    R.SCT<EB.SLV16.COB.DATE> =TODAY
    R.SCT<EB.SLV16.CARD.TYPE> ='D'
    R.SCT <EB.SLV16.IVA> = '0'
    R.SCT <EB.SLV16.ERROR> = Y.MSG
    R.SCT<EB.SLV16.STATUS>=R.SCT.STATUS

;*Campos Auditoria
    R.SCT<EB.SLV16.INPUTTER> = OPERATOR
    R.SCT<EB.SLV16.AUTHORISER>= OPERATOR
    R.SCT<EB.SLV16.CURR.NO> += 1
    X = OCONV(DATE(),"D-")
    V$TIMEDATE = TIMEDATE()
    V$TIMEDATE = V$TIMEDATE[1,5]
    CONVERT ":" TO "" IN V$TIMEDATE
    X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
    R.SCT<EB.SLV16.DATE.TIME>		   = X
    R.SCT<EB.SLV16.RECORD.STATUS>	   = 'AUTH'

;*Escribir Nuevo Registro
;*-----------------------
    CALL F.WRITE (FN.SCT,IDOFS, R.SCT)
    CALL JOURNAL.UPDATE(FN.SCT)

    RETURN

    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'VisaDebLogs'
    R.ID   = 'SLV.V.VISA.COM.TRAN ':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN


    END
