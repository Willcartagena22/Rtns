*-----------------------------------------------------------------------------
* <Rating>-66</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.ACCT.STMT.PDF.GEN(TEMP.ARRAY)
*-----------------------------------------------------------------------------
*
*------------------------------------------------------------------------------
* Modification History :
* Id                 Date                By            Description
*20150729N      29/07/2015            Nishant           Changes in the Spool file as per the CORR0362-Account_Statement
*                                                      and Mapping provided in Mail.
*20150825N      25/08/2015            Nishant          Addtion of the Debit count and amount in header
*                                                      Addition of the credit count and amount in header
*------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.CATEGORY
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_SLV.E.ACCT.COMMON
    $INSERT I_GTS.COMMON 
*-----------------------------------------------------------------------------
    GOSUB INIT ; *
    IF TEMP.ARRAY THEN 
        Y.PGE.CNT = 1
        Y.TEMP.BAL = Y.OPEN.BAL
        CHANGE '~' TO FM IN TEMP.ARRAY
        Y.TEMP.OPEN.BAL = FMT(Y.OPEN.BAL,"R2,")
        Y.SALD.INCIO.DESC = "Saldo al inicio del Periodo"
        TEMP.DAT.ARR = "MOVIMIEN!":'':'!':'':'!':'!':Y.SALD.INCIO.DESC:"!":'':'!':''
        TEMP.DAT.ARR:='!':'':'!':'':'!':Y.TEMP.OPEN.BAL
        GOSUB GET.LINES ; *
        Y.TEMP.SALD.FIN.DESC = "Saldo al Final del Periodo"
        Y.TEMP.SALD.FIN.BAL = Y.TEMP.BAL
        Y.TEMP.SALD.FIN.BAL = FMT(Y.TEMP.BAL,"R2,")
        TEMP.DAT.ARR:=FM:"MOVIMIEN!":'':'!':'':'!':'!':Y.TEMP.SALD.FIN.DESC:"!":'':'!':''
        TEMP.DAT.ARR:='!':'':'!':'':'!':Y.TEMP.SALD.FIN.BAL
        TEMP.DAT.ARR:=FM:"%%EOF"

        GOSUB FORM.HEADER

        Y.OPERATOR = OPERATOR
        Y.WINDOW.NAME = OFS$WINDOW.NAME
        Y.WINDOW.NAME = Y.WINDOW.NAME[8,LEN(Y.WINDOW.NAME )]
        Y.ID.TXT = "AcctStmt_":FMT(Y.WINDOW.NAME,"R%12"):"_":Y.OPERATOR:".txt";*eurias20150928 formato para generacion de link por erro en 0 a la izquierda
        Y.ID.PDF = "AcctStmt_":FMT(Y.WINDOW.NAME,"R%12"):"_":Y.OPERATOR:".pdf"

        *        TEMP.DAT.ARR = Y.HEAD.ARR:FM:TEMP.DAT.ARR
        OPEN FN.OUTPATH TO F.OUTPATH ELSE
            ABORT 201, "No Directory ":F.OUTPATH:" Found"
        END

        WRITE Y.FIN.ARR ON F.OUTPATH, Y.ID.TXT ON ERROR
            ABORT 201, "Not able to write on ":F.OUTPATH
        END

    END

    RETURN

*-----------------------------------------------------------------------------

*** <region name= FORM.HEADER>
FORM.HEADER:
*** <desc> </desc>

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    FN.CATEGORY = 'F.CATEGORY'
    F.CATEGORY = ''
    CALL OPF(FN.CATEGORY,F.CATEGORY)
    IF Y.CUSTOMER THEN
        R.CUSTOMER = '' ; CUSTOMER.ERR = ''
        CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)

        IF R.CUSTOMER THEN
            Y.CUST.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME>
            Y.LF.COLONIA = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.LF.COLONIA.POS>
            Y.LF.CALLE = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.LF.CALLE.POS>
            Y.LF.AVENIDA = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.LF.AVENIDA.POS>
        END
    END
    IF Y.CATEGORY THEN
        R.CATEGORY = '' ; CATEGORY.ERR = ''
        CALL F.READ(FN.CATEGORY,Y.CATEGORY,R.CATEGORY,F.CATEGORY,CATEGORY.ERR)
        IF R.CATEGORY THEN
            Y.CAT.DESC = R.CATEGORY<EB.CAT.DESCRIPTION>
        END
    END


    Y.HEAD.ARR.INIT = "%!"
    Y.HEAD.ARR.INIT:=FM:"(azul_estado_cuenta.jdt) STARTLM"
    Y.HEAD.ARR ="CUENTA00!":ACCT.NO
    Y.HEAD.ARR:=FM:"CLIENTE0!":Y.CUSTOMER:"!":Y.CUST.NAME
*20150729N - S
*    Y.HEAD.ARR:=FM: "DIRECCIO!":Y.LF.COLONIA:" ":Y.LF.CALLE:" ":Y.LF.AVENIDA
*20150729N - E
    Y.HEAD.ARR:=FM:"MONEDA00!":Y.CURRENCY
    Y.HEAD.ARR:=FM:"PRODUCTO!":Y.CAT.DESC
    Y.HEAD.ARR:=FM:"FECHAINI!":OCONV(ICONV(DATE.FROM,"D4"),"D4/E")
    Y.HEAD.ARR:=FM:"FECHAFIN!":OCONV(ICONV(DATE.TO,"D4"),"D4/E")
    Y.HEAD.ARR:=FM:"FECHAEMI!":OCONV(ICONV(TODAY,"D4"),"D4/E")
    Y.HEAD.ARR:=FM:"HORAMEMI!":FIELD(TEMP.ARRAY,"*",19,1)

    Y.HEAD.ARR:=FM:"TOTALDEB!":Y.TEMP.TOT.DR.AMT
    Y.HEAD.ARR:=FM:"TOTALCRE!":Y.TEMP.TOT.CR.AMT
    Y.HEAD.ARR:=FM:"CONTADEB!":Y.TEMP.DR.CNT
    Y.HEAD.ARR:=FM:"CONTACRE!":Y.TEMP.CR.CNT

    Y.TEMP.DAT.CNT = DCOUNT(TEMP.DAT.ARR,FM)
    Y.RUNN.CNT = 2
    Y.PGE.CNT = 1

    Y.FIN.ARR = Y.HEAD.ARR.INIT:FM:Y.HEAD.ARR:FM:"PAGINAS0!":Y.PGE.CNT

    LOOP
        REMOVE Y.DET.VAL FROM TEMP.DAT.ARR SETTING Y.POS.DET.POS
    WHILE Y.DET.VAL:Y.POS.DET.POS
        IF Y.RUNN.CNT LE '30' THEN
            Y.FIN.ARR:=FM:Y.DET.VAL
        END ELSE
            Y.PGE.CNT += 1
            Y.FIN.ARR:=FM:Y.HEAD.ARR:FM:"PAGINAS0!":Y.PGE.CNT:FM:Y.DET.VAL
            Y.RUNN.CNT = 2
        END
        Y.RUNN.CNT++
    REPEAT

    RETURN

*-----------------------------------------------------------------------------

*** <region name= FORM.BODY>
FORM.BODY:
*** <desc> </desc>

    Y.TEMP.DEBIT.DISP = '' ; Y.TEMP.CREDIT.DISP = '' ;  Y.TEMP.BAL.DISP = ''
    Y.CNT.VM.DESC = ''
    Y.CNT.DESC.INT = '1'
    Y.CNT.VM.DESC = DCOUNT(Y.TEMP.DESC,VM)

    IF Y.TEMP.DEBIT THEN
        Y.TEMP.DEBIT.DISP = FMT(Y.TEMP.DEBIT,"R2,")
    END
    IF Y.TEMP.CREDIT THEN
        Y.TEMP.CREDIT.DISP = FMT(Y.TEMP.CREDIT,"R2,")
    END
    Y.TEMP.BAL.DISP = FMT(Y.TEMP.BAL,"R2,")

    IF TEMP.DAT.ARR EQ '' THEN
        TEMP.DAT.ARR:="MOVIMIEN!":Y.TEMP.BOOK.DATE:'!':Y.TEMP.TRAN.REF:'!':Y.TEMP.CHQ.NUM:'!':FIELD(Y.TEMP.DESC,VM,1,1):'!':Y.TEMP.VAL.DATE:'!':Y.TEMP.REC.STAT
        *20150729N -S
        TEMP.DAT.ARR:='!':Y.TEMP.DEBIT.DISP:'!':Y.TEMP.CREDIT.DISP:'!':Y.TEMP.BAL.DISP
        *20150729N -E
    END ELSE
        TEMP.DAT.ARR:=FM:"MOVIMIEN!":Y.TEMP.BOOK.DATE:'!':Y.TEMP.TRAN.REF:'!':Y.TEMP.CHQ.NUM:'!':FIELD(Y.TEMP.DESC,VM,1,1):'!':Y.TEMP.VAL.DATE:'!':Y.TEMP.REC.STAT
        *20150729N -S
        TEMP.DAT.ARR:='!':Y.TEMP.DEBIT.DISP:'!':Y.TEMP.CREDIT.DISP:'!':Y.TEMP.BAL.DISP
        *20150729N -E
    END

    IF Y.CNT.VM.DESC GT '1' THEN
        Y.CNT.DESC.INT = Y.CNT.DESC.INT + 1
        LOOP
        WHILE Y.CNT.DESC.INT LE Y.CNT.VM.DESC
            TEMP.DAT.ARR:=FM:"MOVIMIEN!":"":'!':"":'!':"":'!':FIELD(Y.TEMP.DESC,VM,Y.CNT.DESC.INT,1):'!':"":'!':"":'!':"":'!':"":'!':""
            Y.CNT.DESC.INT = Y.CNT.DESC.INT + 1
        REPEAT
    END

    RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= INIT>
INIT:
*** <desc> </desc>

    FN.EB.SLV.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.EB.SLV.GLOBAL.PARAM = ''
    CALL OPF(FN.EB.SLV.GLOBAL.PARAM,F.EB.SLV.GLOBAL.PARAM)
;* Ruta donde se guarda el archivo
    RUTA.SPOOL.ID = 'RUTA.CONTRATO.SPOOL'
    R.EB.SLV.GLOBAL.PARAM = '' ; ERR.PA = ''
    Y.OUTPATH = ''
    CALL F.READ(FN.EB.SLV.GLOBAL.PARAM, RUTA.SPOOL.ID, R.EB.SLV.GLOBAL.PARAM, F.EB.SLV.GLOBAL.PARAM, ERR.PA)
    Y.OUTPATH = R.EB.SLV.GLOBAL.PARAM<EB.SLV39.VALOR.PARAM>

    FN.OUTPATH = Y.OUTPATH
    F.OUTPATH = ""

    Y.APPLI = "CUSTOMER"
    Y.FIELD = "LF.COLONIA":VM:"LF.CALLE":VM:"LF.AVENIDA"
    Y.POS = ""
    CALL MULTI.GET.LOC.REF(Y.APPLI,Y.FIELD,Y.POS.VAL)
    Y.LF.COLONIA.POS = Y.POS.VAL<1,1>
    Y.LF.CALLE.POS = Y.POS.VAL<1,2>
    Y.LF.AVENIDA.POS = Y.POS.VAL<1,3>

    Y.TEMP.DR.CNT = '0'
    Y.TEMP.CR.CNT = '0'
    Y.TEMP.TOT.DR.AMT = '0'
    Y.TEMP.TOT.CR.AMT = '0'

    RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= GET.LINES>
GET.LINES:
*** <desc> </desc>
    CHANGE VM TO "###" IN TEMP.ARRAY
    LOOP
        REMOVE Y.CURR.ARR FROM TEMP.ARRAY SETTING CUR.POS
    WHILE Y.CURR.ARR : CUR.POS
        CHANGE "###" TO VM IN Y.CURR.ARR
        Y.TEMP.TRAN.REF = ''
        Y.TEMP.DESC = ''
        Y.TEMP.VAL.DATE = ''
        Y.TEMP.REC.STAT = ''
        Y.TEMP.LCY = ''
        Y.TEMP.LCY = ''
        Y.TEMP.DEBIT = ''
        Y.TEMP.CREDIT = ''
        Y.TEMP.CHQ.NUM = ''

        Y.TEMP.BOOK.DATE = FIELD(Y.CURR.ARR,'*',11)
        IF Y.TEMP.BOOK.DATE THEN
            Y.TEMP.TRAN.REF = FIELD(Y.CURR.ARR,'*',12)
            Y.TEMP.DESC = FIELD(Y.CURR.ARR,'*',8)
            Y.TEMP.VAL.DATE = FIELD(Y.CURR.ARR,'*',9)
            Y.TEMP.REC.STAT = FIELD(Y.CURR.ARR,'*',18)
            Y.TEMP.LCY = FIELD(Y.CURR.ARR,'*',6)
            *20150729N -S
            Y.TEMP.CHQ.NUM = FIELD(Y.CURR.ARR,'*',20)
            *20150729N -E
            IF NOT(Y.TEMP.LCY) THEN
                Y.TEMP.LCY = FIELD(Y.CURR.ARR,'*',7)
            END
            IF Y.TEMP.LCY[1,1] EQ '-' THEN
                Y.TEMP.DEBIT = Y.TEMP.LCY[2,99]
                Y.TEMP.DR.CNT +=1
                Y.TEMP.TOT.DR.AMT += Y.TEMP.DEBIT
            END ELSE
                Y.TEMP.CREDIT = Y.TEMP.LCY
                Y.TEMP.CR.CNT +=1
                Y.TEMP.TOT.CR.AMT += Y.TEMP.CREDIT
            END
            Y.TEMP.BAL + = Y.TEMP.LCY
            GOSUB FORM.BODY ; *
        END
    REPEAT

    RETURN
*** </region>
*-----------------------------------------------------------------------------

    END

