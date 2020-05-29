*-----------------------------------------------------------------------------
* <Rating>184</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.GET.TXNGROUP(TXN.CODE,APPLICATION.ID,DEB.CRED,GROUP.CODE)
*-----------------------------------------------------------------------------
* Company Name:  BANCO AZUL
* Developed By:  Praveen - Capgemini
* Date  :        2014/07/21
*------------------------------------------------
* Subroutine Type: API
* Attached to:     N/A.
* Attached as:     Call routine used by other subroutines
* Primary Purpose: Given a transaction code to obtain the transaction associated with the same groups
*-----------------------------------------------------------------------------
* Modification History :
* CYepez		20150312	PACS00444852		 Consideration for deposit with cheque propio. In this case deb and cred side are customer accounts
*eurias 		20160415	tratamiento para reversiones realizadas en FT, no se eliminaba el acumulativo
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.TXN.GROUPTXN.CNT
    $INSERT I_F.SLV.AML.TRANSACTION.GROUP 
    $INSERT I_F.TELLER
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER 
    $INSERT I_F.FUNDS.TRANSFER

    GOSUB INITIALIZE
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------------------------------------------
INITIALIZE:
***********
* Necessary files are opened and initialized

    FN.SLV.AML.TRANSACTION.GROUP = 'F.SLV.AML.TRANSACTION.GROUP'
    F.SLV.AML.TRANSACTION.GROUP = ''
    CALL OPF(FN.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP)

    FN.SLV.TXN.GROUPTXN.CNT = 'F.SLV.TXN.GROUPTXN.CNT'
    F.SLV.TXN.GROUPTXN.CNT = ''
    CALL OPF(FN.SLV.TXN.GROUPTXN.CNT,F.SLV.TXN.GROUPTXN.CNT)

    RETURN
*-----------------------------------------------------------------------------------------------------------------
PROCESS:
********
* Check the application and update the Id in the corresponding tables


    BEGIN CASE

        CASE APPLICATION.ID EQ 'FT'
            Y.ID = 'FT.':TXN.CODE
            GOSUB GROUPTXN.READ
            GOSUB FT.CHECK

        CASE APPLICATION.ID EQ 'TT'
            Y.ID = 'TT.':TXN.CODE
            GOSUB GROUPTXN.READ
            GOSUB TT.CHECK

        CASE APPLICATION.ID NE 'FT' AND APPLICATION.ID NE 'TT'
            Y.ID = TXN.CODE
            GOSUB GROUPTXN.READ
            GOSUB CHECK.APPL

    END CASE
    RETURN

*--------------------------------------------------------------------------------------------------------------
GROUPTXN.READ:
**************
    CALL F.READ(FN.SLV.TXN.GROUPTXN.CNT,Y.ID,R.SLV.TXN.GROUPTXN.CNT,F.SLV.TXN.GROUPTXN.CNT,ERR.SLV.TXN.GROUPTXN.CNT)
    IF  R.SLV.TXN.GROUPTXN.CNT THEN
        Y.TRANS.GRP.ID = R.SLV.TXN.GROUPTXN.CNT
    END
    RETURN
*--------------------------------------------------------------------------------------------------------------------------------
FT.CHECK:
********
    Y.TRANS.GRP.ID.CNT = DCOUNT(Y.TRANS.GRP.ID,FM)
    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.TRANS.GRP.ID.CNT
        Y.GRP.ID = Y.TRANS.GRP.ID<Y.CNT>
        CALL F.READ(FN.SLV.AML.TRANSACTION.GROUP,Y.GRP.ID,R.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP,ERR.SLV.AML.TRANSACTION.GROUP)
        IF R.SLV.AML.TRANSACTION.GROUP THEN
            Y.FT.TXN.ID = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.FT.TXN.ID>
            Y.DEB.CRED = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.FT.DEB.CRED.IND>
        END

        LOCATE TXN.CODE IN Y.FT.TXN.ID<1,1> SETTING Y.POS THEN
        Y.DEBIT.CREDIT = Y.DEB.CRED<1,Y.POS>
        IF DEB.CRED EQ Y.DEBIT.CREDIT THEN
            GROUP.CODE<-1> = Y.GRP.ID 
        END ELSE
        ;*eurias 20160415 tratamiento para ft en reversas delete a los movs para no acumular alerta
        ;*cuanto es reversa el valor se muestra inverso a la operacion si es debito a la cuenta lo representa como credito en la reversion
        ;*se valida si la operacion es de reversas rnau o r acumular el grupo para seguir la evaluacion
        Y.REC.STATUSR.NEW = ''
        Y.REC.STATUSR.NEW = R.NEW(FT.RECORD.STATUS)
          IF V$FUNCTION EQ 'R' OR Y.REC.STATUSR.NEW EQ 'RNAU' THEN
        	 GROUP.CODE<-1> = Y.GRP.ID
          END
       END
    END

    Y.CNT++
    REPEAT
    RETURN
*--------------------------------------------------------------------------------------------------------------
TT.CHECK:
********

    Y.TRANS.GRP.ID.CNT = DCOUNT(Y.TRANS.GRP.ID,FM)
    Y.COUNT = 1
    LOOP
    WHILE Y.COUNT LE Y.TRANS.GRP.ID.CNT
        Y.GRP.ID = Y.TRANS.GRP.ID<Y.COUNT>
        CALL F.READ(FN.SLV.AML.TRANSACTION.GROUP,Y.GRP.ID,R.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP,ERR.SLV.AML.TRANSACTION.GROUP)
        IF R.SLV.AML.TRANSACTION.GROUP THEN
            Y.TELLER.TXN.ID = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.TELLER.TXN.ID>
        END
        LOCATE TXN.CODE IN Y.TELLER.TXN.ID<1,1> SETTING Y.POS THEN
        *PACS00444852
        *GROUP.CODE<-1> = Y.GRP.ID

        Y.ACCT.1 = 'USD1404600010401';*R.NEW(TT.TE.ACCOUNT.1)
        Y.ACCT.2 = 'USD1403000010001';*R.NEW(TT.TE.ACCOUNT.2)
        CALL INT.ACC(Y.ACCT.1,Y.INT.FLG.1)
        CALL INT.ACC(Y.ACCT.2,Y.INT.FLG.2)

        IF Y.INT.FLG.1 OR Y.INT.FLG.2 THEN
            GROUP.CODE<-1> = Y.GRP.ID
        END ELSE
            PARAM.ERR = ''
            FN.SLV.AML.PROFILE.PARAMETER = 'F.SLV.AML.PROFILE.PARAMETER'
            R.SLV.AML.PROFILE.PARAMETER = ''
            CALL CACHE.READ(FN.SLV.AML.PROFILE.PARAMETER,'SYSTEM',R.SLV.AML.PROFILE.PARAMETER,PARAM.ERR)
            IF R.SLV.AML.PROFILE.PARAMETER THEN
                DEPO.POS = '' ; RETO.POS = ''
                Y.DEPOSITO = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.DEPOSITO.GROUP.ID>
                Y.RETIRO = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.RETIRO.GROUP.ID>
                CHANGE VM TO FM IN Y.DEPOSITO
                CHANGE VM TO FM IN Y.RETIRO
                LOCATE Y.GRP.ID IN Y.RETIRO SETTING RETO.POS THEN
                Y.TYP.OPER = 'DEBITO'
                END
                LOCATE Y.GRP.ID IN Y.DEPOSITO SETTING DEPO.POS THEN
                Y.TYP.OPER = 'CREDITO'
                END
                IF DEB.CRED EQ Y.TYP.OPER THEN
                    GROUP.CODE<-1> = Y.GRP.ID
                END
            END
        END

    END
    Y.COUNT++
    REPEAT
    RETURN
*-----------------------------------------------------------------------------------------------------------------
CHECK.APPL:
**********
    Y.TRANS.GRP.ID.CNT = DCOUNT(Y.TRANS.GRP.ID,FM)
    Y.COUNT.ID = 1
    LOOP
    WHILE Y.COUNT.ID LE Y.TRANS.GRP.ID.CNT
        Y.GRP.ID = Y.TRANS.GRP.ID<Y.COUNT.ID>
        CALL F.READ(FN.SLV.AML.TRANSACTION.GROUP,Y.GRP.ID,R.SLV.AML.TRANSACTION.GROUP,F.SLV.AML.TRANSACTION.GROUP,ERR.SLV.AML.TRANSACTION.GROUP)
        IF R.SLV.AML.TRANSACTION.GROUP THEN
            Y.TXN.ID = R.SLV.AML.TRANSACTION.GROUP<SLV.TRANS.TXN.ID>
        END
        LOCATE TXN.CODE IN Y.TXN.ID<1,1> SETTING Y.POS THEN
        GROUP.CODE<-1> = Y.GRP.ID
    END
    Y.COUNT.ID++
    REPEAT
    RETURN
    END
