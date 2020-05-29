*-----------------------------------------------------------------------------
* <Rating>-43</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AML.CHK.CASH.OTR(Y.TXN.ID.CASH.CHK,Y.READ,Y.OUT.UIF.TXN,Y.AMOUNT) 
*-----------------------------------------------------------------------------
** Company Name:  BANCO AZUL
* Developed By:   Abinanthan - Capgemini
* Date  :         2014/11/19
*------------------------------------------------
* Subroutine Type: API
* Attached to:
* Attached as:     Call routine used by other subroutines
* Primary Purpose: To Check Cash or Non-Cash
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------


    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.TELLER
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.ACCOUNT
    
    GOSUB INIT

    RETURN

INIT:
*****
;*Se agrega bandera para identificar el origen de la llamada a la rutina, esto se realiza debido a que en la rutina se agrega un IF donde verifica
;*Si la txn es parametrizada para mostrar o no alertas debido a un cambio que se solicito de Cumpliemto, si no se toma en cuenta dicha bandera la txn no aparece en el rpt acumulado.
   ;*DEBUG
    Y.TXN.ID.CASH.CHK = 'TT180088RYGR'
   
   Y.ORIGEN.CALL = FIELD(Y.TXN.ID.CASH.CHK,'-',2)
   
   Y.TXN.ID.CASH.CHK = FIELD(Y.TXN.ID.CASH.CHK,'-',1)


    APPL = 'TELLER.TRANSACTION':FM:'FT.TXN.TYPE.CONDITION'
    FLD.NAME = 'LF.AML.UIF.D.C':FM:'LF.AML.UIF.D.C'
    FLD.POS = ''
    CALL MULTI.GET.LOC.REF(APPL,FLD.NAME,FLD.POS)
    IF FLD.POS THEN
        Y.TT.LF.AML.UIF.D.C.POS = FLD.POS<1,1>
        Y.FT.LF.AML.UIF.D.C.POS = FLD.POS<2,1>
    END
    FN.SLV.AML.PROFILE.PARAMETER = 'F.SLV.AML.PROFILE.PARAMETER'
    R.SLV.AML.PROFILE.PARAMETER = '' ; ERR.SLV.AML = ''
    CALL CACHE.READ(FN.SLV.AML.PROFILE.PARAMETER,'SYSTEM',R.SLV.AML.PROFILE.PARAMETER, ERR.SLV.AML)
    IF R.SLV.AML.PROFILE.PARAMETER THEN
        Y.NON.CASH.COD = R.SLV.AML.PROFILE.PARAMETER<SLV.PROF.NON.CASH.TXN.CODE>
    END
    GOSUB OPEN.FILES
    IF Y.TXN.ID.CASH.CHK[1,2] EQ 'TT' THEN
        GOSUB TT.PROCESS
    END ELSE
        GOSUB FT.PROCESS
    END
    RETURN


TT.PROCESS:
***********
    IF Y.READ THEN
        R.TELLER = '' ; ERR.TT = '' ; NOC.POS = ''
        R.TELLER = '' ; TELLER.ERR = ''

        FN.TELLER$NAU='F.TELLER$NAU'
        F.TELLER$NAU=''
        Y.ERR = ''
        CALL OPF(FN.TELLER$NAU,F.TELLER$NAU)
        CALL F.READ(FN.TELLER$NAU,Y.TXN.ID.CASH.CHK,R.TELLER,F.TELLER$NAU,Y.ERR)
        IF NOT(R.TELLER) THEN
            CALL F.READ(FN.TELLER,Y.TXN.ID.CASH.CHK,R.TELLER,F.TELLER,TELLER.ERR)
            IF NOT(R.TELLER) THEN
                FN.TELLER$HIS='F.TELLER$HIS'
                F.TELLER$HIS=''
                Y.ERR = ''
                CALL OPF(FN.TELLER$HIS,F.TELLER$HIS)
                CALL EB.READ.HISTORY.REC(F.TELLER$HIS,Y.TXN.ID.CASH.CHK,R.TELLER,Y.ERR)
            END
        END
        IF R.TELLER THEN
            Y.TRANS.CD = R.TELLER<TT.TE.TRANSACTION.CODE>
            Y.AMOUNT = R.TELLER<TT.TE.NET.AMOUNT>
        END
    END ELSE
        Y.TRANS.CD = 10;*R.NEW(TT.TE.TRANSACTION.CODE)
        Y.AMOUNT =   11000;*R.NEW(TT.TE.NET.AMOUNT)
    END

*    END

    LOCATE Y.TRANS.CD IN Y.NON.CASH.COD<1,1> SETTING NOC.POS THEN
    	Y.TXN.ID.CASH.CHK = 'OTR'
    END ELSE
    	Y.TXN.ID.CASH.CHK = 'CASH'
    END
    R.TELLER.TRANSACTION = '' ; TELLER.TRANSACTION.ERR = ''
    FN.TELLER.TRANSACTION  = 'F.TELLER.TRANSACTION'
    CALL CACHE.READ(FN.TELLER.TRANSACTION,Y.TRANS.CD,R.TELLER.TRANSACTION,TELLER.TRANSACTION.ERR)
    IF R.TELLER.TRANSACTION THEN
        Y.OUT.UIF.TXN = R.TELLER.TRANSACTION<TT.TR.LOCAL.REF,Y.TT.LF.AML.UIF.D.C.POS>
    END
    RETURN


OPEN.FILES:
**********
    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER, F.TELLER)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
    
    FN.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.KEYS.PARAM = ''
    CALL OPF(FN.KEYS.PARAM, F.KEYS.PARAM)
    
    FN.ACC = 'F.ACCOUNT'
    F.ACC  = ''
    CALL OPF(FN.ACC, F.ACC)
    
    FN.SLV.AML.MOVS.HIST = 'F.SLV.AML.MOVS.HIST'
 	F.SLV.AML.MOVS.HIST = ''
 	CALL OPF(FN.SLV.AML.MOVS.HIST, F.SLV.AML.MOVS.HIST)
 	
    RETURN

FT.PROCESS:
************

    IF Y.READ THEN
        R.FUNDS.TRANSFER = '' ; ERR.TT = '' ; NOC.POS = ''
        R.FUNDS.TRANSFER = '' ; FUNDS.TRANSFER.ERR = ''
        Y.VERSION = 'FUNDS.TRANSFER,SLV.AML.VIEW'
        FN.FUNDS.TRANSFER$NAU='F.FUNDS.TRANSFER$NAU'
        F.FUNDS.TRANSFER$NAU=''
        Y.ERR = ''
        CALL OPF(FN.FUNDS.TRANSFER$NAU,F.FUNDS.TRANSFER$NAU)
        CALL F.READ(FN.FUNDS.TRANSFER$NAU,Y.TXN.ID.CASH.CHK,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER$NAU,Y.ERR)
        IF NOT(R.FUNDS.TRANSFER) THEN
            CALL F.READ(FN.FUNDS.TRANSFER,Y.TXN.ID.CASH.CHK,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FUNDS.TRANSFER.ERR)
            IF NOT(R.FUNDS.TRANSFER) THEN
                FN.FUNDS.TRANSFER$HIS='F.FUNDS.TRANSFER$HIS'
                F.FUNDS.TRANSFER$HIS=''
                Y.ERR = ''
                CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)
                CALL EB.READ.HISTORY.REC (F.FUNDS.TRANSFER$HIS,Y.TXN.ID.CASH.CHK,R.FUNDS.TRANSFER,Y.ERR)

            END
        END

        IF R.FUNDS.TRANSFER THEN
            Y.TRANS.CD = R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE>
            Y.AMOUNT = R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT>
            Y.ACC.CREDIT = R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>
            IF Y.AMOUNT EQ '' THEN
                Y.AMOUNT =  R.FUNDS.TRANSFER<FT.CREDIT.AMOUNT>
            END
        END
    END 
    ELSE
        Y.TRANS.CD =  R.NEW(FT.TRANSACTION.TYPE)
        Y.AMOUNT =    R.NEW(FT.DEBIT.AMOUNT)
        Y.ACC.CREDIT = R.NEW(FT.CREDIT.ACCT.NO)
	        IF Y.AMOUNT EQ '' THEN
	            Y.AMOUNT =  R.NEW(FT.CREDIT.AMOUNT)
	        END
    	END

    R.FT.TXN.TYPE.CONDITION  = '' ; FT.TXN.TYPE.CONDITION.ERR = ''
    FN.FT.TXN.TYPE.CONDITION   = 'F.FT.TXN.TYPE.CONDITION'
    CALL CACHE.READ(FN.FT.TXN.TYPE.CONDITION,Y.TRANS.CD,R.FT.TXN.TYPE.CONDITION,FT.TXN.TYPE.CONDITION.ERR)
    IF R.FT.TXN.TYPE.CONDITION  THEN
        Y.OUT.UIF.TXN = R.FT.TXN.TYPE.CONDITION<FT6.LOCAL.REF,Y.TT.LF.AML.UIF.D.C.POS>
    END

    ;*Y.TXN.ID.CASH.CHK = 'OTR'
    ;*
    CALL F.READ(FN.KEYS.PARAM, 'SLV.AML.UIF.OTR', RECORD.KEYS.PARAM, F.KEYS.PARAM, ERRO.KEYS.PARAM)
    CALL F.READ(FN.KEYS.PARAM, 'SLV.TRANSAC.NOT.UIF', RECORD.KP, F.KEYS.PARAM, ERRO.KP)
    
    ;* Se agrega validacion para que no tome en cuenta, cuando la FT sea por DAP, ya que se tomara la  TFS
    ;*-----------------------------------------------------------------------------------------------------
    IF Y.ORIGEN.CALL EQ '' THEN
	    FIND Y.TRANS.CD IN RECORD.KP SETTING NOC.POS.KP ELSE
		    FIND Y.TRANS.CD IN RECORD.KEYS.PARAM SETTING NOC.POS THEN
		    	Y.TXN.ID.CASH.CHK = 'OTR'
		    END ELSE
		    	Y.TXN.ID.CASH.CHK = 'CASH'
		    END
		END    
	END
	ELSE
	   FIND Y.TRANS.CD IN RECORD.KEYS.PARAM SETTING NOC.POS THEN
	   		Y.TXN.ID.CASH.CHK = 'OTR'
	   END ELSE
       		Y.TXN.ID.CASH.CHK = 'CASH'
	   END
	END
    RETURN
    END
