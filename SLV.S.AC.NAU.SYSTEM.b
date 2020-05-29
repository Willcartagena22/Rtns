*-----------------------------------------------------------------------------
* <Rating>199</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.AC.NAU.SYSTEM(YFILE,YTYPE,YENTRYID,YENTRY,YOVER)
*-----------------------------------------------------------------------------
* Company Name:    BANCO AZUL
* Developed By:    Praveen - Capgemini
* Date  :          2014/08/13
*------------------------------------------------
* Subroutine Type:  N/A
* Attached to:      N/A
* Attached as:      N/A
* Primary Purpose:  It processes the ACCT.NAU.SBRTN present in SLV.ACCOUNT.PARAMETER table.
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.ACCOUNT.PARAMETER

    GOSUB INITIALISE
    GOSUB READ.SYSTEM.RECORD
    GOSUB PROCESS

    RETURN
*-----------------------------------------------------------------------------
INITIALISE:
**********

    FN.SLV.ACCOUNT.PARAMETER = 'F.SLV.ACCOUNT.PARAMETER'
    F.SLV.ACCOUNT.PARAMETER = ''
    CALL OPF(FN.SLV.ACCOUNT.PARAMETER,F.SLV.ACCOUNT.PARAMETER)

    Y.SLV.ACC.ID  = 'SYSTEM'

    RETURN
*--------------------------------------------------------------------------------------------------------
READ.SYSTEM.RECORD:
*****************

    CALL CACHE.READ(FN.SLV.ACCOUNT.PARAMETER,Y.SLV.ACC.ID,R.SLV.ACCOUNT.PARAMETER,ERR.SLV.ACCOUNT.PARAMETER)

    RETURN
*---------------------------------------------------------------------------------------------------------
PROCESS:
*******

    Y.ACCOUNT.SUBRTN = R.SLV.ACCOUNT.PARAMETER<SLV.ACC.ACCT.NAU.SBRTN>
    Y.CNT.SUBRTN = DCOUNT(Y.ACCOUNT.SUBRTN,VM)

	TEMP.E = E
	
    Y.VERIFICATION.ACC.SUBRTN = R.SLV.ACCOUNT.PARAMETER<SLV.ACC.VERIFICATION.NAU>
    IF Y.CNT.SUBRTN GT 0 THEN
        Y.ERRROR.ROUTINE.LIST = ''
        Y.ERROR.FLAG = 'FALSE'
        Y.CNT = 1
        LOOP
        WHILE Y.CNT LE Y.CNT.SUBRTN

            Y.ROUTINE.NAME = Y.ACCOUNT.SUBRTN<1,Y.CNT>

            LOCATE Y.ROUTINE.NAME IN Y.ERRROR.ROUTINE.LIST<1> SETTING V.POS THEN

        	END ELSE

	            Y.VERIFICATION.AUX = Y.VERIFICATION.ACC.SUBRTN<1,Y.CNT>
	
	            GOSUB VALIDATE.VERIFICATION
	
	            IF Y.ERROR.FLAG EQ 'FALSE' THEN
	
	                E = ''
	                IF Y.ROUTINE.NAME EQ '' THEN
	                    RETURN
	                END
	                IF Y.ROUTINE.NAME EQ 'SLV.S.AML.ACCT.NAU.SUBRTN' THEN
	                    CALL @Y.ROUTINE.NAME(YFILE,YTYPE,YENTRYID,YENTRY,YOVER)
	                END ELSE
	                    CALL @Y.ROUTINE.NAME(YFILE,YTYPE,YENTRYID,YENTRY,YOVER)
	                END
	                IF E THEN
	                    Y.ERRROR.ROUTINE.LIST = Y.ROUTINE.NAME:FM:Y.ERRROR.ROUTINE.LIST
	                    Y.ERROR.FLAG = 'TRUE'
	                END
	
	            END
	        END

        Y.CNT++
    	REPEAT
    END
* Not advisable to play with E variable. so Copied the old value and replaced it.
	E = TEMP.E
	
    RETURN
*----------------------------------------------------------------------------------------------------------
VALIDATE.VERIFICATION:
*********************

    Y.COUNT.VERIF.AUX = DCOUNT(Y.VERIFICATION.AUX,SM)

    IF Y.COUNT.VERIF.AUX GT 0 THEN
        Y.S.IDX = 1
        LOOP

        WHILE Y.S.IDX LE Y.COUNT.VERIF.AUX

            Y.VERIF.ROUTINE = Y.VERIFICATION.ACC.SUBRTN<1,Y.CNT,Y.S.IDX>

            LOCATE Y.VERIF.ROUTINE IN Y.ERRROR.ROUTINE.LIST<1>  SETTING Y.POS THEN
            Y.ERROR.FLAG = 'TRUE'
        END

        Y.S.IDX++
    REPEAT

    END

    RETURN
*--------------------------------------------------------------------------------------------------------------
STORE.ERROR:
************

    AF = V.AF
    AV = V.AV
    AS = V.AS
    ETEXT = V.ERROR.CODE:FM:V.ERROR.VALUE

    CALL STORE.END.ERROR

    RETURN
    END

