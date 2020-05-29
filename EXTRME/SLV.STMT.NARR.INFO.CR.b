*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.STMT.NARR.INFO.CR(APPLIC.ID,APPLIC.REC,STMT.ID,STMT.REC,OUT.TEXT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
************************* Actualizaciones **************************** 
* Version 1.0 	2017.01.06	RCortes 		Adicion del subnarrative para MH NPE
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

    GOSUB INIT ; *
    GOSUB PROCESS ; *

*-----------------------------------------------------------------------------

INIT:

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

	FN.PRM 	= 'F.EB.SLV.GLOBAL.PARAM'
    F.PRM 	= ''
    CALL OPF(FN.PRM, F.PRM)

	;* Para obtener la cta. transitoria de MH NPE
	Y.PRM.ID = 'ACC.NPE.MH'
	
    RETURN


PROCESS:
    
    ;* *****************
    ;* APPLIC.ID	= 'FT15216WN7NX'
    ;* *****************

    ID.FT = FIELD(APPLIC.ID, ";", 1)
    
    GOSUB READ.FT ; *

    CALL F.READ(FN.PRM, Y.PRM.ID, R.PRM, F.PRM, ERR.PRM)
    IF R.PRM THEN
        DEB.ACCT = R.FT<FT.DEBIT.ACCT.NO>
    	Y.CTA.MH = R.PRM<EB.SLV39.VALOR.PARAM>
    	
    	IF DEB.ACCT EQ Y.CTA.MH THEN
    		OUT.TEXT = 'NPE' 
    	END
    	
	END
	
    RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= READ.FT>
READ.FT:
*** <desc> </desc>
R.FT = ''
ERR.FT = ''
Y.ERR= ''
 CALL F.READ(FN.FUNDS.TRANSFER,ID.FT,R.FT,F.FUNDS.TRANSFER,ERR.FT)
    IF NOT(R.FT) THEN
        FN.FUNDS.TRANSFER$HIS='F.FUNDS.TRANSFER$HIS'
        F.FUNDS.TRANSFER$HIS=''
        Y.ERR = ''
        CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)
        CALL EB.READ.HISTORY.REC (F.FUNDS.TRANSFER$HIS,ID.FT,R.FT,Y.ERR)
    END
    
RETURN
*** </region>

END





