*-----------------------------------------------------------------------------
* <Rating>-22</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.BLOQUEO.CJ
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_AA.LOCAL.COMMON
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.TELLER
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS
RETURN

	INIT:
	    FN.AA.ACCOUNT		= 'FBNK.AA.ACCOUNT'
	    F.AA.ACCOUNT		= ''
	    CALL OPF(FN.AA.ACCOUNT, F.AA.ACCOUNT)

		FN.ARRANGEMENT		= 'FBNK.AA.ARRANGEMENT'
	    F.ARRANGEMENT 		= ''
	    CALL OPF(FN.ARRANGEMENT, F.ARRANGEMENT)

		FN.FT 				= 'FBNK.FUNDS.TRANSFER'
		F.FT 				= ''
		CALL OPF(FN.FT, F.FT)
	
		FN.TELLER 			= 'FBNK.TELLER'
		F.TELLER 				= ''
		CALL OPF(FN.TELLER, F.TELLER)
		
		FN.TFS 				= 'FBNK.TELLER.FINANCIAL.SERVICES'
		F.TFS 				= ''
		CALL OPF(FN.TFS, F.TFS)    		
		
		Y.R.ACCOUNT = ''
		Y.ESTADO = ''
		Y.DAP.INDEX = ''
		ARR.ID = ''
	    NO.CUENTA=''
		STRERR = ''
		
		EQU FT  TO  'FUNDS.TRANSFER'
		EQU TT  TO  'TELLER'
		EQU TFS TO  'TELLER.FINANCIAL.SERVICES'
		
	RETURN
	
	PROCESS:
	    ID.APPL     = APPLICATION:PGM.VERSION
		ID.VERSION  = FIELD(ID.APPL, ',', 1)
		
	    BEGIN CASE
	    	CASE ID.VERSION EQ FT
				NO.CUENTA=R.NEW(FT.CREDIT.ACCT.NO)
				
	    	CASE ID.VERSION EQ TT
				NO.CUENTA=R.NEW(TT.TE.ACCOUNT.1)
				
	    	CASE ID.VERSION  EQ TFS
	    		CALL GET.LOC.REF(TFS, 'LF.TFS.LOAN.REF', POS)
	    		NO.CUENTA=R.NEW(TFS.LOCAL.REF)<1, POS>
	    		;*Retorna el ARRANGEMENT
	    END CASE
		
		;*buscar el arrangement por el numero de cuenta
		IF SUBSTRINGS(NO.CUENTA,1,2) NE 'AA' THEN 
			CALL SLV.UTIL.GET.ARR.X.ACC(NO.CUENTA)
		END 
	    ARR.ID = NO.CUENTA
		        	    	
		CALL F.READ(FN.ARRANGEMENT, ARR.ID, R.ARR, F.ARRANGEMENT, Y.ARRANGEMENT.ERR2)
		P.LINE=R.ARR<AA.ARR.PRODUCT.LINE>

		IF R.ARR<AA.ARR.PRODUCT.LINE> EQ 'LENDING' THEN
			CALL GET.LOC.REF('AA.PRD.DES.ACCOUNT','LF.LOAN.STATUS',Y.DAP.INDEX)
		    CALL AA.GET.ARRANGEMENT.CONDITIONS(ARR.ID,'ACCOUNT','ACCOUNT',TODAY,ACCOUNT.ID,R.ACCOUNT,ACCOUNT.ERROR)
		    
			Y.R.ACCOUNT = RAISE(R.ACCOUNT)
			Y.ESTADO = Y.R.ACCOUNT<AA.AC.LOCAL.REF><1,Y.DAP.INDEX>

			IF Y.ESTADO EQ 5 THEN
		    	;*ETEXT = "ST-SLV.PAGO.COB.JUD"
		        ;*CALL STORE.END.ERROR
		        E='ST-SLV.PAGO.COB.JUD'
		        CALL ERR
			END	
		END
	RETURN
END

