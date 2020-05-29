*-----------------------------------------------------------------------------
* <Rating>-63</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.STMT.NARR.INFO(APPLIC.ID,APPLIC.REC,STMT.ID,STMT.REC,OUT.TEXT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*@date 20161215
*@autho eurias
*@util rutina para obtener la descripcion del colector pagado
************************* Actualizaciones **************************** 
* Version 1.1 	2017.01.06	RCortes 		Adicion del subnarrative para MH (NPE & PAGOES) 
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
;*    $INSERT I_F.EB.SLV.STMT.NARR.PARAM
    $INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_F.EB.SLV.COL.FRONT.END 
*-----------------------------------------------------------------------------

    GOSUB INIT ; *
    GOSUB PROCESS ; *

*-----------------------------------------------------------------------------

*** <region name= INIT>
INIT:
*** <desc> </desc>
*FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
*F.FUNDS.TRANSFER = ''
*	Y.NARR = 'SLV.STMT.NARR'
*    FN.EB.SLV.STMT.NARR.PARAM = 'F.EB.SLV.STMT.NARR.PARAM'
*    F.EB.SLV.STMT.NARR.PARAM = ''
*    CALL OPF(FN.EB.SLV.STMT.NARR.PARAM,F.EB.SLV.STMT.NARR.PARAM)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)


    FN.EB.SLV.COLECTOR = 'F.EB.SLV.COLECTOR'
    F.EB.SLV.COLECTOR = ''
    CALL OPF(FN.EB.SLV.COLECTOR,F.EB.SLV.COLECTOR)

	FN.EB.SLV.COL.FE	= 'F.EB.SLV.COL.FRONT.END'
	F.EB.SLV.COL.FE		= ''
	CALL OPF(FN.EB.SLV.COL.FE, F.EB.SLV.COL.FE)


	APPL.FT = 'FUNDS.TRANSFER'
	FIELD.FT = 'LF.COD.CL':VM:'LF.ID.COL.MOD'
	POS.FT = ''
	CALL MULTI.GET.LOC.REF(APPL.FT,FIELD.FT,POS.FT)
	POS.FT.1 = POS.FT<1,1>
	POS.FT.2 = POS.FT<1,2>
	
	
;*GOSUB READ.FILES
    RETURN


PROCESS:
    ID.FT = FIELD(APPLIC.ID, ";", 1) 
    GOSUB READ.FT ; * 
    ;*CALL GET.LOC.REF('FUNDS.TRANSFER','LF.COD.CL',POS.SRC)
    COD.CAT = R.FT<FT.LOCAL.REF,POS.FT.1>
    
    IF COD.CAT EQ '' THEN;*si el campo es vacio viene de modulo CCS central colector system, leo de campo colectores CCS
    	 COD.CAT 		= R.FT<FT.LOCAL.REF,POS.FT.2>
    	 COL.SUB.NARR 	= COD.CAT ;* RCORTES 2016.01.06 - Adiciona la referencia para el sub narrative
    	 COD.CAT 		= 	FIELD(COD.CAT,'-',1)
    END
    
    CALL F.READ(FN.EB.SLV.COLECTOR,COD.CAT,R.EB.SLV.COLECTOR,F.EB.SLV.COLECTOR,ERR.EB.SLV.COLECTOR)
    IF R.EB.SLV.COLECTOR THEN
    	OUT.TEXT = R.EB.SLV.COLECTOR<EB.CL.NOMBRE.COLECTOR>
	END
	
	;* RCORTES 2016.01.06 - Adiciona la referencia para el sub narrative
	IF COL.SUB.NARR THEN
		CALL F.READ(FN.EB.SLV.COL.FE, COL.SUB.NARR, R.COL.FE, F.EB.SLV.COL.FE, ERR.COL.FE)
		IF R.COL.FE THEN
			OUT.TEXT = OUT.TEXT : ' ' :  R.COL.FE<EB.SLV8.RESERVADO.16>
		END
	END
	
    RETURN
*** </region>
**-----------------------------------------------------------------------------
*** <region name= PROCESS>
*PROCESS:
**** <desc> </desc>
*
*    R.FT = APPLIC.REC
*    Y.TRANSACTION  = R.FT<FT.TRANSACTION.TYPE>
*    R.TRANSACC = R.NARR<EB.SLV73.APP.FT.TT>
*    Y.APLICION.SRC = R.NARR<EB.SLV73.APLICACION.SRC>
*    Y.APLICION.CAT = R.NARR<EB.SLV73.APLICACION.CAT>
*    CONT = 1
*    LOOP
*    	REMOVE ID.TRX FROM R.TRANSACC SETTING DELIM
*    WHILE ID.TRX NE ''
*     IF ID.TRX EQ Y.TRANSACTION THEN
*     	CAMPO.SRC = R.NARR<EB.SLV73.CAMPO.SRC,CONT>
*     	CAMPO.CATA = R.NARR<EB.SLV73.CAMPO.CATA,CONT>
*     	CALL GET.LOC.REF(Y.APLICION.SRC,CAMPO.SRC,POS.SRC)
*     	;*CALL GET.STANDARD.SELECTION.DETS('EB.SLV.COLECTOR',SS.REC)
*     	CALL GET.MULTI.LOCAL.APP.REF(Y.APLICION.CAT,CAMPO.CATA,POSITIONS)
*     	POS.SRC
*     	POSITIONS = POSITIONS<1,1>
*
*     	ID.APP = APPLIC.ID
*     	CALL F.READ(FN.EB.SLV.COLECTOR,,R.EB.SLV.COLECTOR,F.EB.SLV.COLECTOR,ERR.EB.SLV.COLECTOR)
*
*
*     	CALL F.READ(FN.EB.SLV.COLECTOR,,R.EB.SLV.COLECTOR,F.EB.SLV.COLECTOR,ERR.EB.SLV.COLECTOR)
*       	OUT.TEXT = R.EB.SLV.COLECTOR<EB.CL.NOMBRE.COLECTOR>
*
*     END
*
*    CONT++
*    REPEAT
*
*    RETURN
**** </region>

**** <region name= READ.FILES>
*READ.FILES:
**** <desc> </desc>
*    CALL F.READ(FN.EB.SLV.STMT.NARR.PARAM,Y.NARR,R.NARR,F.EB.SLV.STMT.NARR.PARAM,ERROR)
*    RETURN
**** </region>

 
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





