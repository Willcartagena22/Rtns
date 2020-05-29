*-----------------------------------------------------------------------------
* <Rating>-34</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.DUI.CUST.FTC
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.SLV.DUI.CUSTOMER.CNT
$INSERT I_F.EB.SLV.CUSTOMER.PLUS
$INSERT I_AA.LOCAL.COMMON
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
	FN.D.CUST  	= 'F.SLV.DUI.CUSTOMER.CNT'
	F.D.CUST    = ''
	CALL OPF(FN.D.CUST, F.D.CUST)
	DIR.NAME    = 'SIC'
	NAME.FILE  = 'LOG.CREACION.CLIENTE.MASIVO.txt'
	S.DUI      = ''
RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	S.DUI = ID.NEW
*	S.DUI = '000005953' ;*007852725 000637158 019372422
	CALL F.READ(FN.D.CUST, S.DUI, R.REG, F.D.CUST, ERR.FT)

	IF ERR.FT THEN
		E='ST-SLV.OFS.CUST.NOF'
		CALL ERR
	END ELSE
		ID.NEW=R.REG<SLV.DUI.CUSTOMER.CODE>

		GOSUB DELETE_AND_OPEN
		STR.ARR = 'REG>> ID.CUSTOMER:':ID.NEW:' - DUI:':S.DUI:' - FECHA:':TODAY
	    WRITEBLK STR.ARR ON SEQ.PTR THEN ;*sin retorno de carro
	    END	
	   	CLOSESEQ SEQ.PTR
	END
	
RETURN

DELETE_AND_OPEN:
	;* Eliminando archivo existente
	;*------------------------------
    DELETESEQ DIR.NAME,NAME.FILE THEN
    END

	;* Abriendo archivo para escritura
	;*---------------------------------
    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
        WEOFSEQ NAME.FILE
    END
RETURN
END
