*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PRUEBA.VAL.USUARIO.SEG
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
	$INSERT I_F.EB.SLV.ALTA.SEGURO
	
	$INSERT I_F.USER
	$INSERT I_F.EB.SLV.KEYS.PARAMS
	$INSERT I_F.ACCOUNT


    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN
    
    INIT:
    FN.USER				='F.USER'
	F.USER				=''
	
	FN.KEYS.PARAMS = 'F.EB.SLV.KEYS.PARAMS'
	F.KEYS.PARAMS  = ''
    RETURN
    
	OPENFILE:
	CALL OPF(FN.USER,F.USER)
	CALL OPF(FN.KEYS.PARAMS, F.KEYS.PARAMS)
	RETURN
	

	PROCESS:
	FECHA.ACTUAL='20180411'
	FECHA.FIN='201'
    FECHA.FINAL=OCONV(FECHA.FIN,'DI')  
	
	VAR.SEGURO.ALTA.EJECUTIVO='ALBA ESTER HERRERA RAMIREZ'
	K.STMT.ARRANGEMENT 	= "SELECT " : FN.USER : " WITH USER.NAME EQ '" : VAR.SEGURO.ALTA.EJECUTIVO :"' AND END.DATE.PROFILE GE ":FECHA.ACTUAL
	CALL EB.READLIST(K.STMT.ARRANGEMENT, ARRANGEMENT.LIST, R.EJEC, NO.OF.RECS, Y.ARRANGEMENT.ERR1)
	VAR.SEGURO.ALTA.USER=ARRANGEMENT.LIST<1>
	CRT ARRANGEMENT.LIST<1>
	CRT ARRANGEMENT.LIST<2>
	CRT ARRANGEMENT.LIST<3>
	
	
	VAR.DATE.TODAY=TODAY
;*Formato: FECHA.PAGO DD/MM/YYYY y tipo calendario 
VAR.VALID.FECHA.PAGO='20150808'
VAR.VALID.FPAGO.NUM=OCONV(VAR.VALID.FECHA.PAGO,'DI')

VAR.VALID.TODAY.NUM=OCONV(VAR.DATE.TODAY,'DI')

CRT 'VAR.DATE.TODAY:  ':VAR.DATE.TODAY
CRT 'VAR.VALID.FPAGO.NUM: ' : VAR.VALID.FPAGO.NUM
CRT 'VAR.VALID  :': VAR.VALID.TODAY.NUM
 

*BEGIN CASE
*	CASE VAR.VALID.FECHA.PAGO NE '' AND VAR.VALID.FECHA.PAGO LT VAR.DATE.TODAY
*	V_NAME_FIELD= EB.SEG.FECHA.PAGO
*	STRERR ='EB-SEG003'
*	GOSUB CRT_ERROR
*
*END CASE
	
	RETURN

END
