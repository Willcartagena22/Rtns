*-----------------------------------------------------------------------------
* <Rating>-27</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.RC.STATUS.ACCOUNT
*-----------------------------------------------------------------------------
* Nombre: SLV.V.RC.STATUS.ACCOUNT
* Descripción: Valida estados de la cuenta para la reserva de cheque
*-----------------------------------------------------------------------------
* Version	Paquete								Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Initial								rmoran		18.12.17	Version Inicial
$INSERT I_COMMON
$INSERT I_EQUATE 
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.RANG.PARAM 
$INSERT I_GTS.COMMON
$INSERT I_TSS.COMMON 
*-----------------------------------------------------------------------------
   
GOSUB INIT
GOSUB PROCESS
RETURN
 
INIT:

	FN.LOCKED.EVENTS	= 'F.AC.LOCKED.EVENTS'
	F.LOCKED.EVENTS		= ''
    FN.ACCOUNT	= 'F.ACCOUNT'
    F.ACCOUNT		= '' 

	FN.TABLE.CUS 		= 'F.CUSTOMER' 
	F.TABLE.CUS 		= '' 
	
	FN.RANG.PARAM = 'F.EB.SLV.RANG.PARAM'
	F.RANG.PARAM = ''
	
	CALL OPF(FN.ACCOUNT,F.ACCOUNT) 
	CALL OPF(FN.TABLE.CUS, F.TABLE.CUS)
	CALL OPF(FN.LOCKED.EVENTS, F.LOCKED.EVENTS)
	CALL OPF(FN.RANG.PARAM, F.RANG.PARAM)
	 

	EQU CAN TO 'CAN'	
	EQU CER TO 'CER'	
	EQU EMB TO 'EMB'
	EQU INA TO 'INA'
	
	EQU RTN TO 'SLV_ISA_SUMMARY'
	EQU RANGO TO 'SLV.CAT.CUENTAS'
	EQU CTA.AHO TO 'CTA.AHO'
	EQU CTA.COR TO 'CTA.COR'
 	 			   
RETURN  
        
PROCESS:  
*Y.ACC='10000000044251'
Y.ACC = COMI;*R.NEW(AC.LCK.ACCOUNT.NUMBER) ;*obtener numero de cuenta
*Y.ACC = '10000000044251'
*Y.ACC = '10000000000408' 
	
CALL F.READ(FN.ACCOUNT,Y.ACC,R.ACC,F.ACCOUNT,ERR) ;* obtener Id cliente (tercero)
		Y.CUST = R.ACC<AC.CUSTOMER>  
		Y.CATEG = TRIM(R.ACC<AC.CATEGORY>) ;* obtener category  

CALL F.READ(FN.RANG.PARAM, RANGO, R.PARAM, F.RANG.PARAM, ERR.RANG.PARAM)

CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO)
		Y.ESTADO = R.ACC<AC.LOCAL.REF, POS.ESTADO> 
		Y.POST.REST = R.ACC<AC.POSTING.RESTRICT>  
	
FIND CTA.COR IN R.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN                
	;* Validar si el Category de la Cuenta se encuentra en el Rango
	IF Y.CATEG GE R.PARAM<EB.SLV56.RANGO.INF><Ap, Vp> AND Y.CATEG LE R.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp> THEN
*		CRT 'cuenta valida ':Y.CATEG
	END
	
	IF NOT(Y.CATEG GE R.PARAM<EB.SLV56.RANGO.INF><Ap, Vp> AND Y.CATEG LE R.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp>) THEN
	;* Validar si el Category de la Cuenta no se encuentra en el Rango	
*		CRT 'Categoria cuenta no valida ':Y.CATEG
			E = 'EB-SLV.I.ACCOUNT.CATEG.FAIL' 
			CALL ERR
	END
	
	
	IF Y.POST.REST EQ '24' OR Y.POST.REST EQ '21' OR Y.POST.REST EQ '25' OR Y.POST.REST EQ '23' THEN 
		;* error porque tiene restricciones 
		E = 'EB-SLV.I.ACC.RESERVA.CHQ' ;*------"Reserva de cheque NO realizada, por restricción en cuenta-----;
		CALL ERR 
*			CRT 'POSTING.RESTRICT=':Y.POST.REST
	END
  
IF Y.ESTADO EQ CAN OR Y.ESTADO EQ CER OR Y.ESTADO EQ EMB OR Y.ESTADO EQ INA THEN
	;* el estado de la cuenta no es apropiado --Reserva de cheque NO realizada, por restricción en cuenta
	E = 'EB-SLV.I.ACC.STATE.RESERVA.CHQ'
*		CRT 'ESTADO DE CUENTA INVALIDO:':Y.ESTADO
	CALL ERR 
END	
	
RETURN
  
END
