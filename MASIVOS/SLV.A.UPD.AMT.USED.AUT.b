*-----------------------------------------------------------------------------
* <Rating>-89</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.UPD.AMT.USED.AUT
*-----------------------------------------------------------------------------
* Nombre: SLV.A.UPD.AMT.USED.AUT
* Descripcion: Rutina Encargada actualizar el monto utilizado al usuario Authoriser y Checker (si aplica) que realizo la autorizacion de la transaccion uno a uno.
*			   Se actualizara monto limite a todos los autorizadores involucrados solamente al momento de que el ultimo usuario autorice la transaccion.
*			   Se coloca en VERSION: FUNDS.TRANSFER,TCIB (AUTH RTN)
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios   25.10.18	   	Version inicial 
* 1.1		psanchez   27.11.18		Correccion en validacion para actualizar con el nuevo monto 
* 1.2		jnramirez  10.12.18		Se agregara para no tomar transacciones de colectores
* 1.3		iTurcios   21.01.19		Se unifican versiones de PSANCHEZ y JNRAMIREZ.
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_System 
$INSERT I_F.EB.SLV.PROTECTION.USAGE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.EB.EXTERNAL.USER
$INSERT I_F.ACCOUNT
*----------------------------------------------------------------------------------------------------
;*Se lee campo para validar si es una transaccion que viene desde Banca Empresas, debido que se usa la misma version para Banca Personas
CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.TCE.NARR', POS.TCE)
;*Debug
*R.NEW(FT.LOCAL.REF)<1,POS.TCE> = 'ACPR'

Y.TYPETRAN = R.NEW(FT.TRANSACTION.TYPE)
IF R.NEW(FT.LOCAL.REF)<1,POS.TCE> AND Y.TYPETRAN NE 'AC64' THEN 
	GOSUB INIT 
	GOSUB PROCESS
END	 
RETURN

INIT:
	FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    
	FN.SLV.PROTECTION.USAGE = 'F.EB.SLV.PROTECTION.USAGE'
	F.SLV.PROTECTION.USAGE = '' 
	CALL OPF(FN.SLV.PROTECTION.USAGE, F.SLV.PROTECTION.USAGE)
	 
	FN.EXT.USER	= 'F.EB.EXTERNAL.USER'
	F.EXT.USER	= ''
	CALL OPF(FN.EXT.USER, F.EXT.USER) 
    
    ;*Firmantes a actualizar monto Limite 
    Y.ID.FIRMANTES	= R.NEW(FT.SIGNATORY)
    
    GOSUB GET.PJ ;*Obtiene Customer Juridico [Y.ID.EMPRESA]
    
    GOSUB GET.MONTO ;*Obtiene Monto de la Txn [Y.MONTO.TOTAL]	
		 	
    ;*Debug
*	Y.ID.EMPRESA = "112582"
*	Y.ID.FIRMANTES =  "112523" : @VM : "112630"	
*	Y.MONTO.TOTAL	= 4
RETURN

PROCESS:
	Y.CANT.FIRMANTES	= DCOUNT(Y.ID.FIRMANTES, @VM)
	FOR I = 1 TO Y.CANT.FIRMANTES
		;*Buscar Isa de los firmantes
		Y.CURR.CUSTOMER	= Y.ID.FIRMANTES<1,I>
		GOSUB GET.EXT.USERS
		FOR EX.US = 1 TO Y.CANT.EXT.USERS
			GOSUB UPD.AMT.UTILISED 
		NEXT EX.US 
	NEXT I
RETURN

GET.EXT.USERS:
	SELECT.EXT.USER = "SELECT " : FN.EXT.USER : " WITH CUSTOMER EQ '" : Y.CURR.CUSTOMER : "'" : " AND USER.TYPE EQ 'CORPORATE'"
	CALL EB.READLIST (SELECT.EXT.USER, ID.EXT, '', REC.READ.EXT.USER, ERR.READ.EXT.USER)
	
	IF ID.EXT THEN
		Y.CANT.EXT.USERS = DCOUNT(ID.EXT, @FM)	
	END
RETURN

UPD.AMT.UTILISED: 
	CALL F.READ(FN.EXT.USER, ID.EXT<EX.US>, REC.EXT.USER, F.EXT.USER, ERR.EXT.USER)							
	Y.CURR.ISA = REC.EXT.USER<EB.XU.ARRANGEMENT>
	
	;*Leyendo EB.SLV.PROTECTION.USAGE para validar si el ARR ISA es correspondiente.
	CALL F.READ(FN.SLV.PROTECTION.USAGE, Y.CURR.ISA, REC.SLV.PRT.USG, F.SLV.PROTECTION.USAGE, ERR.PROT.USG)
	IF REC.SLV.PRT.USG THEN
		;*Leyendo empresas configuradas actualmente en App Local
		Y.CURR.ALLOW.CUS = REC.SLV.PRT.USG<EB.SLV9.EMPRESA>
		LOCATE Y.ID.EMPRESA IN Y.CURR.ALLOW.CUS<1,1> SETTING POS.EMP THEN
			;*Modificando el monto utilizado
			IF REC.SLV.PRT.USG<EB.SLV9.FECHA.UPD, POS.EMP> NE TODAY THEN
   		 		REC.SLV.PRT.USG<EB.SLV9.MONTO.UTILIZADO,POS.EMP> = 0.0
    		END			
			REC.SLV.PRT.USG<EB.SLV9.MONTO.UTILIZADO,POS.EMP> += Y.MONTO.TOTAL							
			REC.SLV.PRT.USG<EB.SLV9.FECHA.UPD, POS.EMP> = TODAY
			GOSUB GET.DATETIME       
			REC.SLV.PRT.USG<EB.SLV9.DATE.TIME.UPD, POS.EMP> = X
			REC.SLV.PRT.USG<EB.SLV9.DATE.TIME> = X			
			REC.SLV.PRT.USG<EB.SLV9.CURR.NO> += 1
			REC.SLV.PRT.USG<EB.SLV9.INPUTTER> = "_" : OPERATOR : "_"
			REC.SLV.PRT.USG<EB.SLV9.AUTHORISER>	= "_" : OPERATOR : "_"
			;* Escribiendo a la tabla local EB.SLV.PROTECTION.USAGE
			CALL F.WRITE(FN.SLV.PROTECTION.USAGE, Y.CURR.ISA, REC.SLV.PRT.USG)													
		END	
	END 
RETURN

GET.PJ:
	Y.CUENTA = R.NEW(FT.DEBIT.ACCT.NO)
	CALL F.READ(FN.ACCOUNT, Y.CUENTA, AC.REC, F.ACCOUNT, AC.ERR)
    IF AC.REC THEN        
        Y.ID.EMPRESA = AC.REC<AC.CUSTOMER>
    END
RETURN

GET.MONTO:
	CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.AMOUNT', POS.LF.AMOUNT)
	Y.DEBIT.AMOUNT = R.NEW(FT.DEBIT.AMOUNT)
    Y.CREDIT.AMOUNT = R.NEW(FT.CREDIT.AMOUNT)	 
    Y.LF.AMOUNT = R.NEW(FT.LOCAL.REF)<1, POS.LF.AMOUNT>
    Y.MONTO.TOTAL = 0.0
    
    IF Y.LF.AMOUNT THEN
        Y.MONTO.TOTAL = Y.LF.AMOUNT
    END ELSE
        Y.MONTO.TOTAL = Y.DEBIT.AMOUNT
        IF Y.MONTO.TOTAL EQ '' THEN
            Y.MONTO.TOTAL = Y.CREDIT.AMOUNT
        END
    END 
RETURN

GET.DATETIME:
	HORA = TIMEDATE()[1,8]
	FECHA = OCONV(DATE(),"D/")
  	X = FECHA[4,2]:'/':FECHA[1,2]:'/':FECHA[7,4]:' ': HORA
RETURN

WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT' 
	R.ID = 'NBP_UPDATE_AMT_USED_AUTH_CHECKER' : '_' : TODAY : '.txt'
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN

END 