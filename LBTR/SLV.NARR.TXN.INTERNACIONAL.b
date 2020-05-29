*-----------------------------------------------------------------------------
* <Rating>-46</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.NARR.TXN.INTERNACIONAL(ACC.NO, ID.FT,T.CODE, NARRATIVE, FLAG.CLIENTE)
*-----------------------------------------------------------------------------
* Nombre: SLV.NARR.TXN.INTERNACIONAL
* Descripcion: Rutina encargada de generar los narrativs para las transacciones generadas desde las versiones de FT CARGO, ABONO, REINTEGRO.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		wrivas  	14.02.19	Version inicial    
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
	$INSERT I_EQUATE
	$INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.SLV.TXN.EXTERIOR
	$INSERT I_F.EB.SLV.TYPE.EXTERIOR.TXN
;*-----------------------------------------------------------------------------
	GOSUB INIT
	GOSUB PROCESS
RETURN
;*-----------------------------------------------------------------------------
INIT:
	RTN = 'SLV.NARR.TXN.INTERNACIONAL'
	
	FN.FT  = "F.FUNDS.TRANSFER"
    F.FT   = ""
*   Open Funds Transfer POS
    CALL OPF (FN.FT, F.FT)
    
    FN.FUNDS.TRANSFER$HIS='F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER$HIS=''
    CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)
    
    FN.TXN.EX = "F.EB.SLV.TXN.EXTERIOR"
    F.TXN.EX = ""
    CALL OPF (FN.TXN.EX, F.TXN.EX)
    
    FN.TYPE.TXN.EX = 'F.EB.SLV.TYPE.EXTERIOR.TXN'
	F.TYPE.TXN.EX = ''
	CALL OPF(FN.TYPE.TXN.EX, F.TYPE.TXN.EX)
	
    Y.ACC.EVALUA = ACC.NO 
    Y.FT = ID.FT
    Y.TCODE = T.CODE
    NARRATIVE = ''
    FLAG.CLIENTE = ''
;*Parametro para Debug:   
    ;*Y.ACC.EVALUA = '10000102037608'
	;*Y.FT = 'FT19009D3GWH'
RETURN
;*-----------------------------------------------------------------------------
PROCESS:
* QUITANDO CARACTERES DEL ID
 Y.FT.F = EREPLACE(Y.FT, '/','')
  Y.FT.F  = FIELD(Y.FT, ' ', 1)

;* REFERENCIA
    Y.FT.F = SWAP(Y.FT,';','') ;* Cambio de Caracteres
	Y.FT.F =	FIELD(Y.FT ,';',1) 
;*Y.FT= 'FT19009HM3B21;1'
;*Obteniendo el registro de la FT 
	CALL F.READ(FN.FT, Y.FT.F, R.FT, F.FT, ERR.FT)
	 IF NOT(R.FT) THEN
        Y.ERR = ''
        CALL EB.READ.HISTORY.REC (F.FUNDS.TRANSFER$HIS,Y.FT.F,R.FT,Y.ERR)
    END
			Y.TIPO.TXN = R.FT<FT.ORDERING.BANK>
			Y.CUENTA.DEBITO = R.FT<FT.DEBIT.ACCT.NO>
			Y.CUENTA.CREDITO = R.FT<FT.CREDIT.ACCT.NO>
;*Y.FT= 'FT19009HM3B2'
 
;* REFERENCIA
    Y.FT.F = SWAP(Y.FT,';','') ;* Cambio de Caracteres
	Y.FT.F =	FIELD(Y.FT ,';',1) 
	;*Obteniendo el registro de FT para determinar si la FT es de cargo o de abono   
	CALL F.READ(FN.TXN.EX, Y.FT.F, R.TXN.EX, F.TXN.EX, ERR.TXN)
		Y.TIPO.FT = R.TXN.EX<EB.TXN.EXT.TIPO.TRANSFERENCIA>
	
	CALL F.READ(FN.TYPE.TXN.EX, Y.TIPO.TXN, RR.TXN.EX, F.TYPE.TXN.EX, ERR.TXNR) 
			Y.NARR.CREDITO = RR.TXN.EX<EB.SLV53.NARRATIVE.C>;*Narrative Credito
			Y.NARR.ABONO = RR.TXN.EX<EB.SLV53.NARRATIVE.A> ;*Narrative abono
			
;*Evaluando si la FT es Cargo
	IF 	Y.TIPO.FT EQ 'CARGO' THEN
			IF Y.TCODE EQ 234 THEN
				IF Y.ACC.EVALUA EQ Y.CUENTA.DEBITO THEN ;*Entonces es Cuenta de cliente
						NARRATIVE = 'Comision':' ': Y.NARR.CREDITO ;*Ej.: Comisión Transferencia LBRT Regional
				END 
				IF Y.ACC.EVALUA EQ Y.CUENTA.CREDITO THEN ;*Entonces es Cuenta corresponsal
					NARRATIVE = 'Transferencia':' ': Y.NARR.CREDITO ;*:' ':'Cta.':Y.CUENTA.DEBITO   ;*Ej.: Transferencia LBRT Regional Cta.10000000239181
				END	
			END ELSE
				IF Y.ACC.EVALUA EQ Y.CUENTA.DEBITO THEN ;*Entonces es Cuenta de cliente
						NARRATIVE = 'Transferencia':' ': Y.NARR.CREDITO ;*Ej.: Transferencia LBRT Regional
						FLAG.CLIENTE = 'Y'
				END 
				IF Y.ACC.EVALUA EQ Y.CUENTA.CREDITO THEN ;*Entonces es Cuenta corresponsal
					NARRATIVE = 'Transferencia':' ': Y.NARR.CREDITO ;*:' ':'Cta.':Y.CUENTA.DEBITO   ;*Ej.: Transferencia LBRT Regional Cta.10000000239181
				END		
			END					
	END
	  
;*Evaluando si la FT es Abono
	IF Y.TIPO.FT EQ 'ABONO' THEN 
		IF Y.ACC.EVALUA EQ Y.CUENTA.DEBITO THEN  ;*Entonces es Cuenta corresponsal
				NARRATIVE = 'Transferencia':' ': Y.NARR.CREDITO ;*:' ':'Cta.':Y.CUENTA.DEBITO ;*Ej.: Transferencia LBRT Regional Cta.10000000239181
				
		END 
		IF Y.ACC.EVALUA EQ Y.CUENTA.CREDITO THEN ;*Entonces es Cuenta de cliente
			NARRATIVE = 'Transferencia':' ': Y.NARR.CREDITO   ;*Ej.: Transferencia LBRT Regional
			FLAG.CLIENTE = 'Y'
		END		
	END	
;*Evaluando si la FT es Reintegro
	IF Y.TIPO.FT EQ 'REINTEGRO' THEN 
		IF Y.ACC.EVALUA EQ Y.CUENTA.DEBITO THEN ;*Entonces es Cuenta de cliente
				NARRATIVE = 'Reintegro transferencia':' ': Y.NARR.CREDITO ;*Ej.: Reintegro transferencia LBRT Regional
				FLAG.CLIENTE = 'Y'
		END 
		IF Y.ACC.EVALUA EQ Y.CUENTA.CREDITO THEN ;*Entonces es Cuenta corresponsal
			NARRATIVE = 'Reintegro transferencia':' ': Y.NARR.CREDITO ;*:' ':'Cta.':Y.CUENTA.DEBITO   ;*Reintegro transferencia LBRT Regional Cta.10000000239181
		END	
	END 		
RETURN
END