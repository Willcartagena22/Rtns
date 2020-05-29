*-----------------------------------------------------------------------------
* <Rating>49</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NEXT.VERSION.DAP
*-----------------------------------------------------------------------------
*
* Nombre: SLV.E.NEXT.VERSION.DAP
* Descripción: Rutina para Enviar a Cobro de Liquidez o a Fondeo de DAP segun
*				sea el caso
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	24.11.2016	Initial Code
* OCornejo	27.02.2017	Se llama Rutina de Envio OFS en lugar de Enviar a Paso 2 Version FT
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_SLV.TFS.ID.DAP.COMMON
$INSERT I_F.SLV.GEN.PARAM
$INSERT I_F.TELLER.FINANCIAL.SERVICES
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB GET.LOC.REF.FIELDS

*---------------------------------------------------------------------------------------------------------------------
* EN REUNION SOSTENIDA HOY 07.02.2017 CON MIGUEL CUELLAR DIJO QUE NO SE DEBIA COBRAR IMPUESTO POR ENTRADA DE EFECTIVO
*   EN DEPOSITOS A PLAZO, PARA DEVOLVER ESTA FUNCIONALIDAD SOLO ES NECESARIO DESCOMENTAR ESTAR PARTE DE LA RUTINA
*;*											 OCORNEJO - 07.02.2017
*---------------------------------------------------------------------------------------------------------------------
* EN REUNION SOSTENIDA AYER 08.02.2017 MIGUEL CUELLAR DIJO QUE SI SE DEBIA COBRAR IMPUESTO : OCORNEJO - 09.02.2017
*---------------------------------------------------------------------------------------------------------------------
GOSUB PROCESS
*---------------------------------------------------------------------------------------------------------------------
RETURN

	INIT:
		FN.GEN.PARAM = 'F.SLV.GEN.PARAM'
		F.GEN.PARAM  = ''
		CALL OPF(FN.GEN.PARAM, F.GEN.PARAM)
		
		EQU TxnLiofCliq TO 'SLV.TXN.LIOF'
    	EQU CLIQ TO 'CLIQ'
    	EQU VersionCobro TO 'TELLER.FINANCIAL.SERVICES,SLV.TFS.RESERVA.CLIQ I F3'
    	EQU VersionFondeo TO 'FUNDS.TRANSFER,AA.TFS.DEP I F3'
    	
    	Y.VERSION  = ''
    	Y.FLG.CLIQ = ''
	RETURN
	
	GET.LOC.REF.FIELDS:
	    APPL.NAME = 'TELLER.FINANCIAL.SERVICES'
	    FLD.NAME  = 'LF.AMT.TAX'
	    FLD.POS   = ''
	    
	    CALL MULTI.GET.LOC.REF(APPL.NAME,FLD.NAME,FLD.POS)
	
	    Y.AMT.TAX.POS = FLD.POS<1,1>
	RETURN
	
	PROCESS:		
		;*Consultando Txn LIOF
		CALL F.READ(FN.GEN.PARAM, TxnLiofCliq, R.GEN.PARAM, F.GEN.PARAM, E.GEN.PARAM)
		
		;*Recorriendo Impuestos Calculados y Determinando si es por Cobro de Liquidez
		Y.TAX.LIST = R.NEW(TFS.LOCAL.REF)<1, Y.AMT.TAX.POS>
		IF Y.TAX.LIST THEN
			;*Cambiar MV 
			Y.TAX.LIST = CHANGE(Y.TAX.LIST,SM,VM)
			
			;*Iterar Impuestos
			NO.TAX = DCOUNT(Y.TAX.LIST, VM)
			FOR I = 1 TO NO.TAX
				Y.TAX = Y.TAX.LIST<1,I>
				IF Y.TAX GT 0 THEN
					;*Si hay Impuesto Validar si la Txn Cobra Liquidez
					FIND R.NEW(TFS.TRANSACTION)<1,I> IN R.GEN.PARAM<SLV.GEN.TX.DATA.PARAM> SETTING Ap, Vp THEN
						IF R.GEN.PARAM<SLV.GEN.TX.TYPE.PARAM><Ap, Vp> EQ CLIQ THEN
							Y.VERSION  = VersionCobro
							BREAK ;*Encontrado Impuesto por Control de Liquidez Romper el Ciclo
						END
					END
				END 
			NEXT I
		END
				
		;*Si No Hay Cobro de Liquidez enviar a Fondeo por FT
		IF Y.VERSION EQ '' THEN
			;*Se reemplaza llamada a version por Envio de OFS : OCORNEJO 27.02.2017
			;*Y.VERSION = VersionFondeo
			CALL SLV.AUTH.OFS.CONT.DAP
			RETURN
		END
		
*		Se mantiene comentado este bloque hasta que pueda enviarse a una next version de TFS para el cobro de liquidez
*		sin tener el problema que cambia al cajero de agencia cuando en una forma de pago queda la agencia de la cuenta
*		que se ve envuelta en el proceso : OCORNEJO 01.03.2017		
*		
*		;*Si hay Cobro de Impuesto Validar que el INPUTTER 
*		;*este Autorizando sino Interrumpir Flujo
*		Y.INPUTTER = R.NEW(TFS.INPUTTER)
*		IF FIELD(Y.INPUTTER,'_',2) NE OPERATOR THEN
*			RETURN
*		END
*		
*		;*Respaldando Id para la Rutina SLV.E.TFS.FONDEO.DAP
*		TFS.ID = ID.NEW
*							
*		;*Enviando a Version Respectiva
*		CALL EB.SET.NEXT.TASK(Y.VERSION)
	RETURN
END
