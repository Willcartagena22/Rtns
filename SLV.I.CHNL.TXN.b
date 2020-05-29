*-----------------------------------------------------------------------------
* <Rating>-80</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.CHNL.TXN(Y.LOG.ID,TAX.FLAG)
**-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*
* Nombre: SLV.I.CHNL.TXN
* Descripción: Rutina para Escribir Registros para Reporte de Otros Medios
*			   Electronicos solicitado por UIF
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* OCornejo	09.03.2016	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE 
$INSERT I_F.EB.SLV.UIF.CHNL.TRX 
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.FT.TXN.TYPE.CONDITION
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB GET
GOSUB PROCESS
RETURN
	
	INIT:
		FN.TXN.CHNL  = 'F.EB.SLV.UIF.CHNL.TRX'
		F.TXN.CHNL   = ''
		
		FN.FT.TXN.TYPE.CONDITION  = 'F.FT.TXN.TYPE.CONDITION'
		F.FT.TXN.TYPE.CONDITION   = ''
		
		;*Constantes
		EQU TXN.ID TO 'TXN.ELECTRONIC.CHANNELS'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.TXN.CHNL, F.TXN.CHNL)
		CALL OPF(FN.FT.TXN.TYPE.CONDITION, F.FT.TXN.TYPE.CONDITION)
	RETURN
	
	PROCESS:		
		;*Debug
		;*Y.FT.TXN.CODE = 'ACRP' 
		
		;*Extraer Condiciones de Transaccion
		;*----------------------------------
		Y.TXN.PARAMS = ''
		CALL SLV.GET.CHNL.TXN.PARAM(TXN.ID, Y.FT.TXN.CODE, Y.TXN.PARAMS)
		
		;*Si No Hay Parametros No Hacer Nada
		;*----------------------------------
		IF Y.TXN.PARAMS EQ '' THEN
			RETURN
		END
		
		;*Recorrer Condiciones
		;*--------------------
		NO.REC = DCOUNT(Y.TXN.PARAMS, FM)
		FOR I = 1 TO NO.REC
			;*Descomponer Arreglo en Variables
			;*--------------------------------
			Y.TXN.TYPE   	  = FIELD(Y.TXN.PARAMS<I>, "*", 1)
			Y.TXN.AMOUNT 	  = FIELD(Y.TXN.PARAMS<I>, "*", 2)
			Y.TXN.ALERT  	  = FIELD(Y.TXN.PARAMS<I>, "*", 3)
			Y.TXN.FREQ.ALERT  = FIELD(Y.TXN.PARAMS<I>, "*", 4)
			
			;*Si el Monto del FT es Mayor al Parametrizado Escribir Registro
			;*--------------------------------------------------------------
			IF Y.FT.AMOUNT GT Y.TXN.AMOUNT THEN
			
				;*Definir Canal de Entrada
				;*------------------------
				Y.CHANNEL = APPLICATION:PGM.VERSION
			
				;*Escribir Registro
				;*-----------------
				GOSUB WRITE.TXN
				BREAK
			END
		NEXT I
	RETURN
	
	WRITE.TXN:
		;*Campos para Almacenar
		;*---------------------
		R.TXN.CHNL<EB.SLV80.ORIGIN.ACC>  = R.NEW(FT.DEBIT.ACCT.NO) 
		R.TXN.CHNL<EB.SLV80.DESTIN.ACC>  = R.NEW(FT.CREDIT.ACCT.NO)
		R.TXN.CHNL<EB.SLV80.AMOUNT> 	 = Y.FT.AMOUNT
		R.TXN.CHNL<EB.SLV80.ORIGIN.CUST> = R.NEW(FT.DEBIT.CUSTOMER)
		R.TXN.CHNL<EB.SLV80.DESTIN.CUST> = R.NEW(FT.CREDIT.CUSTOMER)
		R.TXN.CHNL<EB.SLV80.TXN.DATE> 	 = R.NEW(FT.DEBIT.VALUE.DATE)
		R.TXN.CHNL<EB.SLV80.TXN.TYPE> 	 = Y.FT.TXN.CODE
		R.TXN.CHNL<EB.SLV80.CHANNEL>  	 = Y.CHANNEL[16,4]
		R.TXN.CHNL<EB.SLV80.CHANNEL.ID>  = ''		
		Y.TIME = TIME()
		R.TXN.CHNL<EB.SLV80.RESERVED.1>  = OCONV(Y.TIME, 'MTS')
		
		
		;*Campos Auditoria
		;*----------------
		R.TXN.CHNL<EB.SLV80.INPUTTER,1>	 = R.NEW(FT.INPUTTER)
		R.TXN.CHNL<EB.SLV80.AUTHORISER>  = R.NEW(FT.AUTHORISER)
		R.TXN.CHNL<EB.SLV80.CURR.NO>	 = 1
		R.TXN.CHNL<EB.SLV80.DATE.TIME,1> = R.NEW(FT.DATE.TIME)
		R.TXN.CHNL<EB.SLV80.CO.CODE>	 = R.NEW(FT.CO.CODE)
		R.TXN.CHNL<EB.SLV80.RECORD.STATUS>  = 'AUTH'
		
		;*eurias si tiene cargo asociado verificar que es liof
		;*20160606 eurias se solicito por parte de cumplimiento quitar la logica para mostrar liof en las nuevas alertas
*		IF(R.NEW(FT.LOC.TOT.TAX.AMT))THEN
*				COMMISSION.TYPE = R.NEW(FT.COMMISSION.TYPE)
*			 IF(COMMISSION.TYPE[1,4] EQ 'LIOF')THEN
*					;*Campos para Almacenar
*					;*---------------------
*					R.TXN.CHNL.CHRG<EB.SLV80.ORIGIN.ACC>  = R.NEW(FT.DEBIT.ACCT.NO) 
*					R.TXN.CHNL.CHRG<EB.SLV80.DESTIN.ACC>  = R.NEW(FT.CREDIT.ACCT.NO)
*					R.TXN.CHNL.CHRG<EB.SLV80.AMOUNT> 	 = R.NEW(FT.LOC.TOT.TAX.AMT);*obtener monto cargo liof
*					R.TXN.CHNL.CHRG<EB.SLV80.ORIGIN.CUST> = R.NEW(FT.DEBIT.CUSTOMER)
*					R.TXN.CHNL.CHRG<EB.SLV80.DESTIN.CUST> = R.NEW(FT.CREDIT.CUSTOMER)
*					R.TXN.CHNL.CHRG<EB.SLV80.TXN.DATE> 	 = R.NEW(FT.DEBIT.VALUE.DATE)
*					R.TXN.CHNL.CHRG<EB.SLV80.TXN.TYPE> 	 = Y.FT.TXN.CODE
*					R.TXN.CHNL.CHRG<EB.SLV80.CHANNEL>  	 = Y.CHANNEL[16,4]
*					R.TXN.CHNL.CHRG<EB.SLV80.CHANNEL.ID>  = ''		
*					Y.TIME = TIME()
*					R.TXN.CHNL.CHRG<EB.SLV80.RESERVED.1>  = OCONV(Y.TIME, 'MTS')
*					R.TXN.CHNL.CHRG<EB.SLV80.RESERVED.2>  = Y.LOG.ID
*					
*					;*Campos Auditoria
*					;*----------------
*					R.TXN.CHNL.CHRG<EB.SLV80.INPUTTER,1>	 = R.NEW(FT.INPUTTER)
*					R.TXN.CHNL.CHRG<EB.SLV80.AUTHORISER>  = R.NEW(FT.AUTHORISER)
*					R.TXN.CHNL.CHRG<EB.SLV80.CURR.NO>	 = 1
*					R.TXN.CHNL.CHRG<EB.SLV80.DATE.TIME,1> = R.NEW(FT.DATE.TIME)
*					R.TXN.CHNL.CHRG<EB.SLV80.CO.CODE>	 = R.NEW(FT.CO.CODE)
*					R.TXN.CHNL.CHRG<EB.SLV80.RECORD.STATUS>  = 'AUTH'
*					CALL F.WRITE (FN.TXN.CHNL, Y.ID:'.1', R.TXN.CHNL.CHRG)
*			END
*		END
		
		;*Escribir Registro
		;*-----------------
		CALL F.WRITE (FN.TXN.CHNL, Y.ID, R.TXN.CHNL)
	RETURN
	
	GET:
		Y.ID          = ID.NEW : '.' : TODAY
		Y.FT.TXN.CODE = R.NEW(FT.TRANSACTION.TYPE)
		Y.FT.AMOUNT   = R.NEW(FT.DEBIT.AMOUNT)
		
		;*Para Pagos de TCIB el Debit Amount viene Vacio
		;*----------------------------------------------
		IF Y.FT.AMOUNT EQ '' THEN
			Y.FT.AMOUNT   = R.NEW(FT.CREDIT.AMOUNT)
		END
	RETURN
END
