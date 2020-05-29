*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.UTIL.TFS.GET.FP.CERT(ID.TFS)
*-----------------------------------------------------------------------------
*
* Nombre: SLV.UTIL.TFS.GET.FP.CERT
* Descripción: Rutina para Extraer Formas de Pago
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	28.10.2016	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.EB.SLV.KEYS.PARAMS
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.TFS        = 'F.TELLER.FINANCIAL.SERVICES'
		F.TFS         = ''
		FN.KEY.PARAMS = 'F.EB.SLV.KEYS.PARAMS'
		F.KEY.PARAMS  = ''
		
		Y.AMT = 0		
		Y.ID  = ID.TFS
		
		;*De entrada Retornar Id Padre si lo Hubiese
		CALL SLV.GET.TFS.PADRE.DAP(Y.ID)
		
		EQU ACuentaPropia TO 'ACuentaPropia'
		EQU ACuentaTercero TO 'ACuentaTercero'
		EQU DeCuentaPropia TO 'DeCuentaPropia'
		EQU DeCuentaTercero TO 'DeCuentaTercero'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.TFS, F.TFS)
		CALL OPF(FN.KEY.PARAMS, F.KEY.PARAMS)
	RETURN
	
	PROCESS:
		;*Leer TFS para Extraer Transacciones Aplicadas
		CALL F.READ(FN.TFS, Y.ID, R.TFS, F.TFS, E.TFS)
		NO.TXN = DCOUNT(R.TFS<TFS.TRANSACTION>, VM)
		Y.TXN.ARR     = ''
		Y.TXN.AMT.ARR = ''	;*Arreglo de Txn y Montos Acumulados
		
		;*Iterar Txns para Acumular los Montos a Imprimir
		FOR I = 1 TO NO.TXN	
			;*Extraer Monto de Txn
			Y.AMT = R.TFS<TFS.AMOUNT><1, I>
			Y.TXN = R.TFS<TFS.TRANSACTION><1, I>
			
			;*Acumular Transferencias ya sean Propias o A/De Terceros esto es solo 
			;*para que salgan acumuladas con la misma descripcion en la Certificacion
			IF Y.TXN EQ ACuentaPropia THEN
				Y.TXN = ACuentaTercero
			END
			
			IF Y.TXN EQ DeCuentaPropia THEN
				Y.TXN = DeCuentaTercero
			END
			
			;*Acumular Montos de los Mismos Tipos de Txn
			FIND Y.TXN IN Y.TXN.ARR SETTING P.TXN THEN
				Y.AMT.TXN = FIELD(Y.TXN.AMT.ARR<P.TXN>, '*', 2)
				Y.AMT.TXN += Y.AMT
				Y.TXN.AMT.ARR<P.TXN> = Y.TXN : '*' : Y.AMT.TXN
			END
			ELSE
				;*Sino registrar en los arreglos
				Y.TXN.ARR<-1>     = Y.TXN
				Y.TXN.AMT.ARR<-1> = Y.TXN : '*' : Y.AMT
			END
		NEXT I
		
		;*Iterar Montos Acumulados para Formar String a Pintar en Comprobante
		Y.FORMAS.PAGO = ''
		Y.AMT         = 0
		LOOP
			REMOVE Y.TXN FROM Y.TXN.AMT.ARR SETTING POS
		WHILE Y.TXN NE ''
			;*Asignando Txn
			Y.TRANS = FIELD(Y.TXN, '*', 1)
			BEGIN CASE
				;*Cuando sea Transferencia Settear la Descripcion
				CASE Y.TRANS EQ ACuentaTercero OR Y.TRANS EQ DeCuentaTercero
					Y.DESC = 'Transf. Cta'
			
				CASE 1
					;*Cuando no sea Transferencia Obtener Descripcion
					CALL SLV.GET.LIOF.TRX.PARAM(Y.TRANS, '', '', '', '', Y.DESC)
			END CASE
							
			;*Formatear Monto
			Y.AMT = FIELD(Y.TXN, '*', 2)
			CALL SLV.UTIL.FORMATO.MONEDA(Y.AMT)
			
			;*Armando String				
			Y.FORMAS.PAGO := Y.DESC[0,11] : ': USD ' :  Y.AMT : CHAR(13)
		REPEAT
		
		;*Retornar String a Pintar
		ID.TFS = Y.FORMAS.PAGO
	RETURN
END
