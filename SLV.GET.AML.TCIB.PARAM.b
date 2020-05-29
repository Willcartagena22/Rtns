*-----------------------------------------------------------------------------
* <Rating>516</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.GET.AML.TCIB.PARAM(ALERT.ID, CUSTOMER.NO, ALERT.VALUE)
**-----------------------------------------------------------------------------
*ALERT.ID = IN, CUSTOMER.NO = IN, ALERT.VALUE = OUT
*-----------------------------------------------------------------------------
*
* Nombre: SLV.GET.AML.TCIB.PARAM
* Descripción: Rutina para Extraer Paramatros de Alerta de Contadores TCIB
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* OCornejo	06.04.2016	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.AML.TCIB.PARAM
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB GET.LOCAL.POS
GOSUB PROCESS
RETURN
	
	INIT:
		FN.TCIB.PARAM = 'F.EB.SLV.AML.TCIB.PARAM'
		F.TCIB.PARAM  = ''
		FN.CUS		  = 'F.CUSTOMER'
		F.CUS  		  = ''
		
		;*Constante
		EQU ALERT.PARAM.ID TO 'TCIB'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.TCIB.PARAM, F.TCIB.PARAM)
		CALL OPF(FN.CUS, F.CUS)
	RETURN
	
	PROCESS:
		;*Leer Registro de Transacciones Parametrizadas
		;*---------------------------------------------
		CALL F.READ (FN.TCIB.PARAM, ALERT.PARAM.ID, R.TCIB.PARAM, F.TCIB.PARAM, ERR.TCIB.PARAM)
		
		;*Recorrer Multivalor
		;*-------------------
		NO.REC = DCOUNT(R.TCIB.PARAM, FM)
		FOR I = 1 TO NO.REC
					
			;*Buscar Coincidencia en Codigos de Alerta
			;*----------------------------------------
			IF R.TCIB.PARAM<EB.SLV3.ALERT, I> EQ ALERT.ID THEN
				
				;*Seteando Valor Inicial a Retornar En Caso que el Parametro sea Para Ambos Tipos de Cliente
				;*------------------------------------------------------------------------------------------
				Y.PARAM.VAL = R.TCIB.PARAM<EB.SLV3.LIMIT, I>
				
				;*Especializacion para Buscar por Tipo de Cliente
				;*-----------------------------------------------
				IF R.TCIB.PARAM<EB.SLV3.TYPE.CUSTOMER, I> NE '' THEN
				
					;*Buscar Tipo de Cliente
					;*----------------------
					CALL F.READ (FN.CUS, CUSTOMER.NO, R.CUS, F.CUS, ERR.CUS)
					IF R.CUS<EB.CUS.LOCAL.REF><1, P.SEGMENT> EQ 1 THEN
						Y.TIPO.CLIENTE = 'NATURAL'
					END
					ELSE IF R.CUS<EB.CUS.LOCAL.REF><1, P.SEGMENT> EQ 2 THEN
						Y.TIPO.CLIENTE = 'JURISTIC'
					END
					
					;*Recorrer para Extraer Parametro de Alerta Segun Tipo de Cliente
					;*---------------------------------------------------------------
					NO.REC.SEGMENT = DCOUNT(R.TCIB.PARAM<EB.SLV3.TYPE.CUSTOMER, I>, SM)
					FOR J = 1 TO NO.REC.SEGMENT
						
						;*Asignar Valor Parametrizado para el Tipo de Cliente
						;*---------------------------------------------------
						IF R.TCIB.PARAM<EB.SLV3.TYPE.CUSTOMER, I, J> EQ Y.TIPO.CLIENTE THEN
							Y.PARAM.VAL = R.TCIB.PARAM<EB.SLV3.LIMIT, I, J>
							BREAK
						END
					NEXT J
				END
				
				;*Valor a Retornar
				;*----------------
				ALERT.VALUE = Y.PARAM.VAL
				BREAK
			END
		NEXT I
	RETURN
	
	GET.LOCAL.POS:
		CALL GET.LOC.REF('CUSTOMER',"SEGMENT",P.SEGMENT) ;*Persona Natural o Juridica
	RETURN
END


