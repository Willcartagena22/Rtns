*-----------------------------------------------------------------------------
* <Rating>108</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.NOF.GET.COMMISSION.PARAM(P.PRODUCT , P.AMOUNT, ARR.RES) 
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* 
* Nombre: SLV.NOF.GET.COMMISSION
* Descripción: Rutina para Extraer Registro de Comision a Aplicar Basado en Id
*			   de Producto
*
*-----------------------------------------------------------------------------
* Modification History :
* Autor		Fecha		Comentario
* OCornejo	19.02.2016	Initial Code 
* ITurcios	23.05.16	Se modifica Rutina original para que reciba parametros desde otra rutina y no desde ENQ.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.SLV.PRODUCT.COMMISSION
$INSERT I_F.EB.SLV.CHARGE.RANGE

*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN
    
    INIT:
    	FN.PRODUCT    = 'F.EB.SLV.PRODUCT.COMMISSION'
		F.PRODUCT     = ''
		FN.CHG.RNG    = 'F.EB.SLV.CHARGE.RANGE'
		F.CHG.RNG     = ''
		FN.USURA      = 'F.SLV.MAX.LEGAL.RATES'
		F.USURA       = ''
		
		;*Parametro
	    ;*---------
*	    LOCATE "PRODUCT.ID" IN D.FIELDS<1> SETTING POS1 THEN
*			Y.PRODUCT = D.RANGE.AND.VALUE<POS1>
*		END
		Y.PRODUCT = P.PRODUCT
		
*		LOCATE "AMOUNT" IN D.FIELDS<1> SETTING POS1 THEN
*			Y.AMOUNT = D.RANGE.AND.VALUE<POS1>
*		END
		Y.AMOUNT = P.AMOUNT
		
		EQU USURA.ID TO 'SYSTEM'
    RETURN 
    
    OPENFILE: 
    	CALL OPF(FN.PRODUCT,F.PRODUCT)
    	CALL OPF(FN.CHG.RNG,F.CHG.RNG)
    	CALL OPF(FN.USURA,F.USURA)
    RETURN
    
    PROCESS:
    	;*Extraer Producto
		;*----------------
		CALL F.READ(FN.PRODUCT, Y.PRODUCT, R.PRODUCT, F.PRODUCT, ERR.PRODUCT)
		
		;*Extraer Condiciones del Cargo/Comision
		;*--------------------------------------
		CALL F.READ(FN.CHG.RNG, R.PRODUCT<EB.SLV53.CHARGE.RANGE>, R.CHG.RNG, F.CHG.RNG, ERR.CHG.RNG)
		
		;*Extraer Rango de Usura
		;*----------------------
		CALL F.READ(FN.USURA, USURA.ID, R.USURA, F.USURA, ERR.USURA)
		
		;*Recorrer Multivalor
		;*-------------------
		C.REG = DCOUNT(R.USURA<SLV.MAX.ID.SEGMENTO>, VM)
		FOR I = 1 TO C.REG   		
    	
    		;*Buscar Coincidencia por Producto
    		;*----------------------------------------
    		IF R.USURA<SLV.MAX.ID.SEGMENTO, I> EQ Y.PRODUCT THEN
    		
    			;*Buscar Rango Segun Monto
    			;*------------------------
    			C.REG.AMT = DCOUNT(R.USURA<SLV.MAX.MAX.LEGAL.RATE,I>, SM)
    			FOR J = 1 TO C.REG.AMT
    			
    				;*Si los Rangos Vienen Vacios Asignar Valor
    				;*-----------------------------------------
    				IF R.USURA<SLV.MAX.LOAN.RANGE.FROM,I,J> EQ '' THEN
    					R.USURA<SLV.MAX.LOAN.RANGE.FROM,I,J> = 0
    				END
    				
    				IF R.USURA<SLV.MAX.LOAN.RANGE.TO,I,J> EQ '' THEN
    					R.USURA<SLV.MAX.LOAN.RANGE.TO,I,J> = 9999999999
    				END
    			
    				;*Evaluar el Monto para Determinar la Tasa Maxima cuando se Cumpla Quebrar el Ciclo
    				;*----------------------------------------------- ---------------------------------
    				Y.TEA.MAX = 0
    				IF Y.AMOUNT GE R.USURA<SLV.MAX.LOAN.RANGE.FROM,I,J> AND Y.AMOUNT LE R.USURA<SLV.MAX.LOAN.RANGE.TO,I,J> THEN
    					
    					;*Asignar TEA MAX
    					;*---------------
    					Y.TEA.MAX = R.USURA<SLV.MAX.MAX.LEGAL.RATE,I,J>
    					
    					;*Quebrar
						;*------- 
		    			BREAK
    				END
    			NEXT J
    			
    			;*Quebrar
				;*-------
    			BREAK
    		END
        NEXT I
		  
		;*Construccion de arreglo
		;*-----------------------
 		STR.MOV = ''
 		STR.MOV := Y.PRODUCT					 		: "*"   ;* 1
		STR.MOV := R.CHG.RNG<EB.SLV85.CHARGE.TYPE> 		: "*"   ;* 2
		STR.MOV := R.CHG.RNG<EB.SLV85.CHARGE.PERCENT>	: "*"   ;* 3
		STR.MOV := R.CHG.RNG<EB.SLV85.MIN.CHARGE>   	: "*"   ;* 4
		STR.MOV := R.CHG.RNG<EB.SLV85.MAX.CHARGE>   	: "*"   ;* 5
		STR.MOV := R.PRODUCT<EB.SLV53.FIXED.COST>   	: "*"   ;* 6
		STR.MOV := Y.TEA.MAX 							: "*"   ;* 7
	    
	    ARR.RES<-1> = STR.MOV
    RETURN
END
