*------------------------------------------------------------------------------------------
* <Rating>118</Rating>
*------------------------------------------------------------------------------------------
    SUBROUTINE SLV.E.NOF.SET.CUENTAS(AC.DET.ARR)
*------------------------------------------------------------------------------------------
* RUTINA UTILIZADA PARA MOSTRAR CUENTAS SEGUN MEDIO DE PAGO SELECIONADO - SEGUROS
*------------------------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
* SFUSILLO 					Esta rutina se utiliza para la funcionalidad de SEGUROS 
*							NOFILE que muestra las cuentas del cliente
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
	$INSERT I_F.EB.SLV.ALTA.SEGURO
	$INSERT I_F.CUSTOMER
	$INSERT I_F.ACCOUNT
	$INSERT I_F.CUSTOMER.ACCOUNT
	$INSERT I_F.AA.ARRANGEMENT

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN

INIT:
;*ARCHIVOS
    FN.ALTA.SEG 	= 'F.EB.SLV.ALTA.SEGURO'
    F.ALTA.SEG	= ''
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER	= ''
    FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	FN.ACC.H	= 'F.ACCOUNT$HIS'
	F.ACC.H	= ''
	FN.ACC.CUS	= 'F.CUSTOMER.ACCOUNT'
	F.ACC.CUS	= ''
	
	FN.AA.ARRAN	= 'F.AA.ARRANGEMENT'
	F.AA.ARRAN	= ''
	
	VAR.VALID.BANCO.EMISOR=''
	VAR.VALID.MEDIO.PAGO=''

	
	K.USER.ID 			= OPERATOR
	
	LOCATE "DATOS" IN D.FIELDS<1> SETTING POS.1 THEN
	   VAR.DATOS = D.RANGE.AND.VALUE<POS.1>
	END

		VAR.CLIENTE.ID= SUBSTRINGS(VAR.DATOS,1,6)
		VAR.MEDIO.PAGO= SUBSTRINGS(VAR.DATOS,7,3)

;*VAR.MEDIO.PAGO='CAA'
;*VAR.CLIENTE.ID = 100116
;*-----------------------------------

;*CONSTANTES
;*-----------------------------------

;*PARAMETROS
;*-----------------------------------
   
    RETURN

*APERTURA DE ARCHIVOS A USAR
OPENFILE:
	CALL OPF(FN.ALTA.SEG,F.ALTA.SEG)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.ACC, F.ACC)
	CALL OPF(FN.ACC.CUS, F.ACC.CUS)
	CALL OPF(FN.ACC.CUS, F.ACC.CUS)
	CALL OPF(FN.AA.ARRAN, F.AA.ARRAN)
RETURN


PROCESS:
        
      CALL F.READ(FN.ACC.CUS, VAR.CLIENTE.ID, R.ACC.CUS, F.ACC.CUS, ERR.ACC.CUS)
      	IF VAR.MEDIO.PAGO EQ 'CAA' THEN 
					VAR.RANGO.INF=6000
					VAR.RANGO.SUP=6499
		END
		IF VAR.MEDIO.PAGO EQ 'CCA' THEN
					VAR.RANGO.INF=1000
					VAR.RANGO.SUP=1999
		END
	  AC.DET.ARR<-1> = 'VACIO':"*":'':"*":'':"*":'':"*":''
      V.CONTAR=COUNT(R.ACC.CUS, FM)+1
      B=1
        LOOP WHILE B <= V.CONTAR  DO
        		VAR.CUENTA=R.ACC.CUS<B>
				CALL F.READ(FN.ACC, VAR.CUENTA, R.ACC, F.ACC, ERR.ACC)
				V.ACC.CAT = R.ACC<AC.CATEGORY>
				IF V.ACC.CAT NE '' THEN
					IF V.ACC.CAT GT VAR.RANGO.INF AND V.ACC.CAT LT VAR.RANGO.SUP THEN
						CALL GET.LOC.REF('ACCOUNT', 'LF.ESTADO.CTA', POS.ESTADO)
						VAR.STATUS = R.ACC<AC.LOCAL.REF, POS.ESTADO> 
						VAR.POST.REST = R.ACC<AC.POSTING.RESTRICT>
						VAR.ARR.ID=R.ACC<AC.ARRANGEMENT.ID>
						CALL F.READ(FN.AA.ARRAN, VAR.ARR.ID, R.ARR, F.AA.ARRAN, ERR.ARR)
						VAR.ARR.STATUS=R.ARR<AA.ARR.ARR.STATUS>
						IF VAR.STATUS EQ '' OR VAR.STATUS EQ 'ACT' THEN
							IF VAR.POST.REST EQ 23 OR VAR.POST.REST EQ 26 OR VAR.POST.REST EQ '' THEN
								IF VAR.ARR.STATUS NE 'UNAUTH' AND VAR.ARR.STATUS NE 'CANCELLED' AND VAR.ARR.STATUS NE 'CLOSE' AND VAR.ARR.STATUS NE 'MATURED' AND VAR.ARR.STATUS NE 'REQUEST.CLOSURE' THEN
								;*					1				2				3			4			     5		
								AC.DET.ARR<-1> = VAR.CLIENTE.ID:"*":VAR.CUENTA:"*":V.ACC.CAT:"*":VAR.STATUS:"*":VAR.POST.REST
								END
							END
						END
        			END
        			
        		END
        		
			B=B+1	
			REPEAT
	RETURN

    END
    
    
   
