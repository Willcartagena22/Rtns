*-----------------------------------------------------------------------------
* <Rating>380</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.CUS.VALIDATE.EMAIL
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Nombre Rutina	: SLV.V.CUS.VALIDATE.EMAIL
* Descripción	: Valida que la dirección de correo electronico no permita eñes 
*				  y otros caracteres especiales
*-----------------------------------------------------------------------------
* Modification History :
* Autor    		Fecha     		Version.
* DROSALES  	26.09.2017 		Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILES
GOSUB PROCESS

RETURN

;* --------------------------------------------------------
;* INICIO INIT
;* --------------------------------------------------------	
INIT:

	;*Variables de trabajo
    Y.POS_VALUE			=''
    Y.STRERR			=''
	Y.ID.INVALID.PARAMS	='EMAIL.CARACTERES.NO.VALIDOS'
	R.TABLE.PA			=''
	F.ERR.PA			=''
	Y.CHAR.A.VALIDAR	= ''
	Y.CNT.ARROBAS		= 0
	Y.ERROR.COUNT		= 0
		
	;*Archivos necesarios
    FN.CUS				='F.CUSTOMER'
    F.CUS				=''
	FN.TABLE.PA 		= 'F.EB.SLV.GLOBAL.PARAM'
	F.TABLE.PA 			= ''


RETURN
;* --------------------------------------------------------
;* FIN INIT
;* --------------------------------------------------------	

;* --------------------------------------------------------
;* INICIO OPENFILES
;* --------------------------------------------------------	
OPENFILES:

	CALL OPF(FN.CUS,F.CUS)
	CALL OPF(FN.TABLE.PA, F.TABLE.PA)

RETURN
;* --------------------------------------------------------
;* FIN OPENFILES
;* --------------------------------------------------------	

;* --------------------------------------------------------
;* INICIO PROCESS
;* --------------------------------------------------------		
PROCESS:

	;*EMAIL DEL CLIENTE QUE SE HA INGRESADO
    Y.EMAIL			= R.NEW(EB.CUS.EMAIL.1)
    ;*#DEBUG
	;*Y.EMAIL 		= "edm-aram@gmail.com"
	
	;* VALIDANDO QUE EL CORREO NO VENGA VACIO
	IF Y.EMAIL NE '' THEN
	    Y.EMAIL.FIELD	= EB.CUS.EMAIL.1
	    GOSUB VALIDAR_EMAIL
    END

RETURN
;* --------------------------------------------------------
;* FIN PROCESS
;* --------------------------------------------------------

;* --------------------------------------------------------
;* INICIO VALIDAR_EMAIL
;* --------------------------------------------------------	
VALIDAR_EMAIL:	

	;* LEYENDO CARACTERES DEFINIDOS EN PARAMETROS DE LA CONFIGURACION
	FN.TABLE.PA			='F.EB.SLV.GLOBAL.PARAM'
	CALL F.READ(FN.TABLE.PA, Y.ID.INVALID.PARAMS, R.TABLE.PA, parametrosData, F.ERR.PA)
	Y.ARR.CHARS.CONFIG 	= R.TABLE.PA<EB.SLV39.VALOR.PARAM>
	Y.TOTAL.CHARS 		= DCOUNT(Y.ARR.CHARS.CONFIG,VM)
	
	;* LAS EÑES SE QUITAN DE LA DIRECCION DE CORREO Y SE LES COLOCA UN SIMBOLO "%" COMO IDENTIFICADOR PARA ESTA RUTINA
	CHANGE 'ñ' TO '%' IN Y.EMAIL
	CHANGE 'Ñ' TO '%' IN Y.EMAIL
	CHANGE CHAR(164) TO '%' IN Y.EMAIL;* letra ñ
	CHANGE CHAR(165) TO '%' IN Y.EMAIL;* letra Ñ
	
	;* TOTAL DE CARACTERES QUE TIENE LA DIRECCION DE CORREO
	Y.LEN.EMAIL			= LEN(Y.EMAIL)
	
	;* RECORRIENDO LETRA POR LETRA LA DIRECCION DE CORREO
	FOR i = 1 TO Y.LEN.EMAIL
	
		;* EXTRAYENDO EL CARACTER QUE SE VA A VALIDAR
		Y.CHAR.A.VALIDAR = SUBSTRINGS(Y.EMAIL, i, 1)
	
		;* VALIDANDO EL CARACTER EN EL LISTADO DE CARACTERES ASCII NO VALIDOS SEGUN LAS NORMAS RFC2821 y RFC2822
		FOR COD.ASCII = 0 TO 255
	    	IF (COD.ASCII GE 0 AND COD.ASCII LE 44) OR (COD.ASCII EQ 47) OR (COD.ASCII GE 58 AND COD.ASCII LE 63) OR (COD.ASCII GE 91 AND COD.ASCII LE 94) OR (COD.ASCII EQ 96) OR (COD.ASCII GE 123 AND COD.ASCII LE 255) THEN
				IF Y.CHAR.A.VALIDAR EQ CHAR(COD.ASCII) THEN
					Y.ERROR.COUNT = 1
					IF Y.CHAR.A.VALIDAR EQ '%' THEN
						;*Debug
						;*CRT ""
						;*CRT "Tipo de dato incorrecto (Letra Eñe). Favor revisar!"
						Y.STRERR =Y.EMAIL.FIELD :'   Tipo de dato incorrecto en la dirección de correo (Letra Eñe). Favor revisar!'
						;*CRT ""
					END	ELSE
						;*Debug
						;*CRT ""
						;*CRT "Tipo de dato incorrecto. Favor revisar!:   ":Y.CHAR.A.VALIDAR
						;*CRT ""
						Y.STRERR =Y.EMAIL.FIELD :'   Tipo de dato incorrecto en la dirección de correo. Favor revisar!'
						;*CRT ""
					END
			        GOSUB CRT_ERROR
					BREAK
				END
	        END			        
	    NEXT COD.ASCII
		
		;* EN CASO DE HABER ENCONTRADO ALGUN CARACTER NO VALIDO SE SALE DEL LOOP PRINCIPAL
		IF Y.ERROR.COUNT NE 0 THEN
			BREAK
		END ELSE
			;* SINO, SE SIGUE VALIDANDO EL CARACTER DENTRO DEL LISTADO DE CARACTERES NO VALIDOS DE LA CONFIGURACION
			FOR j=1 TO Y.TOTAL.CHARS
				Y.CARACTER.NO.VALIDO = ''
				Y.CARACTER.NO.VALIDO = FIELD(Y.ARR.CHARS.CONFIG,VM,j)				
				IF Y.CHAR.A.VALIDAR EQ Y.CARACTER.NO.VALIDO THEN
					Y.ERROR.COUNT = 1
					;*Debug
					;*CRT ""
					;*CRT "*** CHAR NO VALIDO:    ":Y.CHAR.A.VALIDAR
					;*CRT ""
					Y.STRERR =Y.EMAIL.FIELD :'   Tipo de dato incorrecto en la dirección de correo. Favor revisar!'
					GOSUB CRT_ERROR
					BREAK
				END				
			NEXT j
		END		
	
		;* EN CASO DE HABER ENCONTRADO ALGUN CARACTER NO VALIDO SE SALE DEL LOOP PRINCIPAL
		IF Y.ERROR.COUNT NE 0 THEN
			BREAK
		END ELSE			
			;* Guardando las veces que aparecen los simbolos de @ 
			IF Y.CHAR.A.VALIDAR EQ '@' THEN			
				Y.CNT.ARROBAS = Y.CNT.ARROBAS + 1				
			END
		END
		
	NEXT i
	
	IF Y.ERROR.COUNT EQ 0 THEN 
		IF Y.CNT.ARROBAS GT 1 THEN
			Y.ERROR.COUNT = 1
			;* Debug
			;*CRT ""
			;*CRT "Formato de correo invalido (Dos arrobas). Favor revisar!"
			Y.STRERR =Y.EMAIL.FIELD :'   Formato de Correo Incorrecto. Favor revisar!'
			;*CRT ""
			GOSUB CRT_ERROR
		END	
	END

RETURN
;* --------------------------------------------------------
;* FIN VALIDAR_EMAIL
;* --------------------------------------------------------		

;* --------------------------------------------------------
;* INICIO CRT_ERROR
;* --------------------------------------------------------		
CRT_ERROR:
    AF  	= Y.EMAIL.FIELD
    AV 		= Y.POS_VALUE
    ETEXT 	= Y.STRERR
    CALL STORE.END.ERROR
RETURN	
;* --------------------------------------------------------
;* FIN CRT_ERROR
;* --------------------------------------------------------		
	
END
