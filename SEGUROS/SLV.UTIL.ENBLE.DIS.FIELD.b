*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.UTIL.ENBLE.DIS.FIELD(FLD,FLAG,LFPOSITION)
*-----------------------------------------------------------------------------
* RUTINA GENERICA PARA MODIFICACION DEL COMPORTAMIENTO DE CAMPOS EN SESION
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE

    GOSUB INIT
    GOSUB PROCESS

    RETURN

INIT:

;*VARIABLES
;*-----------------------------------
VAR.FLD=COUNT(FLD, FM)+1
VAR.INIT=1

;*CONSTANTES
;*-----------------------------------
CONS.NOINPUT='NOINPUT'
CONS.NOCHANGE='NOCHANGE'
CONS.NOMODIFY='NOMODIFY'

RETURN

*PROCESO MAIN
*-----------------------------------------------------------------------------
PROCESS:
LOOP
    WHILE VAR.INIT LE VAR.FLD DO
	BEGIN CASE
		CASE FLAG<VAR.INIT> EQ 'NOEDIT'
			T(FLD<VAR.INIT>,LFPOSITION<VAR.INIT>,1)<3>=CONS.NOINPUT
		CASE FLAG<VAR.INIT> EQ 'NOCHANGE'
			T(FLD<VAR.INIT><LFPOSITION<VAR.INIT>>)<8>=CONS.NOCHANGE
		CASE FLAG<VAR.INIT> EQ 'NOMODIFY'
			T(FLD<VAR.INIT><LFPOSITION<VAR.INIT>>)<3>=CONS.NOMODIFY
		CASE FLAG<VAR.INIT> EQ 'EDIT'
			T((FLD<VAR.INIT>)<LFPOSITION<VAR.INIT>>)=''
	END CASE
        VAR.INIT +=1
REPEAT

RETURN
*-----------------------------------------------------------------------------
    END