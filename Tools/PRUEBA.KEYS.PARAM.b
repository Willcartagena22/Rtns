*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PRUEBA.KEYS.PARAM
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.COMISIONES.TARJETAS
$INSERT I_F.ACCOUNT
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.EB.SLV.KEYS.PARAMS

    
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB OPEN
GOSUB PROCESS


INIT:

 FN.KP='F.EB.SLV.KEYS.PARAMS'
 F.KP=''

RETURN

OPEN:
CALL OPF(FN.KP,F.KP)

RETURN

PROCESS:
PARAMETRO='COLECTOR.CINTEX'
	   	
	FN.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.KEYS.PARAM = ''

    CALL OPF(FN.KEYS.PARAM,F.KEYS.PARAM)
    CALL F.READ(FN.KEYS.PARAM,PARAMETRO,RECORD.KEYS.PARAM,F.KEYS.PARAM,ERR.KEYS.PARAM)

    Y.PARAM.ID = RECORD.KEYS.PARAM<EB.SLV18.PARAM.ID>
    CRT 'Y.PARAM.ID LISTA ':Y.PARAM.ID
    FINDSTR '02' IN FIELD(Y.PARAM.ID, SM, 1) SETTING Ap, Vp THEN
        Y.PARAM.ID = FIELD(RECORD.KEYS.PARAM<EB.SLV18.PARAM.ID>,VM,Vp)
        Y.DESCRIP = FIELD(RECORD.KEYS.PARAM<EB.SLV18.DESCRIPCION>,VM,Vp)
        Y.VALOR = FIELD(RECORD.KEYS.PARAM<EB.SLV18.VALOR>,VM,Vp)
        
        
		CRT 'Y.DESCRIP ':Y.DESCRIP
		CRT 'VALOR :':Y.VALOR



    END
	
   
    END
	   	

RETURN





END
