*-----------------------------------------------------------------------------
* <Rating>-39</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.UTIL.OFS.TRX(TRANS.ID,Y.RECORD,ID.PARAM.OFS,Y.OUT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*@date 20160810
*@autor eurias
*@util rutina para lanzar ofs de forma generica
*@param in TRANS.ID: Id de registro existente, si es registro nuevo NULL
*@param in Y.RECORD: Registro a enviar en ofs en formato de aplicacion que se desea enviar
*@param in ID.PARAM.OFS: id de registro en aplicacion EB.SLV.OFS.PARAM.BUILD donde se establecen los parametros de OFS
*@param Y.OUT: id aplicacion de logs OFS F.OFS.MESSAGE.QUEUE
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.OFS.PARAM.BUILD


    GOSUB INICIALIZAR ;*Initialize the necessary variable
    GOSUB OPF ;*Open the necessary file
    GOSUB LOGICA ;*Call the ofs post message

    RETURN
*-----------------------------------------------------------------------------

*** <region name= INICIALIZAR>
INICIALIZAR:
*** <desc>Initialize the necessary variable </desc>
    FN.EB.SLV.OFS.PARAM.BUILD = 'F.EB.SLV.OFS.PARAM.BUILD'
    F.EB.SLV.OFS.PARAM.BUILD  = ''

    RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= OPF>
OPF:
*** <desc>Open the necessary file </desc>
    CALL OPF (FN.EB.SLV.OFS.PARAM.BUILD, F.EB.SLV.OFS.PARAM.BUILD)

    RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= LOGICA>
LOGICA:
*** <desc>Call the ofs post message </desc>

    CALL CACHE.READ(FN.EB.SLV.OFS.PARAM.BUILD,ID.PARAM.OFS,R.OFS.PARAM,ERR.EB.SLV.OFS.PARAM.BUILD)
    IF R.OFS.PARAM THEN
        OFS.SRC 			= R.OFS.PARAM<EB.SLV68.OFS.SOURCE>
        OFSVERSION 			= R.OFS.PARAM<EB.SLV68.OFS.VERSION>
        NO.OF.AUTH 			= R.OFS.PARAM<EB.SLV68.NO.AUTH>
        Y.FUNCTION			= R.OFS.PARAM<EB.SLV68.FUNCION>
        Y.APP				= R.OFS.PARAM<EB.SLV68.APLICACION>
        PROC 				= 'PROCESS'
        Y.GTS.MODE 			= ''
        R.OFS.RECORD 		= ''
        OFS.ID 				= ''
        OFS.ID.LOG 			= ''
        CALL OFS.BUILD.RECORD(Y.APP,Y.FUNCTION,PROC,OFSVERSION,Y.GTS.MODE,NO.OF.AUTH,TRANS.ID,Y.RECORD,R.OFS.RECORD)
        OFS.MSG = R.OFS.RECORD
        IF OFS.MSG THEN
        CRT OFS.MSG 
            OFS.ID.LOG = ''
            CRT 'OFS.MSG     :' : OFS.MSG 
            CALL OFS.POST.MESSAGE (OFS.MSG, OFS.ID.LOG, OFS.SRC, ERR.OFS.PAY)
            Y.OUT = OFS.ID.LOG
        END
    END
    RETURN
*** </region>

    END
