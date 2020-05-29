*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.OFS.UTIL.OL.TRX(TRANS.ID,Y.RECORD,ID.PARAM.OFS,Y.OUT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*@date 20170216
*@autor eurias
*@util rutina para ejecutar un OFS en linea, semejante a la rutina SLV.OFS.UTIL.TRX la diferencia radica en la ejecucion del OFS este realiza en linea
*@parametros Entrada: 
*TRANS.ID: id de registro, si el registro ya existe o se requiere modificar se envia el id de registro
*Si el registro aun no existe y la aplicacion tiene generado automatico de ids se envia vacio, p.e para un FT nuevo se envia vacio.
*Si es una reversa se debe enviar el id del registro a reversar
*Y.RECORD: Record registro que se enviara para ingreso update. 
*ID.PARAM.OFS: Id de registro de la aplicacion EB.SLV.OFS.PARAM.BUILD que contiene la informacion del ofs a enviar, funcion, estado,version, n. autorizaciones
*Y.OUT: Respuesta de ejecucion OFS  
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_GTS.COMMON
$INSERT I_F.EB.SLV.OFS.PARAM.BUILD
*-----------------------------------------------------------------------------

	GOSUB INICIALIZAR ;*Initialize the necessary variable
    GOSUB OPF ;*Open the necessary file
    GOSUB LOGICA ;*Call the ofs bulk manager 

    RETURN
*-----------------------------------------------------------------------------

*** <region name= INICIALIZAR>
INICIALIZAR:
*** <desc>Initialize the necessary variable </desc>
    FN.EB.SLV.OFS.PARAM.BUILD = 'F.EB.SLV.OFS.PARAM.BUILD'
    F.EB.SLV.OFS.PARAM.BUILD  = ''
;*		TMP.ID=OFS$SOURCE.ID
 ;*       TMP.REC=OFS$SOURCE.REC
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
        Y.ESTADO            = R.OFS.PARAM<EB.SLV68.ESTADO.PARAM>
        PROC 				= 'PROCESS' ;*TODO: enviar parametrizado por si se requiere otra funcion p.e. VALIDATE
        Y.GTS.MODE 			= ''
        R.OFS.RECORD 		= ''
        OFS.ID 				= ''
        OFS.ID.LOG 			= ''
    
       Y.OPTIONS = OFS.SRC
        
	IF Y.ESTADO EQ 'A' THEN;*si el recurso param build esta activo
	    CALL OFS.BUILD.RECORD(Y.APP,Y.FUNCTION,PROC,OFSVERSION,Y.GTS.MODE,NO.OF.AUTH,TRANS.ID,Y.RECORD,R.OFS.RECORD)
        OFS.MSG = R.OFS.RECORD
        IF OFS.MSG THEN
            OFS.ID.LOG = ''
            ;*OFS.MSG ="EB.SLV.GLOBAL.PARAM,TEST/I/PROCESS///,,TEST.2017,DESC.PARAM:1:1=HOLA"
            ;*OFS.MSG = 'DATES,SLV.UPDATE/I/PROCESS///,,':'SV0010001':',FORW.VALUE.MAXIMUM:1:1=':'20170101'
            ;*CALL OFS.BULK.MANAGER(OFS.MSG,RESPONSE,TXN)
            ;*Y.OUT = RESPONSE:FM:TXN
            ;*CALL ofs.addLocalRequest(OFS.MSG,"APPEND",Y.ERROR)
            ;*CALL OFS.CALL.BULK.MANAGER(options, theRequest, theResponse,TXN.RESULT)
            CALL OFS.CALL.BULK.MANAGER(Y.OPTIONS, OFS.MSG, Y.RESPONSE,TXN.RESULT)
        	Y.OUT = TXN.RESULT:FM:Y.RESPONSE
         END
      END ELSE
      	ETEXT = 'EB-OFS.PAR.BUIL.INA':FM:ID.PARAM.OFS
      	CALL STORE.END.ERROR
      END
    END
    
    RETURN

END
