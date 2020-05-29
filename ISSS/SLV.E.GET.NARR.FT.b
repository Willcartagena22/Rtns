*-----------------------------------------------------------------------------
* <Rating>544</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.E.GET.NARR.FT
*-----------------------------------------------------------------------------
* Nombre: SLV.E.GET.NARR.FT
* Referencia: Id FT
* Descripcion: Rutina para obtener Narrative de tabla EB.KEYS.PARAMS y mostrar resultaro en enquiry para reportes FT.REPORT.TC y FT.REPORT.TC.HIST
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	10.01.17	Version inicial
*----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_F.EB.SLV.COL.FRONT.END
*-----------------------------------------------------------------------------

    GOSUB INIT
;*GOSUB OPEN.FILE
    GOSUB PROCESS

*CRT Y.NARRATIVE
    RETURN

INIT:
    FN.FT	= 'F.FUNDS.TRANSFER'
    F.FT	= ''
    CALL OPF(FN.FT,F.FT)

    FN.FT.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FT.NAU  = ''
    CALL OPF(FN.FT.NAU, F.FT.NAU)

    FN.FT$HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FT$HIS  = ''
    CALL OPF(FN.FT$HIS, F.FT$HIS)

    FN.TXN.TYPE  = 'F.FT.TXN.TYPE.CONDITION'
    F.TXN.TYPE   = ''
    CALL OPF(FN.TXN.TYPE, F.TXN.TYPE)

    FN.EB.KEYS.PARAM = 'F.EB.SLV.KEYS.PARAMS'
    F.EB.KEYS.PARAM	 = ''
    CALL OPF(FN.EB.KEYS.PARAM, F.EB.KEYS.PARAM)

    FN.EB.SLV.COLECTOR = 'F.EB.SLV.COLECTOR'
    F.EB.SLV.COLECTOR	 = ''
    CALL OPF(FN.EB.SLV.COLECTOR, F.EB.SLV.COLECTOR)
    
    FN.EB.SLV.FRONT.END = 'F.EB.SLV.COL.FRONT.END'
    F.EB.SLV.FRONT.END  = ''
    CALL OPF(FN.EB.SLV.FRONT.END,F.EB.SLV.FRONT.END)
    
*  	Debug:
	Y.ID.FT	= 'FT1522106HQV;1'
*    Y.ID.FT = O.DATA
    EQU TRX.PAGO.COLECTOR TO 'AC64'
    Y.NARRATIVE = ''
    RETURN

PROCESS:
    Y.ID.FT = FIELD(Y.ID.FT,';',1)
    CALL F.READ(FN.FT, Y.ID.FT, FT.RECORD, F.FT, Y.ERR)

    IF Y.ERR NE '' THEN
        Y.FT.NAU.ERR = ""
        CALL F.READ(FN.FT.NAU, Y.ID.FT, FT.RECORD, F.FT.NAU, Y.FT.NAU.ERR)
        IF Y.FT.NAU.ERR THEN

            CALL F.READ.HISTORY(FN.FT$HIS, Y.ID.FT, FT.RECORD, F.FT$HIS, ERR.HIS)
            CALL F.READ(FN.TXN.TYPE, FT.RECORD<FT.TRANSACTION.TYPE>, TXN.TYPE.REC, F.TXN.TYPE, Y.TXN.TYPE.ERR)
        END ELSE
            CALL F.READ(FN.TXN.TYPE, FT.RECORD<FT.TRANSACTION.TYPE>, TXN.TYPE.REC, F.TXN.TYPE, Y.TXN.TYPE.ERR)
        END
    END ELSE
        CALL F.READ(FN.TXN.TYPE, FT.RECORD<FT.TRANSACTION.TYPE>, TXN.TYPE.REC, F.TXN.TYPE, Y.TXN.TYPE.ERR)
    END

;*Obtener LF de Banca Empresas para FT
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.TCE.NARR', POS.LF.TEC.NARR)
    LF.TCE.NARR		= FT.RECORD<FT.LOCAL.REF, POS.LF.TEC.NARR>
;*Tipo de Txn
    Y.TRAN.TYP		= FT.RECORD<FT.TRANSACTION.TYPE>

    IF LF.TCE.NARR NE '' THEN ;*Si la Txn viene desde Banca Empresas.
        IF Y.TRAN.TYP EQ 'AC64' THEN
        
        CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ID.COL.MOD',POS.LF.ID.COL.MOD)
        ID.APP.LOCAL.COLECTOR = FT.RECORD<FT.LOCAL.REF,POS.LF.ID.COL.MOD>
        
        IF ID.APP.LOCAL.COLECTOR NE '' THEN
           CALL F.READ(FN.EB.SLV.FRONT.END,ID.APP.LOCAL.COLECTOR,RESPONSE.APP.LOCAL,F.EB.SLV.FRONT.END,ERR.FRONT.END)
           ID.COLECTOR.EXTERNO = RESPONSE.APP.LOCAL<EB.SLV8.RESERVADO.3>
           GOSUB GET.INFORMATION.TRX.EXT
           NOMBRE.COLECTOR = SERVICES.RESPONSE
        END ;* END IF ID.APP.LOCAL.COLECTOR
        ELSE
            CALL GET.LOC.REF('FUNDS.TRANSFER','LF.COD.CL', POS.LF.ID.COL)
            ID.COL.PEX = FT.RECORD<FT.LOCAL.REF, POS.LF.ID.COL>
            
            CALL F.READ(FN.EB.SLV.COLECTOR,ID.COL.PEX,RESPUESTA.COLECTOR.LOCAL, F.EB.SLV.COLECTOR , ERR.COLECTOR)
            
            IF ERR.COLECTOR EQ '' THEN
               NOMBRE.COLECTOR = RESPUESTA.COLECTOR.LOCAL<EB.CL.NOMBRE.COLECTOR>
            END
            ELSE
               NOMBRE.COLECTOR = 'Colector'
            END
        END ;*END ELSE ID.APP.LOCAL.COLECTOR
           Y.NARRATIVE = 'Pago de ':NOMBRE.COLECTOR:' - Empresas'
        END ;*END IF Y.TRAN.TYP
    ELSE
    ID.KEYS.PARAMS 	= 'SUB.TXN.TYPE' ;*id con Narratives parametrizados en EB.KEYS.PARAMS
    CALL CACHE.READ(FN.EB.KEYS.PARAM, ID.KEYS.PARAMS, REC.SUBTYPE.TXN, KEYS.PARAM.ER)

    IF REC.SUBTYPE.TXN THEN
        ;*Se forma el Id para traer Narrative en app KEYS.PARAM dentro de registro SUB.TXN.TYPE
        ID.SUB.TYPE 	= Y.TRAN.TYP : '.' : LF.TCE.NARR
        ;*Se lee ids de app Local
        ID.KEYS.PARAM 	= REC.SUBTYPE.TXN<EB.SLV18.PARAM.ID>
        ;*Se busca posición del id para leer narrative
        FIND ID.SUB.TYPE IN ID.KEYS.PARAM SETTING POS.SUB.TYPE, POS.SUB.TYPE2 THEN
        ;* Si tiene descripcion ES
        IF DCOUNT(TXN.TYPE.REC<FT6.DESCRIPTION>, VM) GT 1 THEN
            Y.NARRATIVE =  TXN.TYPE.REC<FT6.DESCRIPTION><1,1> : " " : REC.SUBTYPE.TXN<EB.SLV18.VALOR><1,POS.SUB.TYPE2>
        END ELSE
            Y.NARRATIVE =  TXN.TYPE.REC<FT6.DESCRIPTION> : " " : REC.SUBTYPE.TXN<EB.SLV18.VALOR><1,POS.SUB.TYPE2>
        END
    END
    END
;*Y.NARRATIVE = '--'
    END ;* END ELSE Y.TRAN.TYP

    END
    ELSE ;*Si la Txn viene desde Banca Personas.
    IF Y.TRAN.TYP EQ 'AC64' THEN
        
        CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ID.COL.MOD',POS.LF.ID.COL.MOD)
        ID.APP.LOCAL.COLECTOR = FT.RECORD<FT.LOCAL.REF,POS.LF.ID.COL.MOD>
        
        IF ID.APP.LOCAL.COLECTOR NE '' THEN
           CALL F.READ(FN.EB.SLV.FRONT.END,ID.APP.LOCAL.COLECTOR,RESPONSE.APP.LOCAL,F.EB.SLV.FRONT.END,ERR.FRONT.END)
           ID.COLECTOR.EXTERNO = RESPONSE.APP.LOCAL<EB.SLV8.RESERVADO.3>
           GOSUB GET.INFORMATION.TRX.EXT
           NOMBRE.COLECTOR = SERVICES.RESPONSE
        END ;* END IF ID.APP.LOCAL.COLECTOR
        ELSE
            CALL GET.LOC.REF('FUNDS.TRANSFER','LF.COD.CL', POS.LF.ID.COL)
            ID.COL.PEX = FT.RECORD<FT.LOCAL.REF, POS.LF.ID.COL>
            
            CALL F.READ(FN.EB.SLV.COLECTOR,ID.COL.PEX,RESPUESTA.COLECTOR.LOCAL, F.EB.SLV.COLECTOR , ERR.COLECTOR)
            
            IF ERR.COLECTOR EQ '' THEN
               NOMBRE.COLECTOR = RESPUESTA.COLECTOR.LOCAL<EB.CL.NOMBRE.COLECTOR>
            END
            ELSE
               NOMBRE.COLECTOR = 'Colector'
            END
        END ;*END ELSE ID.APP.LOCAL.COLECTOR
             Y.NARRATIVE = 'Pago de ':NOMBRE.COLECTOR:' - Persona'
    END ;*END IF Y.TRAN.TYP
    ELSE
    IF DCOUNT(TXN.TYPE.REC<FT6.DESCRIPTION>, VM) GT 1 THEN
        Y.NARRATIVE =  TXN.TYPE.REC<FT6.DESCRIPTION><1,2>
    END
    ELSE
    Y.NARRATIVE =  TXN.TYPE.REC<FT6.DESCRIPTION>
    END
    END ;*END ELSE Y.TRAN.TYP
    END
    CRT 'NARRATIVE > ':Y.NARRATIVE
    O.DATA = Y.NARRATIVE
    RETURN


;*@Author: Ronald Ortiz
;*@Date: 01/06/2017
;*@Description: Thread to get the name of the Collector from dbCollectors database
GET.INFORMATION.TRX.EXT:
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "getCollectorName"
    CALLJ.ARGUMENTS.CLT = ID.COLECTOR.EXTERNO
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA.SERVICIO.EXTERNO = CHANGE(CALLJ.RESPONSE.CLT,'"','')
    
    IF LEFT(RESPUESTA.SERVICIO.EXTERNO,4) EQ '|ERR' THEN
      SERVICES.RESPONSE = 'Colector'
    END
    ELSE
      SERVICES.RESPONSE = RESPUESTA.SERVICIO.EXTERNO
    END
    RETURN

    END
