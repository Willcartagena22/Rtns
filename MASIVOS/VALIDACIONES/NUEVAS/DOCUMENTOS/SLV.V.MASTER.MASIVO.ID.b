*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.MASTER.MASIVO.ID
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.MASTER.MASIVOS
    $INSERT I_F.EB.SLV.GENERACION.DOC
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB PROCESS
    RETURN

INIT:
    FN.MASTER.MASIVOS = 'F.EB.SLV.MASTER.MASIVOS'
    F.MASTER.MASIVOS  = ''
    EQU ESTADO.MASTER.MASIVOS TO 'CUENTAS.CREADAS'
    RETURN

OPEN.FILE:
    CALL OPF(FN.MASTER.MASIVOS,F.MASTER.MASIVOS)
    RETURN

PROCESS:
;*MASTER.MASIVOS.ID = R.NEW(EB.SLV69.MASTER.MASIVO.ID)
    MASTER.MASIVOS.ID  = COMI
;*PBTENEMOS EL DETALLE DEL REGISTRO EN LA APLICACION EB.SLV.MASTER.MASIVOS
    CALL F.READ(FN.MASTER.MASIVOS,MASTER.MASIVOS.ID,MASTER.MASIVOS.R,F.MASTER.MASIVOS,MASTER.MASIVOS.ERR)
    MASTER.MASIVOS.COMPANY = MASTER.MASIVOS.R<EB.SLV32.AGENCIA>
    MASTER.MASIVOS.ESTADO  = MASTER.MASIVOS.R<EB.SLV32.ESTADO>
    BEGIN CASE
        CASE MASTER.MASIVOS.COMPANY NE ID.COMPANY
            ETEXT = 'ID master masivos ':MASTER.MASIVOS.ID:', no pertenece a su agencia.'
            CALL STORE.END.ERROR
        CASE MASTER.MASIVOS.ESTADO NE ESTADO.MASTER.MASIVOS
            ETEXT = 'Estado ':MASTER.MASIVOS.ESTADO:', no valido.'
            CALL STORE.END.ERROR
        CASE 1
            R.NEW(EB.SLV69.NOMBRE)   = MASTER.MASIVOS.R<EB.SLV32.NOMBRE>
            R.NEW(EB.SLV69.ESTADO)   = MASTER.MASIVOS.ESTADO
            R.NEW(EB.SLV69.AGENCIA)  = MASTER.MASIVOS.COMPANY
            R.NEW(EB.SLV69.CLIENTES) = MASTER.MASIVOS.R<EB.SLV32.CLIENTES>
            R.NEW(EB.SLV69.CUENTAS)  = MASTER.MASIVOS.R<EB.SLV32.CUENTAS>
    END CASE

    RETURN

    END
