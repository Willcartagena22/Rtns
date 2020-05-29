*-----------------------------------------------------------------------------
* <Rating>-64</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.E.GENERAR.DOCUMENTOS.MASIVOS(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.EB.SLV.MASTER.MASIVOS
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_DAS.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_DAS.EB.EXTERNAL.USER
    
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB GET.FILTER.VALUE
    GOSUB PROCESS
    RETURN

INIT:
    FN.ITEMS.MASIVOS  = 'F.EB.SLV.ITEMS.MASIVOS'
    F.ITEMS.MASIVOS   = ''
    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT  = ''
    FN.MASTER.MASIVOS = 'F.EB.SLV.MASTER.MASIVOS'
    F.MASTER.MASIVOS  = ''
    FN.ARR='F.AA.ARRANGEMENT.ACTIVITY'
    F.ARR=''
    EQU ESTADO.MASTER.MASIVOS TO 'CUENTAS.CREADAS'
    RETURN

OPEN.FILE:
    CALL OPF(FN.ITEMS.MASIVOS,F.ITEMS.MASIVOS)
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
    CALL OPF(FN.MASTER.MASIVOS,F.MASTER.MASIVOS)
    CALL OPF(FN.ARR,F.ARR)
    RETURN

GET.FILTER.VALUE:
    LOCATE "MASTER.ID" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
    ID.CARGA.MASIVA = D.RANGE.AND.VALUE<PAR.POS.1>
    END
    RETURN

PROCESS:
    
    ;*OBTENEMOS EL DETALLE DEL REGISTRO EN LA APLICACION EB.SLV.MASTER.MASIVOS
    CALL F.READ(FN.MASTER.MASIVOS,ID.CARGA.MASIVA,R.MASTER,F.MASTER.MASIVOS,ERR.MASTER)
    
    IF R.MASTER<EB.SLV32.ESTADO> NE ESTADO.MASTER.MASIVOS THEN
      RETURN
    END
    
    STMT.ITEMS.MASIVOS = "SELECT ":FN.ITEMS.MASIVOS:" WITH @ID LIKE '":ID.CARGA.MASIVA:"...' "
    CALL EB.READLIST (STMT.ITEMS.MASIVOS, KEY.LIST, '', ITEMS.COUNT, SYSTEM.RETURN.CODE)

    LOOP
        REMOVE ITEM.ID FROM KEY.LIST SETTING POS.ID
    WHILE ITEM.ID NE ''
        CALL F.READ(FN.ITEMS.MASIVOS,ITEM.ID,R.ITEM,F.ITEMS.MASIVOS,ITEM.ERR)
        ARRANGEMENT.ID = R.ITEM<EB.SLV3.ACCOUNT>
        CUSTOMER.ID    = R.ITEM<EB.SLV3.ID.CUSTOMER>
        ;*OBTENEMOS EL DETALLE DEL ARRANGEMENT
        CALL F.READ(FN.AA.ARRANGEMENT,ARRANGEMENT.ID,R.ARRANGEMENT,F.AA.ARRANGEMENT,ERR.ARRANGEMENT)
        
        ;*Generemos los documentos Declaración Jurada y Perfil del cliente
        ACCOUNT.NUMBER = R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
        CALL SLV.E.NOF.ACC.DJ.CRG.MSV(CUSTOMER.ID,ACCOUNT.NUMBER,ITEM.ID,ID.CARGA.MASIVA)
        
               ;*Generar ContratoBanca
*        GOSUB OBTENER.ARR.BANCA
*        NOM.CONT='CBMSV-':ID.CARGA.MASIVA:'-':CUSTOMER.ID
*                
*        IF AA.ARR.ID NE '' THEN
*	        IF AA.ARR.ID THEN     
*	                
**	    	CALL SLV.E.NOF.TC.CONTRATO.MAS(AA.ARR.ID,NOM.CONT)    
*	        	        
*	        END
*	        
*        END
        
    REPEAT
    A.INFO = 'OK':'*':'OK':'*':'OK'
    RETURN
    
    
*  OBTENER.ARR.BANCA:
*    THE.ARGS = CUSTOMER.ID
*    SEL.LIST = DAS$CUSARRANGEMENT
*    CALL DAS('AA.ARRANGEMENT.ACTIVITY',SEL.LIST,THE.ARGS,TABLE.SUFFIX)
*    IF SEL.LIST THEN
*        CALL F.READ(FN.ARR,SEL.LIST,REC.ARR,F.ARR,REC.ERROR)
*        IF REC.ARR THEN
*            ;*Id Isa
*            AA.ARR.ID=REC.ARR<AA.ARR.ACT.ARRANGEMENT>
*            ;*Estado del Acuerdo de banca en linea
*            Y.STATUS.ARR = REC.ARR<AA.ARR.ACT.RECORD.STATUS>
*            ;* Verificar el estado del Acuerdo de banca en línea para el cliente.
*            IF Y.STATUS.ARR EQ 'INAU' THEN
*                Y.MSG.ERROR = 'EB-SLV.ARR.STATUS'
*                AA.ARR.ID=''
*                
*            END
*      END
*     TEXTO.ARCHIVO='AA.ARR.ID : ':AA.ARR.ID:'Y.STATUS.ARR : ':Y.STATUS.ARR
*     GOSUB  ESCRIBIR.ARCHIVO
*  RETURN
*    
*    
*ESCRIBIR.ARCHIVO:
*	    DIR.NAME= 'MASIVOS'
*	    R.ID   = 'Cuentas.txt'
*	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
*	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
*	    END
*	    CLOSESEQ SEQ.PTR
*    RETURN

    END
