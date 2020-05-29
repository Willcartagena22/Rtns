*-----------------------------------------------------------------------------
* <Rating>-64</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE EJEMPLO.PRODUCT.BUILDER
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_System
$INSERT I_AA.LOCAL.COMMON
$INSERT I_F.AA.PRODUCT.ACCESS
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
$INSERT I_AA.APP.COMMON
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.EB.EXTERNAL.USER
*-----------------------------------------------------------------------------
       GOSUB INIT
       GOSUB PROCESS
       
       RETURN
       INIT:
             FN.ARRANGEMENT = 'F.AA.ARRANGEMENT'
             F.ARRANGEMENT = ''
             CALL OPF(FN.ARRANGEMENT, F.ARRANGEMENT)
             FN.PRODUCT = 'F.AA.PRODUCT'
             F.PRODUCT = ''
             CALL OPF(FN.PRODUCT, F.PRODUCT)
             FN.EXT.US  = "F.EB.EXTERNAL.USER"
             F.EXT.US   = ""
             CALL OPF (FN.EXT.US, F.EXT.US)
             
             EQU EXT.EXTERNAL.USER TO 'EXT.EXTERNAL.USER'
             
             APPL.ARR ='AA.PRD.DES.PRODUCT.ACCESS'
             FIELDNAME.ARR       ='LF.MONEDA.AZUL':VM:'LF.MINIMUM.AZUL':VM:'LF.MAXIMUM.AZUL':VM:'LF.PERCENT.AZUL'
             CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)

       RETURN
       
       
       
       PROCESS:
***********************************************************************
* DEBUG:
*EXT.USER = 'DAVIDJR'
      EXT.PN.CUSTOMER = '101629'
*
***********************************************************************   
             ;* Obtener Nombre de usuario
*            EXT.USER = System.getVariable(EXT.EXTERNAL.USER)
*            Y.TXT = EXT.USER
*            GOSUB WRITE_LOG_FILE
             ;*Buscar Acuerdo de Banca en linea del usuario
             CALL F.READ(FN.EXT.US,EXT.USER,EXT.ISA,F.EXT.US,ERR.EXT.US)
             
             Y.ACUERDO.ISA = EXT.ISA<EB.XU.ARRANGEMENT>;*Acuerdo ISA de Banca en linea
             
             SELECT.PAGOS= "SELECT " :FN.MONEDA.AZUL:" WITH CUSTOMER EQ ":EU.CUSTOMER:
*            Y.TXT = Y.ACUERDO.ISA
*            GOSUB WRITE_LOG_FILE
             ;*Lectura de cuentas configurado actualmente en el product access 
        CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ACUERDO.ISA, 'PRODUCT.ACCESS', '', TODAY, R.ID.PROD.ACC, R.PRODUCT.ACCESS , ERR.PRODUCT.ACCESS)
             REC.PRODUCT.ACCESS = RAISE(R.PRODUCT.ACCESS) 
             Y.LF.MONEDA.AZUL=REC.PRODUCT.ACCESS<AA.PRODA.LOCAL.REF><1,POS.ARR<1,1>>
*            CRT 'Y.LF.MONEDA.AZUL':' ':Y.LF.MONEDA.AZUL
*            Y.TXT = 'Y.LF.MONEDA.AZUL':' ': Y.LF.MONEDA.AZUL
*            GOSUB WRITE_LOG_FILE
             Y.LF.MINIMUM.AZUL=REC.PRODUCT.ACCESS<AA.PRODA.LOCAL.REF><1,POS.ARR<1,2>>
*            CRT 'Y.LF.MINIMUM.AZUL':' ': Y.LF.MINIMUM.AZUL
*            Y.TXT = 'Y.LF.MINIMUM.AZUL':' ': Y.LF.MINIMUM.AZUL
*            GOSUB WRITE_LOG_FILE
             Y.LF.MAXIMUM.AZUL=REC.PRODUCT.ACCESS<AA.PRODA.LOCAL.REF><1,POS.ARR<1,3>>
*            CRT 'Y.LF.MAXIMUM.AZUL':' ': Y.LF.MAXIMUM.AZUL
*            Y.TXT = 'Y.LF.MAXIMUM.AZUL':' ': Y.LF.MAXIMUM.AZUL
*            GOSUB WRITE_LOG_FILE
             Y.LF.PERCENT.AZUL=REC.PRODUCT.ACCESS<AA.PRODA.LOCAL.REF><1,POS.ARR<1,4>>
*            CRT 'Y.LF.PERCENT.AZUL':' ': Y.LF.PERCENT.AZUL
*            Y.TXT = 'Y.LF.PERCENT.AZUL':' ': Y.LF.PERCENT.AZUL
*            GOSUB WRITE_LOG_FILE
             
             GOSUB GET_ENQUIRY_DATA

       RETURN
;* Informacion a retornar
GET_ENQUIRY_DATA:
       ;* Construccion del arreglo
       STR.PD =''
       STR.PD := Y.LF.MONEDA.AZUL : "*" ;*1
       STR.PD := Y.LF.MINIMUM.AZUL : "*" ;*2
       STR.PD := Y.LF.MAXIMUM.AZUL : "*" ;*3
       STR.PD := Y.LF.PERCENT.AZUL  ;*4
       
*      Y.TXT ='STR.PD = ' : STR.PD
*      GOSUB WRITE_LOG_FILE
*      CRT Y.TXT
       ENQ.DATA<-1> = STR.PD
*      Y.TXT = 'ENQ.DATA = ' ENQ.DATA
     CRT 'STR.PD': STR.PD
*      GOSUB WRITE_LOG_FILE
RETURN

;* Archivo para Revisión de Errores
*WRITE_LOG_FILE:
*      DIR.NAME = 'CHQ.OUT'
*      R.ID =  'ps_' : TODAY : '.txt'
*
*      OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
*            WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
*            END
*      CLOSESEQ SEQ.PTR 
*RETURN
END
