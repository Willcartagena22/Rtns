*-----------------------------------------------------------------------------
* <Rating>270</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.DELETE.USER.HID
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

GOSUB USER.LIST
RETURN


USER.LIST:
     DIR.NAME	= 'DELETEHID'
*    DIR.NAME	= "C:\temp"
    
    R.ID   		= "USERS.txt"
    
OPENSEQ DIR.NAME, R.ID TO FILE_VAR ELSE ABORT 201, "report file"
LOOP
READSEQ VAR_LINE FROM FILE_VAR THEN CRT VAR_LINE ELSE EXIT
GOSUB HID.USER.DELETE
REPEAT
CLOSESEQ MyPath
RETURN



HID.USER.DELETE:

	VAR_LINE		  = EXTERNAL.USER.ID			
    Y.CLASS           = 'com.mad.banco.azul.hid.t24.T24HID'
    Y.METODO          = 'borrarUsuario'
    Y.IN.PARAMETER    = EXTERNAL.USER.ID
    Y.ERROR.RESPONSE  = ''
    CALL EB.CALLJ(Y.CLASS,Y.METODO,Y.IN.PARAMETER,Y.OUT.PARAMETER,Y.ERROR.RESPONSE)
    RESPONSE          = LEFT(Y.OUT.PARAMETER,2)
    TEXTO.ARCHIVO="USER : ":EXTERNAL.USER.ID:" RESPUESTA: ":RESPONSE
    GOSUB ESCRIBIR.ARCHIVO
    
    
    RETURN
    
ESCRIBIR.ARCHIVO:
   DIR.NAME2	= 'DELETEHID'
*   DIR.NAME2	= "C:\temp"
    
    R.ID2   		= 'RESPONSE_USERS' : TODAY : '.txt'

    OPENSEQ DIR.NAME2,R.ID2 TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN
    
    
    
    
    
    
    
    
END
