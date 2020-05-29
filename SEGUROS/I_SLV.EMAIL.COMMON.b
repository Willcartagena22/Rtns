
*-----------------------------------------------------------------------------
* TAM: EMail Common File : 
*-----------------------------------------------------------------------------
* Allows to keep constant for create and convert Email on INFO Basis Format to XML (T24EmailExt.xsd)
*-----------------------------------------------------------------------------
* Constants
* MAIL.FIRST.LINE                  First Line into XML File
* MAIL.SECOND.LINE                 Second Line into XML File  
*-----------------------------------------------------------------------------
EQU E_MAIL.FIRST.LINE  TO '<?xml version="1.0" encoding="UTF-8"?>'
EQU E_MAIL.SECOND.LINE TO '<T24MailPackage xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="T24Email.xsd">'
*-----------------------------------------------------------------------------
EQU E_MAIL.ID         TO 1   ;* Email Identifier will be transform to <Email id="identifier">
EQU E_MAIL.PASSWORD   TO 2   ;* When Authentication is needed, password is required <Email password="xxxxxxxx">
EQU E_MAIL.FROM       TO 3   ;* From Address
EQU E_MAIL.REPLYTO    TO 4   ;* Reply   
EQU E_MAIL.TO         TO 5   ;* To Address
EQU E_MAIL.CC         TO 6   ;* Copy To Address
EQU E_MAIL.BCC        TO 7   ;* Copy To Address
EQU E_MAIL.TYPE       TO 8   ;* Type of Message (text ot text/html)
EQU E_MAIL.SUBJECT    TO 9   ;* Mail Sibject
EQU E_MAIL.BODY       TO 10  ;* Mail Body
EQU E_MAIL.ATTACHMENT TO 11  ;* Attachament 
