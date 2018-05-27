*/---------------------------------------------------------------------\
*| This file is part of SAPlink.                                       |
*|                                                                     |
*| Copyright 2014 SAPlink project members                              |
*|                                                                     |
*| Licensed under the Apache License, Version 2.0 (the "License");     |
*| you may not use this file except in compliance with the License.    |
*| You may obtain a copy of the License at                             |
*|                                                                     |
*|     http://www.apache.org/licenses/LICENSE-2.0                      |
*|                                                                     |
*| Unless required by applicable law or agreed to in writing, software |
*| distributed under the License is distributed on an "AS IS" BASIS,   |
*| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or     |
*| implied.                                                            |
*| See the License for the specific language governing permissions and |
*| limitations under the License.                                      |
*\---------------------------------------------------------------------/
REPORT  ZSAPLINK_INSTALLER_TEMPLATE.

type-pools: seor, abap, icon.
data retFileTable type FILETABLE.
data retRc type sysubrc.
data retUserAction type i.

data tempXMLString type string.
data ixmlNugget type ref to if_ixml_document.

data pluginExists type flag.
data objectExists type flag.
data flag type flag.
data statusMsg type string.
DATA  y2all type flag.
selection-screen begin of Line.
  SELECTION-SCREEN COMMENT 1(20) fileCom FOR FIELD NUGGFIL.
  parameters NUGGFIL(300) type c modif id did obligatory.
selection-screen end of Line.

selection-screen begin of Line.
  SELECTION-SCREEN COMMENT 1(20) checkCom FOR FIELD NUGGFIL.
  parameters overwrt type c as checkbox default 'X'.
selection-screen end of Line.



start-of-selection.
clear tempXMLString.
perform uploadXMLFromLM using NUGGFIL tempXMLString.
perform CONVERTSTRINGTOIXMLDOC using tempXMLString changing ixmlNugget.
perform installNugget using ixmlNugget overwrt.


*/--------------------------------------------------------------------\
*| Selection screen events                                            |
initialization.
  fileCom = 'Installation Nugget'.
  checkCom = 'Overwrite Originals'.


at selection-screen on value-request for NUGGFIL.
  call method CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
      exporting
        MULTISELECTION = abap_false
        FILE_FILTER = '*.nugg'
        DEFAULT_EXTENSION = 'nugg'
      changing
        FILE_TABLE = retFileTable
        rc = retRc
        user_Action = retUserAction.
  read table retFileTable into NUGGFIL index 1.
  refresh retFileTable.

*\--------------------------------------------------------------------/


*/--------------------------------------------------------------------\
*| Forms from the SAPLink Installer                                   |
*|                                                                     |
form uploadXMLFromLM using p_filename xmlString type string .
  DATA retfiletable TYPE filetable.
  DATA retrc TYPE sysubrc.
  DATA retuseraction TYPE i.
  DATA temptable TYPE table_of_strings.
  DATA temptable_bin TYPE TABLE OF x255.
  DATA filelength TYPE i.
  DATA l_filename TYPE string.

  l_filename = p_filename.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'       " File Type Binary
    IMPORTING
      filelength              = filelength
    CHANGING
      data_tab                = temptable_bin
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN '1'.
        PERFORM writemessage USING 'E' 'File Open Error'.
      WHEN OTHERS.
        PERFORM writemessage USING 'E' 'Unknown Error occured'.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = filelength
    IMPORTING
      text_buffer  = xmlstring
    TABLES
      binary_tab   = temptable_bin.
  IF sy-subrc <> 0.
    " Just catch the sy-subrc when there was nothing replaced
    sy-subrc = 0.
  ENDIF.
*  call method CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
*        exporting
*          FILENAME = l_fileName
*        changing
*          data_tab = tempTable.
*  PERFORM createstring USING temptable CHANGING xmlstring.
endform.
*\--------------------------------------------------------------------/
form createString
      using
        tempTable type table_of_strings
      changing
        bigString type string.

data sTemp type string.
  loop at tempTable into sTemp.
    concatenate bigString sTemp CL_ABAP_CHAR_UTILITIES=>NEWLINE into
    bigString.
  endloop.

endform.
*/----------------------------------------------------------------------



*/--------------------------------------------------------------------\
*| Forms from the SAPLink Root Class                                  |
form CONVERTSTRINGTOIXMLDOC
      using
        i_xmlString type string
      changing
        ixmlDocument type ref to if_ixml_document.

  data xmlString type string.
  data ixml type ref to if_ixml.
  data streamFactory type ref to IF_IXML_STREAM_FACTORY.
  data iStream type ref to if_ixml_istream.
  data ixmlParser type ref to if_ixml_parser.
  data xmlDoc type ref to if_ixml_document.

  xmlString = i_xmlString.
  " Make sure to convert Windows Line Break to Unix as
  " this linebreak is used to get a correct import
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
    IN xmlString WITH cl_abap_char_utilities=>newline.

  ixml = cl_ixml=>create( ).
  xmlDoc = ixml->create_document( ).
  streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  iStream = streamFactory->CREATE_ISTREAM_STRING( xmlString ).
  iXMLParser = iXML->create_parser(  stream_factory = streamFactory
                                     istream        = iStream
                                     document       = xmlDoc ).
  iXMLParser->parse( ).
  ixmlDocument = xmlDoc.

endform.

*|                                                                     |
*|                                                                     |

FORM GETOBJECTInfoFROMIXMLDOC
      using ixmlDocument type ref to if_ixml_document
      changing objTypeName type string objName type string.
data rootNode type ref to IF_IXML_NODE.
data rootAttr type ref to IF_IXML_NAMED_NODE_MAP.
data AttrNode type ref to IF_IXML_NODE.
data nodeName type string.

  rootNode ?= ixmlDocument->GET_ROOT_ELEMENT( ).

* get object type
  objTypeName = rootNode->GET_NAME( ).
  translate objTypeName to upper case.

* get object name
  rootAttr = rootNode->GET_ATTRIBUTES( ).
  AttrNode = rootAttr->GET_ITEM( 0 ).
  objName = AttrNode->GET_VALUE( ).

ENDFORM.

*/--------------------------------------------------------------------\
*|  Nugget Class                                                      |
form     installNugget
      using xmlDoc type ref to if_ixml_document overwrite type c.
types: begin of t_objectTable,
         classname type string,
         object type ko100-object,
         text type ko100-text,
       end of t_objectTable.


data iterator type ref to IF_IXML_NODE_ITERATOR.
data ixml type ref to if_ixml.
data Namefilter type ref to IF_IXML_NODE_FILTER.
data parentFilter type ref to IF_IXML_NODE_FILTER.
data currentNode type ref to if_ixml_node.
data newNode type ref to if_ixml_node.
data rval type i.
data ixmlDocument type ref to if_ixml_document.
data _objName type string.
data objType type string.
data objectTable type table of t_objectTable.
data objectLine type t_objectTable.
data exists type flag.
data sTemp type string.
data nameCollision type flag.
data l_targetObject type ref to zsaplink.
data l_installObject type string.
data l_excClass type ref to ZCX_SAPLINK.
data tempcname type string.

  ixml = cl_ixml=>create( ).
  nameFilter = xmlDoc->create_filter_name( name = 'nugget' ).
  parentFilter = xmlDoc->create_filter_parent( nameFilter ).
  iterator = xmlDoc->create_iterator_filtered( parentFilter ).

  currentNode ?= iterator->get_next( ).
  while currentNode is not initial.
    clear exists.
    ixmlDocument = ixml->create_document( ).
    newNode = currentNode->clone( ).
    rval = ixmlDocument->append_child( newNode ).

    call method zsaplink=>GETOBJECTInfoFROMIXMLDOC
      exporting
        ixmlDocument = ixmlDocument
      importing
        objtypename = objType
        objname     = _objName.

*  call method zsaplink=>getplugins( changing objectTable = objectTable )
*.
*
*  read table objectTable into objectLine with key object = objType.
*
*  if sy-subrc = 0.

    translate objType to upper case.
    case objtype.
      when 'CLAS'.
        tempcname = 'ZSAPLINK_CLASS'.
      when 'PROG'.
        tempcname = 'ZSAPLINK_PROGRAM'.
      when others.
    endcase.

    create object l_targetObject type (tempcname)
      exporting name = _objName.

    objectExists = l_targetObject->checkexists( ).

    if objectExists = 'X' and overWrt = ''.
      write :/  objType, _objName,
      ' exists on this system , if you wish to install this Nugget '
      & 'please set the Overwrite Originals checkbox.'
          .
    elseif objectExists = 'X' and overWrt = 'X'.

      if l_targetObject is not initial.

      if y2all <> 'X'.
        concatenate objType _objName into sTemp separated by space.
        perform confirmOverwrite using sTemp
                              changing flag.
        if flag = '1'. "yes
        elseif flag = '2'. "yes to all
          y2all = 'X'.
        elseif flag = 'A'. "cancel
          write / 'Import cancelled by user'.
*          Flag = 'X'.
          exit.
        endif.
       endif.
        try.
          l_installObject = l_targetObject->createObjectfromiXMLDoc(
                                          ixmlDocument = ixmlDocument
                                          overwrite = overWrt ).

          catch ZCX_SAPLINK into l_excClass.
            statusMsg = l_excClass->get_text( ).
            Flag = 'X'.
        endtry.
        if l_installObject is not initial.
          concatenate 'Installed: ' objType l_installObject
           into statusMsg separated by space.
        endif.
      else.
        statusMsg = 'an undetermined error occured'.
        Flag = 'X'.
      endif.

    else.
        try.
          l_installObject = l_targetObject->createObjectfromiXMLDoc(
                                          ixmlDocument = ixmlDocument
                                          overwrite = overWrt ).

          catch ZCX_SAPLINK into l_excClass.
            statusMsg = l_excClass->get_text( ).
            Flag = 'X'.
        endtry.
        if l_installObject is not initial.
          concatenate 'Installed: ' objType l_installObject
           into statusMsg separated by space.
        endif.
    endif.
  currentNode ?= iterator->get_next( ).
  write: / Statusmsg.
endwhile.
endform.

*/----------------------confirmOverwrite------------------------------\
form confirmOverwrite using l_objInfo type string
                   changing l_answer type flag.

data l_message type string.
data l_title type string.

  clear l_answer.
  l_title = 'Overwrite confirm. Proceed with CAUTION!'.

  concatenate 'You have selected to overwrite originals.'
    l_objinfo 'will be overwritten. Are you sure?'
    into l_message separated by space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR                    = l_title
      text_question               = l_message
      TEXT_BUTTON_1               = 'Yes'
      TEXT_BUTTON_2               = 'Yes to all'
      DEFAULT_BUTTON              = '1'
      DISPLAY_CANCEL_BUTTON       = 'X'
    IMPORTING
      ANSWER                      = l_answer
            .
endform.
*\--------------------------------------------------------------------/
*/---------------------writeMessage-----------------------\
FORM writemessage USING value(p_type) TYPE sy-msgty
                        value(p_msg).
  CASE p_type.
    WHEN 'E' OR 'A' OR 'X'.
      WRITE / icon_led_red AS ICON.
    WHEN 'W'.
      WRITE / icon_led_yellow AS ICON.
    WHEN OTHERS.
      WRITE / icon_led_green AS ICON.
  ENDCASE.

  WRITE p_msg.
ENDFORM.                    "WriteMessage
