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
REPORT  zsaplink_steamroller.

TYPES: BEGIN OF t_class,
         class TYPE seoclsname,
       END OF t_class.

DATA progobject TYPE REF TO zsaplink_program.
DATA progobjectnew TYPE REF TO zsaplink_program.
DATA progname TYPE string.
DATA prognamenew TYPE string.
DATA ixmlprog TYPE REF TO if_ixml_document.
DATA ixmlprognew TYPE REF TO if_ixml_document.
DATA rootnode TYPE REF TO if_ixml_element.
DATA sourcenode TYPE REF TO if_ixml_element.
DATA definitionnode TYPE REF TO if_ixml_element.
DATA implementationnode TYPE REF TO if_ixml_element.

DATA reportfound TYPE flag.
DATA headfound TYPE flag.
DATA sourcestring TYPE string.
DATA sourcetableold TYPE table_of_strings.
DATA sourcetabletemp TYPE table_of_strings.
DATA definitiontabletemp TYPE table_of_strings.
DATA implementationtabletemp TYPE table_of_strings.
DATA sourcetablehead TYPE table_of_strings.
DATA sourcetablebody TYPE table_of_strings.
DATA sourcetablenew TYPE table_of_strings.
DATA sourceline TYPE string.
DATA tempstring TYPE string.
DATA excclass TYPE REF TO zcx_saplink.
DATA errormsg TYPE string.

DATA classtable TYPE STANDARD TABLE OF t_class.
DATA classline TYPE t_class.
DATA classobject TYPE REF TO zsaplink_class.
DATA ixmlclass TYPE REF TO if_ixml_document.
DATA ixmlclasslocal TYPE REF TO if_ixml_document.

DATA installobject TYPE string.
DATA rc TYPE sysubrc.
DATA sourcestringnew TYPE string.

DATA: it_strings TYPE table_of_strings,
      wa_strings TYPE string.



SELECTION-SCREEN BEGIN OF BLOCK main WITH FRAME TITLE text-man.
PARAMETERS prog TYPE progname.
PARAMETERS fltprg TYPE progname.
PARAMETERS overwr TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK main.

SELECT-OPTIONS: so_clas FOR classline-class NO INTERVALS.

DATA wa_class LIKE LINE OF so_clas.

INITIALIZATION.
  prog            = 'ZSAPLINK_INSTALLER_TEMPLATE'.
  fltprg          = 'ZSAPLINK_INSTALLER'.
  so_clas-sign    = 'I'.
  so_clas-option  = 'EQ'.
  so_clas-low     = 'ZCX_SAPLINK'.      APPEND so_clas.
  so_clas-low     = 'ZSAPLINK'.         APPEND so_clas.
  so_clas-low     = 'ZSAPLINK_OO'.      APPEND so_clas.
  so_clas-low     = 'ZSAPLINK_CLASS'.   APPEND so_clas.
  so_clas-low     = 'ZSAPLINK_PROGRAM'. APPEND so_clas.

END-OF-SELECTION.
* get original program as XML
  progname = prog.
  prognamenew = fltprg.

  TRY.
      CREATE OBJECT progobject EXPORTING name = progname.
      ixmlprog = progobject->createixmldocfromobject( ).
    CATCH zcx_saplink INTO excclass.
      errormsg = excclass->get_text( ).
      WRITE errormsg.
      EXIT.
  ENDTRY.

* extract source from xml
  rootnode = ixmlprog->find_from_name( 'PROG' ).
  sourcenode = rootnode->find_from_name( 'source' ).
  sourcestring = sourcenode->get_value( ).
  SPLIT sourcestring AT cl_abap_char_utilities=>newline
  INTO TABLE sourcetableold.

* split source at header and body of code to insert new code
  LOOP AT sourcetableold INTO sourceline.
    IF headfound = 'X'.
      APPEND LINES OF sourcetableold FROM sy-tabix TO sourcetablebody.
      EXIT.
    ELSEIF reportfound = 'X'.
      IF sourceline CA '.' AND sourceline(1) <> '*'.
        headfound = 'X'.
      ENDIF.
      APPEND sourceline TO sourcetablehead.
    ELSEIF sourceline CP '*REPORT*' AND sourceline(1) <> '*'.
      IF sourceline CA '.'.
        headfound = 'X'.
      ELSE.
        reportfound = 'X'.
      ENDIF.
      " Replace original program name with new program name
      REPLACE FIRST OCCURRENCE OF progname IN sourceline WITH prognamenew.
      APPEND sourceline TO sourcetablehead.
    ELSE.
      APPEND sourceline TO sourcetablehead.
    ENDIF.
  ENDLOOP.



* temp solution for getting classes to convert
  LOOP AT so_clas INTO wa_class WHERE sign = 'I' AND option = 'EQ'.
    classline-class = wa_class-low.
    APPEND classline TO classtable.
  ENDLOOP.

* add original header to new source
  APPEND LINES OF sourcetablehead TO sourcetablenew.
  CLEAR sourceline.
  APPEND sourceline TO sourcetablenew.

* steamroll this bad boy
  LOOP AT classtable INTO classline.
    FREE: classobject, ixmlclass, ixmlclasslocal,
          rootnode, sourcenode.
    CLEAR: tempstring, sourcetabletemp[].
    tempstring = classline-class.
*   get definitions of global classes as XML
    CREATE OBJECT classobject EXPORTING name = tempstring.
    classobject->mv_steamroller = abap_true.
    ixmlclass = classobject->createixmldocfromobject( ).
*   call class to convert global class XML to local source code XML
    ixmlclasslocal = zsaplink_tools=>globalclasstolocalclass( ixmlclass ).
*   insert local class defs into new source
    rootnode = ixmlclasslocal->find_from_name( 'localClass' ).
    definitionnode = rootnode->find_from_name( 'definition' ).
    CLEAR tempstring.
    tempstring = definitionnode->get_value( ).
    SPLIT tempstring AT cl_abap_char_utilities=>newline
    INTO TABLE definitiontabletemp.
    APPEND LINES OF definitiontabletemp TO sourcetablenew.

    implementationnode = rootnode->find_from_name( 'implementation' ).
    CLEAR tempstring.
    tempstring = implementationnode->get_value( ).
    SPLIT tempstring AT cl_abap_char_utilities=>newline
    INTO TABLE sourcetabletemp.
    APPEND LINES OF sourcetabletemp TO implementationtabletemp.
*    append lines of sourceTableTemp to sourceTableNew.
  ENDLOOP.
  APPEND LINES OF implementationtabletemp TO sourcetablenew.
* add original code back to new code
  APPEND LINES OF sourcetablebody TO sourcetablenew.
* create string from new souce code
  LOOP AT sourcetablenew INTO sourceline.
    CONCATENATE sourcestringnew sourceline cl_abap_char_utilities=>newline
      INTO sourcestringnew.
  ENDLOOP.

*--------------------------------------------------------------------*
* Since pragmas before closing period are not supported in older SAP-versions
* The steamroller does not create a universal installer.
* --> Delete pragmas in installer
*--------------------------------------------------------------------*
  REPLACE ALL OCCURRENCES OF REGEX '##\w*.' IN sourcestringnew WITH '.'.
*--------------------------------------------------------------------*

* change progam name in attributes to new prog name
  ixmlprognew = ixmlprog.
  rootnode = ixmlprognew->find_from_name( 'PROG' ).
  rc = rootnode->set_attribute( name = 'NAME' value = prognamenew ).
* add new source
  sourcenode = rootnode->find_from_name( 'source' ).
  rc = sourcenode->if_ixml_node~set_value( sourcestringnew ).

* use saplink to create new program
  TRY.
      CREATE OBJECT progobjectnew EXPORTING name = prognamenew.
      installobject = progobjectnew->createobjectfromixmldoc(
                                      ixmldocument = ixmlprognew
                                      overwrite = overwr ).
    CATCH zcx_saplink INTO excclass.
      errormsg = excclass->get_text( ).
      WRITE errormsg.
      EXIT.
  ENDTRY.

  WRITE: progname, ' has been steamrolled into ', prognamenew.
