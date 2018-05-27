class ZSAPLINK_TOOLS definition
  public
  create public .

public section.
  type-pools SEOO .
  type-pools SEOR .

  class-methods GLOBALCLASSTOLOCALCLASS
    importing
      !GLOBAL_CLASS_XML type ref to IF_IXML_DOCUMENT
    returning
      value(LOCAL_CLASS_XML) type ref to IF_IXML_DOCUMENT .
protected section.

  class-methods GETSTRUCTUREFROMATTRIBUTES
    importing
      !NODE type ref to IF_IXML_ELEMENT
    changing
      !STRUCTURE type DATA .
  class-methods BUILDTABLEFROMSTRING
    importing
      !SOURCE type STRING
    returning
      value(SOURCETABLE) type TABLE_OF_STRINGS .
  class-methods BUILDSOURCESTRING
    importing
      !SOURCETABLE type RSWSOURCET optional
      !PAGETABLE type O2PAGELINE_TABLE optional
    returning
      value(SOURCESTRING) type STRING .
private section.
ENDCLASS.



CLASS ZSAPLINK_TOOLS IMPLEMENTATION.


method BUILDSOURCESTRING.
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
data sTemp type string.
data pageLine type O2PAGELINE.

  if sourceTable is not initial.
    loop at sourceTable into sTemp.
      concatenate sourceString sTemp CL_ABAP_CHAR_UTILITIES=>NEWLINE
        into sourceString.
    endloop.
  elseif pageTable is not initial.
    loop at pageTable into pageLine.
      concatenate sourceString pageLine-line
        CL_ABAP_CHAR_UTILITIES=>NEWLINE
        into sourceString.
    endloop.
  endif.

* remove extra newline characters for conversion comparison consistency
  shift sourceString left deleting leading
    CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  shift sourceString right deleting trailing
    CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  shift sourceString left deleting leading space.
endmethod.


method BUILDTABLEFROMSTRING.
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
  split source at CL_ABAP_CHAR_UTILITIES=>NEWLINE
    into table sourceTable.
endmethod.


method GETSTRUCTUREFROMATTRIBUTES.
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
*\---------------------------------------------------------------------/
data attributeList type ref to IF_IXML_NAMED_NODE_MAP.
data nodeIterator type ref to IF_IXML_NODE_ITERATOR.
data attributeNode type ref to if_ixml_node.
data value type string.
data name type string.
field-symbols <value> type any.

  clear structure.
  attributeList = node->GET_ATTRIBUTES( ).
  nodeIterator = attributeList->create_iterator( ).
  attributeNode = nodeIterator->get_next( ).
  while attributeNode is not initial.
    name = attributeNode->get_name( ).
    if name = 'VERSION'.
      value = '0'.
    else.
      value = attributeNode->get_value( ).
    endif.
    assign component name of structure structure to <value>.
    if sy-subrc = 0.
      <value> = value.
    endif.
    attributeNode = nodeIterator->get_next( ).
  endwhile.






*    .-"-.
*  .'=^=^='.
* /=^=^=^=^=\
*:^=SAPLINK=^;
*|^ EASTER  ^|
*:^=^EGG^=^=^:
* \=^=^=^=^=/
*  `.=^=^=.'
*    `~~~`
* Don't like the way we did something?
* Help us fix it!  Tell us what you think!
* http://saplink.org
endmethod.


METHOD globalclasstolocalclass.
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
  DATA xmldoc	TYPE REF TO if_ixml_document.
  DATA xmldocout TYPE REF TO if_ixml_document.
  DATA ixml TYPE REF TO if_ixml.
  DATA objname TYPE string.
  DATA defnodeout TYPE REF TO if_ixml_element.
  DATA impnodeout TYPE REF TO if_ixml_element.
  DATA dependnodeout TYPE REF TO if_ixml_element.
  DATA dependchildnodeout TYPE REF TO if_ixml_element.
  DATA rootnodeout TYPE REF TO if_ixml_element.
  DATA rc TYPE sysubrc.
  DATA strparent TYPE string.
  DATA strtemp TYPE string.

  DATA it_flatclass TYPE TABLE OF string.
  DATA it_flatclassimp TYPE TABLE OF string.
  DATA it_flatclassdef TYPE TABLE OF string.
  DATA wa_flatclass TYPE string.
  DATA it_attrib TYPE TABLE OF vseoattrib.
  DATA it_types TYPE TABLE OF seoo_type_r.
  DATA it_interfaces TYPE TABLE OF seor_implementing_r.
  DATA it_methodprops TYPE TABLE OF vseomethod.
  DATA it_params TYPE TABLE OF vseoparam.
  DATA it_except TYPE TABLE OF vseoexcep.

  DATA reportstring TYPE string.

  DATA usefl TYPE c.
  DATA wa_flatclass_tmp TYPE string.
  DATA off TYPE int4.


  DATA rootnode TYPE REF TO if_ixml_element.
  DATA classheader TYPE vseoclass.
  DATA classkey TYPE seoclskey.
  DATA not_active TYPE boolean.
  DATA filter TYPE REF TO if_ixml_node_filter.
  DATA iterator TYPE REF TO if_ixml_node_iterator.
  DATA node TYPE REF TO if_ixml_element.
  DATA otrnode TYPE REF TO if_ixml_element.
  DATA filter2 TYPE REF TO if_ixml_node_filter.
  DATA iterator2 TYPE REF TO if_ixml_node_iterator.
  DATA attribproperties TYPE vseoattrib.
  DATA methodproperties TYPE vseomethod.
  DATA methodredefinition TYPE seoredef.
  DATA methodredeftable TYPE STANDARD TABLE OF seoredef WITH KEY clsname
  refclsname version mtdname.
  DATA superclass TYPE vseoextend.
  DATA superclasskey TYPE vseoextend.
  DATA includename TYPE program.
  DATA methodsourcenode TYPE REF TO if_ixml_node.
  DATA sourcenode TYPE REF TO if_ixml_node.
  DATA source TYPE string.
  DATA sourcetable TYPE TABLE OF string.
  DATA methodkey TYPE seocpdkey.
  DATA forwarddeclarationrow TYPE vseotypep.
  DATA forwarddeclarations TYPE STANDARD TABLE OF vseotypep.
  DATA node2 TYPE REF TO if_ixml_element.
  DATA paraminfo TYPE vseoparam.
  DATA exceptinfo TYPE vseoexcep.

  DATA _objtype TYPE string.
  DATA checkexists TYPE flag.

  DATA inheritancenode TYPE REF TO if_ixml_element.
  DATA redefnode TYPE REF TO if_ixml_element.
  DATA inheritance TYPE seor_inheritance_r.
  DATA redefinitions TYPE seor_redefinitions_r.
  DATA redefinition LIKE LINE OF redefinitions.

*  _devclass = devclass.
  _objtype = 'CLAS'.

  xmldoc = global_class_xml.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = classheader.

  classkey-clsname = classheader-clsname.
  objname = classkey-clsname.

**//
  CONCATENATE
    'CLASS' objname 'DEFINITION'
  INTO wa_flatclass SEPARATED BY space.
  APPEND wa_flatclass TO it_flatclass.
  CLEAR wa_flatclass.

*is it inheriting?
* inheritance
  inheritancenode = rootnode->find_from_name( zsaplink_oo=>c_xml_key_inheritance ).
  IF inheritancenode IS BOUND.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = inheritancenode
      CHANGING
        structure = inheritance.
    IF inheritance IS NOT INITIAL.
      CONCATENATE
        space space
        'INHERITING FROM'
        inheritance-refclsname
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
      CLEAR wa_flatclass.
      strparent = inheritance-refclsname.
    ENDIF.
*   redefs
*    filter = inheritanceNode->create_filter_name( 'redefinition' ).
*    iterator = inheritanceNode->create_iterator_filtered( filter ).
*    redefNode ?= iterator->get_next( ).
*    WHILE redefNode IS NOT INITIAL.
*      CALL METHOD getstructurefromattributes
*        EXPORTING
*          node      = redefNode
*        CHANGING
*          structure = redefinition.
*      append redefinition to redefinitions.
*      redefNode ?= iterator->get_next( ).
*    ENDWHILE.

  ENDIF.

*is it final?
  IF classheader-clsfinal = 'X'.
    CONCATENATE
      space space
      'FINAL'
    INTO wa_flatclass SEPARATED BY space.
    APPEND wa_flatclass TO it_flatclass.
    CLEAR wa_flatclass.
  ENDIF.



  CASE classheader-exposure.
    WHEN '0'.
*   private
      CONCATENATE
        space space
        'CREATE PRIVATE'
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
      CLEAR wa_flatclass.

    WHEN '1'.
*   protected
      CONCATENATE
        space space
        'CREATE PROTECTED'
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
      CLEAR wa_flatclass.

    WHEN '2'.
*   public
      CONCATENATE
        space space
        'CREATE PUBLIC'
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
      CLEAR wa_flatclass.

*    when '3'.
**   Abstract
*      concatenate
*        SPACE space
*        'ABSTRACT'
*      into wa_flatClass separated by space.
*      append wa_flatClass to it_flatClass.
*      clear wa_flatClass.

  ENDCASE.

*is it abstract?
  IF classheader-clsabstrct = 'X'.
    CONCATENATE
      space space
      'ABSTRACT'
    INTO wa_flatclass SEPARATED BY space.
    APPEND wa_flatclass TO it_flatclass.
    CLEAR wa_flatclass.
  ENDIF.

* has friends?
  DATA: wa_friend TYPE seofriends.

  filter = xmldoc->create_filter_name( zsaplink_oo=>c_xml_key_friends ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  IF node IS NOT INITIAL.
    CONCATENATE
      space space
      'FRIENDS'
    INTO wa_flatclass SEPARATED BY space.
    APPEND wa_flatclass TO it_flatclass.
    CLEAR wa_flatclass.
  ENDIF.
  WHILE node IS NOT INITIAL.
    CLEAR wa_friend.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_friend.
    wa_friend-version = '0'.
    IF wa_friend-clsname = classheader-clsname.
      CONCATENATE
        space space space space
        wa_friend-refclsname
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
      CLEAR wa_flatclass.
    ENDIF.
    node ?= iterator->get_next( ).
  ENDWHILE.




  CONCATENATE
    space space
    '.'
  INTO wa_flatclass SEPARATED BY space.
  APPEND wa_flatclass TO it_flatclass.
  CLEAR wa_flatclass.

**\\

  classheader-version = '0'.
  superclass = rootnode->get_attribute( name = 'REFCLSNAME' ).
  IF superclass IS NOT INITIAL.
* set something for inheritence
    superclasskey-clsname = classkey-clsname.
    superclasskey-refclsname = superclass.
    superclasskey-version = '0'.
    superclasskey-state = '1'.

  ENDIF.

  DATA aobjname TYPE trobj_name.


*Add attributes to new class
  filter = xmldoc->create_filter_name( 'attribute' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
*   create OTR texts if necessary (for exception classes)
    otrnode = node->find_from_name( zsaplink_oo=>c_xml_key_sotr ).
    IF otrnode IS NOT INITIAL.
*      me->createotrfromnode( otrnode ).
    ENDIF.

*   create attribute
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = attribproperties.
    APPEND attribproperties TO it_attrib.
    node ?= iterator->get_next( ).
  ENDWHILE.
*exit.

*/***TPJ - Added Logic for TYPES  -------------------*/
  DATA: types           TYPE seoo_types_r,
        type_properties LIKE LINE OF types.

  filter = xmldoc->create_filter_name( 'types' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR type_properties.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = type_properties.
    type_properties-version = '0'.
    IF type_properties-clsname = classheader-clsname.
      APPEND type_properties TO it_types.
    ENDIF.
    node ?= iterator->get_next( ).
  ENDWHILE.
*/***TPJ - End of Added Logic for TYPES  -------------------*/



*// ewH: Added Logic for Implementings(interfaces)-->
  DATA: it_implementings TYPE seor_implementings_r,
        wa_implementings LIKE LINE OF it_implementings.

  filter = xmldoc->create_filter_name( 'implementing' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_implementings.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_implementings.
    APPEND wa_implementings TO it_interfaces.
    node ?= iterator->get_next( ).
  ENDWHILE.
*//<--ewH: End of Added Logic for Implementings(interfaces)



*Add Methods to new class
  filter = xmldoc->create_filter_name( 'method' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR methodproperties.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = methodproperties.

*   only create metadata if method is not a redefinition
    READ TABLE redefinitions INTO redefinition
    WITH KEY mtdname = methodproperties-cmpname.
    IF sy-subrc = 0.
      node ?= iterator->get_next( ).
      CONTINUE.
    ENDIF.


    IF methodproperties-clsname <> classheader-clsname.
      MOVE-CORRESPONDING methodproperties TO methodredefinition.
      methodredefinition-clsname = classheader-clsname.
      methodredefinition-refclsname = methodproperties-clsname.
      methodredefinition-version = '0'.
      methodredefinition-mtdabstrct = ''.
      methodredefinition-mtdname = methodproperties-cmpname.
      APPEND methodredefinition TO methodredeftable.

      node ?= iterator->get_next( ).
      CONTINUE.
    ENDIF.

    APPEND methodproperties TO it_methodprops.

    filter2 = node->create_filter_name( 'parameter' ).
    iterator2 = node->create_iterator_filtered( filter2 ).
    node2 ?= iterator2->get_next( ).
    WHILE node2 IS NOT INITIAL.
      CLEAR paraminfo.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node2
        CHANGING
          structure = paraminfo.
      IF paraminfo IS NOT INITIAL.
        APPEND paraminfo TO it_params.
      ENDIF.
      node2 ?= iterator2->get_next( ).
    ENDWHILE.
    filter2 = node->create_filter_name( 'exception' ).
    iterator2 = node->create_iterator_filtered( filter2 ).
    node2 ?= iterator2->get_next( ).
    WHILE node2 IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node2
        CHANGING
          structure = exceptinfo.
      IF exceptinfo IS NOT INITIAL.
        APPEND exceptinfo TO it_except.
      ENDIF.
      node2 ?= iterator2->get_next( ).

    ENDWHILE.
    node ?= iterator->get_next( ).
  ENDWHILE.
*// <--ewH: end redesign method/inheritances

*// ewh: continuation of backward compatibility hack-->

  DATA classtype TYPE seoclstype.
  DATA impkeys TYPE seor_implementing_keys.
  DATA iserror TYPE boolean.

*create forward declarations
* Not used as we include full sections. "Rene 01.10.2012
*  filter = xmldoc->create_filter_name( 'forwardDeclaration' ).
*  iterator = xmldoc->create_iterator_filtered( filter ).
*  node ?= iterator->get_next( ).
*
*  forwarddeclarationrow-clsname = classkey-clsname.
*  forwarddeclarationrow-version = '0'.
*  forwarddeclarationrow-tputype = ''.
*  forwarddeclarationrow-explicit =  'X'.
*  forwarddeclarationrow-implicit = ''.
*
*  WHILE node IS NOT INITIAL.
*    source = node->get_value( ).
*    forwarddeclarationrow-typegroup = source.
*    TYPE-POOLS seot.
*    DATA forwarddectable TYPE seot_typepusages_w.
*    DATA forwarddecrow LIKE LINE OF forwarddectable.
*    CLEAR forwarddecrow.
*    REFRESH forwarddectable.
*    forwarddecrow-clsname = classkey-clsname.
*    forwarddecrow-version = '0'.
*    forwarddecrow-tputype = '0'.
*    forwarddecrow-explicit =  'X'.
*    forwarddecrow-implicit = ''.
*    forwarddecrow-typegroup = source.
*    APPEND forwarddecrow TO forwarddectable.
*
*    node ?= iterator->get_next( ).
*  ENDWHILE.

**//
* public section
  wa_flatclass = 'public section.'.
  APPEND wa_flatclass TO it_flatclass.
  CLEAR wa_flatclass.

*insert code for public section
*/--------------------------------------------------------------------\
  REFRESH sourcetable .
  includename = cl_oo_classname_service=>get_pubsec_name( classkey-clsname  ).
  READ REPORT includename INTO sourcetable.

  IF sourcetable IS NOT INITIAL.

    CLEAR usefl.
    LOOP AT sourcetable INTO wa_flatclass.
      IF usefl IS INITIAL.
        wa_flatclass_tmp = wa_flatclass.
        TRANSLATE wa_flatclass_tmp TO UPPER CASE.
        CONDENSE wa_flatclass_tmp NO-GAPS.
        SHIFT wa_flatclass_tmp LEFT DELETING LEADING space.
*   This is not a great way to do this...
        FIND 'PUBLICSECTION.' IN wa_flatclass_tmp MATCH OFFSET off.
        IF sy-subrc = 0 AND off = 0.
          usefl = 'X'.
        ENDIF.
      ELSE.
        APPEND wa_flatclass TO it_flatclass.
      ENDIF.

    ENDLOOP.
    CLEAR wa_flatclass.
    CLEAR usefl.
  ENDIF.

** interfaces
*  loop at it_interfaces into wa_implementings
*        where exposure = '2'.
*    concatenate
*      space space space space
*      'interfaces'
*      wa_implementings-REFCLSNAME
*    into wa_flatClass separated by space.
*    append wa_flatClass to it_flatClass.
*    clear wa_flatClass.
*
*    if wa_implementings-IMPFINAL = 'X'.
*      concatenate
*        space space space space space space
*        'all methods final'
*      into wa_flatClass separated by space.
*      append wa_flatClass to it_flatClass.
*      clear wa_flatClass.
*    elseif wa_implementings-IMPABSTRCT = 'X'.
*      concatenate
*        space space space space space space
*        'all methods abstract'
*      into wa_flatClass separated by space.
*      append wa_flatClass to it_flatClass.
*      clear wa_flatClass.
*    endif.
*
*    concatenate
*      space space space space space space
*      '.'
*    into wa_flatClass separated by space.
*    append wa_flatClass to it_flatClass.
*    clear wa_flatClass.
*
*  endloop.
*
*  break uspty90.

** aliases

** data / class-data / constants

** events

** methods / class-methods



* protected section
*  wa_flatClass = 'protected section.'.
*  append wa_flatClass to it_flatClass.
*  clear wa_flatClass.

*insert code for protected section
*|--------------------------------------------------------------------|
  REFRESH sourcetable .
  includename = cl_oo_classname_service=>get_prosec_name( classkey-clsname ).
  READ REPORT includename INTO sourcetable.
  IF sourcetable IS NOT INITIAL.
    LOOP AT sourcetable INTO wa_flatclass.
      APPEND wa_flatclass TO it_flatclass.
    ENDLOOP.
    CLEAR wa_flatclass.

  ENDIF.


** interfaces

** aliases

** types

** data / class-data / constants

** events

** methods / class-methods



* private section
*  wa_flatClass = 'private section.'.
*  append wa_flatClass to it_flatClass.
*  clear wa_flatClass.

*insert code for private section
*|--------------------------------------------------------------------|
  REFRESH sourcetable .
  includename = cl_oo_classname_service=>get_prisec_name( classkey-clsname ).
  READ REPORT includename INTO sourcetable.
  IF sourcetable IS NOT INITIAL.
    LOOP AT sourcetable INTO wa_flatclass.
      APPEND wa_flatclass TO it_flatclass.
    ENDLOOP.
    CLEAR wa_flatclass.

  ENDIF.

** interfaces

** aliases

** types

** data / class-data / constants

** events

** methods / class-methods

  wa_flatclass = 'ENDCLASS.'.
  APPEND wa_flatclass TO it_flatclass.
  CLEAR wa_flatclass.
  APPEND wa_flatclass TO it_flatclass.

  it_flatclassdef = it_flatclass.
  CLEAR it_flatclass.

  CONCATENATE
    'CLASS'
    objname
    'IMPLEMENTATION.'
  INTO wa_flatclass SEPARATED BY space.
  APPEND wa_flatclass TO it_flatclass.
  CLEAR wa_flatclass.




*Insert source code into the methods
  filter = xmldoc->create_filter_name( 'method' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = methodproperties.
*    methodkey-clsname = objname.
*    methodkey-cpdname = methodproperties-cmpname.
*    aobjname = methodkey.
*    if methodproperties-CLSNAME = objname.

    methodsourcenode = node->find_from_name( 'source' ).
    IF methodsourcenode IS NOT INITIAL.
      source = methodsourcenode->get_value( ).
      sourcetable = buildtablefromstring( source ).

      LOOP AT sourcetable INTO wa_flatclass.
        IF wa_flatclass IS NOT INITIAL.
          IF wa_flatclass(1) <> '*'.
            CONCATENATE
              space space
              wa_flatclass
            INTO wa_flatclass SEPARATED BY space.
          ENDIF.
        ENDIF.
        APPEND wa_flatclass TO it_flatclass.
      ENDLOOP.
      CLEAR wa_flatclass.
    ELSEIF classheader-clsabstrct <> 'X'.
      CONCATENATE
        space space
        'method'
        methodproperties-cmpname
        '.'
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
      CLEAR wa_flatclass.
      CONCATENATE
        space space
        'endmethod.'
      INTO wa_flatclass SEPARATED BY space.
      APPEND wa_flatclass TO it_flatclass.
    ENDIF.
*    endif.

    node ?= iterator->get_next( ).
  ENDWHILE.

*// ewH: create interface methods-->
  filter = xmldoc->create_filter_name( 'interfaceMethod' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = methodkey.
*    aobjname = methodkey.

    IF methodkey-clsname = objname.

      methodsourcenode = node->find_from_name( 'source' ).
      IF methodsourcenode IS NOT INITIAL.
        source = methodsourcenode->get_value( ).
        sourcetable = buildtablefromstring( source ).

        LOOP AT sourcetable INTO wa_flatclass.
          IF wa_flatclass IS NOT INITIAL.
            IF wa_flatclass(1) <> '*'.
              CONCATENATE
                space space
                wa_flatclass
              INTO wa_flatclass SEPARATED BY space.
            ENDIF.
          ENDIF.
          APPEND wa_flatclass TO it_flatclass.
        ENDLOOP.
        CLEAR wa_flatclass.

      ENDIF.
    ENDIF.

    node ?= iterator->get_next( ).
  ENDWHILE.

  wa_flatclass = 'ENDCLASS.'.
  APPEND wa_flatclass TO it_flatclass.
  CLEAR wa_flatclass.

  it_flatclassimp = it_flatclass.
  CLEAR it_flatclass.

**insert code for local implementation
*  DATA _classname TYPE seoclsname.
*  _classname = objname.
*  sourcenode = xmldoc->find_from_name( 'localImplementation' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    sourcetable = buildtablefromstring( source ).
*  ENDIF.
*
**insert code for local types
*  sourcenode = xmldoc->find_from_name( 'localTypes' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    sourcetable = buildtablefromstring( source ).
*  ENDIF.
*
**insert code for local macros
*  sourcenode = xmldoc->find_from_name( 'localMacros' ).
*  IF sourcenode IS NOT INITIAL.
*    source = sourcenode->get_value( ).
*    sourcetable = buildtablefromstring( source ).
*  ENDIF.

  ixml = cl_ixml=>create( ).
  xmldocout = ixml->create_document( ).
  rootnodeout = xmldoc->create_element( 'localClass' ).
  rc = rootnodeout->set_attribute( name = 'NAME' value = objname ).

  defnodeout = xmldocout->create_element( 'definition' ).
  reportstring = buildsourcestring( sourcetable = it_flatclassdef ).
  rc = defnodeout->if_ixml_node~set_value( reportstring ).
  rc = rootnodeout->append_child( defnodeout ).

  impnodeout = xmldocout->create_element( 'implementation' ).
  reportstring = buildsourcestring( sourcetable = it_flatclassimp ).
  rc = impnodeout->if_ixml_node~set_value( reportstring ).
  rc = rootnodeout->append_child( impnodeout ).

  dependnodeout = xmldocout->create_element( 'dependencies' ).
  rc = rootnodeout->append_child( dependnodeout ).

  IF strparent IS NOT INITIAL.
    TRANSLATE strparent TO UPPER CASE.
    IF strparent(1) = 'Z' OR strparent(1) = 'Y'.
      dependchildnodeout = xmldocout->create_element( 'CLAS' ).
      rc = dependchildnodeout->if_ixml_node~set_value( strparent ).
      rc = dependnodeout->append_child( dependchildnodeout ).
    ENDIF.
  ENDIF.
  LOOP AT it_interfaces INTO wa_implementings.
    strtemp = wa_implementings-refclsname.
    TRANSLATE strtemp TO UPPER CASE.
    IF strtemp(1) = 'Z' OR strtemp(1) = 'Y'.
      dependchildnodeout = xmldocout->create_element( 'INTF' ).
      rc = dependchildnodeout->if_ixml_node~set_value( strtemp ).
      rc = dependnodeout->append_child( dependchildnodeout ).
    ENDIF.
  ENDLOOP.
  LOOP AT it_attrib INTO attribproperties WHERE typtype = '3'.
    strtemp = attribproperties-type.
    TRANSLATE strtemp TO UPPER CASE.
    IF strtemp(1) = 'Z' OR strtemp(1) = 'Y'.

    ENDIF.
  ENDLOOP.

  rc = xmldocout->append_child( rootnodeout ).
  local_class_xml = xmldocout.





ENDMETHOD.
ENDCLASS.
