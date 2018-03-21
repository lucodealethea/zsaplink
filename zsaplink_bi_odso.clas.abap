class ZSAPLINK_BI_ODSO definition
  public
  inheriting from ZSAPLINK
  final
  create public .

*"* public components of class ZSAPLINK_BI_ODSO
*"* do not include other source files here!!!
public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_BI_ODSO
*"* do not include other source files here!!!

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_BI_ODSO
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPLINK_BI_ODSO IMPLEMENTATION.


method CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Claudio Ciardelli
*      claudio.ciardelli@gmail.com

  DATA: l_name TYPE ddobjname.

  l_name = objname.

* Check if cube exists
  CALL METHOD cl_rsd_dta=>check_name_for_new_dta
    EXPORTING
      i_infoprov        = l_name
      i_tlogo           = rs_c_tlogo-ods_object
*        I_BWAPPL          =
    EXCEPTIONS
      name_invalid      = 1
      name_already_used = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
*    MESSAGE ID   sy-msgid TYPE 'I' NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exists = 'X'.
    EXIT.
  ENDIF.


ENDMETHOD.


METHOD CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Claudio Ciardelli
*      claudio.ciardelli@gmail.com

*xml nodes
  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA rc         TYPE sysubrc.
  DATA l_odsoname TYPE ddobjname.
  l_odsoname = objname.

  TYPE-POOLS rsdg.

  DATA: l_s_details        TYPE BAPI6116.
  DATA: l_s_return         TYPE BAPIRET2.

  DATA: l_t_infoobjects  TYPE TABLE OF BAPI6116IO.
  DATA: l_t_navigationattributes
                         TYPE TABLE OF BAPI6116NA.
  DATA: l_t_indexes      TYPE TABLE OF BAPI6116IN.
  DATA: l_t_indexesinfoobjects
                         TYPE TABLE OF BAPI6116II.

  DATA: l_s_infoobjects  TYPE BAPI6116IO.
  DATA: l_s_navigationattributes
                         TYPE BAPI6116NA.
  DATA: l_s_indexes      TYPE BAPI6116IN.
  DATA: l_s_indexesinfoobjects
                         TYPE BAPI6116II.


  DATA: infoobjects_node  TYPE REF TO if_ixml_element.
  DATA: navigationattributes_node
                         TYPE REF TO if_ixml_element.
  DATA: indexes_node     TYPE REF TO if_ixml_element.
  DATA: indexesinfoobjects_node
                         TYPE REF TO if_ixml_element.


* get ods object
  CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
   EXPORTING
    OBJVERS                    = RS_C_OBJVERS-ACTIVE
    ODSOBJECT                  = l_odsoname
   IMPORTING
    DETAILS                    = l_s_details
    RETURN                     = l_s_return
   TABLES
    INFOOBJECTS                = l_t_infoobjects
    NAVIGATIONATTRIBUTES       = l_t_navigationattributes
    INDEXES                    = l_t_indexes
    INDEXESINFOOBJECTS         = l_t_indexesinfoobjects
            .

* Create parent node
  DATA _objtype TYPE string.
  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).


  setattributesfromstructure( node = rootnode structure = l_s_details ).

* infoobjects
  LOOP AT l_t_infoobjects INTO l_s_infoobjects.
    infoobjects_node = xmldoc->create_element( 'infoobject' ).
    setattributesfromstructure(
        node = infoobjects_node structure = l_s_infoobjects ).
    rc = rootnode->append_child( infoobjects_node ).
  ENDLOOP.

* navigationattributes
  LOOP AT l_t_navigationattributes INTO l_s_navigationattributes.
    navigationattributes_node = xmldoc->create_element(
        'navigationattibute' ).
    setattributesfromstructure(
        node = navigationattributes_node
        structure = l_s_navigationattributes ).
    rc = rootnode->append_child( navigationattributes_node ).
  ENDLOOP.

* indexes
  LOOP AT l_t_indexes INTO l_s_indexes.
    indexes_node = xmldoc->create_element(
        'index' ).
    setattributesfromstructure(
        node = indexes_node
        structure = l_s_indexes ).
    rc = rootnode->append_child( indexes_node ).
  ENDLOOP.

* indexesinfoobjects
  LOOP AT l_t_indexesinfoobjects INTO l_s_indexesinfoobjects.
    indexesinfoobjects_node = xmldoc->create_element(
        'indexinfoobject' ).
    setattributesfromstructure(
        node = indexesinfoobjects_node
        structure = l_s_indexesinfoobjects ).
    rc = rootnode->append_child( indexesinfoobjects_node ).
  ENDLOOP.

*\--------------------------------------------------------------------/
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Claudio Ciardelli
*      claudio.ciardelli@gmail.com

  TYPE-POOLS rsdg.


*xml nodes
  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA filter      TYPE REF TO if_ixml_node_filter.
  DATA iterator    TYPE REF TO if_ixml_node_iterator.
  DATA l_subrc     TYPE sysubrc.
  DATA checkexists TYPE flag.
  DATA l_objtype   TYPE string.



  DATA: l_s_details        TYPE BAPI6116.
  DATA: l_t_return         TYPE TABLE OF BAPIRET2.
  DATA: l_s_return         TYPE BAPIRET2.
  DATA: l_t_infoobjects  TYPE TABLE OF BAPI6116IO.
  DATA: l_t_navigationattributes
                         TYPE TABLE OF BAPI6116NA.
  DATA: l_t_indexes      TYPE TABLE OF BAPI6116IN.
  DATA: l_t_indexesinfoobjects
                         TYPE TABLE OF BAPI6116II.

  DATA: l_s_infoobjects  TYPE BAPI6116IO.
  DATA: l_s_navigationattributes
                         TYPE BAPI6116NA.
  DATA: l_s_indexes      TYPE BAPI6116IN.
  DATA: l_s_indexesinfoobjects
                         TYPE BAPI6116II.


  DATA: infoobjects_node  TYPE REF TO if_ixml_element.
  DATA: navigationattributes_node
                         TYPE REF TO if_ixml_element.
  DATA: indexes_node     TYPE REF TO if_ixml_element.
  DATA: indexesinfoobjects_node
                         TYPE REF TO if_ixml_element.

  DATA: l_objectExists   TYPE flag.
  DATA: l_createODS      TYPE flag.


* Get object type
  l_objtype = getobjecttype( ).

* Check if object exists
  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.


  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( l_objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = l_s_details.

  objname = l_s_details-odsobject.


* retrieve Tabl details

* infoobjects
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'infoobject' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR infoobjects_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = l_s_infoobjects.
    APPEND l_s_infoobjects TO l_t_infoobjects.
    node ?= iterator->get_next( ).
  ENDWHILE.

* navigationattributes
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'navigationattribute' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR navigationattributes_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = l_s_navigationattributes.
    APPEND l_s_navigationattributes TO l_t_navigationattributes.
    node ?= iterator->get_next( ).
  ENDWHILE.

* indexes
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'index' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR indexes_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = l_s_indexes.
    APPEND l_s_indexes TO l_t_indexes.
    node ?= iterator->get_next( ).
  ENDWHILE.

* indexesinfoobjects
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'indexinfoobject' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR indexesinfoobjects_node.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = l_s_indexesinfoobjects.
    APPEND l_s_indexesinfoobjects TO l_t_indexesinfoobjects.
    node ?= iterator->get_next( ).
  ENDWHILE.

  l_objectExists = me->checkexists( ).

* Actually create object

  if overwrite = 'X'.
    if l_objectExists = 'X'.
      l_createODS = ''.
    else.
* Create object
      l_createODS = 'X'.
    endif.
  else.
* Create object
    l_createODS = 'X'.
  endif.


  if l_createODS = 'X'.
    CALL FUNCTION 'BAPI_ODSO_CREATE'
      EXPORTING
        DETAILS                    = l_s_details
* IMPORTING
*   ODSOBJECT                  =
      TABLES
       INFOOBJECTS                = l_t_infoobjects
       NAVIGATIONATTRIBUTES       = l_t_navigationattributes
       INDEXES                    = l_t_indexes
       INDEXESINFOOBJECTS         = l_t_indexesinfoobjects
       RETURN                     = l_t_return
              .
  else.
    CALL FUNCTION 'BAPI_ODSO_CHANGE'
      EXPORTING
        ODSOBJECT            = l_s_details-odsobject
        DETAILS              = l_s_details
      TABLES
        INFOOBJECTS          = l_t_infoobjects
        NAVIGATIONATTRIBUTES = l_t_navigationattributes
        INDEXES              = l_t_indexes
        INDEXESINFOOBJECTS   = l_t_indexesinfoobjects
        RETURN               = l_t_return.
  endif.

  read table l_t_return index 1 into l_s_return.
  CASE l_s_return-type.
    WHEN 'E' OR 'W'.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>system_error.

    loop at l_t_return into l_s_return.
    MESSAGE ID l_s_return-id TYPE l_s_return-type NUMBER
       l_s_return-number
       WITH l_s_return-message_v1 l_s_return-message_v2
            l_s_return-message_v3 l_s_return-message_v4.
    endloop.
  ENDCASE.

  name = objname.

ENDMETHOD.


METHOD DELETEOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Claudio Ciardelli
*      claudio.ciardelli@gmail.com
GET TIME.
ENDMETHOD.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Claudio Ciardelli
*      claudio.ciardelli@gmail.com

  objecttype = 'ODSO'.  "ODS Objects (Only tested under BW3.5)

endmethod.
ENDCLASS.
