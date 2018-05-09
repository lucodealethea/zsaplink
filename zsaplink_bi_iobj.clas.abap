class ZSAPLINK_BI_IOBJ definition
  public
  inheriting from ZSAPLINK
  final
  create public .

*"* public components of class ZSAPLINK_BI_IOBJ
*"* do not include other source files here!!!
public section.
  type-pools RS .

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



CLASS ZSAPLINK_BI_IOBJ IMPLEMENTATION.


method checkexists.
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
*      John Danoff
*      john_danoff@hotmail.com

  data: l_iobjnm type bapi6108-infoobject.

  data: l_s_return type bapiret2.

  l_iobjnm = me->objname.

* Check if InfoObject exists
  call function 'BAPI_IOBJ_GETDETAIL'
    exporting
      version                        = rs_c_objvers-active
      infoobject                     = l_iobjnm
    importing
*     DETAILS                        =
      return                         = l_s_return
*   TABLES
*     COMPOUNDS                      =
*     ATTRIBUTES                     =
*     NAVIGATIONATTRIBUTES           =
*     ATRNAVINFOPROVIDER             =
*     HIERARCHYCHARACTERISTICS       =
*     ELIMINATION                    =
                                       .

  if l_s_return-type = 'E'.
    if l_s_return-id = 'RSAR' and l_s_return-number = 316.
      exists = rs_c_false.
    else.
      message id   l_s_return-id type l_s_return-type number l_s_return-number
              with l_s_return-message_v1 l_s_return-message_v2 l_s_return-message_v3 l_s_return-message_v4.
    endif.
  else.
    exists = rs_c_true.
  endif.

endmethod.


method createixmldocfromobject.
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
*      John Danoff
*      john_danoff@hotmail.com

* object name
  data: l_iobjnm   type bapi6108-infoobject.
  data: l_rc type i.

  l_iobjnm = me->objname.

* XML nodes
  data: l_s_details type bapi6108.

  data: l_s_return type bapiret2.

  data: l_s_compounds type bapi6108cm,
        l_t_compounds type standard table of bapi6108cm.

  data: l_s_attributes type bapi6108at,
        l_t_attributes type standard table of bapi6108at.

  data: l_s_navigationattributes type bapi6108an,
        l_t_navigationattributes type standard table of bapi6108an.

  data: l_s_atrnavinfoprovider type bapi6108np,
        l_t_atrnavinfoprovider type standard table of bapi6108np.

  data: l_s_hierarchycharacteristics type bapi6108hc,
        l_t_hierarchycharacteristics type standard table of bapi6108hc.

  data: l_s_elimination type bapi6108ie,
        l_t_elimination type standard table of bapi6108ie.

  data: l_r_compounds_node type ref to if_ixml_element.
  data: l_r_attributes_node type ref to if_ixml_element.
  data: l_r_navigationattributes_node type ref to if_ixml_element.
  data: l_r_atrnavinfoprovider_node type ref to if_ixml_element.
  data: l_r_hierarchychar_node type ref to if_ixml_element.
  data: l_r_elimination_node type ref to if_ixml_element.

* get Info Object
  call function 'BAPI_IOBJ_GETDETAIL'
    exporting
      version                  = rs_c_objvers-active
      infoobject               = l_iobjnm
    importing
      details                  = l_s_details
      return                   = l_s_return
    tables
      compounds                = l_t_compounds
      attributes               = l_t_attributes
      navigationattributes     = l_t_navigationattributes
      atrnavinfoprovider       = l_t_atrnavinfoprovider
      hierarchycharacteristics = l_t_hierarchycharacteristics
      elimination              = l_t_elimination.

* Create the parent node
  data: l_r_rootnode type ref to if_ixml_element.
  data: l_objtype type string.
  l_objtype = me->getobjecttype( ).
  l_r_rootnode = xmldoc->create_element( l_objtype ).

  me->setattributesfromstructure( node = l_r_rootnode structure = l_s_details ).

* compounds
  loop at l_t_compounds into l_s_compounds.
    l_r_compounds_node = me->xmldoc->create_element( 'compounds' ).
    me->setattributesfromstructure( node = l_r_compounds_node structure = l_s_compounds ).
    l_rc = l_r_rootnode->append_child( l_r_compounds_node ).
  endloop.

* attributes
  loop at l_t_attributes into l_s_attributes.
    l_r_attributes_node = me->xmldoc->create_element( 'attributes' ).
    me->setattributesfromstructure( node = l_r_attributes_node structure = l_s_attributes ).
    l_rc = l_r_rootnode->append_child( l_r_attributes_node ).
  endloop.

* navigationattributes
  loop at l_t_navigationattributes into l_s_navigationattributes.
    l_r_navigationattributes_node = me->xmldoc->create_element( 'navigationattributes' ).
    me->setattributesfromstructure( node = l_r_navigationattributes_node structure = l_s_navigationattributes ).
    l_rc = l_r_rootnode->append_child( l_r_navigationattributes_node ).
  endloop.

* atrnavinfoprovider
  loop at l_t_atrnavinfoprovider into l_s_atrnavinfoprovider.
    l_r_atrnavinfoprovider_node = me->xmldoc->create_element( 'atrnavinfoprovider' ).
    me->setattributesfromstructure( node = l_r_atrnavinfoprovider_node structure = l_s_atrnavinfoprovider ).
    l_rc = l_r_rootnode->append_child( l_r_atrnavinfoprovider_node ).
  endloop.

* hierarchycharacteristics
  loop at l_t_hierarchycharacteristics into l_s_hierarchycharacteristics.
    l_r_hierarchychar_node = me->xmldoc->create_element( 'hierarchycharacteristics' ).
    me->setattributesfromstructure( node = l_r_hierarchychar_node structure = l_s_hierarchycharacteristics ).
    l_rc = l_r_rootnode->append_child( l_r_hierarchychar_node ).
  endloop.

* elimination.
  loop at l_t_elimination into l_s_elimination.
    l_r_elimination_node = me->xmldoc->create_element( 'elimination' ).
    me->setattributesfromstructure( node = l_r_elimination_node structure = l_s_elimination ).
    l_rc = l_r_rootnode->append_child( l_r_elimination_node ).
  endloop.

*\--------------------------------------------------------------------/
  l_rc = xmldoc->append_child( l_r_rootnode ).
  ixmldocument = me->xmldoc.
endmethod.


method createobjectfromixmldoc.
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
*      John Danoff
*      john_danoff@hotmail.com


* object name
  data: l_iobjnm   type bapi6108-infoobject.

  l_iobjnm = me->objname.

* XML nodes
  data: l_r_rootnode type ref to if_ixml_element.
  data: l_r_node     type ref to if_ixml_element.
  data: l_r_filter   type ref to if_ixml_node_filter.
  data: l_r_iterator type ref to if_ixml_node_iterator.

  data: l_rc  type i,
        l_msg type string.

  data: l_s_details type bapi6108.

  data: l_s_return type bapiret2,
        l_t_return type standard table of bapiret2.

  data: l_s_compounds type bapi6108cm,
        l_t_compounds type standard table of bapi6108cm.

  data: l_s_attributes type bapi6108at,
        l_t_attributes type standard table of bapi6108at.

  data: l_s_navigationattributes type bapi6108an,
        l_t_navigationattributes type standard table of bapi6108an.

  data: l_s_atrnavinfoprovider type bapi6108np,
        l_t_atrnavinfoprovider type standard table of bapi6108np.

  data: l_s_hierarchycharacteristics type bapi6108hc,
        l_t_hierarchycharacteristics type standard table of bapi6108hc.

  data: l_s_elimination type bapi6108ie,
        l_t_elimination type standard table of bapi6108ie.

  data: l_r_compounds_node type ref to if_ixml_element.
  data: l_r_attributes_node type ref to if_ixml_element.
  data: l_r_navigationattributes_node type ref to if_ixml_element.
  data: l_r_atrnavinfoprovider_node type ref to if_ixml_element.
  data: l_r_hierarchychar_node type ref to if_ixml_element.
  data: l_r_elimination_node type ref to if_ixml_element.

* Get object type
  data: l_objtype type string.
  l_objtype = me->getobjecttype( ).

* Check if the object exists
  data: l_object_exists type rs_bool.
  l_object_exists = me->checkexists( ).

*  checkexists = checkexists( ).
  if l_object_exists = rs_c_true.
    if overwrite is initial.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>existing.
    else.
*     delete object for new install
      me->deleteobject( ).
    endif.
  endif.

  me->xmldoc = ixmldocument.
  l_r_rootnode = me->xmldoc->find_from_name( l_objtype ).

  call method me->getstructurefromattributes
    exporting
      node      = l_r_rootnode
    changing
      structure = l_s_details.

* retrieve table details

* compounds
  free: l_r_filter, l_r_iterator, l_r_node.
  l_r_filter = me->xmldoc->create_filter_name( 'compounds' ).
  l_r_iterator = me->xmldoc->create_iterator_filtered( l_r_filter ).
  l_r_node ?= l_r_iterator->get_next( ).
  while l_r_node is not initial.
    clear l_r_compounds_node.
    call method me->getstructurefromattributes
      exporting
        node      = l_r_node
      changing
        structure = l_s_compounds.
    append l_s_compounds to l_t_compounds.
    l_r_node ?= l_r_iterator->get_next( ).
  endwhile.

* attributes
  free: l_r_filter, l_r_iterator, l_r_node.
  l_r_filter = me->xmldoc->create_filter_name( 'attributes' ).
  l_r_iterator = me->xmldoc->create_iterator_filtered( l_r_filter ).
  l_r_node ?= l_r_iterator->get_next( ).
  while l_r_node is not initial.
    clear l_r_attributes_node.
    call method me->getstructurefromattributes
      exporting
        node      = l_r_node
      changing
        structure = l_s_attributes.
    append l_s_attributes to l_t_attributes.
    l_r_node ?= l_r_iterator->get_next( ).
  endwhile.

* navigationattributes
  free: l_r_filter, l_r_iterator, l_r_node.
  l_r_filter = me->xmldoc->create_filter_name( 'navigationattributes' ).
  l_r_iterator = me->xmldoc->create_iterator_filtered( l_r_filter ).
  l_r_node ?= l_r_iterator->get_next( ).
  while l_r_node is not initial.
    clear l_r_navigationattributes_node.
    call method me->getstructurefromattributes
      exporting
        node      = l_r_node
      changing
        structure = l_s_navigationattributes.
    append l_s_navigationattributes to l_t_navigationattributes.
    l_r_node ?= l_r_iterator->get_next( ).
  endwhile.

* atrnavinfoprovider
  free: l_r_filter, l_r_iterator, l_r_node.
  l_r_filter = me->xmldoc->create_filter_name( 'atrnavinfoprovider' ).
  l_r_iterator = me->xmldoc->create_iterator_filtered( l_r_filter ).
  l_r_node ?= l_r_iterator->get_next( ).
  while l_r_node is not initial.
    clear l_r_atrnavinfoprovider_node.
    call method me->getstructurefromattributes
      exporting
        node      = l_r_node
      changing
        structure = l_s_atrnavinfoprovider.
    append l_s_atrnavinfoprovider to l_t_atrnavinfoprovider.
    l_r_node ?= l_r_iterator->get_next( ).
  endwhile.

* hierarchycharacteristics
  free: l_r_filter, l_r_iterator, l_r_node.
  l_r_filter = me->xmldoc->create_filter_name( 'hierarchycharacteristics' ).
  l_r_iterator = me->xmldoc->create_iterator_filtered( l_r_filter ).
  l_r_node ?= l_r_iterator->get_next( ).
  while l_r_node is not initial.
    clear l_r_hierarchychar_node.
    call method me->getstructurefromattributes
      exporting
        node      = l_r_node
      changing
        structure = l_s_hierarchycharacteristics.
    append l_s_hierarchycharacteristics to l_t_hierarchycharacteristics.
    l_r_node ?= l_r_iterator->get_next( ).
  endwhile.

* elimination
  free: l_r_filter, l_r_iterator, l_r_node.
  l_r_filter = me->xmldoc->create_filter_name( 'elimination' ).
  l_r_iterator = me->xmldoc->create_iterator_filtered( l_r_filter ).
  l_r_node ?= l_r_iterator->get_next( ).
  while l_r_node is not initial.
    clear l_r_elimination_node.
    call method me->getstructurefromattributes
      exporting
        node      = l_r_node
      changing
        structure = l_s_elimination.
    append l_s_elimination to l_t_elimination.
    l_r_node ?= l_r_iterator->get_next( ).
  endwhile.

* Actually create the object
  data: l_create_object type rs_bool.
  if overwrite = rs_c_true.
    if l_object_exists = rs_c_true.
      l_create_object = rs_c_false.
    else.
* Create object
      l_create_object = rs_c_true.
    endif.
  else.
* Create object
    l_create_object = rs_c_true.
  endif.

  if l_create_object = rs_c_true.
    call function 'BAPI_IOBJ_CREATE'
      exporting
        details                        = l_s_details
      importing
*        INFOOBJECT                     =
        return                         = l_s_return
      tables
        compounds                      = l_t_compounds
        attributes                     = l_t_attributes
        navigationattributes           = l_t_navigationattributes
        atrnavinfoprovider             = l_t_atrnavinfoprovider
        hierarchycharacteristics       = l_t_hierarchycharacteristics
        elimination                    = l_t_elimination
        returntable                    = l_t_return.
  else.
    call function 'BAPI_IOBJ_CHANGE'
      exporting
        infoobject               = l_iobjnm
        details                  = l_s_details
      importing
        return                   = l_s_return
      tables
        compounds                = l_t_compounds
        attributes               = l_t_attributes
        navigationattributes     = l_t_navigationattributes
        atrnavinfoprovider       = l_t_atrnavinfoprovider
        hierarchycharacteristics = l_t_hierarchycharacteristics
        elimination              = l_t_elimination
        returntable              = l_t_return.
  endif.

  if l_s_return-type = 'E' or l_s_return-type = 'W'.
    message id l_s_return-id type l_s_return-type number l_s_return-number
       with l_s_return-message_v1 l_s_return-message_v2
            l_s_return-message_v3 l_s_return-message_v4
       into l_msg.
    loop at l_t_return into l_s_return.
      message id l_s_return-id type l_s_return-type number l_s_return-number
         with l_s_return-message_v1 l_s_return-message_v2
              l_s_return-message_v3 l_s_return-message_v4
         into l_msg.
    endloop.
    raise exception type zcx_saplink
      exporting textid = zcx_saplink=>system_error.
  endif.

* Activate the object
  data: l_s_infoobjects type bapi6108io,
        l_t_infoobjects type standard table of bapi6108io.

  data: l_s_infoobjects_error type bapi6108io,
        l_t_infoobjects_error type standard table of bapi6108io.

  l_s_infoobjects-infoobject = l_iobjnm.
  append l_s_infoobjects to l_t_infoobjects.

  call function 'BAPI_IOBJ_ACTIVATE_MULTIPLE'
    tables
      infoobjects       = l_t_infoobjects
      return            = l_t_return
      infoobjects_error = l_t_infoobjects_error.

  loop at l_t_return into l_s_return.
    message id l_s_return-id type l_s_return-type number l_s_return-number
       with l_s_return-message_v1 l_s_return-message_v2
            l_s_return-message_v3 l_s_return-message_v4
       into l_msg.
    if l_s_return-type = 'E' or l_s_return-type = 'W'.
      raise exception type zcx_saplink
        exporting textid = zcx_saplink=>system_error.
    endif.
  endloop.

  name = me->objname.

endmethod.


method DELETEOBJECT.
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
*      John Danoff
*      john_danoff@hotmail.com
       GET TIME.

endmethod.


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
*      John Danoff
*      john_danoff@hotmail.com

  objecttype = 'IOBJ'.  "Info Objects

endmethod.
ENDCLASS.
