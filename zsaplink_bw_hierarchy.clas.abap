CLASS zsaplink_bw_hierarchy DEFINITION
  PUBLIC
  INHERITING FROM zsaplink
  FINAL
  CREATE PUBLIC .

*"* public components of class ZSAPLINK_BW_HIERARCHY
*"* do not include other source files here!!!
PUBLIC SECTION.

  CONSTANTS co_objecttype TYPE string VALUE 'HIER'. "#EC NOTEXT

  METHODS checkexists
    REDEFINITION .
  METHODS createixmldocfromobject
    REDEFINITION .
  METHODS createobjectfromixmldoc
    REDEFINITION .
  METHODS valuehelp
    REDEFINITION .
PROTECTED SECTION.
*"* protected components of class ZSAPLINK_BW_HIERARCHY
*"* do not include other source files here!!!

  DATA infoobject TYPE rsiobjnm .
  DATA hierarchy_id TYPE rshieid .
  DATA hierarchy_version TYPE rsversion .
  CONSTANTS co_nodename_descriptions TYPE string VALUE 'Descriptions'. "#EC NOTEXT
  CONSTANTS co_nodename_description TYPE string VALUE 'Description'. "#EC NOTEXT
  CONSTANTS co_nodename_levels TYPE string VALUE 'Levels'. "#EC NOTEXT
  CONSTANTS co_nodename_level TYPE string VALUE 'Level'. "#EC NOTEXT
  CONSTANTS co_nodename_intervals TYPE string VALUE 'Intervals'. "#EC NOTEXT
  CONSTANTS co_nodename_interval TYPE string VALUE 'Interval'. "#EC NOTEXT
  CONSTANTS co_nodename_elements TYPE string VALUE 'HierarchyElements'. "#EC NOTEXT
  CONSTANTS co_nodename_element TYPE string VALUE 'HierarchyElement'. "#EC NOTEXT
  CONSTANTS co_nodename_node_texts TYPE string VALUE 'NodeTexts'. "#EC NOTEXT
  CONSTANTS co_nodename_node_text TYPE string VALUE 'NodeText'. "#EC NOTEXT
  CONSTANTS co_nodename_nodes TYPE string VALUE 'Nodes'. "#EC NOTEXT
  CONSTANTS co_nodename_node TYPE string VALUE 'Node'. "#EC NOTEXT

  METHODS get_iobj_of_hierarchy
    RETURNING
      value(return) TYPE rsiobjnm .
  METHODS get_saplink_data
    EXPORTING
      !hierarchy_catalog_entry TYPE rshiedir
      !hierarchy_descriptions TYPE rssh_t_rshiedirt
      !hierarchy_tab_ref TYPE REF TO data
      !hierarchy_intervals TYPE rssh_t_jtab
      !hierarchy_nodes TYPE rssh_t_rsmhiernode
      !hierarchy_node_texts TYPE rssh_t_rsthiernode
      !hierarchy_levels TYPE rssh_t_level .
  METHODS get_hierarchy_tab_name
    IMPORTING
      !infoobject TYPE rsiobjnm
    RETURNING
      value(return) TYPE string .
  METHODS import_hierarchy
    IMPORTING
      !hierarchy_catalog_entry TYPE rshiedir
      !hierarchy_descriptions TYPE rssh_t_rshiedirt
      !hierarchy_tab_ref TYPE REF TO data
      !hierarchy_intervals TYPE rssh_t_jtab
      !hierarchy_nodes TYPE rssh_t_rsmhiernode
      !hierarchy_node_texts TYPE rssh_t_rsthiernode
      !hierarchy_levels TYPE rssh_t_level
      !overwrite TYPE boolean
    RAISING
      zcx_saplink .
  METHODS extr_hierarchy_cat_from_ixml
    IMPORTING
      !ixml_rootnode TYPE REF TO if_ixml_element
    RETURNING
      value(return) TYPE rshiedir .
  CLASS-METHODS extract_table_elem_from_ixml
    IMPORTING
      !ixml_parentnode TYPE REF TO if_ixml_element
      !table_elem_name TYPE string
      !record_elem_name TYPE string
    CHANGING
      !data TYPE STANDARD TABLE .
  CLASS-METHODS push_table_to_ixml_elem
    IMPORTING
      !table_elem_name TYPE string
      !ixml_parentnode TYPE REF TO if_ixml_element
      !table_content TYPE STANDARD TABLE
      !record_elem_name TYPE string
      !xmldoc TYPE REF TO if_ixml_document .
  CLASS-METHODS hierarchy_auth_check
    IMPORTING
      !infoobject TYPE rsiobjnm
      !hierarchy_name TYPE rshienm
      !version TYPE rsversion
      !activity TYPE activ_auth
    RAISING
      zcx_saplink .

  METHODS deleteobject
    REDEFINITION .
  METHODS getobjecttype
    REDEFINITION .
PRIVATE SECTION.
*"* private components of class ZSAPLINK_BW_HIERARCHY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPLINK_BW_HIERARCHY IMPLEMENTATION.


METHOD checkexists.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/


  SELECT COUNT( * )
    FROM rshiedir
    WHERE hieid   EQ me->objname AND
          objvers EQ 'A'.

  IF sy-subrc EQ 0 AND
     sy-dbcnt GT 0.
    exists = abap_true.
  ELSE. "IF sy-subrc EQ 0 AND...
    exists = abap_false.
  ENDIF. "IF sy-subrc EQ 0 AND...

ENDMETHOD. "METHOD checkexists


METHOD createixmldocfromobject.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/


  DATA:
    hierarchy_catalog_entry   TYPE rshiedir,
    hierarchy_descriptions    TYPE rssh_t_rshiedirt,
    hierarchy_tab_ref         TYPE REF TO data,
    hierarchy_intervals       TYPE rssh_t_jtab,
    hierarchy_nodes           TYPE rssh_t_rsmhiernode,
    hierarchy_node_texts      TYPE rssh_t_rsthiernode,
    hierarchy_levels          TYPE rssh_t_level,

    objecttype                TYPE string,
    ixml_rootnode             TYPE REF TO if_ixml_element.


  FIELD-SYMBOLS:
    <hierarchy_tab>           TYPE STANDARD TABLE.


  me->get_saplink_data( IMPORTING hierarchy_catalog_entry = hierarchy_catalog_entry
                                  hierarchy_descriptions  = hierarchy_descriptions
                                  hierarchy_tab_ref       = hierarchy_tab_ref
                                  hierarchy_intervals     = hierarchy_intervals
                                  hierarchy_nodes         = hierarchy_nodes
                                  hierarchy_node_texts    = hierarchy_node_texts
                                  hierarchy_levels        = hierarchy_levels ).


  me->infoobject = hierarchy_catalog_entry-iobjnm.
  me->hierarchy_version = hierarchy_catalog_entry-version.
  me->hierarchy_id = hierarchy_catalog_entry-hieid.


  objecttype = me->getobjecttype( ).
  ixml_rootnode = xmldoc->create_element( objecttype ).


* store hierarchy catatlog entry in iXML
  me->setattributesfromstructure( node      = ixml_rootnode
                                  structure = hierarchy_catalog_entry ).



* store hierarchy descriptions entry in iXML
  push_table_to_ixml_elem( table_elem_name  = co_nodename_descriptions
                           record_elem_name = co_nodename_description
                           table_content    = hierarchy_descriptions
                           ixml_parentnode  = ixml_rootnode
                           xmldoc           = xmldoc ).



* store nodes and leafs of hierarchy entry in iXML
  ASSIGN hierarchy_tab_ref->* TO <hierarchy_tab>.
  push_table_to_ixml_elem( table_elem_name  = co_nodename_elements
                           record_elem_name = co_nodename_element
                           table_content    = <hierarchy_tab>
                           ixml_parentnode  = ixml_rootnode
                           xmldoc           = xmldoc ).



* store hierarchy intervals entry in iXML
  push_table_to_ixml_elem( table_elem_name  = co_nodename_intervals
                           record_elem_name = co_nodename_interval
                           table_content    = hierarchy_intervals
                           ixml_parentnode  = ixml_rootnode
                           xmldoc           = xmldoc ).



* store hierarchy nodes entry in iXML
  push_table_to_ixml_elem( table_elem_name  = co_nodename_nodes
                           record_elem_name = co_nodename_node
                           table_content    = hierarchy_nodes
                           ixml_parentnode  = ixml_rootnode
                           xmldoc           = xmldoc ).



* store texts of hierarchy nodes entry in iXML
  push_table_to_ixml_elem( table_elem_name  = co_nodename_node_texts
                           record_elem_name = co_nodename_node_text
                           table_content    = hierarchy_node_texts
                           ixml_parentnode  = ixml_rootnode
                           xmldoc           = xmldoc ).



* store hierarchy levels entry in iXML
  push_table_to_ixml_elem( table_elem_name  = co_nodename_levels
                           record_elem_name = co_nodename_level
                           table_content    = hierarchy_levels
                           ixml_parentnode  = ixml_rootnode
                           xmldoc           = xmldoc ).



  xmldoc->append_child( ixml_rootnode ).
  ixmldocument = xmldoc.

ENDMETHOD. "METHOD createixmldocfromobject


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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/


  DATA:
    hierarchy_catalog_entry   TYPE rshiedir,
    hierarchy_descriptions    TYPE rssh_t_rshiedirt,
    hierarchy_tab_ref         TYPE REF TO data,
    hierarchy_intervals       TYPE rssh_t_jtab,
    hierarchy_nodes           TYPE rssh_t_rsmhiernode,
    hierarchy_node_texts      TYPE rssh_t_rsthiernode,
    hierarchy_levels          TYPE rssh_t_level,

    hierarchy_tab_name        TYPE string,

    objecttype                TYPE string,
    ixml_rootnode             TYPE REF TO if_ixml_element.

  FIELD-SYMBOLS:
    <hierarchy_tab>           TYPE STANDARD TABLE.


  objecttype = me->getobjecttype( ).
  me->xmldoc = ixmldocument.
  ixml_rootnode = xmldoc->find_from_name( objecttype ).


* extract hierarchy catalog entry from iXML
  hierarchy_catalog_entry = me->extr_hierarchy_cat_from_ixml( ixml_rootnode ).


  me->infoobject = hierarchy_catalog_entry-iobjnm.
  me->hierarchy_version = hierarchy_catalog_entry-version.
  me->hierarchy_id = hierarchy_catalog_entry-hieid.


* extract hierarchy descriptions from iXML
  extract_table_elem_from_ixml( EXPORTING ixml_parentnode  = ixml_rootnode
                                          table_elem_name  = co_nodename_descriptions
                                          record_elem_name = co_nodename_description
                                CHANGING  data             = hierarchy_descriptions ).



* extract hierarchy intervals from iXML
  extract_table_elem_from_ixml( EXPORTING ixml_parentnode  = ixml_rootnode
                                          table_elem_name  = co_nodename_intervals
                                          record_elem_name = co_nodename_interval
                                CHANGING  data             = hierarchy_intervals ).



* extract hierarchy nodes from iXML
  extract_table_elem_from_ixml( EXPORTING ixml_parentnode  = ixml_rootnode
                                          table_elem_name  = co_nodename_nodes
                                          record_elem_name = co_nodename_node
                                CHANGING  data             = hierarchy_nodes ).



* extract texts of hierarchy nodes from iXML
  extract_table_elem_from_ixml( EXPORTING ixml_parentnode  = ixml_rootnode
                                          table_elem_name  = co_nodename_node_texts
                                          record_elem_name = co_nodename_node_text
                                CHANGING  data             = hierarchy_node_texts ).



* extract hierarchy levels from iXML
  extract_table_elem_from_ixml( EXPORTING ixml_parentnode  = ixml_rootnode
                                          table_elem_name  = co_nodename_levels
                                          record_elem_name = co_nodename_level
                                CHANGING  data             = hierarchy_levels ).



* extract hierarchy elements (nodes & leafs) from iXML
  hierarchy_tab_name = me->get_hierarchy_tab_name( hierarchy_catalog_entry-iobjnm ).
  CREATE DATA hierarchy_tab_ref TYPE STANDARD TABLE OF (hierarchy_tab_name).
  ASSIGN hierarchy_tab_ref->* TO <hierarchy_tab>.

  extract_table_elem_from_ixml( EXPORTING ixml_parentnode  = ixml_rootnode
                                          table_elem_name  = co_nodename_elements
                                          record_elem_name = co_nodename_element
                                CHANGING  data             = <hierarchy_tab> ).



* create hierarchy
  CALL METHOD me->import_hierarchy
    EXPORTING
      hierarchy_catalog_entry = hierarchy_catalog_entry
      hierarchy_descriptions  = hierarchy_descriptions
      hierarchy_tab_ref       = hierarchy_tab_ref
      hierarchy_intervals     = hierarchy_intervals
      hierarchy_nodes         = hierarchy_nodes
      hierarchy_node_texts    = hierarchy_node_texts
      hierarchy_levels        = hierarchy_levels
      overwrite               = overwrite.



  CONCATENATE 'Hierarchy'(003)
              hierarchy_catalog_entry-hienm
              'on InfoObject'(004)
              hierarchy_catalog_entry-iobjnm
    INTO name
    SEPARATED BY space.

ENDMETHOD. "METHOD createobjectfromixmldoc


METHOD deleteobject.                                         "#EC NEEDED
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/


* intentionally no code implemented

ENDMETHOD. "METHOD deleteobject


METHOD extract_table_elem_from_ixml.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  DATA:
    ixml_tablenode            TYPE REF TO if_ixml_element,

    ixml_nodelist             TYPE REF TO if_ixml_node_list,
    ixml_filter               TYPE REF TO if_ixml_node_filter,
    ixml_node_iterator        TYPE REF TO if_ixml_node_iterator,
    ixml_record_element       TYPE REF TO if_ixml_element,

    new_table_record          TYPE REF TO data,

    tabledescr                TYPE REF TO cl_abap_tabledescr,
    structdescr               TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS:
    <new_table_record>        TYPE ANY.


  tabledescr ?= cl_abap_tabledescr=>describe_by_data( data ).
  structdescr ?= tabledescr->get_table_line_type( ).
  CREATE DATA new_table_record TYPE HANDLE structdescr.
  ASSIGN new_table_record->* TO <new_table_record>.


  ixml_tablenode = ixml_parentnode->find_from_name( table_elem_name ).
  ixml_filter = ixml_tablenode->create_filter_name( record_elem_name ).
  ixml_nodelist = ixml_tablenode->get_children( ).
  ixml_node_iterator = ixml_nodelist->create_iterator_filtered( ixml_filter ).
  ixml_record_element ?= ixml_node_iterator->get_next( ).
  WHILE NOT ixml_record_element IS INITIAL.
    getstructurefromattributes( EXPORTING node      = ixml_record_element
                                CHANGING  structure = <new_table_record> ).
    INSERT <new_table_record> INTO TABLE data.

    ixml_record_element ?= ixml_node_iterator->get_next( ).
  ENDWHILE. "WHILE NOT ixml_record_element IS INITIAL

ENDMETHOD. "METHOD extract_table_elem_from_ixml


METHOD extr_hierarchy_cat_from_ixml.

  getstructurefromattributes( EXPORTING node      = ixml_rootnode
                              CHANGING  structure = return ).

ENDMETHOD. "METHOD extr_hierarchy_cat_from_ixml


METHOD getobjecttype.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  objecttype = co_objecttype.

ENDMETHOD. "METHOD getobjecttype


METHOD get_hierarchy_tab_name.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  CASE infoobject+0(1).
    WHEN '0'.
      CONCATENATE '/BI0/H' infoobject+1 INTO return.

    WHEN OTHERS.
      CONCATENATE '/BIC/H' infoobject INTO return.

  ENDCASE. "CASE infoobject+0(1)

ENDMETHOD. "METHOD get_hierarchy_tab_name


METHOD get_iobj_of_hierarchy.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  SELECT SINGLE iobjnm
    FROM rshiedir
    INTO return
    WHERE hieid   EQ me->objname AND
          objvers EQ 'A'.

ENDMETHOD. "METHOD get_iobj_of_hierarchy


METHOD get_saplink_data.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/


  DATA:
    hierarchy                 TYPE REF TO cl_rssh_hierarchy_base,
    hierarchy_id              TYPE rshieid,
    infoobject                TYPE rsiobjnm.


  infoobject = me->get_iobj_of_hierarchy( ).
  hierarchy_id = me->objname.


  CREATE OBJECT hierarchy
    EXPORTING
      i_iobjnm            = infoobject
*      i_no_htab           = rs_c_false
      i_hieid             = hierarchy_id
      i_objvers           = 'A'
*      i_s_hiesel          =
*      i_loadotherver      =
    EXCEPTIONS
      canceled            = 01
      hierarchy_not_found = 02
      OTHERS              = 99.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF. "IF sy-subrc NE 0


  hierarchy->load_hierarchy( ).



  CALL METHOD hierarchy->get_hierarchy
    IMPORTING
      e_s_hiedir    = hierarchy_catalog_entry
      e_t_hiedirt   = hierarchy_descriptions
      e_r_htab      = hierarchy_tab_ref
*      e_t_htab      =
      e_t_jtab      = hierarchy_intervals
      e_t_hiernode  = hierarchy_nodes
      e_t_hiernodet = hierarchy_node_texts
      e_t_level     = hierarchy_levels.

ENDMETHOD. "METHOD get_saplink_data


METHOD hierarchy_auth_check.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  DATA:
    error_message   TYPE string.


  CALL FUNCTION 'RSSB_AUTHORITY_ADMWB_HIERARCHY'
    EXPORTING
      i_iobjnm            = infoobject
      i_hienm             = hierarchy_name
      i_version           = version
      i_actvt             = activity
      i_try_display       = abap_false
*    IMPORTING
*      E_DISPLAY_ONLY      =
    EXCEPTIONS
      user_not_authorized = 01
      OTHERS              = 99.

  IF sy-subrc EQ 0.
*   user is athorized to create hierarchy
  ELSEIF sy-subrc EQ 01.
*   user ist not authorized to create hierarchy
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_authorized.
  ELSE.
*   another error ocurred while executng RSSB_AUTHORITY_ADMWB_HIERARCHY
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO error_message.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = error_message.
  ENDIF.

ENDMETHOD. "METHOD hierarchy_auth_check


METHOD import_hierarchy.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  TYPE-POOLS:
    rssb.

  DATA:
    hierarchy                 TYPE REF TO cl_rssh_hierarchy_base,
    hierarchy_selection       TYPE rssh_s_dfiobjhiesel,
    new_hierarchy_catalog     TYPE rshiedir,
    error_message             TYPE string,
    already_exists            TYPE boolean.

  FIELD-SYMBOLS:
    <hierarchy_tab>           TYPE STANDARD TABLE.


  MOVE-CORRESPONDING hierarchy_catalog_entry TO hierarchy_selection.

  CREATE OBJECT hierarchy
    EXPORTING
      i_iobjnm = hierarchy_catalog_entry-iobjnm.


  IF me->checkexists( ) EQ abap_true.
    IF overwrite EQ abap_true.
*     delete existing hierarchy
      already_exists = abap_true.
    ELSE. "IF overwrite EQ abap_true
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ENDIF. "IF overwrite EQ abap_true
  ELSE. "IF me->checkexists( ) EQ abap_true AND...
    already_exists = abap_false.
  ENDIF. "IF me->checkexists( ) EQ abap_true AND...


* create hierarchy
  CASE already_exists.
    WHEN abap_true.
*     check, whether user is authorized to change the hierarchy
      hierarchy_auth_check( infoobject     = hierarchy_catalog_entry-iobjnm
                            hierarchy_name = hierarchy_catalog_entry-hienm
                            version        = hierarchy_catalog_entry-version
                            activity       = rssb_c_auth_actvt-change ).

      hierarchy->create_hierarchy( i_s_hiesel = hierarchy_selection
                                   i_newone   = abap_false ).

    WHEN abap_false.
*     check, whether user is authorized to create the hierarchy
      hierarchy_auth_check( infoobject     = hierarchy_catalog_entry-iobjnm
                            hierarchy_name = hierarchy_catalog_entry-hienm
                            version        = hierarchy_catalog_entry-version
                            activity       = rssb_c_auth_actvt-create ).

      hierarchy->create_hierarchy( i_s_hiesel = hierarchy_selection
                                   i_newone   = abap_true ).
  ENDCASE. "CASE already_exists


* fill hierarchy (nodes, leafs, texts, ...)
  ASSIGN hierarchy_tab_ref->* TO <hierarchy_tab>.
  hierarchy->set_hierarchy( i_t_hiedirt   = hierarchy_descriptions
                            i_t_htab      = <hierarchy_tab>
                            i_t_jtab      = hierarchy_intervals
                            i_t_hiernodet = hierarchy_node_texts
                            i_t_level     = hierarchy_levels ).


* save hierarchy
  hierarchy->save_hierarchy( ).


** activate hierarchy
*  hierarchy->get_hierarchy( IMPORTING e_s_hiedir = new_hierarchy_catalog ).
*
*  CALL FUNCTION 'RSSH_HIERARCHY_ACTIVATE'
*    EXPORTING
*      i_hieid                = new_hierarchy_catalog-hieid
**      I_OBJVERS              = RS_C_OBJVERS-MODIFIED
*    EXCEPTIONS
*      name_error             = 01
*      iobj_not_found         = 02
*      hierarchy_not_found    = 03
*      hierarchy_active_error = 04
*      OTHERS                 = 99.
*
*  IF sy-subrc NE 0.
**   activation failed
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*      INTO error_message.
*
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING
*        textid = zcx_saplink=>error_message
*        msg    = error_message.
*  ENDIF. "IF sy-subrc NE 0


* dequeue hierarchy
  hierarchy->clear_hierarchy( i_create_new_ref = abap_false
                              i_no_dequeue     = abap_false ).

ENDMETHOD. "METHOD import_hierarchy


METHOD push_table_to_ixml_elem.

  DATA:
    ixml_tableelem    TYPE REF TO if_ixml_element,
    ixml_recordelem   TYPE REF TO if_ixml_element.

  FIELD-SYMBOLS:
    <table_record>    TYPE ANY.


  ixml_tableelem = xmldoc->create_element( table_elem_name ).
  LOOP AT table_content ASSIGNING <table_record>.
    ixml_recordelem = xmldoc->create_element( record_elem_name ).
    setattributesfromstructure( node = ixml_recordelem
                                structure = <table_record> ).
    ixml_tableelem->append_child( ixml_recordelem ).
  ENDLOOP. "LOOP AT table_content ASSIGNING <table_record>
  ixml_parentnode->append_child( ixml_tableelem ).

ENDMETHOD. "METHOD push_table_to_ixml_elem


METHOD valuehelp.
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

*/---------------------------------------------------------------------\
*|                                                                     |
*|   Author:    Mike Schernbeck                                        |
*|              mike.schernbeck@googlemail.com                         |
*|                                                                     |
*\---------------------------------------------------------------------/

  DATA:
    infoobject_of_hierarchy   TYPE rsiobjnm,
    popup_answer              TYPE string,
    selected_hierarchy        TYPE rssh_s_hiedirtxt.


* ask user for name of InfoObject
  CALL FUNCTION 'POPUP_TO_GET_VALUE'
    EXPORTING
      fieldname           = 'IOBJNM'
      tabname             = 'RSDIOBJ'
      titel               = 'On which Characteristic is the Hierarchy based on?'(001)
      valuein             = infoobject_of_hierarchy
    IMPORTING
      answer              = popup_answer
      valueout            = infoobject_of_hierarchy
    EXCEPTIONS
      fieldname_not_found = 01
      OTHERS              = 99.

  IF sy-subrc EQ 0.
    IF popup_answer NE 'C' AND    "user clicked cancel button
       NOT infoobject_of_hierarchy IS INITIAL.

      CALL FUNCTION 'RSSH_HIERARCHY_F4'
        EXPORTING
          i_iobjnm              = infoobject_of_hierarchy
*         I_T_HIETYPE           =
          i_title               = 'Select Hierarchy'(002)
        IMPORTING
*         E_S_DFTXTHIESEL       =
          e_s_hiedirtxt         = selected_hierarchy
        EXCEPTIONS
          cancelled             = 01
          OTHERS                = 99.

      IF sy-subrc EQ 0.
        e_objname = selected_hierarchy-hieid.
      ELSE. "IF sy-subrc EQ 0
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF. "IF sy-subrc EQ 0

    ENDIF. "IF popup_answer NE 'C'
  ELSE. "IF sy-subrc EQ 0
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF. "IF sy-subrc EQ 0

ENDMETHOD. "METHOD valuehelp
ENDCLASS.
