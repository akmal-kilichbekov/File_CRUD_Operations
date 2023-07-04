CLASS zcl_file_crud_operations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_field_name,
             fieldname TYPE fieldname,
           END OF ty_field_name.

    METHODS validation
      IMPORTING iv_first_block    TYPE boolean
                iv_table_name     TYPE tabname OPTIONAL
                iv_file_name      TYPE rlgrap-filename OPTIONAL
      RETURNING VALUE(rv_message) TYPE string.

    METHODS display_records
      IMPORTING iv_table_name TYPE tabname.

    METHODS download_records
      IMPORTING iv_table_name TYPE tabname.

    METHODS download_table_format
      IMPORTING iv_table_name TYPE tabname.

    METHODS modify_records
      IMPORTING iv_file_path      TYPE rlgrap-filename
      RETURNING VALUE(rv_message) TYPE string.

    METHODS upload_records
      IMPORTING iv_file_path      TYPE rlgrap-filename
      RETURNING VALUE(rv_message) TYPE string.

    METHODS delete_records
      IMPORTING iv_file_path      TYPE rlgrap-filename
      RETURNING VALUE(rv_message) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS cv_excel_extension TYPE string VALUE 'XLS'.

    DATA mr_data       TYPE REF TO data.

    METHODS generate_dynamic_table
      IMPORTING iv_table_name    TYPE tabname
      RETURNING VALUE(ro_struct) TYPE REF TO cl_abap_tabledescr.

    METHODS generate_table_entries
      IMPORTING iv_file_path         TYPE rlgrap-filename
      RETURNING VALUE(rv_table_name) TYPE tabname.

    METHODS get_table_columns
      IMPORTING iv_table_name    TYPE tabname
      RETURNING VALUE(rt_fields) TYPE iuuc_t_ddfield.

ENDCLASS.



CLASS ZCL_FILE_CRUD_OPERATIONS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->DISPLAY_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_NAME                  TYPE        TABNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_records.
    " Data Declaration
    DATA lr_alv_object TYPE REF TO cl_salv_table.
    DATA lr_data       TYPE REF TO data.
    DATA: lr_layout    TYPE REF TO cl_salv_layout,
          ls_key       TYPE        salv_s_layout_key,
          lv_restrict  TYPE        salv_de_layout_restriction,
          lr_events    TYPE REF TO cl_salv_events_table.

    DATA: lr_content TYPE REF TO cl_salv_form_header_info,
          l_text     TYPE string.

    FIELD-SYMBOLS: <ft_any_table>  TYPE ANY TABLE.

    " Generate dynamic select
    DATA(lo_tabtype) = generate_dynamic_table( iv_table_name = iv_table_name ).

    " Create any table
    CREATE DATA lr_data TYPE HANDLE lo_tabtype.
    ASSIGN lr_data->* TO <ft_any_table>.

    " Dynamic select
    SELECT *
      FROM (iv_table_name)
      INTO CORRESPONDING FIELDS OF TABLE <ft_any_table>.


    " ALV Display
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lr_alv_object
                                CHANGING  t_table      = <ft_any_table> ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        RETURN.
    ENDTRY.

    ls_key-report = sy-repid.

    " Activate the toolbar
    lr_alv_object->get_functions( )->set_all( abap_true ).

    " Manage layout information
    lr_layout = lr_alv_object->get_layout( ).

    lr_layout->set_key( ls_key ).

    " Allow users to save layouts
    lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

    " Set content information
    lr_alv_object->set_top_of_list( NEW cl_salv_form_header_info( text    = |{ iv_table_name } table records.|
                                                                  tooltip = |{ iv_table_name } table records.| ) ).
    " Display the data
    lr_alv_object->display( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->VALIDATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FIRST_BLOCK                 TYPE        BOOLEAN
* | [--->] IV_TABLE_NAME                  TYPE        TABNAME(optional)
* | [--->] IV_FILE_NAME                   TYPE        RLGRAP-FILENAME(optional)
* | [<-()] RV_MESSAGE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validation.
    CONSTANTS: cv_first_blog_msg  TYPE string VALUE 'Please fill the input parameter',
               cv_second_blog_msg TYPE string VALUE 'Please upload the file'.

    " Check if this is first processing block and if parameters provided
    rv_message = COND #( WHEN iv_first_block = abap_true  AND iv_table_name IS INITIAL THEN cv_first_blog_msg
                         WHEN iv_first_block = abap_false AND iv_file_name  IS INITIAL THEN cv_second_blog_msg ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->DELETE_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        RLGRAP-FILENAME
* | [<-()] RV_MESSAGE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete_records.
    FIELD-SYMBOLS: <ft_any_table>  TYPE STANDARD TABLE.

    " Generate the correct table entries
    DATA(lv_table_name) = generate_table_entries( iv_file_path ).

    " Get mapped data
    ASSIGN mr_data->* TO <ft_any_table>.

    " Insert data
    DELETE (lv_table_name) FROM TABLE <ft_any_table>.
    IF sy-subrc = 0.
      rv_message = 'Entries are deleted successfully'.
    ELSE.
      rv_message = 'Entries are not deleted successfully'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->DOWNLOAD_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_NAME                  TYPE        TABNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_records.
    " Data Declaration
    DATA lr_alv_object TYPE REF TO cl_salv_table.
    DATA lr_data       TYPE REF TO data.
    DATA lv_path       TYPE string.
    DATA lv_fullpath   TYPE string.
    DATA lt_field_name TYPE STANDARD TABLE OF ty_field_name.

    FIELD-SYMBOLS: <ft_any_table>  TYPE STANDARD TABLE.

    " Generate dynamic select
    DATA(lo_tabtype) = generate_dynamic_table( iv_table_name = iv_table_name ).

    " Create any table
    CREATE DATA lr_data TYPE HANDLE lo_tabtype.
    ASSIGN lr_data->* TO <ft_any_table>.

    " Dynamic select
    SELECT *
      FROM (iv_table_name)
      INTO CORRESPONDING FIELDS OF TABLE <ft_any_table>.

    " Proces if data is not initial
    DATA(lv_filename) = |Download { iv_table_name } table records { sy-datum }|.

    " Append field names
    lt_field_name = VALUE #( FOR ls_fieldname IN get_table_columns( iv_table_name ) ( CORRESPONDING #( ls_fieldname ) ) ).

    " Get Path and file name
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title      = 'Enter File Name'
        default_extension = cv_excel_extension
        default_file_name = lv_filename
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath ).

    " Processing with Excel downloading
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = lv_filename
        write_field_separator = 'X'
      TABLES
        data_tab              = <ft_any_table>
        fieldnames            = lt_field_name.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->DOWNLOAD_TABLE_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_NAME                  TYPE        TABNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_table_format.
    DATA lv_path       TYPE string.
    DATA lv_fullpath   TYPE string.
    DATA lt_field_name TYPE STANDARD TABLE OF ty_field_name.
    DATA lr_data       TYPE REF TO data.

    FIELD-SYMBOLS: <ft_any_table>  TYPE STANDARD TABLE.

    " Generate dynamic select
    DATA(lo_tabtype) = generate_dynamic_table( iv_table_name = iv_table_name ).

    " Create any table
    CREATE DATA lr_data TYPE HANDLE lo_tabtype.
    ASSIGN lr_data->* TO <ft_any_table>.

    " Proces if data is not initial
    DATA(lv_filename) = |Download { iv_table_name } format { sy-datum }|.

    " Get Path and file name
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'Enter File Name'
        default_extension = cv_excel_extension
        default_file_name = lv_filename
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath.

    " Append field names
    lt_field_name = VALUE #( FOR ls_fieldname IN get_table_columns( iv_table_name ) ( CORRESPONDING #( ls_fieldname ) ) ).

    " Processing with Excel downloading
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = lv_filename
        write_field_separator = 'X'
      TABLES
        data_tab              = <ft_any_table>
        fieldnames            = lt_field_name.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_CRUD_OPERATIONS->GENERATE_DYNAMIC_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_NAME                  TYPE        TABNAME
* | [<-()] RO_STRUCT                      TYPE REF TO CL_ABAP_TABLEDESCR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_dynamic_table.
    DATA: lo_struct_type TYPE REF TO cl_abap_structdescr,
          lt_comp_tab    TYPE cl_abap_structdescr=>component_table.

    " Get the structure of table
    lo_struct_type ?= cl_abap_typedescr=>describe_by_name( iv_table_name ).
    lt_comp_tab  = lo_struct_type->get_components( ).

    " Create appropriate structure
    lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
    ro_struct     = cl_abap_tabledescr=>create( lo_struct_type ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_CRUD_OPERATIONS->GENERATE_TABLE_ENTRIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        RLGRAP-FILENAME
* | [<-()] RV_TABLE_NAME                  TYPE        TABNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_table_entries.
    DATA lt_excel_data TYPE STANDARD TABLE OF alsmex_tabline.
    DATA lt_fieldnames  TYPE STANDARD TABLE OF alsmex_tabline.

    FIELD-SYMBOLS: <ft_any_table>  TYPE STANDARD TABLE.

    " Upload the file
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename    = iv_file_path
        i_begin_col = 1
        i_begin_row = 1
        i_end_col   = 13
        i_end_row   = 8
      TABLES
        intern      = lt_excel_data.

    " Check if excel contains data
    CHECK lt_excel_data IS NOT INITIAL.
    SORT lt_excel_data BY row col.

    " Get the only field names
    lt_fieldnames = VALUE #( FOR ls_excel IN lt_excel_data WHERE ( row = 1 ) ( CORRESPONDING #( ls_excel ) ) ).

    DATA(lo_tabtype) = generate_dynamic_table( CONV #( lt_excel_data[ row = 1 col = 1 ]-value ) ).

    " Create any table
    CLEAR mr_data.
    CREATE DATA mr_data TYPE HANDLE lo_tabtype.
    ASSIGN mr_data->* TO <ft_any_table>.

    " Process with data mapping
    LOOP AT lt_excel_data ASSIGNING FIELD-SYMBOL(<fs_excel_data>) WHERE row > 1.
      " Assign new table data
      IF <fs_excel_data>-col = 1.
        APPEND INITIAL LINE TO <ft_any_table> ASSIGNING FIELD-SYMBOL(<fs_uploaded_entry>).
      ENDIF.

      " Map the values to the correct fields
      ASSIGN COMPONENT lt_fieldnames[ col = <fs_excel_data>-col ]-value OF STRUCTURE <fs_uploaded_entry> TO FIELD-SYMBOL(<fv_field_value>).
      CHECK sy-subrc = 0.
      <fv_field_value> = <fs_excel_data>-value.
    ENDLOOP.

    " Map the data
    CHECK <ft_any_table> IS NOT INITIAL.

    " Assign the table name
    rv_table_name = VALUE #( lt_fieldnames[ 1 ]-value OPTIONAL ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_CRUD_OPERATIONS->GET_TABLE_COLUMNS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_NAME                  TYPE        TABNAME
* | [<-()] RT_FIELDS                      TYPE        IUUC_T_DDFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_table_columns.
    " Get standard fields
    CALL FUNCTION 'DD_NAMETAB_TO_DDFIELDS'
      EXPORTING
        tabname  = iv_table_name
      TABLES
        ddfields = rt_fields.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->MODIFY_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        RLGRAP-FILENAME
* | [<-()] RV_MESSAGE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD modify_records.
    FIELD-SYMBOLS: <ft_any_table>  TYPE STANDARD TABLE.

    " Generate the correct table entries
    DATA(lv_table_name) = generate_table_entries( iv_file_path ).

    " Get mapped data
    ASSIGN mr_data->* TO <ft_any_table>.

    " Insert data
    MODIFY (lv_table_name) FROM TABLE <ft_any_table>.
    IF sy-subrc = 0.
      rv_message = 'Entries are modified successfully'.
    ELSE.
      rv_message = 'Entries are not modified successfully'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_CRUD_OPERATIONS->UPLOAD_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        RLGRAP-FILENAME
* | [<-()] RV_MESSAGE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_records.
    FIELD-SYMBOLS: <ft_any_table>  TYPE STANDARD TABLE.

    " Generate the correct table entries
    DATA(lv_table_name) = generate_table_entries( iv_file_path ).

    " Get mapped data
    ASSIGN mr_data->* TO <ft_any_table>.

    " Insert data
    INSERT (lv_table_name) FROM TABLE <ft_any_table>.
    IF sy-subrc = 0.
      rv_message = 'Entries are inserted successfully'.
    ELSE.
      rv_message = 'Entries are not inserted successfully'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
