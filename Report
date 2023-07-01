*&---------------------------------------------------------------------*
*& Report ZRP_UPLOAD_CRUD_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRP_UPLOAD_CRUD_FILE.

TABLES sscrfields.
DATA lv_message TYPE string.

* Selection Screen Block 1
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: pr_table TYPE tabname.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN PUSHBUTTON 1(22)  TEXT-003 USER-COMMAND table_format.
  SELECTION-SCREEN PUSHBUTTON 27(22) TEXT-004 USER-COMMAND view_records.
  SELECTION-SCREEN PUSHBUTTON 53(22) TEXT-005 USER-COMMAND download_record.
SELECTION-SCREEN END OF BLOCK block1.

* Selection Screen Block 2
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_upload TYPE rlgrap-filename.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN PUSHBUTTON 1(22)  TEXT-006 USER-COMMAND upload_records.
  SELECTION-SCREEN PUSHBUTTON 27(22) TEXT-007 USER-COMMAND modify_records.
  SELECTION-SCREEN PUSHBUTTON 53(22) TEXT-008 USER-COMMAND delete_records.
SELECTION-SCREEN END OF BLOCK block2.

" Value help for File upload

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upload.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_upload.

* Start-of-Selection
AT SELECTION-SCREEN.

  " Get the class instance
  CLEAR lv_message.
  DATA(lo_crud_helper) = NEW zcl_file_crud_operations( ).

  CASE sy-ucomm.
    WHEN 'TABLE_FORMAT'.
      " Initial validation logic
      lv_message = lo_crud_helper->validation( iv_first_block = abap_true
                                               iv_table_name  = pr_table ).

      " Process downloading the table format
      CHECK lv_message IS INITIAL.
      lo_crud_helper->download_table_format( pr_table ).
    WHEN 'VIEW_RECORDS'.
      " Initial validation Logic
      lv_message = lo_crud_helper->validation( iv_first_block = abap_true
                                               iv_table_name  = pr_table ).

      " Process view records
      CHECK lv_message IS INITIAL.
      lo_crud_helper->display_records( to_upper( pr_table ) ).
    WHEN 'DOWNLOAD_RECORD'.
      " Initial validation logic
      lv_message = lo_crud_helper->validation( iv_first_block = abap_true
                                               iv_table_name  = pr_table ).

      " Process downloading records
      CHECK lv_message IS INITIAL.
      lo_crud_helper->download_records( pr_table ).
    WHEN 'UPLOAD_RECORDS'.
      " Initial validation logic
      lv_message = lo_crud_helper->validation( iv_first_block = abap_false
                                               iv_file_name   = p_upload ).

      " Process uploading records
      CHECK lv_message IS INITIAL.
      MESSAGE lo_crud_helper->upload_records( p_upload ) TYPE 'I'.
    WHEN 'MODIFY_RECORDS'.
      " Process modifying records
      lv_message = lo_crud_helper->validation( iv_first_block = abap_false
                                               iv_file_name   = p_upload ).

      " Process modifying records
      CHECK lv_message IS INITIAL.
      MESSAGE lo_crud_helper->modify_records( p_upload ) TYPE 'I'.
    WHEN 'DELETE_RECORDS'.
      " Process deleting records
      lv_message = lo_crud_helper->validation( iv_first_block = abap_false
                                               iv_file_name   = p_upload ).

      " Process deleting records
      CHECK lv_message IS INITIAL.
      MESSAGE lo_crud_helper->delete_records( p_upload ) TYPE 'I'.
  ENDCASE.

  " Process validation
  CHECK lv_message IS NOT INITIAL.
  MESSAGE lv_message TYPE 'E'.
