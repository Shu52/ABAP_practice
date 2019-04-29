REPORT Z_ABPTRAINSHU_EXER55.
*****************************************************************************
*Title: Abstract class
*Author: Shu
*Date: 04/29/2019
*Purpose: I had already done the taskes of making the calculator class abstract
* inheritance. Added FINAL to subclasses/children
******************************************************************************
"Classes to catch errors in runtime
CLASS lcx_no_option_selected DEFINITION
  INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcx_out_of_range DEFINITION
  INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcx_divide_by_zero DEFINITION
  INHERITING FROM cx_static_check.
ENDCLASS.
****************************************************************************
"Parent Abstract class with constructor
CLASS lcl_cal_super DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      "Constructor imports and assigns user input
      constructor
        IMPORTING
                  im_int1 TYPE i
                  im_int2 TYPE i
                  "RAISING is used to declare the class-based exceptions
                  " exc1 exc2 ... that can be propagated from the method to the caller
                  "in this case cx_sy_arithmetic_error is a global class-based exception
        RAISING   cx_sy_arithmetic_error,
      "the returning value here is an interger that has been assigned the
      "value of the answer after calculate method is called.
      result
        RETURNING VALUE(rt_result) TYPE i.
    "Protected Section can only be addressed by children and it self
  PROTECTED SECTION.
    DATA: lv_int1   TYPE i,
          lv_int2   TYPE i,
          lv_result TYPE i.
    "Abstract method that is redefined in children classes
    METHODS:
      calculate ABSTRACT.
ENDCLASS.

CLASS lcl_cal_super IMPLEMENTATION.
  "Constructor takes user input and assigns to class variables
  METHOD constructor.

    lv_int1 = im_int1.
    lv_int2 = im_int2.

  ENDMETHOD.
  "Result calls abstract method calculate on it self
  " and assigns result to returning type
  METHOD result.

    me->calculate( ).
    rt_result = me->lv_result.

  ENDMETHOD.
ENDCLASS.
"Make internal table
*" Internal Table type referencing LCL_CAL
TYPES: BEGIN OF gy_cal,
         operation TYPE REF TO lcl_cal_super,
       END OF gy_cal.

TYPES: gy_cal_tab TYPE STANDARD TABLE OF gy_cal.

************************************************************************
*" Addition Operation Inheriting from LCL_CAL_SUPER
CLASS lcl_add DEFINITION FINAL
 INHERITING FROM lcl_cal_super.
  "Redefine calculate
  PROTECTED SECTION.
    METHODS: calculate REDEFINITION.

ENDCLASS.

CLASS lcl_add IMPLEMENTATION.
  "calculate does the calculation and assigns it to lv_result
  METHOD calculate.

    lv_result = lv_int1 + lv_int2.

  ENDMETHOD.

ENDCLASS.

"The rest of the children are more of the same
****************************************************************************
*" Subtraction Operation Inheriting from LCL_CAL_SUPER
CLASS lcl_subtract DEFINITION Final
 INHERITING FROM lcl_cal_super.

  PROTECTED SECTION.
    METHODS: calculate REDEFINITION.

ENDCLASS.

CLASS lcl_subtract IMPLEMENTATION.

  METHOD calculate.

    lv_result = lv_int1 - lv_int2.

  ENDMETHOD.

ENDCLASS.
*****************************************************************************
*" Multiplication Operation Inheriting from LCL_CAL_SUPER
CLASS lcl_multiply DEFINITION Final
 INHERITING FROM lcl_cal_super.

  PROTECTED SECTION.
    METHODS: calculate REDEFINITION.

ENDCLASS.

CLASS lcl_multiply IMPLEMENTATION.

  METHOD calculate.

    lv_result = lv_int1 * lv_int2.

  ENDMETHOD.

ENDCLASS.
**************************************************************************
*" Division Operation Inheriting from LCL_CAL_SUPER
CLASS lcl_divide DEFINITION FINAL
 INHERITING FROM lcl_cal_super.

  PROTECTED SECTION.
    METHODS: calculate REDEFINITION.

ENDCLASS.

CLASS lcl_divide IMPLEMENTATION.

  METHOD calculate.

    lv_result = lv_int1 / lv_int2.

  ENDMETHOD.

ENDCLASS.
***************************************************************************
*" Modulation Operation Inheriting from LCL_CAL_SUPER
CLASS lcl_modulo DEFINITION FINAL
 INHERITING FROM lcl_cal_super.

  PROTECTED SECTION.
    METHODS: calculate REDEFINITION.

ENDCLASS.

CLASS lcl_modulo IMPLEMENTATION.

  METHOD calculate.

    lv_result = lv_int1 MOD lv_int2.

  ENDMETHOD.

ENDCLASS.
************************************************************************
"Class to handle user input
*" Model Class returning Classes
CLASS lcl_cal_model DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
                  im_int1     TYPE i
                  im_int2     TYPE i
                  im_add      TYPE c
                  im_subtract TYPE c
                  im_multiple TYPE c
                  im_divide   TYPE c
                  im_mod      TYPE c
                  "Brings in error classes
        RAISING   lcx_no_option_selected
                  lcx_out_of_range
                  lcx_divide_by_zero.
    "Static Methods
    CLASS-METHODS
      performcalculation
        EXPORTING rt_cal TYPE gy_cal_tab
                  "Class based exception
        RAISING   cx_sy_arithmetic_error.
    "class data
  PRIVATE SECTION.
    CLASS-DATA:
      lv_int1     TYPE i,
      lv_int2     TYPE i,
      lv_add      TYPE c,
      lv_subtract TYPE c,
      lv_multiple TYPE c,
      lv_divide   TYPE c,
      lv_mod      TYPE c.
ENDCLASS.


CLASS lcl_cal_model IMPLEMENTATION.

  METHOD constructor.
    "if absolute of user input is less than 100
    IF abs( im_int1 ) > 100
    OR abs( im_int2 ) > 100.
      "call this exception
      RAISE EXCEPTION TYPE lcx_out_of_range.
      "if no operations are marked
    ELSEIF im_add = abap_false
    AND im_subtract = abap_false
    AND im_multiple = abap_false
    AND im_divide = abap_false
    AND im_mod = abap_false.
      "Call this exception
      RAISE EXCEPTION TYPE lcx_no_option_selected.
      "If 2nd value is 0 for divide or modulus
    ELSEIF ( im_divide = abap_true OR im_mod = abap_true )
    AND im_int2 = 0.
      "Raise this exception
      RAISE EXCEPTION TYPE lcx_divide_by_zero.
    ENDIF.
    "assign constructor values to class private values
    lv_int1 = im_int1.
    lv_int2 = im_int2.
    lv_add = im_add.
    lv_subtract = im_subtract.
    lv_multiple = im_multiple.
    lv_divide = im_divide.
    lv_mod = im_mod.
  ENDMETHOD.

  METHOD performcalculation.
    "This is a lot of If's and ENDIF's.
    " It is written this way to allow multi selects
    IF lv_add IS NOT INITIAL.
      "Make object type
      DATA lo_cal TYPE REF TO lcl_cal_super.

*  " Add Addition Class
      lo_cal = NEW lcl_add( im_int1 = lv_int1
                            im_int2 = lv_int2 ).
      "Add to internal table type of standard table gy_cal_tab
      " which is type of gy_cal which is type ref of lcl_cal_super
      APPEND INITIAL LINE TO rt_cal ASSIGNING FIELD-SYMBOL(<fs>).

      <fs>-operation = lo_cal.

      UNASSIGN <fs>.
      "important to clear this in each conditional.
      " lo_cal is reused several times
      CLEAR: lo_cal.
    ENDIF.

    IF lv_subtract IS NOT INITIAL.
*  " Add Subtraction Class
      lo_cal = NEW lcl_subtract( im_int1 = lv_int1
                                 im_int2 = lv_int2 ).

      APPEND INITIAL LINE TO rt_cal ASSIGNING <fs>.

      <fs>-operation = lo_cal.

      UNASSIGN <fs>.
      "Told ya
      CLEAR: lo_cal.
    ENDIF.

    IF lv_multiple IS NOT INITIAL.
*  " Add Subtraction Class
      lo_cal = NEW lcl_multiply( im_int1 = lv_int1
                                 im_int2 = lv_int2 ).

      APPEND INITIAL LINE TO rt_cal ASSIGNING <fs>.

      <fs>-operation = lo_cal.

      UNASSIGN <fs>.
      CLEAR: lo_cal.
    ENDIF.

    IF lv_divide IS NOT INITIAL.
*  " Add Subtraction Class
      lo_cal = NEW lcl_divide( im_int1 = lv_int1
                               im_int2 = lv_int2 ).

      APPEND INITIAL LINE TO rt_cal ASSIGNING <fs>.

      <fs>-operation = lo_cal.

      UNASSIGN <fs>.
      CLEAR: lo_cal.
    ENDIF.

    IF lv_mod IS NOT INITIAL.
*  " Add Subtraction Class
      lo_cal = NEW lcl_modulo( im_int1 = lv_int1
                               im_int2 = lv_int2 ).

      APPEND INITIAL LINE TO rt_cal ASSIGNING <fs>.

      <fs>-operation = lo_cal.

      UNASSIGN <fs>.
      CLEAR: lo_cal.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
*********************************************************************
CLASS lcl_cal_view DEFINITION.
  "This class makes two views. One is write statments and the other is alv grid
  PUBLIC SECTION.
    METHODS:
      "This constructor passes in two boolean values based on user input
      constructor
        IMPORTING
          im_write TYPE abap_bool
          im_alv   TYPE abap_bool.
    CLASS-METHODS
      displayresult
      "standard table gy_cal_tab
                  " which is type of gy_cal which is type ref of lcl_cal_super
        IMPORTING im_cal TYPE gy_cal_tab.

  PRIVATE SECTION.
    CLASS-DATA:
      "Local bools
      lv_write TYPE abap_bool,
      lv_alv   TYPE abap_bool.
ENDCLASS.

CLASS lcl_cal_view IMPLEMENTATION.

  METHOD constructor.
    lv_write = im_write.
    lv_alv = im_alv.
  ENDMETHOD.

  METHOD displayresult.
*    " Internal Table type referencing LCL_CAL
    TYPES: BEGIN OF ty_cal,
             result TYPE i,
           END OF ty_cal.
    "make working area?
    TYPES: ty_cal_tab TYPE STANDARD TABLE OF ty_cal.
    DATA: ly_cal TYPE ty_cal_tab.
    "loop at passed in value and append to local table
    LOOP AT im_cal
      "this field symbol is type of im_cal which is parent abstract class
      ASSIGNING FIELD-SYMBOL(<fs_cal>).
      "put into the array/table
      APPEND INITIAL LINE TO ly_cal ASSIGNING FIELD-SYMBOL(<fs_cal_tab>).
      "work area result equals parent field symbol result method.
      "lY_cal now holds result
      <fs_cal_tab>-result = <fs_cal>-operation->result( ).
    ENDLOOP.

    IF lv_write = abap_true.
      "loop at table and write results
      LOOP AT ly_cal ASSIGNING FIELD-SYMBOL(<fs>).
        WRITE: / <fs>-result.
      ENDLOOP.
    ELSEIF lv_alv = abap_true.
      "make field catalog for alv grid
      DATA: li_fieldcat TYPE slis_t_fieldcat_alv.
      "Define column name/description
      li_fieldcat = VALUE #( ( fieldname = 'Result' ) ).

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*         i_interface_check           = SPACE    " Interface consistency check log output
*         i_bypassing_buffer          = SPACE    " Ignore all buffers
*         i_buffer_active             = SPACE    " Buffering active
*         i_callback_program          = SPACE    " Name of the calling program
*         i_callback_pf_status_set    = SPACE    " Set EXIT routine to status
*         i_callback_user_command     = SPACE    " EXIT routine for command handling
*         i_callback_top_of_page      = SPACE    " EXIT routine for handling TOP-OF-PAGE
*         i_callback_html_top_of_page = SPACE    " EXIT routine for HTML TOP-OF-PAGE
*         i_callback_html_end_of_list = SPACE    " EXIT routine for HTML END-OF-LIST
*         i_structure_name            =     " Internal output table structure name
*         i_background_id             =     " Object ID of wallpaper
*         i_grid_title                =     " Control title
*         i_grid_settings             =     " Grid settings
*         is_layout   =     " List layout specifications
          it_fieldcat = li_fieldcat   " Field catalog with field descriptions
*         it_excluding                =     " Table of inactive function codes
*         it_special_groups           =     " Grouping fields for column selection
*         it_sort     =     " Sort criteria for first list display
*         it_filter   =     " Filter criteria for first list output
*         is_sel_hide =     " Selection information modification
*         i_default   = 'X'    " Initial variant active/inactive logic
*         i_save      = SPACE    " Variants can be saved
*         is_variant  =     " Variant information
*         it_events   =     " Table of events to perform
*         it_event_exit               =     " Standard fcode exit requests table
*         is_print    =     " Print information
*         is_reprep_id                =     " Initialization key for Re/Re interface
*         i_screen_start_column       = 0    " Coordinates for list in dialog box
*         i_screen_start_line         = 0    " Coordinates for list in dialog box
*         i_screen_end_column         = 0    " Coordinates for list in dialog box
*         i_screen_end_line           = 0    " Coordinates for list in dialog box
*         i_html_height_top           = 0    " HTML_TOP_OF_PAGE Height
*         i_html_height_end           = 0    " HTML_END_OF_PAGE Height
*         it_alv_graphics             =     " Parameter for ALV graphic
*         it_hyperlink                =     " Hyperlinks
*         it_add_fieldcat             =     " Additional Field Catalog Options
*         it_except_qinfo             =
*         ir_salv_fullscreen_adapter  =     " Adapter Fullscreen -> Table
*          IMPORTING
*         e_exit_caused_by_caller     =     " Delete list in CALLBACK_USER_COMMAND
*         es_exit_caused_by_user      =     " How the user left the list
        TABLES
          t_outtab    = ly_cal    " Table with data to be displayed
*          EXCEPTIONS
*         program_error               = 1
*         others      = 2
        .
      IF sy-subrc <> 0.
        "system description of error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


" Input VIEW
SELECTION-SCREEN BEGIN OF BLOCK ss01 WITH FRAME TITLE title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (18) input1 FOR FIELD p_int1.
PARAMETERS: p_int1 TYPE i.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (18) input2 FOR FIELD p_int2.
PARAMETERS: p_int2 TYPE i.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK ss01.

SELECTION-SCREEN BEGIN OF BLOCK ss02 WITH FRAME TITLE title2.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) add FOR FIELD p_add.
  PARAMETERS: p_add  AS CHECKBOX USER-COMMAND 1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) sub FOR FIELD p_minus.
  PARAMETERS: p_minus AS CHECKBOX USER-COMMAND 2.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) multi FOR FIELD p_multi.
  PARAMETERS:p_multi AS CHECKBOX USER-COMMAND 3.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) div FOR FIELD p_quot.
  PARAMETERS:p_quot AS CHECKBOX USER-COMMAND 4.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) mod FOR FIELD p_mode.
  PARAMETERS:p_mode AS CHECKBOX USER-COMMAND 5.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK ss02.

SELECTION-SCREEN BEGIN OF BLOCK ss03 WITH FRAME TITLE title3.
*PARAMETERS: p_writ RADIOBUTTON GROUP rg01,
*            p_alv  RADIOBUTTON GROUP rg01.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) writ FOR FIELD p_writ.
  PARAMETERS: p_writ RADIOBUTTON GROUP rg01.
  SELECTION-SCREEN END OF LINE.SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (18) alv FOR FIELD p_alv.
  PARAMETERS: p_alv RADIOBUTTON GROUP rg01.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK ss03.

INITIALIZATION.
  title = |Enter Positive Whole Numbers 100 or less!|.
  title2 = |Select Operations|.
  title3 = |Select View For Results|.
  input1 = |Enter First Value|.
  input2 = |Enter Second Value|.
  add = |Addition|.
  sub = |Subtraction|.
  multi = |Mulitiplication|.
  div = |Division|.
  mod = |Modulus|.
  writ = |Write Statements|.
  alv = |ALV Grid|.

START-OF-SELECTION.


  " Model Class
  TRY.
    "Create object of model and fill values.
    "If an error occurs the model assigns the exception
      DATA(go_model) = NEW lcl_cal_model(
          im_int1     = p_int1
          im_int2     = p_int2
          im_add      = p_add
          im_subtract = p_minus
          im_multiple = p_multi
          im_divide   = p_quot
          im_mod      = p_mode
      ).
    "If exception from model is this then message that
    CATCH lcx_no_option_selected.  "
      MESSAGE 'Select an operator to continue' TYPE 'I' DISPLAY LIKE 'E'.
      "return to slection screen
      LEAVE LIST-PROCESSING.
    CATCH lcx_out_of_range.  "
      MESSAGE 'Please enter number between 0 and 100' TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    CATCH lcx_divide_by_zero.  "
      MESSAGE 'Error: Division by zero' TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
  TRY.
    "call method to do the math
      go_model->performcalculation(
        IMPORTING
          rt_cal                 = DATA(lv_cal)
      ).
    "If method causes this exception then show system message
    CATCH cx_sy_arithmetic_error.    " .
      LEAVE LIST-PROCESSING.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.
  "Clear the object
  CLEAR go_model.

"Create view object and load constructor
  DATA(go_view) = NEW lcl_cal_view(
      im_write = p_writ
      im_alv   = p_alv
  ).
"pass in table and display correct view
  go_view->displayresult( im_cal = lv_cal ).
"Clear object
  CLEAR go_view.