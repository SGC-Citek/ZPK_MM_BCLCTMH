CLASS zcl_mm_bclctmh DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_rap_query_provider.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MM_BCLCTMH IMPLEMENTATION.


  METHOD if_rap_query_provider~select.
    CASE io_request->get_entity_id( ).
      WHEN 'ZI_MM_BCLCTMH'.
        zcl_mm_bclctmh_manage=>get_instance( )->get_data(
            EXPORTING io_request = io_request
            IMPORTING et_data    = DATA(lt_data) ).

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_data ).
        ENDIF.
        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( lt_data ) ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
